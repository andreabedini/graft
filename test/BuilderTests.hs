{-# LANGUAGE OverloadedStrings #-}

module BuilderTests (builderProperties) where

import Control.Monad (foldM)
import Data.Char (ord)
import qualified Data.List as List
import qualified Data.Text as Text
import Graft
import qualified Graft.Build as Build
import qualified Graft.Builder as Builder
import qualified Graft.Green as Green
import Test.QuickCheck
import TestModel (halfMax, hugeDepth, overflows)

-- An independent source-ordered tree, with no caches or builder positions.
data Tree = Branch Int [Tree] | Leaf Int String
  deriving (Eq, Show)

instance Arbitrary Tree where
  arbitrary = sized genTree
  shrink (Leaf kind text) = [Leaf kind s | s <- shrink text]
  shrink (Branch kind items) = items ++ [Branch kind xs | xs <- shrinkList shrink items]

genTree :: Int -> Gen Tree
genTree size
  | size <= 0 = Leaf <$> chooseInt (0, 3) <*> genText
  | otherwise = frequency
      [ (2, genTree 0)
      , (3, do
          count <- chooseInt (0, min 4 size)
          Branch <$> chooseInt (0, 3) <*> vectorOf count (genTree (size `div` (count + 1))))
      ]
  where
    genText = chooseInt (0, 12) >>= \n -> vectorOf n
      (elements "ab09 \t\r\n\0éλ中😀\x0301\x10FFFF")

genForest :: Gen [Tree]
genForest = chooseInt (0, 5) >>= \n -> vectorOf n (resize 12 arbitrary)

source :: Tree -> String
source (Leaf _ text) = text
source (Branch _ items) = concatMap source items

byteLength :: String -> TextLength
byteLength = sum . map width
  where
    width c
      | ord c <= 0x7F = 1
      | ord c <= 0x7FF = 2
      | ord c <= 0xFFFF = 3
      | otherwise = 4

compile :: Tree -> GreenElement Int
compile (Leaf kind text) = Token (Green.token kind (Text.pack text))
compile (Branch kind items) = Node (Green.node kind (map compile items))

decode :: GreenElement Int -> Tree
decode (Token token) = Leaf (Green.tokenKind token) (Text.unpack (Green.text token))
decode (Node node) = Branch (Green.nodeKind node) (map decode (Green.children node))

cacheChecks :: GreenElement Int -> Property
cacheChecks element = conjoin $
  (Green.length element === byteLength (source (decode element))) : case element of
    Token _ -> []
    Node node -> map cacheChecks (Green.children node)

matches :: Tree -> Either BuilderError (GreenNode Int) -> Property
matches expected result = case result of
  Left err -> counterexample (show err) False
  Right node -> conjoin
    [ decode (Node node) === expected
    , Text.unpack (Green.renderNode node) === source expected
    , cacheChecks (Node node)
    ]

-- Stream start/token/finish events from the oracle rather than using Green.node.
emit :: Tree -> Builder Int -> Either BuilderError (Builder Int)
emit (Leaf kind text) builder = Right (Builder.token kind (Text.pack text) builder)
emit (Branch kind items) builder =
  foldM (flip emit) (Builder.startNode kind builder) items >>= Builder.finishNode

emitAll :: [Tree] -> Builder Int -> Either BuilderError (Builder Int)
emitAll items builder = foldM (flip emit) builder items

-- The monadic layer streams the same events with bracketed nodes.
build :: Tree -> Build Int ()
build (Leaf kind text) = Build.token kind (Text.pack text)
build (Branch kind items) = Build.node kind (mapM_ build items)

propMonadicConstruction :: Tree -> Property
propMonadicConstruction tree = conjoin
  [ matches (Branch 9 [tree]) (Build.exec (Build.node 9 (build tree)))
  , fmap fst (Build.run (Build.node 9 (build tree >> pure 'x'))) === Right 'x'
  ]

propMonadicWrapping :: Property
propMonadicWrapping = forAll ((,,) <$> genForest <*> genForest <*> genForest) $ \(prefix, middle, suffix) ->
  matches (Branch 9 (prefix ++ [Branch 8 (middle ++ suffix)])) $ Build.exec $ Build.node 9 $ do
    mapM_ build prefix
    marker <- Build.checkpoint
    mapM_ build middle
    Build.nodeAt marker 8 (mapM_ build suffix)

propMonadicBacktracking :: Property
propMonadicBacktracking = forAll ((,) <$> genForest <*> genForest) $ \(abandoned, kept) ->
  matches (Branch 9 kept) $ Build.exec $ Build.node 9 $ do
    saved <- Build.save
    mapM_ build abandoned
    Build.restore saved
    mapM_ build kept

propConstruction :: Tree -> Property
propConstruction tree = matches (Branch 9 [tree]) $ do
  builder <- emit tree (Builder.startNode 9 Builder.empty)
  Builder.finishNode builder >>= Builder.finish

propWrapping :: Property
propWrapping = forAll ((,,) <$> genForest <*> genForest <*> genForest) $ \(prefix, middle, suffix) ->
  classify (null middle) "empty initial suffix" $
  classify (not (null suffix)) "emission after wrapping starts" $
  matches (Branch 9 (prefix ++ [Branch 8 (middle ++ suffix)])) $ do
    before <- emitAll prefix (Builder.startNode 9 Builder.empty)
    let marker = Builder.checkpoint before
    pending <- emitAll middle before
    wrapped <- Builder.startNodeAt marker 8 pending
    extended <- emitAll suffix wrapped
    Builder.finishNode extended >>= Builder.finishNode >>= Builder.finish

propRepeatedWrapping :: Property
propRepeatedWrapping = forAll genForest $ \items ->
  let seed = Leaf 1 "λ"
      expected = Branch 9 [List.foldl' (\left right -> Branch 8 [left, right]) seed items]
      initial = Builder.startNode 9 Builder.empty
      marker = Builder.checkpoint initial
      wrap builder item = do
        opened <- Builder.startNodeAt marker 8 builder
        emit item opened >>= Builder.finishNode
  in matches expected $ do
    first <- emit seed initial
    folded <- foldM wrap first items
    Builder.finishNode folded >>= Builder.finish

propBranching :: Property
propBranching = forAll ((,,) <$> genForest <*> genForest <*> genForest) $ \(prefix, left, right) ->
  let saved = List.foldl' (flip (Builder.element . compile)) (Builder.startNode 9 Builder.empty) prefix
      complete items = emitAll items saved >>= Builder.finishNode >>= Builder.finish
  in conjoin
    [ matches (Branch 9 (prefix ++ left)) (complete left)
    , matches (Branch 9 (prefix ++ right)) (complete right)
    , matches (Branch 9 prefix) (Builder.finishNode saved >>= Builder.finish)
    ]

propCheckpointBounds :: Property
propCheckpointBounds = forAll (chooseInt (2, 30)) $ \count ->
  let initial = Builder.startNode 9 Builder.empty
      pending = List.foldl' (\builder _ -> Builder.token 1 "x" builder) initial [1 .. count]
      marker = Builder.checkpoint pending
      prefix = List.foldl' (\builder _ -> Builder.token 1 "x" builder)
        (Builder.empty :: Builder Int) [1 .. count]
  in case Builder.finishNode pending of
    Left err -> counterexample (show err) False
    Right collapsed -> conjoin
      [ Builder.startNodeAt marker 8 collapsed === Left (CheckpointOutOfBounds count 1)
      , Builder.startNodeAt (Builder.checkpoint Builder.empty) 8 (Builder.startNode 9 prefix)
          === Left (CheckpointBeforeCurrentNode 0 count)
      , matches (Branch 9 (replicate count (Leaf 1 "x"))) (Builder.finish collapsed)
      ]

propBalance :: Property
propBalance = forAll (chooseInt (1, 30)) $ \depth ->
  let opened = List.foldl' (\builder kind -> Builder.startNode kind builder) Builder.empty [1 .. depth]
      -- At each step an open node must be reported, even if the forest already
      -- contains one completed node. Finishing cannot implicitly close it.
      check remaining builder = conjoin
        [ Builder.finish builder === Left (UnclosedNodes remaining)
        , case Builder.finishNode builder of
            Left err -> counterexample (show err) False
            Right closed
              | remaining == 1 -> matches (foldr (\kind inner -> Branch kind [inner])
                  (Branch depth []) [1 .. depth - 1]) (Builder.finish closed)
              | otherwise -> check (remaining - 1) closed
        ]
  in check depth opened

regressions :: Property
regressions = once $ conjoin
  [ Builder.finishNode (Builder.empty :: Builder Int) === Left NoOpenNode
  , Builder.finish (Builder.empty :: Builder Int) === Left EmptyBuilder
  , Builder.finish (Builder.token 1 "" blank) === Left TokenRoot
  , Builder.finish (Builder.token 1 "a" (Builder.token 1 "b" blank)) === Left (MultipleRoots 2)
  , Builder.finish (Builder.startNode 9 (Builder.token 1 "a" blank)) === Left (UnclosedNodes 1)
  , matches (Branch 9 []) (Builder.finishNode (Builder.startNode 9 Builder.empty) >>= Builder.finish)
  , matches (Branch 9 [Leaf 1 "a", Leaf 1 "b"]) (do
      let pending = Builder.token 1 "b" (Builder.token 1 "a" Builder.empty)
      opened <- Builder.startNodeAt (Builder.checkpoint Builder.empty) 9 pending
      Builder.finishNode opened >>= Builder.finish)
  , matches (Branch 9 [Branch 8 [Leaf 1 "a"]]) (do
      let initial = Builder.startNode 9 Builder.empty
          marker = Builder.checkpoint initial
      outer <- Builder.startNodeAt marker 8 (Builder.token 1 "a" initial)
      Builder.finishNode outer >>= Builder.finishNode >>= Builder.finish)
  -- The end position may be reused to create successive empty nodes.
  , matches (Branch 9 [Branch 8 [], Branch 7 []]) (do
      let initial = Builder.startNode 9 Builder.empty
      emptyNode <- Builder.startNodeAt (Builder.checkpoint initial) 8 initial >>= Builder.finishNode
      next <- Builder.startNodeAt (Builder.checkpoint emptyNode) 7 emptyNode >>= Builder.finishNode
      Builder.finishNode next >>= Builder.finish)
  -- Markers are numeric positions, with no provenance or history validation.
  , matches (Branch 9 [Leaf 1 "a", Branch 8 [Leaf 1 "b"]]) (do
      let foreignMarker = Builder.checkpoint (Builder.token (1 :: Int) "unrelated" Builder.empty)
          initial = Builder.startNode 9 Builder.empty
          pending = Builder.token 1 "b" (Builder.token 1 "a" initial)
      opened <- Builder.startNodeAt foreignMarker 8 pending
      Builder.finishNode opened >>= Builder.finishNode >>= Builder.finish)
  , matches (Branch 7 [Branch 9 [Leaf 1 "a", Leaf 1 "b"], Leaf 1 "c", Branch 8 []]) (do
      let initial = Builder.startNode 9 Builder.empty
          pending = Builder.token 1 "b" (Builder.token 1 "a" initial)
          oldMarker = Builder.checkpoint pending
      collapsed <- Builder.finishNode pending
      let revived = Builder.token 1 "c" collapsed
      opened <- Builder.startNodeAt oldMarker 8 revived
      -- The collapsed root and new token precede the revived position.
      emptyNode <- Builder.finishNode opened
      Builder.startNodeAt (Builder.checkpoint Builder.empty) 7 emptyNode >>= Builder.finishNode >>= Builder.finish)
  -- Show prints calls that rebuild an equal builder, with open nodes placed
  -- at their start positions even when they were opened at a checkpoint.
  , either (\err -> counterexample (show err) False) (\opened -> show opened === concat
      [ "Builder.element (Token (Green.token 1 \"b\")) "
      , "(Builder.element (Token (Green.token 1 \"a\")) (Builder.startNode 8 (Builder.startNode 9 Builder.empty)))"
      ]) (do
        let initial = Builder.startNode 9 blank
        Builder.startNodeAt (Builder.checkpoint initial) 8 (Builder.token 1 "b" (Builder.token 1 "a" initial)))
  , show (Builder.empty :: Builder Int) === "Builder.empty"
  -- Reused subtrees whose lengths overflow fail when the node closes.
  , overflows (Builder.finishNode (Builder.element (halfMax (1 :: Int)) (Builder.element (halfMax (1 :: Int)) (Builder.startNode 9 Builder.empty))))
  , conjoin
      [ Green.nodeLength hugeRoot === 2 ^ hugeDepth
      , Green.nodeKind hugeRoot === 9
      , length (Green.children hugeRoot) === 1
      ]
  ]
  where
    blank :: Builder Int
    blank = Builder.empty
    -- Reuse cached huge lengths without rendering or comparing the shared DAG.
    huge :: GreenElement Int
    huge = iterate (\element -> Node (Green.node 8 [element, element]))
      (Token (Green.token 1 "x")) !! hugeDepth
    hugeRoot = case Builder.finishNode (Builder.element huge (Builder.startNode 9 Builder.empty)) >>= Builder.finish of
      Right node -> node
      Left err -> error (show err)

monadicErrors :: Property
monadicErrors = once $ conjoin
  [ Build.exec (Build.finishNode :: Build Int ()) === Left NoOpenNode
  , Build.exec (Build.startNode (9 :: Int)) === Left (UnclosedNodes 1)
  , Build.exec (pure () :: Build Int ()) === Left EmptyBuilder
  , Build.exec (Build.token (1 :: Int) "a") === Left TokenRoot
  , Build.exec (Build.node (9 :: Int) (Build.token 1 "a") >> Build.node 9 (pure ())) === Left (MultipleRoots 2)
  -- A checkpoint taken inside a node lies past the end once it closes.
  , Build.exec (Build.node (9 :: Int) (Build.token 1 "a"
      >> Build.node 8 (Build.token 1 "b" >> Build.token 1 "c" >> Build.checkpoint)
      >>= \marker -> Build.nodeAt marker 7 (pure ())))
      === Left (CheckpointOutOfBounds 3 2)
  ]

builderProperties :: [(String, Property)]
builderProperties =
  [ ("builder streamed construction, rendering and caches", property propConstruction)
  , ("builder checkpoint wrapping with later emission", propWrapping)
  , ("builder repeated checkpoint wrapping", propRepeatedWrapping)
  , ("builder persistent branches and green element reuse", propBranching)
  , ("builder checkpoint bounds and recovery", propCheckpointBounds)
  , ("builder balanced construction at every close", propBalance)
  , ("builder empty, root, checkpoint lifetime and shared-length regressions", regressions)
  , ("monadic builder construction and results", property propMonadicConstruction)
  , ("monadic builder checkpoint wrapping", propMonadicWrapping)
  , ("monadic builder saved states", propMonadicBacktracking)
  , ("monadic builder errors", monadicErrors)
  ]

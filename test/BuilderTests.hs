{-# LANGUAGE OverloadedStrings #-}

module BuilderTests (builderProperties) where

import Control.Monad (foldM)
import Data.Char (ord)
import qualified Data.List as List
import qualified Data.Text as Text
import Graft.Builder
import Graft.Green
import Test.QuickCheck

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
compile (Leaf kind text) = Token (greenToken kind (Text.pack text))
compile (Branch kind items) = Node (greenNode kind (map compile items))

decode :: GreenElement Int -> Tree
decode (Token token) = Leaf (tokenKind token) (Text.unpack (tokenText token))
decode (Node node) = Branch (nodeKind node) (map decode (nodeChildren node))

cacheChecks :: GreenElement Int -> Property
cacheChecks element = conjoin $
  (elementTextLength element === byteLength (source (decode element))) : case element of
    Token _ -> []
    Node node -> map cacheChecks (nodeChildren node)

matches :: Tree -> Either BuilderError (GreenNode Int) -> Property
matches expected result = case result of
  Left err -> counterexample (show err) False
  Right node -> conjoin
    [ decode (Node node) === expected
    , Text.unpack (renderNode node) === source expected
    , cacheChecks (Node node)
    ]

-- Stream start/token/finish events from the oracle rather than using greenNode.
emit :: Tree -> GreenNodeBuilder Int -> Either BuilderError (GreenNodeBuilder Int)
emit (Leaf kind text) builder = Right (builderToken kind (Text.pack text) builder)
emit (Branch kind items) builder =
  foldM (flip emit) (startNode kind builder) items >>= finishNode

emitAll :: [Tree] -> GreenNodeBuilder Int -> Either BuilderError (GreenNodeBuilder Int)
emitAll items builder = foldM (flip emit) builder items

propConstruction :: Tree -> Property
propConstruction tree = matches (Branch 9 [tree]) $ do
  builder <- emit tree (startNode 9 newBuilder)
  finishNode builder >>= finishBuilder

propWrapping :: Property
propWrapping = forAll ((,,) <$> genForest <*> genForest <*> genForest) $ \(prefix, middle, suffix) ->
  classify (null middle) "empty initial suffix" $
  classify (not (null suffix)) "emission after wrapping starts" $
  matches (Branch 9 (prefix ++ [Branch 8 (middle ++ suffix)])) $ do
    before <- emitAll prefix (startNode 9 newBuilder)
    let marker = checkpoint before
    pending <- emitAll middle before
    wrapped <- startNodeAt marker 8 pending
    extended <- emitAll suffix wrapped
    finishNode extended >>= finishNode >>= finishBuilder

propRepeatedWrapping :: Property
propRepeatedWrapping = forAll genForest $ \items ->
  let seed = Leaf 1 "λ"
      expected = Branch 9 [List.foldl' (\left right -> Branch 8 [left, right]) seed items]
      initial = startNode 9 newBuilder
      marker = checkpoint initial
      wrap builder item = do
        opened <- startNodeAt marker 8 builder
        emit item opened >>= finishNode
  in matches expected $ do
    first <- emit seed initial
    folded <- foldM wrap first items
    finishNode folded >>= finishBuilder

propBranching :: Property
propBranching = forAll ((,,) <$> genForest <*> genForest <*> genForest) $ \(prefix, left, right) ->
  let saved = List.foldl' (flip (builderElement . compile)) (startNode 9 newBuilder) prefix
      complete items = emitAll items saved >>= finishNode >>= finishBuilder
  in conjoin
    [ matches (Branch 9 (prefix ++ left)) (complete left)
    , matches (Branch 9 (prefix ++ right)) (complete right)
    , matches (Branch 9 prefix) (finishNode saved >>= finishBuilder)
    ]

propCheckpointBounds :: Property
propCheckpointBounds = forAll (chooseInt (2, 30)) $ \count ->
  let initial = startNode 9 newBuilder
      pending = List.foldl' (\builder _ -> builderToken 1 "x" builder) initial [1 .. count]
      marker = checkpoint pending
      prefix = List.foldl' (\builder _ -> builderToken 1 "x" builder)
        (newBuilder :: GreenNodeBuilder Int) [1 .. count]
  in case finishNode pending of
    Left err -> counterexample (show err) False
    Right collapsed -> conjoin
      [ startNodeAt marker 8 collapsed === Left (CheckpointOutOfBounds (fromIntegral count) 1)
      , startNodeAt (checkpoint newBuilder) 8 (startNode 9 prefix)
          === Left (CheckpointBeforeCurrentNode 0 (fromIntegral count))
      , matches (Branch 9 (replicate count (Leaf 1 "x"))) (finishBuilder collapsed)
      ]

propBalance :: Property
propBalance = forAll (chooseInt (1, 30)) $ \depth ->
  let opened = List.foldl' (\builder kind -> startNode kind builder) newBuilder [1 .. depth]
      -- At each step an open node must be reported, even if the forest already
      -- contains one completed node. Finishing cannot implicitly close it.
      check remaining builder = conjoin
        [ finishBuilder builder === Left (UnclosedNodes (fromIntegral remaining))
        , case finishNode builder of
            Left err -> counterexample (show err) False
            Right closed
              | remaining == 1 -> matches (foldr (\kind inner -> Branch kind [inner])
                  (Branch depth []) [1 .. depth - 1]) (finishBuilder closed)
              | otherwise -> check (remaining - 1) closed
        ]
  in check depth opened

regressions :: Property
regressions = once $ conjoin
  [ finishNode (newBuilder :: GreenNodeBuilder Int) === Left NoOpenNode
  , finishBuilder (newBuilder :: GreenNodeBuilder Int) === Left EmptyBuilder
  , finishBuilder (builderToken 1 "" emptyBuilder) === Left TokenRoot
  , finishBuilder (builderToken 1 "a" (builderToken 1 "b" emptyBuilder)) === Left (MultipleRoots 2)
  , finishBuilder (startNode 9 (builderToken 1 "a" emptyBuilder)) === Left (UnclosedNodes 1)
  , matches (Branch 9 []) (finishNode (startNode 9 newBuilder) >>= finishBuilder)
  , matches (Branch 9 [Leaf 1 "a", Leaf 1 "b"]) (do
      let pending = builderToken 1 "b" (builderToken 1 "a" newBuilder)
      opened <- startNodeAt (checkpoint newBuilder) 9 pending
      finishNode opened >>= finishBuilder)
  , matches (Branch 9 [Branch 8 [Leaf 1 "a"]]) (do
      let initial = startNode 9 newBuilder
          marker = checkpoint initial
      outer <- startNodeAt marker 8 (builderToken 1 "a" initial)
      finishNode outer >>= finishNode >>= finishBuilder)
  -- The end position may be reused to create successive empty nodes.
  , matches (Branch 9 [Branch 8 [], Branch 7 []]) (do
      let initial = startNode 9 newBuilder
      emptyNode <- startNodeAt (checkpoint initial) 8 initial >>= finishNode
      next <- startNodeAt (checkpoint emptyNode) 7 emptyNode >>= finishNode
      finishNode next >>= finishBuilder)
  -- Markers are numeric positions, with no provenance or history validation.
  , matches (Branch 9 [Leaf 1 "a", Branch 8 [Leaf 1 "b"]]) (do
      let foreignMarker = checkpoint (builderToken (1 :: Int) "unrelated" newBuilder)
          initial = startNode 9 newBuilder
          pending = builderToken 1 "b" (builderToken 1 "a" initial)
      opened <- startNodeAt foreignMarker 8 pending
      finishNode opened >>= finishNode >>= finishBuilder)
  , matches (Branch 7 [Branch 9 [Leaf 1 "a", Leaf 1 "b"], Leaf 1 "c", Branch 8 []]) (do
      let initial = startNode 9 newBuilder
          pending = builderToken 1 "b" (builderToken 1 "a" initial)
          oldMarker = checkpoint pending
      collapsed <- finishNode pending
      let revived = builderToken 1 "c" collapsed
      opened <- startNodeAt oldMarker 8 revived
      -- The collapsed root and new token precede the revived position.
      emptyNode <- finishNode opened
      startNodeAt (checkpoint newBuilder) 7 emptyNode >>= finishNode >>= finishBuilder)
  , conjoin
      [ nodeTextLength hugeRoot === 2 ^ (70 :: Int)
      , nodeKind hugeRoot === 9
      , length (nodeChildren hugeRoot) === 1
      ]
  ]
  where
    emptyBuilder :: GreenNodeBuilder Int
    emptyBuilder = newBuilder
    -- Reuse cached huge lengths without rendering or comparing the shared DAG.
    huge :: GreenElement Int
    huge = iterate (\element -> Node (greenNode 8 [element, element]))
      (Token (greenToken 1 "x")) !! 70
    hugeRoot = case finishNode (builderElement huge (startNode 9 newBuilder)) >>= finishBuilder of
      Right node -> node
      Left err -> error (show err)

builderProperties :: [(String, Property)]
builderProperties =
  [ ("builder streamed construction, rendering and caches", property propConstruction)
  , ("builder checkpoint wrapping with later emission", propWrapping)
  , ("builder repeated checkpoint wrapping", propRepeatedWrapping)
  , ("builder persistent branches and green element reuse", propBranching)
  , ("builder checkpoint bounds and recovery", propCheckpointBounds)
  , ("builder balanced construction at every close", propBalance)
  , ("builder empty, root, checkpoint lifetime and shared-length regressions", regressions)
  ]

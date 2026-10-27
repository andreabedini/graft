{-# LANGUAGE OverloadedStrings #-}

module ChildEditTests (childEditProperties) where

import Data.List (isPrefixOf)
import qualified Data.Text as Text
import Graft
import qualified Graft.Cursor as Cursor
import qualified Graft.Edit as Edit
import qualified Graft.Green as Green
import qualified Graft.Range as Range
import Test.QuickCheck
import TestModel

genForest :: Gen [Model]
genForest = chooseInt (0, 5) >>= \n -> vectorOf n (resize 12 arbitrary)

genNode :: Gen Model
genNode = Branch <$> genKind <*> genForest

-- Wrapping supplies at least one node, even when the generated tree is a leaf.
genTarget :: Bool -> Gen (Model, Path, Kind, [Model])
genTarget nonempty = do
  model <- Branch Root . (: []) <$> resize 20 arbitrary
  (path, kind, items) <- elements
    [ (path, kind, items) | (path, _, Branch kind items, _) <- contexts model
                         , not nonempty || not (null items) ]
  pure (model, path, kind, items)

modelReplace :: Path -> Model -> Model -> Model
modelReplace [] replacement _ = replacement
modelReplace (index : rest) replacement (Branch kind items) =
  Branch kind (take index items ++ [modelReplace rest replacement (items !! index)]
    ++ drop (index + 1) items)
modelReplace _ _ (Leaf _ _) = error "invalid oracle path"

matches :: Show err => Model -> Either err (GreenElement Kind) -> Property
matches expected result = case result of
  Left err -> counterexample (show err) False
  Right edited -> conjoin
    [ edited === compile expected
    , Text.unpack (Green.render edited) === source expected
    , counterexample "cache invariant" (validCaches edited)
    , conjoin
        [ case Cursor.at path (Cursor.fromGreen edited) of
            Left err -> counterexample (show (path, err)) False
            Right focus -> conjoin
              [ Cursor.green focus === compile selected
              , Cursor.range focus === Range.fromLength (byteLength prefix) (byteLength (source selected))
              ]
        | (path, prefix, selected, _) <- contexts expected
        ]
    ]

asNode :: Model -> GreenNode Kind
asNode model = case compile model of
  Node node -> node
  Token _ -> error "expected oracle node"

childCursor :: Int -> Cursor Kind -> Cursor Kind
childCursor index = either (error . show) id . Cursor.childAt index

-- The tree an edit produced, or Nothing, so that plans and path helpers
-- with different error types compare.
accepted :: Either err (GreenElement Kind) -> Maybe (GreenElement Kind)
accepted = either (const Nothing) Just

propDirectSplice :: Property
propDirectSplice = forAll genNode $ \model -> case model of
  Branch kind items -> forAll genForest $ \replacements ->
    forAll (chooseInt (0, length items)) $ \start ->
    forAll (chooseInt (start, length items)) $ \end ->
      classify (start == end) "insertion" $
      classify (null replacements && start /= end) "deletion" $
      matches (Branch kind (take start items ++ replacements ++ drop end items))
        (Node <$> Green.spliceChildren start end (map compile replacements) (asNode model))
  Leaf _ _ -> error "expected oracle node"

-- Map only untouched occurrences. Ancestors/focus change structurally, and
-- children inside the removed range have no corresponding occurrence.
mapUntouched :: Path -> Int -> Int -> Int -> Path -> Maybe Path
mapUntouched target start end replacementCount old
  | old `isPrefixOf` target = Nothing
  | target `isPrefixOf` old = case drop (length target) old of
      index : rest
        | index < start -> Just old
        | index >= end -> Just (target ++ (index + replacementCount - (end - start)) : rest)
        | otherwise -> Nothing
      [] -> Nothing
  | otherwise = Just old

propNestedSplice :: Property
propNestedSplice = forAll (genTarget False) $ \(model, path, kind, items) ->
  forAll genForest $ \replacements ->
  forAll (chooseInt (0, length items)) $ \start ->
  forAll (chooseInt (start, length items)) $ \end ->
    let root = compile model
        expected = modelReplace path (Branch kind (take start items ++ replacements ++ drop end items)) model
        edited = spliceChildrenAt path start end (map compile replacements) root
    in classify (not (null path)) "nested node" $ case Cursor.at path (Cursor.fromGreen root) of
      Left err -> counterexample (show err) False
      Right focus -> conjoin
        [ matches expected edited
        , Cursor.green (Cursor.root focus) === root
        , Cursor.path focus === path
        , Text.unpack (Green.render root) === source model
        , case edited of
            Left err -> counterexample (show err) False
            Right result -> conjoin
              [ counterexample (show (oldPath, newPath)) $
                  elementAt newPath result === Right (compile untouched)
              | (oldPath, _, untouched, _) <- contexts model
              , Just newPath <- [mapUntouched path start end (length replacements) oldPath]
              ]
        ]

propInsertion :: Property
propInsertion = forAll (genTarget False) $ \(model, path, kind, items) ->
  forAll (resize 12 arbitrary) $ \replacement ->
  forAll (chooseInt (0, length items)) $ \index ->
    let target = Branch kind items
        updated = Branch kind (take index items ++ [replacement] ++ drop index items)
        element = compile replacement
        root = compile model
        direct = Green.insertChild index element (asNode target)
        edited = insertChildAt path index element root
    in classify (index == length items) "append" $
      conjoin
        [ matches updated (Node <$> direct)
        , matches (modelReplace path updated model) edited
        , (direct >>= Green.removeChild index) === Right (asNode target)
        , (edited >>= removeChildAt path index) === Right root
          -- the same insertion anchored at a sibling, where there is one
        , case Cursor.at path (Cursor.fromGreen root) of
            Left err -> counterexample (show err) False
            Right focus ->
              let anchored
                    | index < length items = Edit.insertBefore (childCursor index focus) [element]
                    | index > 0 = Edit.insertAfter (childCursor (index - 1) focus) [element]
                    | otherwise = Edit.splice focus 0 0 [element]
              in accepted (Edit.apply root [anchored]) === accepted edited
        ]

propRemoval :: Property
propRemoval = forAll (genTarget True) $ \(model, path, kind, items) ->
  forAll (chooseInt (0, length items - 1)) $ \index ->
    let target = Branch kind items
        updated = Branch kind (take index items ++ drop (index + 1) items)
        root = compile model
        direct = Green.removeChild index (asNode target)
        edited = removeChildAt path index root
    in conjoin
      [ matches updated (Node <$> direct)
      , matches (modelReplace path updated model) edited
      , (direct >>= Green.insertChild index (compile (items !! index))) === Right (asNode target)
      , case Cursor.at path (Cursor.fromGreen root) of
          Left err -> counterexample (show err) False
          Right focus -> accepted (Edit.apply root [Edit.remove (childCursor index focus)]) === accepted edited
      ]

propNoOp :: Property
propNoOp = forAll (genTarget False) $ \(model, path, _, items) ->
  forAll (chooseInt (0, length items)) $ \start ->
  forAll (chooseInt (start, length items)) $ \end ->
    let root = compile model
        originalSlice = map compile (take (end - start) (drop start items))
    in conjoin
      [ spliceChildrenAt path start start [] root === Right root
      , spliceChildrenAt path start end originalSlice root === Right root
      ]

propInvalidBounds :: Property
propInvalidBounds = forAll (genTarget False) $ \(model, path, kind, items) ->
  let node = asNode (Branch kind items)
      count = length items
      bad = [(minBound, -1, NegativeChildBoundary minBound)
            , (0, -1, NegativeChildBoundary (-1))
            , (count + 2, count + 1, ReversedChildRange (count + 2) (count + 1))
            , (count + 1, count + 1, ChildBoundaryOutOfBounds (count + 1))
            , (count, maxBound, ChildBoundaryOutOfBounds maxBound)]
      root = compile model
  in conjoin $
    [ conjoin
        [ Green.spliceChildren start end [] node === Left err
        , spliceChildrenAt path start end [] root === Left (EditError err)
        , case Cursor.at path (Cursor.fromGreen root) of
            Left pathErr -> counterexample (show pathErr) False
            Right focus -> Edit.apply root [Edit.splice focus start end []] === Left (InvalidSplice 0 err)
        ]
    | (start, end, err) <- bad
    ] ++
    [ Green.insertChild (-1) root node === Left (NegativeChildBoundary (-1))
    , Green.removeChild (-1) node === Left (NegativeChildBoundary (-1))
    , Green.removeChild count node === Left (ChildBoundaryOutOfBounds count)
    , Green.removeChild maxBound node === Left (ChildBoundaryOutOfBounds maxBound)
    ]

propInvalidTargets :: Model -> Property
propInvalidTargets model = conjoin
  [ case selected of
      Leaf _ _ -> conjoin
        [ spliceChildrenAt path (-1) (-2) [] root === Left (EditError CannotEditTokenChildren)
        , insertChildAt path (-1) root root === Left (EditError CannotEditTokenChildren)
        , removeChildAt path (-1) root === Left (EditError CannotEditTokenChildren)
        , case Cursor.at path (Cursor.fromGreen root) of
            Left err -> counterexample (show err) False
            Right focus ->
              Edit.apply root [Edit.splice focus (-1) (-2) []] === Left (InvalidSplice 0 CannotEditTokenChildren)
        , badPath (path ++ [0]) CannotDescendIntoToken
        , badPath (path ++ [-1]) (NegativeChildIndex (-1))
        ]
      Branch _ items -> conjoin
        [ badPath (path ++ [-1]) (NegativeChildIndex (-1))
        , badPath (path ++ [length items]) (ChildIndexOutOfBounds (length items))
        ]
  | (path, _, selected, _) <- contexts model
  ]
  where
    root = compile model
    badPath path err = conjoin
      [ spliceChildrenAt path (-1) (-2) [] root === Left (ChildEditPathError err)
      , insertChildAt path (-1) root root === Left (ChildEditPathError err)
      , removeChildAt path (-1) root === Left (ChildEditPathError err)
      ]

regressions :: Property
regressions = once $ conjoin
  [ Green.insertChild 0 (tok "") empty === Right (Green.node Group [tok ""])
  , Green.removeChild 0 (Green.node Group [tok "x"]) === Right empty
  , Green.removeChild 0 empty === Left (ChildBoundaryOutOfBounds 0)
  , fmap Green.renderNode (Green.spliceChildren 1 2 [tok " + ", tok "中"] original)
      === Right "λ + 中\r\n-- keep\0"
  , fmap Green.renderNode (Green.spliceChildren 0 3 [] original) === Right ""
  , fmap Green.nodeKind (Green.spliceChildren 0 3 [] original) === Right Group
  , Green.spliceChildren 0 maxBound (error "invalid edit must not inspect replacements") empty
      === Left (ChildBoundaryOutOfBounds maxBound)
  , Green.spliceChildren maxBound maxBound [] empty === Left (ChildBoundaryOutOfBounds maxBound)
  , Green.spliceChildren minBound maxBound [] empty === Left (NegativeChildBoundary minBound)
  , conjoin
      [ Green.length hugeEdited === 2 ^ hugeDepth + 1
      , case Cursor.at [1] (Cursor.fromGreen hugeEdited) of
          Left err -> counterexample (show err) False
          Right focus -> Cursor.range focus === Range.fromLength (2 ^ hugeDepth) 1
      ]
  , -- Replacing children with as many of the same total length must still
    -- move the offsets inside the range.
    conjoin
      [ counterexample which $ case edit of
          Left err -> counterexample err False
          Right edited -> conjoin
            [ counterexample "cache invariant" (validCaches edited)
            , Green.render edited === "abc\r\n-- keep\0"
            , case Cursor.at [1] (Cursor.fromGreen edited) of
                Left err -> counterexample (show err) False
                Right focus -> Cursor.range focus === Range.fromLength 1 2
            ]
      | (which, edit) <-
          [ ("plan", either (Left . show) Right (spliceChildrenAt [] 0 2 [tok "a", tok "bc"] (Node original)))
          , ("green", either (Left . show) (Right . Node) (Green.spliceChildren 0 2 [tok "a", tok "bc"] original))
          ]
      ]
  , case Green.insertChild 0 (Node empty) (Green.node Root [tok "x"]) of
      Left err -> counterexample (show err) False
      Right node -> case Cursor.at [1] (Cursor.fromGreen (Node node)) of
        Left err -> counterexample (show err) False
        Right focus -> conjoin [Cursor.path focus === [1], Cursor.range focus === Range.fromLength 0 1]
  ]
  where
    empty = Green.node Group []
    tok = Token . Green.token Name
    original = Green.node Group [tok "λ", tok "\t", tok "\r\n-- keep\0"]
    huge = iterate (\element -> Node (Green.node Group [element, element])) (tok "x") !! hugeDepth
    hugeEdited = case Green.insertChild 1 (tok "z") (Green.node Root [huge]) of
      Right node -> Node node
      Left err -> error (show err)

childEditProperties :: [(String, Property)]
childEditProperties =
  [ ("child splicing shape, source, caches and ranges", propDirectSplice)
  , ("nested child splicing, shifted paths and persistent cursors", propNestedSplice)
  , ("child insertion and removal inverse", propInsertion)
  , ("child removal and restoration", propRemoval)
  , ("empty and identity child splices", propNoOp)
  , ("child boundary errors and precedence", propInvalidBounds)
  , ("child edit path and token-target errors", property propInvalidTargets)
  , ("child edit empty, trivia, Unicode, extreme-index and shared-length regressions", regressions)
  ]

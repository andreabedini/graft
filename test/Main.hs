{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AstTests (astProperties)
import BuilderTests (builderProperties)
import ChildEditTests (childEditProperties)
import Control.Monad (unless)
import Data.List (find, isPrefixOf)
import qualified Data.Text as Text
import Graft
import System.Exit (exitFailure)
import Test.QuickCheck
import TestModel

propRendering :: Model -> Property
propRendering model =
  Text.unpack (renderElement (compile model)) === source model

propLengths :: Model -> Property
propLengths model = conjoin
  [ counterexample (show path) $
      case elementAt path (compile model) of
        Left err -> counterexample (show err) False
        Right tree -> elementTextLength tree === byteLength (source selected)
  | (path, _, selected, _) <- contexts model
  ]

propToken :: Property
propToken = forAll genText $ \text ->
  let token = greenToken Name (Text.pack text)
  in conjoin
      [ Text.unpack (tokenText token) === text
      , renderToken token === Text.pack text
      , tokenTextLength token === byteLength text
      , tokenKind token === Name
      ]

propChunking :: Property
propChunking = forAll genText $ \text ->
  forAll (chooseInt (0, length text)) $ \n ->
    let (before, after) = splitAt n text
        leaf s = Token (greenToken Name (Text.pack s))
        split = greenNode Root [leaf before, Node (greenNode Group [leaf "", leaf after])]
        whole = greenNode Root [leaf text]
    in conjoin
        [ renderNode split === renderNode whole
        , nodeTextLength split === byteLength text
        ]

propReplacement :: Model -> Model -> Property
propReplacement model new =
  forAll (elements (contexts model)) $ \(path, prefix, selected, suffix) ->
    checkCoverage $
    cover 10 (null path) "root" $
    cover 10 (not (null path)) "non-root" $
    cover 5 (length path > 1) "nested" $
    cover 10 (isLeaf selected) "token target" $
    cover 10 (not (isLeaf selected)) "node target" $
    cover 10 (isLeaf new) "token replacement" $
    cover 10 (not (isLeaf new)) "node replacement" $
    case replaceAt path (compile new) (compile model) of
      Left err -> counterexample (show err) False
      Right edited -> conjoin
        [ Text.unpack (renderElement edited) === prefix ++ source new ++ suffix
        , elementAt path edited === Right (compile new)
        , elementTextLength edited + byteLength (source selected)
            === byteLength (source model) + byteLength (source new)
        , counterexample "all edited caches" (validCaches edited)
        , Text.unpack (renderElement (compile model)) === source model
        , conjoin
            [ counterexample ("unaffected path " ++ show other) $
                elementAt other edited === Right (compile subtree)
            | (other, _, subtree, _) <- contexts model
            , not (path `isPrefixOf` other || other `isPrefixOf` path)
            ]
        ]

propIdentity :: Model -> Property
propIdentity model = forAll (elements (contexts model)) $ \(path, _, selected, _) ->
  replaceAt path (compile selected) (compile model) === Right (compile model)

propInvalidPaths :: Model -> Model -> Property
propInvalidPaths model new =
  forAll (elements (contexts model)) $ \(path, _, selected, _) ->
    let (step, err) = case selected of
          Leaf _ _ -> (0, CannotDescendIntoToken)
          Branch _ children -> (length children, ChildIndexOutOfBounds (length children))
        invalid = path ++ [step]
        negative = path ++ [-1]
        root = compile model
    in conjoin
        [ elementAt invalid root === Left err
        , replaceAt invalid (compile new) root === Left err
        , elementAt negative root === Left (NegativeChildIndex (-1))
        , replaceAt negative (compile new) root === Left (NegativeChildIndex (-1))
        ]

propChildReplacement :: [Model] -> Model -> Property
propChildReplacement children new =
  let node = greenNode Root (map compile children)
  in forAll (chooseInt (-1, length children)) $ \i ->
      let expected
            | i < 0 = Left (NegativeChildIndex i)
            | i >= length children = Left (ChildIndexOutOfBounds i)
            | otherwise = Right (compile (Branch Root
                (take i children ++ [new] ++ drop (i + 1) children)))
      in (Node <$> replaceChild i (compile new) node) === expected

-- Cursor descriptions deliberately compare an occurrence's path and range as
-- well as its green value, so equal shared subtrees cannot be confused.
describe :: Cursor Kind -> (Path, GreenElement Kind, TextRange)
describe cursor = (cursorPath cursor, cursorGreen cursor, cursorRange cursor)

modelOccurrences :: Model -> [(Path, GreenElement Kind, TextRange)]
modelOccurrences model =
  [ (path, compile selected, rangeFromLength (byteLength prefix) (byteLength (source selected)))
  | (path, prefix, selected, _) <- contexts model
  ]

isLeaf :: Model -> Bool
isLeaf (Leaf _ _) = True
isLeaf _ = False

propCursorOccurrences :: Model -> Property
propCursorOccurrences model =
  let cursor = rootCursor (compile model)
      expected = modelOccurrences model
      leafPaths = [path | (path, _, selected, _) <- contexts model, isLeaf selected]
  in conjoin
      [ map describe (cursorDescendants cursor) === expected
      , map cursorPath (cursorTokens cursor) === leafPaths
      , conjoin
          [ fmap describe (cursorAt path cursor) === Right occurrence
          | occurrence@(path, _, _) <- expected
          ]
      ]

propCursorNavigation :: Model -> Property
propCursorNavigation model = forAll (elements (contexts model)) $ \(path, _, selected, _) ->
  checkCoverage $
  cover 10 (null path) "root" $
  cover 10 (not (null path)) "non-root" $
  cover 5 (length path > 1) "nested" $
  cover 5 (null (source selected)) "empty focus" $
  case cursorAt path modelRoot of
    Left err -> counterexample (show err) False
    Right focus ->
      let expectedChildren =
            [ occurrence
            | occurrence@(other, _, _) <- modelOccurrences model
            , length other == length path + 1, path `isPrefixOf` other
            ]
          expectedParent = case reverse path of
            [] -> Nothing
            _ : rest -> find (\(other, _, _) -> other == reverse rest) (modelOccurrences model)
          expectedSibling delta = case reverse path of
            [] -> Nothing
            index : rest -> find (\(other, _, _) -> other == reverse rest ++ [index + delta])
              (modelOccurrences model)
          maybeFirst [] = Nothing
          maybeFirst (value : _) = Just value
      in conjoin
          [ describe (cursorRoot focus) === describe modelRoot
          , map cursorPath (cursorAncestors focus) === [take n path | n <- reverse [0 .. length path]]
          , fmap describe (cursorParent focus) === expectedParent
          , map describe (cursorChildren focus) === expectedChildren
          , fmap describe (cursorFirstChild focus) === maybeFirst expectedChildren
          , fmap describe (cursorLastChild focus) === maybeFirst (reverse expectedChildren)
          , fmap describe (cursorNextSibling focus) === expectedSibling 1
          , fmap describe (cursorPreviousSibling focus) === expectedSibling (-1)
          , fmap describe (cursorNextSibling focus >>= cursorPreviousSibling)
              === (describe focus <$ cursorNextSibling focus)
          , fmap describe (cursorPreviousSibling focus >>= cursorNextSibling)
              === (describe focus <$ cursorPreviousSibling focus)
          , case selected of
              Leaf _ _ -> fmap cursorGreen (cursorChildAt 0 focus) === Left CannotDescendIntoToken
              Branch _ kids -> conjoin
                [ fmap describe (cursorChildAt i focus) === Right occurrence
                | (i, occurrence) <- zip [0 .. length kids - 1] expectedChildren
                ]
          , fmap cursorGreen (cursorChildAt (-1) focus) === Left (NegativeChildIndex (-1))
          , conjoin
              [ fmap cursorGreen (cursorAt relative focus)
                  === elementAt relative (compile selected)
              | (relative, _, _, _) <- contexts selected
              ]
          ]
  where
    modelRoot = rootCursor (compile model)

propCursorTokenNavigation :: Model -> Property
propCursorTokenNavigation model =
  forAll (elements (contexts model)) $ \(path, _, _, _) ->
    case cursorAt path (rootCursor (compile model)) of
      Left err -> counterexample (show err) False
      Right focus ->
        let leafPaths = [other | (other, _, selected, _) <- contexts model, isLeaf selected]
            inside = filter (path `isPrefixOf`) leafPaths
            after = filter (\other -> other > path && not (path `isPrefixOf` other)) leafPaths
            before = filter (< path) leafPaths
            maybeHead [] = Nothing
            maybeHead (value : _) = Just value
        in conjoin
            [ map cursorPath (cursorTokens focus) === inside
            , fmap cursorPath (cursorFirstToken focus) === maybeHead inside
            , fmap cursorPath (cursorLastToken focus) === maybeHead (reverse inside)
            , fmap cursorPath (cursorNextToken focus) === maybeHead after
            , fmap cursorPath (cursorPreviousToken focus) === maybeHead (reverse before)
            ]

propCursorReplacement :: Model -> Model -> Property
propCursorReplacement model replacement =
  forAll (elements (contexts model)) $ \(path, prefix, selected, suffix) ->
    case cursorAt path (rootCursor (compile model)) of
      Left err -> counterexample (show err) False
      Right focus ->
        let edited = replaceFocus (compile replacement) focus
            expectedRange = rangeFromLength (byteLength prefix) (byteLength (source replacement))
        in conjoin
            [ Text.unpack (renderElement edited) === prefix ++ source replacement ++ suffix
            , counterexample "rebuilt caches" (validCaches edited)
            , fmap cursorRange (cursorAt path (rootCursor edited)) === Right expectedRange
            , Text.unpack (renderElement (cursorGreen focus)) === source selected
            , cursorGreen (cursorRoot focus) === compile model
            , conjoin
                [ fmap describe (cursorAt other (rootCursor edited))
                    === Right (other, compile subtree,
                      rangeFromLength expectedStart (byteLength (source subtree)))
                | (other, otherPrefix, subtree, _) <- contexts model
                , not (path `isPrefixOf` other || other `isPrefixOf` path)
                , let shift = if other > path
                        then byteLength (source replacement) - byteLength (source selected)
                        else 0
                , let expectedStart = byteLength otherPrefix + shift
                ]
            ]

propCursorInvalidPaths :: Model -> Property
propCursorInvalidPaths model = forAll (elements (contexts model)) $ \(path, _, selected, _) ->
  let (index, err) = case selected of
        Leaf _ _ -> (0, CannotDescendIntoToken)
        Branch _ kids -> (length kids, ChildIndexOutOfBounds (length kids))
      cursor = rootCursor (compile model)
  in conjoin
      [ fmap cursorGreen (cursorAt (path ++ [index]) cursor) === Left err
      , fmap cursorGreen (cursorAt (path ++ [-1]) cursor) === Left (NegativeChildIndex (-1))
      ]

offsetPaths :: TokenAtOffset (Cursor Kind) -> TokenAtOffset Path
offsetPaths NoToken = NoToken
offsetPaths (SingleToken cursor) = SingleToken (cursorPath cursor)
offsetPaths (BetweenTokens left right) = BetweenTokens (cursorPath left) (cursorPath right)

propCursorOffsetLookup :: Model -> Property
propCursorOffsetLookup model =
  forAll (elements (contexts model)) $ \(scope, _, _, _) ->
  forAll (chooseInteger (0, toInteger (byteLength (source model)) + 1)) $ \rawOffset ->
    let offset = fromInteger rawOffset
        touching =
          [ path
          | (path, prefix, Leaf _ text, _) <- contexts model
          , scope `isPrefixOf` path
          , not (null text)
          , byteLength prefix <= offset
          , offset <= byteLength prefix + byteLength text
          ]
        expected = case touching of
          [] -> NoToken
          [path] -> SingleToken path
          left : right : _ -> BetweenTokens left right
    in case cursorAt scope (rootCursor (compile model)) of
        Left err -> counterexample (show err) False
        Right focus -> offsetPaths (tokenAtOffset offset focus) === expected

propCoveringRange :: Model -> Property
propCoveringRange model =
  forAll (chooseInteger (0, toInteger len + 1)) $ \a ->
  forAll (chooseInteger (0, toInteger len + 1)) $ \b ->
    let start = fromInteger (min a b)
        end = fromInteger (max a b)
        query = rangeFromLength start (end - start)
        containing =
          [ path
          | (path, prefix, selected, _) <- contexts model
          , byteLength prefix <= start
          , end <= byteLength prefix + byteLength (source selected)
          ]
        -- Lexicographic maximum under the leftmost covering top-level branch:
        -- retain an initial chain only, never jump to an ambiguous later sibling.
        expected = case containing of
          [] -> Nothing
          first : rest -> Just (foldl descend first rest)
        descend current candidate
          | current `isPrefixOf` candidate = candidate
          | otherwise = current
    in fmap cursorPath (coveringElement query (rootCursor (compile model))) === expected
  where
    len = byteLength (source model)

propRanges :: NonNegative Integer -> NonNegative Integer -> NonNegative Integer -> Property
propRanges (NonNegative rawStart) (NonNegative rawLen) (NonNegative rawPoint) =
  let start = fromInteger rawStart
      len = fromInteger rawLen
      point = fromInteger rawPoint
      range = rangeFromLength start len
  in conjoin
      [ rangeStart range === start
      , rangeEnd range === start + len
      , rangeLength range === len
      , rangeIsEmpty range === (len == 0)
      , textRange start (start + len) === Just range
      , rangeContains point range === (start <= point && point < start + len)
      , rangeContainsRange range (rangeFromLength (start + len) 0) === True
      , if len > 0 then textRange (start + len) start === Nothing else property True
      ]

cursorRegressions :: Property
cursorRegressions = once $ conjoin
  [ map cursorPath (cursorTokens tree) === [[1, 0], [1, 1], [2], [3, 0], [3, 1], [5], [6]]
  , fmap cursorPath (cursorFirstToken tree) === Just [1, 0]
  , fmap cursorPath (cursorLastToken tree) === Just [6]
  , fmap cursorPath (cursorFirstChild tree >>= cursorNextToken) === Just [1, 0]
  , fmap cursorPath (cursorLastChild tree >>= cursorPreviousToken) === Just [5]
  , atPath [1, 1] (fmap cursorPath . cursorNextToken) (Just [2])
  , atPath [2] (fmap cursorPath . cursorNextToken) (Just [3, 0])
  , atPath [5] (fmap cursorPath . cursorPreviousToken) (Just [3, 1])
  , atPath [3] (fmap cursorPath . cursorPreviousToken) (Just [2])
  , atPath [3] (fmap cursorPath . cursorNextToken) (Just [5])
  , atPath [3, 0] cursorRange (rangeFromLength 1 4)
  , offsetPaths (tokenAtOffset 0 tree) === SingleToken [1, 1]
  , offsetPaths (tokenAtOffset 1 tree) === BetweenTokens [1, 1] [3, 0]
  , offsetPaths (tokenAtOffset 3 tree) === SingleToken [3, 0]
  , offsetPaths (tokenAtOffset 5 tree) === BetweenTokens [3, 0] [5]
  , offsetPaths (tokenAtOffset 6 tree) === SingleToken [5]
  , offsetPaths (tokenAtOffset 7 tree) === NoToken
  , atPath [3] (offsetPaths . tokenAtOffset 1) (SingleToken [3, 0])
  , atPath [3] (offsetPaths . tokenAtOffset 0) NoToken
  , fmap cursorPath (coveringElement (rangeFromLength 0 0) tree) === Just [0]
  , fmap cursorPath (coveringElement (rangeFromLength 1 0) tree) === Just [1, 1]
  , fmap cursorPath (coveringElement (rangeFromLength 6 0) tree) === Just [5]
  , fmap cursorPath (coveringElement (rangeFromLength 1 4) tree) === Just [3, 0]
  , fmap cursorPath (coveringElement (rangeFromLength 0 5) tree) === Just []
  , fmap cursorPath (coveringElement (rangeFromLength 6 1) tree) === Nothing
  , tokensEmpty (rootCursor (Node (greenNode Group [])))
  , offsetPaths (tokenAtOffset 0 (rootCursor (tok ""))) === NoToken
  , fmap cursorPath (cursorFirstToken (rootCursor (tok ""))) === Just []
  , cursorRange sharedSecond === rangeFromLength 1 1
  , cursorPath sharedSecond === [1]
  , cursorGreen sharedSecond === shared
  , renderElement (replaceFocus (tok "long") sharedSecond) === "along"
  , cursorRange sharedSecond === rangeFromLength 1 1
  , rangeEnd (cursorRange hugeCursor) === 2 ^ (40 :: Int)
  , offsetPaths (tokenAtOffset (2 ^ (40 :: Int)) hugeCursor)
      === SingleToken (replicate 40 1)
  ]
  where
    tok = Token . greenToken Name
    empty = Node (greenNode Group [])
    tree = rootCursor (Node (greenNode Root
      [ empty, Node (greenNode Group [tok "", tok "a", empty]), tok ""
      , Node (greenNode Group [tok "😀", tok ""]), empty, tok "b", tok ""
      ]))
    atPath path inspect expected = case cursorAt path tree of
      Left err -> counterexample (show err) False
      Right focus -> inspect focus === expected
    tokensEmpty cursor = conjoin
      [ map cursorPath (cursorTokens cursor) === []
      , fmap cursorPath (cursorFirstToken cursor) === Nothing
      , fmap cursorPath (cursorLastToken cursor) === Nothing
      , offsetPaths (tokenAtOffset 0 cursor) === NoToken
      ]
    shared = Node (greenNode Group [tok "a"])
    sharedSecond = case cursorChildAt 1 (rootCursor (Node (greenNode Root [shared, shared]))) of
      Right cursor -> cursor
      Left err -> error (show err)
    hugeCursor = rootCursor (iterate (\child -> Node (greenNode Group [child, child])) (tok "x") !! 40)

regressions :: Property
regressions = once $ conjoin
  [ renderNode empty === ""
  , nodeTextLength empty === 0
  , renderNode original === "x\t{- keep -}\r\n😀e\x0301\0"
  , nodeTextLength original === 22
  , fmap renderNode (replaceChild 0 (tok "y") original)
      === Right "y\t{- keep -}\r\n😀e\x0301\0"
  , elementTextLength huge === 2 ^ (40 :: Int)
  , nodeKind original === Root
  , elementKind (tok "x") === Name
  , fmap renderElement (replaceAt [] (tok "new") (Node original)) === Right "new"
  , replaceChild maxBound (tok "x") empty === Left (ChildIndexOutOfBounds maxBound)
  ]
  where
    empty = greenNode Root []
    tok = Token . greenToken Name
    original = greenNode Root
      [ tok "x"
      , Token (greenToken Space "\t")
      , Token (greenToken Comment "{- keep -}")
      , Node (greenNode Group [tok "\r\n", tok "😀e\x0301\0", tok ""])
      ]
    -- A tiny shared DAG with a logical size above Word64. Never render it.
    huge = iterate (\child -> Node (greenNode Group [child, child])) (tok "x") !! 40

main :: IO ()
main = do
  results <- sequence $
    [ run "lossless rendering" propRendering
    , run "cached UTF-8 lengths at every path" propLengths
    , run "token round-trip and byte length" propToken
    , run "independent token boundaries" propChunking
    , run "replacement source, caches and unaffected branches" propReplacement
    , run "identity replacement" propIdentity
    , run "invalid paths" propInvalidPaths
    , run "immediate child replacement" propChildReplacement
    , run "empty, trivia, Unicode and unbounded-length regressions" regressions
    , run "cursor occurrences and source ranges" propCursorOccurrences
    , run "cursor parent, child and sibling navigation" propCursorNavigation
    , run "cursor token navigation" propCursorTokenNavigation
    , run "persistent cursor replacement and shifted ranges" propCursorReplacement
    , run "cursor invalid paths" propCursorInvalidPaths
    , run "boundary-aware token lookup" propCursorOffsetLookup
    , run "covering range lookup" propCoveringRange
    , run "half-open range invariants" propRanges
    , run "cursor empty, boundary, shared-occurrence and large-range regressions" cursorRegressions
    ] ++ [run title prop | (title, prop) <- builderProperties ++ childEditProperties ++ astProperties]
  unless (all isSuccess results) exitFailure
  where
    run title prop = do
      putStrLn title
      quickCheckWithResult stdArgs {maxSuccess = 1000} prop

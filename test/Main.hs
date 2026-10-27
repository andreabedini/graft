{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AstTests (astProperties)
import BuilderTests (builderProperties)
import ChildEditTests (childEditProperties)
import CursorTests (cursorProperties)
import EditTests (editProperties)
import FoldTests (foldProperties)
import Control.Monad (unless)
import Data.Bits (finiteBitSize)
import Data.Hashable (hash)
import Data.List (find, isPrefixOf)
import qualified Data.Text as Text
import Graft
import qualified Graft.Cursor as Cursor
import qualified Graft.Edit as Edit
import qualified Graft.Green as Green
import qualified Graft.Range as Range
import LineIndexTests (lineIndexProperties)
import System.Exit (exitFailure)
import Test.QuickCheck
import TestModel

propRendering :: Model -> Property
propRendering model =
  Text.unpack (Green.render (compile model)) === source model

propLengths :: Model -> Property
propLengths model = conjoin
  [ counterexample (show path) $
      case elementAt path (compile model) of
        Left err -> counterexample (show err) False
        Right tree -> Green.length tree === byteLength (source selected)
  | (path, _, selected, _) <- contexts model
  ]

propToken :: Property
propToken = forAll genText $ \text ->
  let token = Green.token Name (Text.pack text)
  in conjoin
      [ Text.unpack (Green.text token) === text
      , Green.text token === Text.pack text
      , Green.tokenLength token === byteLength text
      , Green.tokenKind token === Name
      ]

propChunking :: Property
propChunking = forAll genText $ \text ->
  forAll (chooseInt (0, length text)) $ \n ->
    let (before, after) = splitAt n text
        leaf s = Token (Green.token Name (Text.pack s))
        split = Green.node Root [leaf before, Node (Green.node Group [leaf "", leaf after])]
        whole = Green.node Root [leaf text]
    in conjoin
        [ Green.renderNode split === Green.renderNode whole
        , Green.nodeLength split === byteLength text
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
        [ Text.unpack (Green.render edited) === prefix ++ source new ++ suffix
        , elementAt path edited === Right (compile new)
        , Green.length edited + byteLength (source selected)
            === byteLength (source model) + byteLength (source new)
        , counterexample "all edited caches" (validCaches edited)
        , Text.unpack (Green.render (compile model)) === source model
        , conjoin
            [ counterexample ("unaffected path " ++ show other) $
                elementAt other edited === Right (compile subtree)
            | (other, _, subtree, _) <- contexts model
            , not (path `isPrefixOf` other || other `isPrefixOf` path)
            ]
        ]

-- Mapping kinds keeps text and caches; equal trees built separately hash alike.
propKindsAndHashes :: Model -> Property
propKindsAndHashes model =
  let tree = compile model
      next kind = if kind == maxBound then minBound else succ kind
  in conjoin
      [ fmap id tree === tree
      , fmap (next . next) tree === fmap next (fmap next tree)
      , Green.render (fmap fromEnum tree) === Green.render tree
      , Green.length (fmap next tree) === Green.length tree
      , Green.kind (fmap next tree) === next (Green.kind tree)
      , counterexample "mapped caches" (validCaches (fmap next tree))
      , hash (fmap fromEnum tree) === hash (fmap fromEnum (compile model))
      , hash (Cursor.range (Cursor.fromGreen tree)) === hash (Range.fromLength 0 (byteLength (source model)))
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
  let node = Green.node Root (map compile children)
  in forAll (chooseInt (-1, length children)) $ \i ->
      let expected
            | i < 0 = Left (NegativeChildBoundary i)
            | i >= length children = Left (ChildBoundaryOutOfBounds i)
            | otherwise = Right (compile (Branch Root
                (take i children ++ [new] ++ drop (i + 1) children)))
      in (Node <$> Green.replaceChild i (compile new) node) === expected

-- Cursor descriptions deliberately compare an occurrence's path and range as
-- well as its green value, so equal shared subtrees cannot be confused.
describe :: Cursor Kind -> (Path, GreenElement Kind, TextRange)
describe cursor = (Cursor.path cursor, Cursor.green cursor, Cursor.range cursor)

modelOccurrences :: Model -> [(Path, GreenElement Kind, TextRange)]
modelOccurrences model =
  [ (path, compile selected, Range.fromLength (byteLength prefix) (byteLength (source selected)))
  | (path, prefix, selected, _) <- contexts model
  ]

isLeaf :: Model -> Bool
isLeaf (Leaf _ _) = True
isLeaf _ = False

propCursorOccurrences :: Model -> Property
propCursorOccurrences model =
  let cursor = Cursor.fromGreen (compile model)
      expected = modelOccurrences model
      leafPaths = [path | (path, _, selected, _) <- contexts model, isLeaf selected]
  in conjoin
      [ map describe (Cursor.descendants cursor) === expected
      , map Cursor.path (Cursor.descendantTokens cursor) === leafPaths
      , conjoin
          [ fmap describe (Cursor.at path cursor) === Right occurrence
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
  case Cursor.at path modelRoot of
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
          [ describe (Cursor.root focus) === describe modelRoot
          , map Cursor.path (Cursor.ancestors focus) === [take n path | n <- reverse [0 .. length path - 1]]
          , fmap describe (Cursor.parent focus) === expectedParent
          , map describe (Cursor.children focus) === expectedChildren
          , fmap describe (Cursor.firstChild focus) === maybeFirst expectedChildren
          , fmap describe (Cursor.lastChild focus) === maybeFirst (reverse expectedChildren)
          , fmap describe (Cursor.nextSibling focus) === expectedSibling 1
          , fmap describe (Cursor.previousSibling focus) === expectedSibling (-1)
          , fmap describe (Cursor.nextSibling focus >>= Cursor.previousSibling)
              === (describe focus <$ Cursor.nextSibling focus)
          , fmap describe (Cursor.previousSibling focus >>= Cursor.nextSibling)
              === (describe focus <$ Cursor.previousSibling focus)
          , case selected of
              Leaf _ _ -> fmap Cursor.green (Cursor.childAt 0 focus) === Left CannotDescendIntoToken
              Branch _ kids -> conjoin
                [ fmap describe (Cursor.childAt i focus) === Right occurrence
                | (i, occurrence) <- zip [0 .. length kids - 1] expectedChildren
                ]
          , fmap Cursor.green (Cursor.childAt (-1) focus) === Left (NegativeChildIndex (-1))
          , conjoin
              [ fmap Cursor.green (Cursor.at relative focus)
                  === elementAt relative (compile selected)
              | (relative, _, _, _) <- contexts selected
              ]
          ]
  where
    modelRoot = Cursor.fromGreen (compile model)

propCursorTokenNavigation :: Model -> Property
propCursorTokenNavigation model =
  forAll (elements (contexts model)) $ \(path, _, _, _) ->
    case Cursor.at path (Cursor.fromGreen (compile model)) of
      Left err -> counterexample (show err) False
      Right focus ->
        let leafPaths = [other | (other, _, selected, _) <- contexts model, isLeaf selected]
            inside = filter (path `isPrefixOf`) leafPaths
            after = filter (\other -> other > path && not (path `isPrefixOf` other)) leafPaths
            before = filter (< path) leafPaths
            maybeHead [] = Nothing
            maybeHead (value : _) = Just value
        in conjoin
            [ map Cursor.path (Cursor.descendantTokens focus) === inside
            , fmap Cursor.path (Cursor.firstToken focus) === maybeHead inside
            , fmap Cursor.path (Cursor.lastToken focus) === maybeHead (reverse inside)
            , fmap Cursor.path (Cursor.nextToken focus) === maybeHead after
            , fmap Cursor.path (Cursor.previousToken focus) === maybeHead (reverse before)
            ]

propCursorWalk :: Model -> Property
propCursorWalk model = conjoin
  [ map (fmap Cursor.path) (Cursor.walk (Cursor.fromGreen (compile model))) === walk [] model
  , conjoin
      [ fmap (Text.unpack . Cursor.text) (Cursor.at path (Cursor.fromGreen (compile model))) === Right (source selected)
      | (path, _, selected, _) <- contexts model
      ]
  ]
  where
    walk path (Leaf _ _) = [Enter path, Leave path]
    walk path (Branch _ children) =
      Enter path : concat [walk (path ++ [i]) child | (i, child) <- zip [0 ..] children] ++ [Leave path]

-- Replacing through a cursor keeps the rest of the tree, shifts the ranges
-- after the edit, and leaves the cursor on the original.
propCursorReplacement :: Model -> Model -> Property
propCursorReplacement model replacement =
  forAll (elements (contexts model)) $ \(path, prefix, selected, suffix) ->
    case Cursor.at path (Cursor.fromGreen root) of
      Left err -> counterexample (show err) False
      Right focus ->
        let edited = applied root [Edit.replace focus (compile replacement)]
            expectedRange = Range.fromLength (byteLength prefix) (byteLength (source replacement))
        in conjoin
            [ Text.unpack (Green.render edited) === prefix ++ source replacement ++ suffix
            , counterexample "rebuilt caches" (validCaches edited)
            , fmap Cursor.range (Cursor.at path (Cursor.fromGreen edited)) === Right expectedRange
            , Text.unpack (Green.render (Cursor.green focus)) === source selected
            , Cursor.tree focus === root
            , conjoin
                [ fmap describe (Cursor.at other (Cursor.fromGreen edited))
                    === Right (other, compile subtree,
                      Range.fromLength expectedStart (byteLength (source subtree)))
                | (other, otherPrefix, subtree, _) <- contexts model
                , not (path `isPrefixOf` other || other `isPrefixOf` path)
                , let shift = if other > path
                        then byteLength (source replacement) - byteLength (source selected)
                        else 0
                , let expectedStart = byteLength otherPrefix + shift
                ]
            ]
  where
    root = compile model

propCursorInvalidPaths :: Model -> Property
propCursorInvalidPaths model = forAll (elements (contexts model)) $ \(path, _, selected, _) ->
  let (index, err) = case selected of
        Leaf _ _ -> (0, CannotDescendIntoToken)
        Branch _ kids -> (length kids, ChildIndexOutOfBounds (length kids))
      cursor = Cursor.fromGreen (compile model)
  in conjoin
      [ fmap Cursor.green (Cursor.at (path ++ [index]) cursor) === Left err
      , fmap Cursor.green (Cursor.at (path ++ [-1]) cursor) === Left (NegativeChildIndex (-1))
      ]

offsetPaths :: TokenAtOffset (Cursor Kind) -> TokenAtOffset Path
offsetPaths NoToken = NoToken
offsetPaths (SingleToken cursor) = SingleToken (Cursor.path cursor)
offsetPaths (BetweenTokens left right) = BetweenTokens (Cursor.path left) (Cursor.path right)

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
    in case Cursor.at scope (Cursor.fromGreen (compile model)) of
        Left err -> counterexample (show err) False
        Right focus -> offsetPaths (Cursor.tokenAt offset focus) === expected

propCoveringRange :: Model -> Property
propCoveringRange model =
  forAll (chooseInteger (0, toInteger len + 1)) $ \a ->
  forAll (chooseInteger (0, toInteger len + 1)) $ \b ->
    let start = fromInteger (min a b)
        end = fromInteger (max a b)
        query = Range.fromLength start (end - start)
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
    in fmap Cursor.path (Cursor.covering query (Cursor.fromGreen (compile model))) === expected
  where
    len = byteLength (source model)

propRanges :: NonNegative Integer -> NonNegative Integer -> NonNegative Integer -> Property
propRanges (NonNegative rawStart) (NonNegative rawLen) (NonNegative rawPoint) =
  let start = fromInteger rawStart
      len = fromInteger rawLen
      point = fromInteger rawPoint
      range = Range.fromLength start len
  in conjoin
      [ Range.start range === start
      , Range.end range === start + len
      , Range.length range === len
      , Range.isEmpty range === (len == 0)
      , Range.fromEnds start (start + len) === Just range
      , Range.member point range === (start <= point && point < start + len)
      , Range.fromLength (start + len) 0 `Range.isSubrangeOf` range === True
      , range `Range.isSubrangeOf` Range.fromLength 0 (start + len + 1) === True
      , Range.fromLength 0 (start + len + 1) `Range.isSubrangeOf` range === False
      , if len > 0 then Range.fromEnds (start + len) start === Nothing else property True
      ]

-- Error values are strict in their positions, so forcing one, as rnf does,
-- forces the positions it reports.
strictErrors :: Property
strictErrors = once $ conjoin
  [ failsWith "undefined" (NegativeChildBoundary undefined)
  , failsWith "undefined" (ReversedChildRange 1 undefined)
  , failsWith "undefined" (ChildIndexOutOfBounds undefined)
  , failsWith "undefined" (CheckpointOutOfBounds undefined 1)
  ]

-- Ranges that would break the start <= end invariant, or start before the
-- text, are rejected rather than built.
invalidRanges :: Property
invalidRanges = once $ conjoin
  [ failsWith "invalid range" (Range.fromLength 5 (-3))
  , failsWith "invalid range" (Range.fromLength (-1) 2)
  , failsWith "invalid range" (Range.fromLength maxBound 1)
  , Range.end (Range.fromLength 0 maxBound) === maxBound
  , Range.fromEnds (-5) 3 === Nothing
  , Range.fromEnds 5 2 === Nothing
  , Range.fromEnds 0 0 === Just (Range.fromLength 0 0)
  ]

cursorRegressions :: Property
cursorRegressions = once $ conjoin
  [ map Cursor.path (Cursor.descendantTokens tree) === [[1, 0], [1, 1], [2], [3, 0], [3, 1], [5], [6]]
  , fmap Cursor.path (Cursor.firstToken tree) === Just [1, 0]
  , fmap Cursor.path (Cursor.lastToken tree) === Just [6]
  , fmap Cursor.path (Cursor.firstChild tree >>= Cursor.nextToken) === Just [1, 0]
  , fmap Cursor.path (Cursor.lastChild tree >>= Cursor.previousToken) === Just [5]
  , atPath [1, 1] (fmap Cursor.path . Cursor.nextToken) (Just [2])
  , atPath [2] (fmap Cursor.path . Cursor.nextToken) (Just [3, 0])
  , atPath [5] (fmap Cursor.path . Cursor.previousToken) (Just [3, 1])
  , atPath [3] (fmap Cursor.path . Cursor.previousToken) (Just [2])
  , atPath [3] (fmap Cursor.path . Cursor.nextToken) (Just [5])
  , atPath [3, 0] Cursor.range (Range.fromLength 1 4)
  , offsetPaths (Cursor.tokenAt 0 tree) === SingleToken [1, 1]
  , offsetPaths (Cursor.tokenAt 1 tree) === BetweenTokens [1, 1] [3, 0]
  , offsetPaths (Cursor.tokenAt 3 tree) === SingleToken [3, 0]
  , offsetPaths (Cursor.tokenAt 5 tree) === BetweenTokens [3, 0] [5]
  , offsetPaths (Cursor.tokenAt 6 tree) === SingleToken [5]
  , offsetPaths (Cursor.tokenAt 7 tree) === NoToken
  , atPath [3] (offsetPaths . Cursor.tokenAt 1) (SingleToken [3, 0])
  , atPath [3] (offsetPaths . Cursor.tokenAt 0) NoToken
  , fmap Cursor.path (Cursor.covering (Range.fromLength 0 0) tree) === Just [0]
  , fmap Cursor.path (Cursor.covering (Range.fromLength 1 0) tree) === Just [1, 1]
  , fmap Cursor.path (Cursor.covering (Range.fromLength 6 0) tree) === Just [5]
  , fmap Cursor.path (Cursor.covering (Range.fromLength 1 4) tree) === Just [3, 0]
  , fmap Cursor.path (Cursor.covering (Range.fromLength 0 5) tree) === Just []
  , fmap Cursor.path (Cursor.covering (Range.fromLength 6 1) tree) === Nothing
  , tokensEmpty (Cursor.fromGreen (Node (Green.node Group [])))
  , offsetPaths (Cursor.tokenAt 0 (Cursor.fromGreen (tok ""))) === NoToken
  , fmap Cursor.path (Cursor.firstToken (Cursor.fromGreen (tok ""))) === Just []
  , Cursor.range sharedSecond === Range.fromLength 1 1
  -- A cursor shows its parts with the parentheses each needs.
  , fmap show (Cursor.childAt 1 (Cursor.fromGreen (Node (Green.node Root [tok "a", tok "b"]))))
      === Right "Cursor [1] (Range.fromLength 1 1) (Token (Green.token Name \"b\"))"
  -- Abstract values show as the public calls that rebuild them.
  , show (Just (Range.fromLength 5 3)) === "Just (Range.fromLength 5 3)"
  , show (Node (Green.node Group [Token (Green.token Space "\t")]))
      === "Node (Green.node Group [Token (Green.token Space \"\\t\")])"
  , Cursor.path sharedSecond === [1]
  , Cursor.green sharedSecond === shared
  , fmap Green.render (Edit.apply (Cursor.tree sharedSecond) [Edit.replace sharedSecond (tok "long")]) === Right "along"
  , Cursor.range sharedSecond === Range.fromLength 1 1
  , Range.end (Cursor.range hugeCursor) === 2 ^ hugeDepth
  , offsetPaths (Cursor.tokenAt (2 ^ hugeDepth) hugeCursor)
      === SingleToken (replicate hugeDepth 1)
  ]
  where
    tok = Token . Green.token Name
    empty = Node (Green.node Group [])
    tree = Cursor.fromGreen (Node (Green.node Root
      [ empty, Node (Green.node Group [tok "", tok "a", empty]), tok ""
      , Node (Green.node Group [tok "😀", tok ""]), empty, tok "b", tok ""
      ]))
    atPath path inspect expected = case Cursor.at path tree of
      Left err -> counterexample (show err) False
      Right focus -> inspect focus === expected
    tokensEmpty cursor = conjoin
      [ map Cursor.path (Cursor.descendantTokens cursor) === []
      , fmap Cursor.path (Cursor.firstToken cursor) === Nothing
      , fmap Cursor.path (Cursor.lastToken cursor) === Nothing
      , offsetPaths (Cursor.tokenAt 0 cursor) === NoToken
      ]
    shared = Node (Green.node Group [tok "a"])
    sharedSecond = case Cursor.childAt 1 (Cursor.fromGreen (Node (Green.node Root [shared, shared]))) of
      Right cursor -> cursor
      Left err -> error (show err)
    hugeCursor = Cursor.fromGreen (iterate (\child -> Node (Green.node Group [child, child])) (tok "x") !! hugeDepth)

-- Shared subtrees can describe more text than an Int can count. Building or
-- editing such a node fails instead of wrapping its length, which once let
-- render write outside its buffer.
lengthOverflow :: Property
lengthOverflow = once $ conjoin
  [ overflows (Green.node Root [half, half])
  , overflows (Green.node Root [half, Node (Green.node Group [half])])
  , overflows (edited (Green.replaceChild 1 half (Green.node Root [half, tok "x"])))
  , overflows (edited (Green.insertChild 1 half (Green.node Root [half])))
  , overflows (edited (Green.spliceChildren 0 0 [half, half] (Green.node Root [])))
  , overflows (Edit.apply wide [Edit.replace (edited (Cursor.at [1] (Cursor.fromGreen wide))) half])
  , overflows (Green.render (Node (Green.node Root [half, half, half, half, tok "AB"])))
  , Green.length half === TextLength (2 ^ (finiteBitSize (0 :: Int) - 2))
  , Green.length (Node (Green.node Root [half, Node (Green.node Group [])])) === Green.length half
  ]
  where
    half = halfMax Group
    wide = Node (Green.node Root [half, tok "x"])
    edited :: Show e => Either e a -> a
    edited = either (error . show) id
    tok = Token . Green.token Name

regressions :: Property
regressions = once $ conjoin
  [ Green.renderNode empty === ""
  , Green.nodeLength empty === 0
  , Green.renderNode original === "x\t{- keep -}\r\n😀e\x0301\0"
  , Green.nodeLength original === 22
  , fmap Green.renderNode (Green.replaceChild 0 (tok "y") original)
      === Right "y\t{- keep -}\r\n😀e\x0301\0"
  , Green.length huge === 2 ^ hugeDepth
  , Green.nodeKind original === Root
  , Green.kind (tok "x") === Name
  , fmap Green.render (replaceAt [] (tok "new") (Node original)) === Right "new"
  , Green.replaceChild maxBound (tok "x") empty === Left (ChildBoundaryOutOfBounds maxBound)
  ]
  where
    empty = Green.node Root []
    tok = Token . Green.token Name
    original = Green.node Root
      [ tok "x"
      , Token (Green.token Space "\t")
      , Token (Green.token Comment "{- keep -}")
      , Node (Green.node Group [tok "\r\n", tok "😀e\x0301\0", tok ""])
      ]
    -- A tiny shared DAG with a logical size of 2 ^ hugeDepth. Never render it.
    huge = iterate (\child -> Node (Green.node Group [child, child])) (tok "x") !! hugeDepth

main :: IO ()
main = do
  results <- sequence $
    [ run "lossless rendering" propRendering
    , run "cached UTF-8 lengths at every path" propLengths
    , run "token round-trip and byte length" propToken
    , run "independent token boundaries" propChunking
    , run "replacement source, caches and unaffected branches" propReplacement
    , run "identity replacement" propIdentity
    , run "kind mapping and structural hashing" propKindsAndHashes
    , run "invalid paths" propInvalidPaths
    , run "immediate child replacement" propChildReplacement
    , run "empty, trivia, Unicode and unbounded-length regressions" regressions
    , run "cursor occurrences and source ranges" propCursorOccurrences
    , run "cursor parent, child and sibling navigation" propCursorNavigation
    , run "cursor token navigation" propCursorTokenNavigation
    , run "enter/leave walk and occurrence text" propCursorWalk
    , run "replacement through a cursor and shifted ranges" propCursorReplacement
    , run "cursor invalid paths" propCursorInvalidPaths
    , run "boundary-aware token lookup" propCursorOffsetLookup
    , run "covering range lookup" propCoveringRange
    , run "half-open range invariants" propRanges
    , run "invalid ranges are rejected" invalidRanges
    , run "error values are strict in their positions" strictErrors
    , run "cursor empty, boundary, shared-occurrence and large-range regressions" cursorRegressions
    , run "length overflow fails instead of wrapping" lengthOverflow
    ] ++ [run title prop | (title, prop) <- builderProperties ++ childEditProperties ++ cursorProperties ++ editProperties ++ foldProperties ++ astProperties ++ lineIndexProperties]
  unless (all isSuccess results) exitFailure
  where
    run title prop = do
      putStrLn title
      quickCheckWithResult stdArgs {maxSuccess = 1000} prop

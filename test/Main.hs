{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Char (chr, ord)
import Data.List (isPrefixOf)
import qualified Data.Text as Text
import Graft
import System.Exit (exitFailure)
import Test.QuickCheck

data Kind = Root | Group | Name | Space | Comment
  deriving (Eq, Show, Enum, Bounded)

-- The oracle stores no lengths and renders through String, independently of
-- the library's cached representation and text builder.
data Model = Branch Kind [Model] | Leaf Kind String
  deriving (Eq, Show)

instance Arbitrary Model where
  arbitrary = sized genModel
  shrink (Leaf kind text) = [Leaf kind s | s <- shrink text, all scalar s]
  shrink (Branch kind children) =
    children ++ [Branch kind xs | xs <- shrinkList shrink children]

scalar :: Char -> Bool
scalar c = ord c < 0xD800 || ord c > 0xDFFF

genChar :: Gen Char
genChar = frequency
  [ (5, elements "abcXYZ09 \t\r\n\0")
  , (3, elements ['é', 'λ', '中', '\x0301', '\x1F600', '\x10FFFF'])
  , (1, chr <$> oneof [chooseInt (0, 0xD7FF), chooseInt (0xE000, 0x10FFFF)])
  ]

genText :: Gen String
genText = chooseInt (0, 20) >>= \n -> vectorOf n genChar

genKind :: Gen Kind
genKind = elements [minBound .. maxBound]

genModel :: Int -> Gen Model
genModel size
  | size <= 0 = Leaf <$> genKind <*> genText
  | otherwise = frequency
      [ (2, Leaf <$> genKind <*> genText)
      , (3, do
          count <- chooseInt (0, min 5 size)
          Branch <$> genKind <*> vectorOf count (genModel (size `div` (count + 1))))
      ]

compile :: Model -> GreenElement Kind
compile (Leaf kind text) = Token (greenToken kind (Text.pack text))
compile (Branch kind children) = Node (greenNode kind (map compile children))

source :: Model -> String
source (Leaf _ text) = text
source (Branch _ children) = concatMap source children

-- Independent UTF-8 width oracle for Unicode scalar values.
byteLength :: String -> TextLength
byteLength = sum . map width
  where
    width c
      | ord c <= 0x7F = 1
      | ord c <= 0x7FF = 2
      | ord c <= 0xFFFF = 3
      | otherwise = 4

validCaches :: GreenElement Kind -> Bool
validCaches tree = elementTextLength tree == byteLength (Text.unpack (renderElement tree))
  && case tree of
    Token _ -> True
    Node node -> all validCaches (nodeChildren node)

-- Each occurrence supplies its path and surrounding source. Contexts come
-- from the model's leaf text, not from the editing implementation.
contexts :: Model -> [(Path, String, Model, String)]
contexts model = ([], "", model, "") : case model of
  Leaf _ _ -> []
  Branch _ children -> concat
    [ [ (i : path, concatMap source before ++ prefix, selected,
          suffix ++ concatMap source after)
      | (path, prefix, selected, suffix) <- contexts child
      ]
    | (i, child) <- zip [0 ..] children
    , let before = take i children
    , let after = drop (i + 1) children
    ]

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
  where
    isLeaf (Leaf _ _) = True
    isLeaf _ = False

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

regressions :: Property
regressions = once $ conjoin
  [ renderNode empty === ""
  , nodeTextLength empty === 0
  , renderNode original === "x\t{- keep -}\r\n😀e\x0301\0"
  , nodeTextLength original === 22
  , fmap renderNode (replaceChild 0 (tok "y") original)
      === Right "y\t{- keep -}\r\n😀e\x0301\0"
  , elementTextLength huge === 2 ^ (70 :: Int)
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
    huge = iterate (\child -> Node (greenNode Group [child, child])) (tok "x") !! 70

main :: IO ()
main = do
  results <- sequence
    [ run "lossless rendering" propRendering
    , run "cached UTF-8 lengths at every path" propLengths
    , run "token round-trip and byte length" propToken
    , run "independent token boundaries" propChunking
    , run "replacement source, caches and unaffected branches" propReplacement
    , run "identity replacement" propIdentity
    , run "invalid paths" propInvalidPaths
    , run "immediate child replacement" propChildReplacement
    , run "empty, trivia, Unicode and unbounded-length regressions" regressions
    ]
  unless (all isSuccess results) exitFailure
  where
    run title prop = do
      putStrLn title
      quickCheckWithResult stdArgs {maxSuccess = 1000} prop

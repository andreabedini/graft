module TestModel
  ( Kind (..), Model (..), scalar, genChar, genText, genKind, genModel
  , compile, source, byteLength, validCaches, contexts, hugeDepth, halfMax, overflows, failsWith
  , PathEditError (..), elementAt, replaceAt, applied, spliceChildrenAt, insertChildAt, removeChildAt
  ) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Data.Bits (finiteBitSize)
import Data.List (isInfixOf)
import Data.Char (chr, ord)
import qualified Data.Text as Text
import Graft
import qualified Graft.Cursor as Cursor
import qualified Graft.Edit as Edit
import qualified Graft.Green as Green
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
compile (Leaf kind text) = Token (Green.token kind (Text.pack text))
compile (Branch kind children) = Node (Green.node kind (map compile children))

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

-- Depth of the shared regression DAGs: their logical length 2 ^ hugeDepth
-- exceeds any real text but stays below maxBound on 32- and 64-bit Int.
hugeDepth :: Int
hugeDepth = finiteBitSize (0 :: Int) - 24

-- A shared DAG of logical length 2 ^ (bits - 2), so that two of them side by
-- side overflow Int. Never render, compare or deeply force it.
halfMax :: kind -> GreenElement kind
halfMax kind = iterate (\child -> Node (Green.node kind [child, child])) (Token (Green.token kind (Text.pack "x")))
  !! (finiteBitSize (0 :: Int) - 2)

-- Whether evaluating a value to weak head normal form fails with the
-- length-overflow error, rather than wrapping around or succeeding.
overflows :: a -> Property
overflows = failsWith "exceeds maxBound"

-- Whether evaluating a value to weak head normal form fails with an error
-- whose message contains the given text.
failsWith :: String -> a -> Property
failsWith expected value = ioProperty $ do
  result <- try (evaluate value)
  pure $ case result of
    Left (ErrorCall message) -> counterexample message (expected `isInfixOf` message)
    Right _ -> counterexample ("no error mentioning " ++ show expected) False

validCaches :: GreenElement Kind -> Bool
validCaches tree = Green.length tree == byteLength (Text.unpack (Green.render tree))
  && case tree of
    Token _ -> True
    Node node -> all validCaches (Green.children node)

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


-- Edits by path, written with cursors and plans as applications write them.
-- The properties state their expectations in terms of paths.
elementAt :: Path -> GreenElement k -> Either PathError (GreenElement k)
elementAt path root = Cursor.green <$> Cursor.at path (Cursor.fromGreen root)

replaceAt :: Eq k => Path -> GreenElement k -> GreenElement k -> Either PathError (GreenElement k)
replaceAt path replacement root = do
  focus <- Cursor.at path (Cursor.fromGreen root)
  pure (applied root [Edit.replace focus replacement])

-- A plan on cursors of the base can only fail for the reasons the test
-- states; anything else is a bug in the test.
applied :: Eq k => GreenElement k -> [Edit k] -> GreenElement k
applied root edits = either (error . show) id (Edit.apply root edits)

data PathEditError = ChildEditPathError PathError | EditError ChildEditError
  deriving (Eq, Show)

editAt :: Eq k => (Cursor k -> Edit k) -> Path -> GreenElement k -> Either PathEditError (GreenElement k)
editAt edit path root = case Cursor.at path (Cursor.fromGreen root) of
  Left err -> Left (ChildEditPathError err)
  Right focus -> case Edit.apply root [edit focus] of
    Left (InvalidSplice _ err) -> Left (EditError err)
    Left other -> error ("unexpected plan error: " ++ show other)
    Right edited -> Right edited

spliceChildrenAt
  :: Eq k => Path -> Int -> Int -> [GreenElement k] -> GreenElement k -> Either PathEditError (GreenElement k)
spliceChildrenAt path start end replacements = editAt (\focus -> Edit.splice focus start end replacements) path

insertChildAt :: Eq k => Path -> Int -> GreenElement k -> GreenElement k -> Either PathEditError (GreenElement k)
insertChildAt path index element = editAt (\focus -> Edit.splice focus index index [element]) path

removeChildAt :: Eq k => Path -> Int -> GreenElement k -> Either PathEditError (GreenElement k)
removeChildAt path index = editAt (\focus -> Edit.splice focus index (index + 1) []) path

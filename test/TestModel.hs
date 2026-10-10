module TestModel
  ( Kind (..), Model (..), scalar, genChar, genText, genKind, genModel
  , compile, source, byteLength, validCaches, contexts
  ) where

import Data.Char (chr, ord)
import qualified Data.Text as Text
import Graft.Edit (Path)
import Graft.Green
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


module LineIndexTests (lineIndexProperties) where

import Data.Char (ord)
import qualified Data.Text as Text
import Graft
import qualified Graft.LineIndex as LineIndex
import Test.QuickCheck
import TestModel (byteLength, genText)

-- An independent oracle over Strings: each character's byte offset, line,
-- byte column and UTF-16 column, plus the position of the end of the text.
data Place = Place
  { placeOffset :: TextLength
  , placeLine :: Int
  , placeUtf8 :: Int
  , placeUtf16 :: Int
  , placeAfterCarriage :: Bool
  }
  deriving (Show)

places :: String -> [Place]
places = go 0 0 0 0 False
  where
    go offset line utf8 utf16 afterCarriage text = Place offset line utf8 utf16 afterCarriage : case text of
      [] -> []
      '\r' : rest@('\n' : _) -> go (offset + 1) line (utf8 + 1) (utf16 + 1) True rest
      c : rest
        | c == '\n' || c == '\r' -> go (offset + 1) (line + 1) 0 0 False rest
        | otherwise -> go (offset + byteLength [c]) line (utf8 + width8 c) (utf16 + width16 c) False rest
    width8 c = fromIntegral (byteLength [c])
    width16 c = if ord c > 0xFFFF then 2 else 1

terminators :: String -> Int
terminators ('\r' : '\n' : rest) = 1 + terminators rest
terminators (c : rest) | c == '\r' || c == '\n' = 1 + terminators rest
terminators (_ : rest) = terminators rest
terminators [] = 0

propPositions :: Property
propPositions = forAll genText $ \text ->
  let index = LineIndex.fromText (Text.pack text)
  in conjoin
      [ LineIndex.lineCount index === terminators text + 1
      , conjoin
          [ counterexample (show place) $ conjoin
              [ LineIndex.utf8Position (placeOffset place) index === Just (Position (placeLine place) (placeUtf8 place))
              , LineIndex.utf16Position (placeOffset place) index === Just (Position (placeLine place) (placeUtf16 place))
              , placeAfterCarriage place .||. conjoin
                  [ LineIndex.fromUtf8Position (Position (placeLine place) (placeUtf8 place)) index === Just (placeOffset place)
                  , LineIndex.fromUtf16Position (Position (placeLine place) (placeUtf16 place)) index === Just (placeOffset place)
                  ]
              ]
          | place <- places text
          ]
      ]

propInvalid :: Property
propInvalid = forAll genText $ \text ->
  let index = LineIndex.fromText (Text.pack text)
      size = byteLength text
      starts = map placeOffset (places text)
      inside = [offset | offset <- [0 .. size], offset `notElem` starts]
  in conjoin
      [ LineIndex.utf8Position (-1) index === Nothing
      , LineIndex.utf8Position (size + 1) index === Nothing
      , LineIndex.utf16Position (size + 1) index === Nothing
      , conjoin [LineIndex.utf16Position offset index === Nothing | offset <- inside]
      , conjoin [fmap positionLine (LineIndex.utf8Position offset index) =/= Nothing | offset <- inside]
      , LineIndex.fromUtf16Position (Position (LineIndex.lineCount index) 0) index === Nothing
      , LineIndex.fromUtf16Position (Position 0 (-1)) index === Nothing
      ]

regressions :: Property
regressions = once $ conjoin
  [ LineIndex.lineCount (LineIndex.fromText (Text.pack "")) === 1
  , LineIndex.lineCount (LineIndex.fromText (Text.pack "a\r\nb\rc\n")) === 4
  , map (`LineIndex.lineStart` LineIndex.fromText (Text.pack "a\r\nb\rc\n")) [0 .. 4] === map (fmap TextLength) [Just 0, Just 3, Just 5, Just 7, Nothing]
  -- Columns past the end of a line clamp to its content, before "\r\n".
  , LineIndex.fromUtf16Position (Position 0 99) (LineIndex.fromText (Text.pack "ab\r\ncd")) === Just 2
  , LineIndex.fromUtf8Position (Position 1 99) (LineIndex.fromText (Text.pack "ab\r\ncd")) === Just 6
  -- U+1F600 takes four bytes and two UTF-16 code units.
  , LineIndex.utf16Position 5 (LineIndex.fromText (Text.pack "x\x1F600y")) === Just (Position 0 3)
  , LineIndex.fromUtf16Position (Position 0 2) (LineIndex.fromText (Text.pack "x\x1F600y")) === Nothing
  , LineIndex.fromUtf16Position (Position 0 3) (LineIndex.fromText (Text.pack "x\x1F600y")) === Just 5
  ]

lineIndexProperties :: [(String, Property)]
lineIndexProperties =
  [ ("line index positions agree with a String oracle", propPositions)
  , ("line index rejects offsets inside characters and missing lines", propInvalid)
  , ("line index terminators, clamping and surrogate regressions", regressions)
  ]

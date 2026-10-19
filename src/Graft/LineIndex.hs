{-# LANGUAGE BangPatterns #-}

-- | Conversion between the UTF-8 byte offsets that Graft uses and line and
-- column positions, including the UTF-16 columns that the Language Server
-- Protocol uses by default.
--
-- A 'LineIndex' describes one source text. Build it from the rendered tree,
-- and rebuild it after an edit; it does not follow the tree.
--
-- Lines end at @\\n@, @\\r\\n@ or a lone @\\r@, as in the Language Server
-- Protocol. Lines and columns are zero-based. A line's terminator belongs to
-- that line, so an offset just after a @\\r@ that precedes @\\n@ still lies
-- on the @\\r@'s line.
--
-- This module is meant to be imported qualified; its types are also
-- exported by "Graft":
--
-- > import Graft
-- > import qualified Graft.LineIndex as LineIndex
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Graft.LineIndex as LineIndex
-- >>> let index = LineIndex.fromText "let x =\r\n  \"\x1F600\" in x\n"
-- >>> LineIndex.utf8Position 16 index
-- Just (Utf8Position {utf8Line = 1, utf8Column = 7})
-- >>> LineIndex.utf16Position 16 index
-- Just (Utf16Position {utf16Line = 1, utf16Column = 5})
-- >>> LineIndex.fromUtf16Position (Utf16Position 1 5) index
-- Just 16
module Graft.LineIndex
  ( LineIndex
  , fromText
  , lineCount
  , lineStart
  , Utf8Position (..)
  , utf8Position
  , fromUtf8Position
  , Utf16Position (..)
  , utf16Position
  , fromUtf16Position
  ) where

import Control.DeepSeq (NFData (..))
import Data.Bits ((.&.))
import Data.Hashable (Hashable (..))
import Data.Primitive.PrimArray
import Data.Text (Text)
import qualified Data.Text.Array as Array
import Data.Text.Internal (Text (..))
import Data.Word (Word8)
import Graft.Green.Internal (TextLength (..))

-- | A text together with the byte offset at which each of its lines starts.
data LineIndex = LineIndex !Text !(PrimArray Int)

instance NFData LineIndex where
  rnf (LineIndex text _) = rnf text

instance Show LineIndex where
  showsPrec precedence (LineIndex text _) = showParen (precedence > 10) $
    showString "LineIndex.fromText " . showsPrec 11 text

-- | A zero-based line and a column counted in UTF-8 bytes. The two kinds of
-- position are separate types so that a column in one unit cannot be passed
-- where the other is expected.
data Utf8Position = Utf8Position
  { utf8Line :: !Int
  , utf8Column :: !Int
  }
  deriving (Eq, Ord, Show)

instance NFData Utf8Position where
  rnf (Utf8Position _ _) = ()

instance Hashable Utf8Position where
  hashWithSalt salt (Utf8Position line column) = salt `hashWithSalt` line `hashWithSalt` column

-- | A zero-based line and a column counted in UTF-16 code units, as the
-- Language Server Protocol counts them by default.
data Utf16Position = Utf16Position
  { utf16Line :: !Int
  , utf16Column :: !Int
  }
  deriving (Eq, Ord, Show)

instance NFData Utf16Position where
  rnf (Utf16Position _ _) = ()

instance Hashable Utf16Position where
  hashWithSalt salt (Utf16Position line column) = salt `hashWithSalt` line `hashWithSalt` column

-- | Index the lines of a text, in time proportional to its length.
fromText :: Text -> LineIndex
fromText text@(Text _ _ len) = LineIndex text (primArrayFromList (0 : go 0))
  where
    go !index
      | index >= len = []
      | byte == 10 = (index + 1) : go (index + 1)
      | byte == 13 && (index + 1 >= len || byteAt text (index + 1) /= 10) = (index + 1) : go (index + 1)
      | otherwise = go (index + 1)
      where
        byte = byteAt text index

-- | Number of lines. An empty text, and a text ending in a terminator, end
-- with an empty line.
lineCount :: LineIndex -> Int
lineCount (LineIndex _ starts) = sizeofPrimArray starts

-- | Offset at which a line starts, or 'Nothing' for a line that does not exist.
lineStart :: Int -> LineIndex -> Maybe TextLength
lineStart line (LineIndex _ starts)
  | line < 0 || line >= sizeofPrimArray starts = Nothing
  | otherwise = Just (TextLength (indexPrimArray starts line))

-- | Line and UTF-8 byte column of an offset in @[0, length]@. Any byte offset
-- is accepted, including one inside a multibyte character.
utf8Position :: TextLength -> LineIndex -> Maybe Utf8Position
utf8Position (TextLength offset) index@(LineIndex text _)
  | offset < 0 || offset > textBytes text = Nothing
  | otherwise = Just (Utf8Position line (offset - startOf index line))
  where
    line = lineOf offset index

-- | Offset of a line and UTF-8 byte column. A column past the end of the
-- line's content is clamped to it, as the Language Server Protocol requires;
-- a line that does not exist returns 'Nothing'.
fromUtf8Position :: Utf8Position -> LineIndex -> Maybe TextLength
fromUtf8Position (Utf8Position line column) index
  | line < 0 || line >= lineCount index || column < 0 = Nothing
  | otherwise = Just (TextLength (start + min column (contentEnd index line - start)))
  where
    start = startOf index line

-- | Line and UTF-16 column of an offset in @[0, length]@, or 'Nothing' if the
-- offset lies outside the text or inside a multibyte character.
utf16Position :: TextLength -> LineIndex -> Maybe Utf16Position
utf16Position (TextLength offset) index@(LineIndex text _)
  | offset < 0 || offset > textBytes text = Nothing
  | offset < textBytes text && continuation (byteAt text offset) = Nothing
  | otherwise = Just (Utf16Position line (go 0 (startOf index line)))
  where
    line = lineOf offset index
    go !units !position
      | position >= offset = units
      | otherwise = go (units + utf16Units (byteAt text position)) (position + 1)

-- | Offset of a line and UTF-16 column. A column past the end of the line's
-- content is clamped to it, as the Language Server Protocol requires. Returns
-- 'Nothing' for a line that does not exist or a column that falls between
-- the two halves of a surrogate pair.
fromUtf16Position :: Utf16Position -> LineIndex -> Maybe TextLength
fromUtf16Position (Utf16Position line column) index@(LineIndex text _)
  | line < 0 || line >= lineCount index || column < 0 = Nothing
  | otherwise = TextLength <$> go 0 (startOf index line)
  where
    end = contentEnd index line
    go !units !position
      | units > column = Nothing
      | units == column || position >= end = Just position
      | otherwise = go (units + utf16Units (byteAt text position)) (next position)
    next position
      | position + 1 < end && continuation (byteAt text (position + 1)) = next (position + 1)
      | otherwise = position + 1

-- The last line whose start is at or before the offset.
lineOf :: Int -> LineIndex -> Int
lineOf offset (LineIndex _ starts) = go 0 (sizeofPrimArray starts - 1)
  where
    go low high
      | low >= high = low
      | indexPrimArray starts middle <= offset = go middle high
      | otherwise = go low (middle - 1)
      where
        middle = (low + high + 1) `div` 2

startOf :: LineIndex -> Int -> Int
startOf (LineIndex _ starts) = indexPrimArray starts

-- The end of a line's content, before its terminator.
contentEnd :: LineIndex -> Int -> Int
contentEnd index@(LineIndex text starts) line = stripCarriage (stripNewline end)
  where
    start = startOf index line
    end
      | line + 1 < sizeofPrimArray starts = indexPrimArray starts (line + 1)
      | otherwise = textBytes text
    stripNewline position
      | position > start && byteAt text (position - 1) == 10 = position - 1
      | otherwise = position
    stripCarriage position
      | position > start && byteAt text (position - 1) == 13 = position - 1
      | otherwise = position

textBytes :: Text -> Int
textBytes (Text _ _ len) = len

byteAt :: Text -> Int -> Word8
byteAt (Text array start _) index = Array.unsafeIndex array (start + index)

continuation :: Word8 -> Bool
continuation byte = byte .&. 0xC0 == 0x80

-- UTF-16 code units contributed by the character that starts with this byte;
-- continuation bytes contribute none.
utf16Units :: Word8 -> Int
utf16Units byte
  | continuation byte = 0
  | byte >= 0xF0 = 2
  | otherwise = 1

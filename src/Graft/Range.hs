-- | Half-open ranges of UTF-8 byte offsets: a range includes its start and
-- excludes its end.
--
-- >>> let range = rangeFromLength 4 3
-- >>> (rangeStart range, rangeEnd range)
-- (4,7)
-- >>> map (`rangeContains` range) [3, 4, 6, 7]
-- [False,True,True,False]
-- >>> textRange 7 4
-- Nothing
module Graft.Range
  ( TextRange
  , textRange
  , rangeFromLength
  , rangeStart
  , rangeEnd
  , rangeLength
  , rangeIsEmpty
  , rangeContains
  , rangeContainsRange
  ) where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))
import Graft.Green (TextLength)

-- | A range of byte offsets whose start is no greater than its end. The
-- endpoints need not fall on character boundaries.
data TextRange = TextRange !TextLength !TextLength
  deriving (Eq, Ord)

-- | Shows the 'rangeFromLength' call that rebuilds the range.
instance Show TextRange where
  showsPrec precedence (TextRange start end) = showParen (precedence > 10) $
    showString "rangeFromLength " . showsPrec 11 start . showChar ' ' . showsPrec 11 (end - start)

instance NFData TextRange where
  rnf (TextRange _ _) = ()

instance Hashable TextRange where
  hashWithSalt salt (TextRange start end) = salt `hashWithSalt` start `hashWithSalt` end

-- | The range from a start to an end, or 'Nothing' if the end comes first.
textRange :: TextLength -> TextLength -> Maybe TextRange
textRange start end
  | start <= end = Just (TextRange start end)
  | otherwise = Nothing

-- | The range with a start and a length, which must not be negative.
rangeFromLength :: TextLength -> TextLength -> TextRange
rangeFromLength start len = TextRange start (start + len)

-- | Inclusive start offset.
rangeStart :: TextRange -> TextLength
rangeStart (TextRange start _) = start

-- | Exclusive end offset.
rangeEnd :: TextRange -> TextLength
rangeEnd (TextRange _ end) = end

-- | Number of bytes in the range.
rangeLength :: TextRange -> TextLength
rangeLength (TextRange start end) = end - start

-- | Whether the range has length zero.
rangeIsEmpty :: TextRange -> Bool
rangeIsEmpty range = rangeStart range == rangeEnd range

-- | Whether an offset lies in the range. The end is excluded, so an empty
-- range contains no offsets.
rangeContains :: TextLength -> TextRange -> Bool
rangeContains offset range = rangeStart range <= offset && offset < rangeEnd range

-- | @rangeContainsRange outer inner@ is whether @inner@ lies within @outer@,
-- including an empty @inner@ at either end of @outer@.
rangeContainsRange :: TextRange -> TextRange -> Bool
rangeContainsRange outer inner =
  rangeStart outer <= rangeStart inner && rangeEnd inner <= rangeEnd outer

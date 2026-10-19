-- | Half-open ranges of UTF-8 byte offsets: a range includes its start and
-- excludes its end.
--
-- This module is meant to be imported qualified; 'TextRange' is also
-- exported by "Graft":
--
-- > import Graft
-- > import qualified Graft.Range as Range
--
-- >>> import qualified Graft.Range as Range
-- >>> let range = Range.fromLength 4 3
-- >>> (Range.start range, Range.end range)
-- (4,7)
-- >>> map (`Range.member` range) [3, 4, 6, 7]
-- [False,True,True,False]
-- >>> Range.fromLength 5 2 `Range.isSubrangeOf` range
-- True
-- >>> Range.fromEnds 7 4
-- Nothing
module Graft.Range
  ( TextRange
  , fromEnds
  , fromLength
  , start
  , end
  , length
  , isEmpty
  , member
  , isSubrangeOf
  ) where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))
import Graft.Green.Internal (TextLength)
import Prelude hiding (length)

-- | A range of byte offsets whose start is no greater than its end, and
-- whose start is not negative. The endpoints need not fall on character
-- boundaries.
data TextRange = TextRange !TextLength !TextLength
  deriving (Eq, Ord)

-- | Shows the 'fromLength' call that rebuilds the range.
instance Show TextRange where
  showsPrec precedence (TextRange from to) = showParen (precedence > 10) $
    showString "Range.fromLength " . showsPrec 11 from . showChar ' ' . showsPrec 11 (to - from)

instance NFData TextRange where
  rnf (TextRange _ _) = ()

instance Hashable TextRange where
  hashWithSalt salt (TextRange from to) = salt `hashWithSalt` from `hashWithSalt` to

-- | The range from a start to an end, or 'Nothing' if the end comes first.
fromEnds :: TextLength -> TextLength -> Maybe TextRange
fromEnds from to
  | 0 <= from && from <= to = Just (TextRange from to)
  | otherwise = Nothing

-- | The range with a start and a length. A negative start or length, or an
-- end past @maxBound :: Int@, is an error; use 'fromEnds' to check endpoints
-- that may be invalid.
fromLength :: TextLength -> TextLength -> TextRange
fromLength from len
  | from < 0 || len < 0 || to < 0 =
      error ("Graft.Range.fromLength: invalid range " ++ show from ++ " " ++ show len)
  | otherwise = TextRange from to
  where
    to = from + len

-- | The first offset in the range.
start :: TextRange -> TextLength
start (TextRange from _) = from

-- | The offset just past the end of the range.
end :: TextRange -> TextLength
end (TextRange _ to) = to

-- | The number of bytes in the range.
length :: TextRange -> TextLength
length (TextRange from to) = to - from

-- | Whether the range has length zero.
isEmpty :: TextRange -> Bool
isEmpty range = start range == end range

-- | Whether an offset lies in the range. The end is excluded, so an empty
-- range contains no offsets.
member :: TextLength -> TextRange -> Bool
member offset range = start range <= offset && offset < end range

-- | @isSubrangeOf inner outer@ is whether @inner@ lies within @outer@,
-- including an empty @inner@ at either end of @outer@. The argument order is
-- that of 'Data.List.isPrefixOf', so it reads well infix.
isSubrangeOf :: TextRange -> TextRange -> Bool
isSubrangeOf inner outer = start outer <= start inner && end inner <= end outer

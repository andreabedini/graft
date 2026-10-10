-- | Validated half-open UTF-8 byte ranges. Offsets share the unbounded
-- 'TextLength' unit used by green-tree caches.
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

import Graft.Green (TextLength)

-- | A half-open interval with start no greater than end. The constructor is
-- hidden; neither endpoint is required to be a Unicode character boundary.
data TextRange = TextRange !TextLength !TextLength
  deriving (Eq, Ord, Show)

-- | Construct a range from start and end, rejecting reversed endpoints.
textRange :: TextLength -> TextLength -> Maybe TextRange
textRange start end
  | start <= end = Just (TextRange start end)
  | otherwise = Nothing

-- | Construct a range from start and byte length. Unbounded arithmetic means
-- that the endpoint cannot overflow.
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

-- | Whether both endpoints coincide.
rangeIsEmpty :: TextRange -> Bool
rangeIsEmpty range = rangeStart range == rangeEnd range

-- | Whether an offset lies in the half-open interval. Empty ranges contain
-- no offsets, and the end offset is excluded.
rangeContains :: TextLength -> TextRange -> Bool
rangeContains offset range = rangeStart range <= offset && offset < rangeEnd range

-- | @rangeContainsRange outer inner@ tests endpoint containment. An empty
-- inner range at either endpoint of the outer range is contained.
rangeContainsRange :: TextRange -> TextRange -> Bool
rangeContainsRange outer inner =
  rangeStart outer <= rangeStart inner && rangeEnd inner <= rangeEnd outer

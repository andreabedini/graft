-- | Persistent edits to the children of a green node. Each edit returns a
-- new node and leaves the original usable; untouched children are shared
-- between the two.
--
-- To edit inside a tree, navigate to the node with a cursor and edit through
-- it ("Graft.Cursor"); the cursor rebuilds the ancestors.
--
-- >>> :set -XOverloadedStrings
-- >>> let tok = Token . greenToken 't'
-- >>> let list = greenNode 'l' [tok "a", tok ", ", tok "b"]
-- >>> renderNode <$> replaceChild 2 (tok "c") list
-- Right "a, c"
-- >>> renderNode <$> spliceChildren 3 3 [tok ", ", tok "d"] list
-- Right "a, b, d"
-- >>> replaceChild 5 (tok "c") list
-- Left (ChildBoundaryOutOfBounds 5)
module Graft.Edit
  ( ChildEditError (..)
  , replaceChild
  , spliceChildren
  , insertChild
  , removeChild
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Graft.Green
import Graft.Green.Internal (childCount, setChild, spliceChildrenUnchecked)

-- | Why a child edit failed. 'CannotEditTokenChildren' means the target is a
-- token; the others report a bad child position (or both positions of a
-- reversed range).
data ChildEditError
  = CannotEditTokenChildren
  | NegativeChildBoundary Int
  | ReversedChildRange Int Int
  | ChildBoundaryOutOfBounds Int
  deriving (Eq, Show)

instance Exception ChildEditError

instance NFData ChildEditError where
  rnf err = err `seq` ()

-- | @spliceChildren start end replacements node@ replaces the children at
-- positions @start@ up to but not including @end@. With @start == end@ it
-- inserts; with no replacements it deletes. Positions run from 0 to the
-- number of children. Nothing is added for you: include any separators or
-- whitespace in the replacements.
--
-- Errors are checked in this order: a negative start or end, then
-- @end < start@, then a start or end past the last child. Cost is
-- proportional to the number of children plus replacements.
spliceChildren
  :: Int -> Int -> [GreenElement kind] -> GreenNode kind
  -> Either ChildEditError (GreenNode kind)
spliceChildren start end replacements node
  | start < 0 = Left (NegativeChildBoundary start)
  | end < 0 = Left (NegativeChildBoundary end)
  | end < start = Left (ReversedChildRange start end)
  | start > childCount node = Left (ChildBoundaryOutOfBounds start)
  | end > childCount node = Left (ChildBoundaryOutOfBounds end)
  | otherwise = Right (spliceChildrenUnchecked start end replacements node)

-- | Insert a child before the given position; the number of children
-- appends. As @spliceChildren index index [element]@.
insertChild
  :: Int -> GreenElement kind -> GreenNode kind
  -> Either ChildEditError (GreenNode kind)
insertChild index element = spliceChildren index index [element]

-- | Remove the child at a position. Removing the last child leaves an empty
-- node of the same kind.
removeChild :: Int -> GreenNode kind -> Either ChildEditError (GreenNode kind)
removeChild index node
  | index < 0 = Left (NegativeChildBoundary index)
  | index >= childCount node = Left (ChildBoundaryOutOfBounds index)
  | otherwise = Right (spliceChildrenUnchecked index (index + 1) [] node)

-- | Replace the child at a position.
replaceChild :: Int -> GreenElement kind -> GreenNode kind -> Either ChildEditError (GreenNode kind)
replaceChild index replacement node
  | index < 0 = Left (NegativeChildBoundary index)
  | index >= childCount node = Left (ChildBoundaryOutOfBounds index)
  | otherwise = Right (setChild index replacement node)

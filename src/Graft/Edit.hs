-- | Persistent edits to green trees, selecting elements by child path. Each
-- edit returns a new tree and leaves the original usable; untouched subtrees
-- are shared between the two.
--
-- To make several edits in one region, or to edit what you found by
-- navigating, edit through a cursor instead ("Graft.Cursor").
module Graft.Edit
  ( -- * Paths
    Path
  , PathError (..)
  , elementAt
  , replaceAt
  , replaceChild
    -- * Child splicing
  , ChildEditError (..)
  , spliceChildren
  , insertChild
  , removeChild
  , spliceChildrenAt
  , insertChildAt
  , removeChildAt
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Graft.Green
import Graft.Green.Internal (childAt, childCount, setChild, spliceChildrenUnchecked)

-- | Zero-based child indices, counting both nodes and tokens. The empty path
-- denotes the root. Paths belong to a particular tree version; an edit may
-- invalidate paths inside the replaced subtree. Child insertion/removal also
-- shifts paths of following siblings and their descendants.
type Path = [Int]

-- | Failure at the first invalid step in a path. Indices are relative to
-- the node reached at that step. No partial result is returned.
data PathError
  = NegativeChildIndex Int
  | ChildIndexOutOfBounds Int
  | CannotDescendIntoToken
  deriving (Eq, Show)

instance Exception PathError

instance NFData PathError where
  rnf (NegativeChildIndex _) = ()
  rnf (ChildIndexOutOfBounds _) = ()
  rnf CannotDescendIntoToken = ()

-- | Look up a subtree or token. Takes time proportional to path depth.
elementAt :: Path -> GreenElement kind -> Either PathError (GreenElement kind)
elementAt [] root = Right root
elementAt (index : rest) root
  | index < 0 = Left (NegativeChildIndex index)
  | otherwise = case root of
      Token _ -> Left CannotDescendIntoToken
      Node node
        | index >= childCount node -> Left (ChildIndexOutOfBounds index)
        | otherwise -> elementAt rest (childAt node index)

-- | @replaceAt path replacement root@ replaces the selected element.
-- An empty path replaces the whole root. Replacement may change kind or switch
-- between node and token; grammar validation is the caller's responsibility.
--
-- Only ancestor nodes on the path are rebuilt, each by copying its child
-- array; sibling elements are reused and the original root remains usable.
-- Ancestor lengths are adjusted by the change in the replaced child's length.
-- Cost is proportional to the sum of ancestor child counts; replacement text
-- and untouched descendants are not traversed.
replaceAt :: Path -> GreenElement kind -> GreenElement kind -> Either PathError (GreenElement kind)
replaceAt [] replacement _ = Right replacement
replaceAt (index : rest) replacement root
  | index < 0 = Left (NegativeChildIndex index)
  | otherwise = case root of
      Token _ -> Left CannotDescendIntoToken
      Node node -> Node <$> modifyChild index (replaceAt rest replacement) node

-- | Replace one immediate child, preserving the root node and its kind.
-- Uses the same validation and sharing rules as 'replaceAt'.
replaceChild :: Int -> GreenElement kind -> GreenNode kind -> Either PathError (GreenNode kind)
replaceChild index replacement = modifyChild index (const (Right replacement))

-- | Invalid child editing. Path failures retain the original 'PathError'.
-- 'CannotEditTokenChildren' means that the selected target is a token.
-- Boundary errors carry the offending position; a reversed range carries
-- start and end. Positions count nodes and tokens, including empty elements.
data ChildEditError
  = ChildEditPathError PathError
  | CannotEditTokenChildren
  | NegativeChildBoundary Int
  | ReversedChildRange Int Int
  | ChildBoundaryOutOfBounds Int
  deriving (Eq, Show)

instance Exception ChildEditError

instance NFData ChildEditError where
  rnf (ChildEditPathError err) = rnf err
  rnf _ = ()

-- | @spliceChildren start end replacements node@ replaces the half-open child
-- range @[start,end)@. Both boundaries must lie between zero and the child
-- count, inclusive. Equal boundaries insert; an empty replacement deletes.
-- The node's kind is retained. No text, separators or trivia are synthesized.
--
-- Validation checks negative start, negative end, reversed range, start bounds,
-- then end bounds, in that order. Invalid input returns no partial result.
-- Untouched children are reused. Cost is proportional to the number of
-- children plus replacements: the child array is copied and the offsets of
-- following children are shifted. Descendant text is not traversed. The
-- replacement list must be finite.
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

-- | Insert one immediate child before the given index. The child count is a
-- valid position and appends; zero inserts into an empty node. Uses the same
-- validation, sharing and cache rules as 'spliceChildren'.
insertChild
  :: Int -> GreenElement kind -> GreenNode kind
  -> Either ChildEditError (GreenNode kind)
insertChild index element = spliceChildren index index [element]

-- | Remove one existing immediate child. Unlike insertion, the child count
-- is out of bounds. Negative indices return 'NegativeChildBoundary'; a missing
-- child returns 'ChildBoundaryOutOfBounds'. Retains the node's kind
-- and follows the same sharing and cache rules as 'spliceChildren'.
removeChild :: Int -> GreenNode kind -> Either ChildEditError (GreenNode kind)
removeChild index node
  | index < 0 = Left (NegativeChildBoundary index)
  | index >= childCount node = Left (ChildBoundaryOutOfBounds index)
  | otherwise = Right (spliceChildrenUnchecked index (index + 1) [] node)

-- | Splice the children of the node selected by a root-relative path and
-- return a new root. An empty path selects the root node. Path validation
-- precedes target validation (a token has no editable children), which precedes
-- boundary validation. A successful edit rebuilds the ancestors on the path.
-- Paths and byte offsets after the edited range can change in the new snapshot.
spliceChildrenAt
  :: Path -> Int -> Int -> [GreenElement kind] -> GreenElement kind
  -> Either ChildEditError (GreenElement kind)
spliceChildrenAt path start end replacements =
  editNodeAt path (spliceChildren start end replacements)

-- | Insert a child into the path-selected node. The index is relative to that
-- node; its child count appends. Validation and persistence match 'spliceChildrenAt'.
insertChildAt
  :: Path -> Int -> GreenElement kind -> GreenElement kind
  -> Either ChildEditError (GreenElement kind)
insertChildAt path index element = editNodeAt path (insertChild index element)

-- | Remove an existing child of the path-selected node. Validation and
-- persistence match 'spliceChildrenAt'; the node itself is retained, even
-- after removing its last child.
removeChildAt
  :: Path -> Int -> GreenElement kind -> Either ChildEditError (GreenElement kind)
removeChildAt path index = editNodeAt path (removeChild index)

editNodeAt
  :: Path -> (GreenNode kind -> Either ChildEditError (GreenNode kind))
  -> GreenElement kind -> Either ChildEditError (GreenElement kind)
editNodeAt path edit = go path
  where
    go [] (Token _) = Left CannotEditTokenChildren
    go [] (Node node) = Node <$> edit node
    go (index : rest) element
      | index < 0 = Left (ChildEditPathError (NegativeChildIndex index))
      | otherwise = case element of
          Token _ -> Left (ChildEditPathError CannotDescendIntoToken)
          Node node
            | index >= childCount node -> Left (ChildEditPathError (ChildIndexOutOfBounds index))
            | otherwise -> (\child -> Node (setChild index child node)) <$> go rest (childAt node index)

modifyChild
  :: Int
  -> (GreenElement kind -> Either PathError (GreenElement kind))
  -> GreenNode kind
  -> Either PathError (GreenNode kind)
modifyChild index edit node
  | index < 0 = Left (NegativeChildIndex index)
  | index >= childCount node = Left (ChildIndexOutOfBounds index)
  | otherwise = (\child -> setChild index child node) <$> edit (childAt node index)

-- | Persistent editing and lookup using paths of child indices.
module Graft.Edit
  ( Path
  , PathError (..)
  , elementAt
  , replaceAt
  , replaceChild
  , ChildEditError (..)
  , spliceChildren
  , insertChild
  , removeChild
  , spliceChildrenAt
  , insertChildAt
  , removeChildAt
  ) where

import Data.Bifunctor (first)
import Graft.Green

-- | Zero-based child indices, counting both nodes and tokens. The empty path
-- denotes the root. Paths belong to a particular tree version; an edit may
-- invalidate paths inside the replaced subtree.
type Path = [Int]

-- | Failure at the first invalid step in a path. Indices are relative to
-- the node reached at that step. No partial result is returned.
data PathError
  = NegativeChildIndex Int
  | ChildIndexOutOfBounds Int
  | CannotDescendIntoToken
  deriving (Eq, Show)

-- | Look up a subtree or token. Takes time proportional to the sum of the
-- child indices along the path, plus path depth.
elementAt :: Path -> GreenElement kind -> Either PathError (GreenElement kind)
elementAt [] root = Right root
elementAt (index : rest) root
  | index < 0 = Left (NegativeChildIndex index)
  | otherwise = case root of
      Token _ -> Left CannotDescendIntoToken
      Node node -> do
        child <- childAt index (nodeChildren node)
        elementAt rest child
  where
    childAt _ [] = Left (ChildIndexOutOfBounds index)
    childAt 0 (child : _) = Right child
    childAt n (_ : children) = childAt (n - 1) children

-- | @replaceAt path replacement root@ replaces the selected element.
-- An empty path replaces the whole root. Replacement may change kind or switch
-- between node and token; grammar validation is the caller's responsibility.
--
-- Only ancestor nodes and child-list prefixes on the path are rebuilt. Sibling
-- elements and list suffixes are reused; the original root remains usable.
-- Ancestor lengths are recomputed from immediate children's cached lengths.
-- Cost is proportional to the sum of ancestor child counts plus path depth;
-- replacement text and untouched descendants are not traversed.
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

-- | @spliceChildren start end replacements node@ replaces the half-open child
-- range @[start,end)@. Both boundaries must lie between zero and the child
-- count, inclusive. Equal boundaries insert; an empty replacement deletes.
-- The node's kind is retained. No text, separators or trivia are synthesized.
--
-- Validation checks negative start, negative end, reversed range, start bounds,
-- then end bounds, in that order. Invalid input returns no partial result.
-- Untouched elements and the following list suffix are reused. Cost is a
-- traversal of immediate children and replacements to establish the new cache;
-- descendant text is not traversed. Inputs must be finite, as for 'greenNode'.
spliceChildren
  :: Int -> Int -> [GreenElement kind] -> GreenNode kind
  -> Either ChildEditError (GreenNode kind)
spliceChildren start end replacements node
  | start < 0 = Left (NegativeChildBoundary start)
  | end < 0 = Left (NegativeChildBoundary end)
  | end < start = Left (ReversedChildRange start end)
  | otherwise = greenNode (nodeKind node) <$> go start (nodeChildren node)
  where
    go 0 items = (replacements ++) <$> dropSelected (end - start) items
    go _ [] = Left (ChildBoundaryOutOfBounds start)
    go remaining (item : items) = (item :) <$> go (remaining - 1) items
    dropSelected 0 items = Right items
    dropSelected _ [] = Left (ChildBoundaryOutOfBounds end)
    dropSelected remaining (_ : items) = dropSelected (remaining - 1) items

-- | Insert one immediate child before the given index. The child count is a
-- valid position and appends; zero inserts into an empty node. Uses the same
-- validation, sharing and cache rules as 'spliceChildren'.
insertChild
  :: Int -> GreenElement kind -> GreenNode kind
  -> Either ChildEditError (GreenNode kind)
insertChild index element = spliceChildren index index [element]

-- | Remove one existing immediate child. Unlike insertion, the child count
-- is out of bounds. Negative indices return 'NegativeChildBoundary'; a missing
-- child returns 'ChildBoundaryOutOfBounds'. Validation never adds one to the
-- index, so even @maxBound :: Int@ cannot overflow. Retains the node's kind
-- and follows the same sharing and cache rules as 'spliceChildren'.
removeChild :: Int -> GreenNode kind -> Either ChildEditError (GreenNode kind)
removeChild index node
  | index < 0 = Left (NegativeChildBoundary index)
  | otherwise = greenNode (nodeKind node) <$> go index (nodeChildren node)
  where
    go _ [] = Left (ChildBoundaryOutOfBounds index)
    go 0 (_ : items) = Right items
    go remaining (item : items) = (item :) <$> go (remaining - 1) items

-- | Splice the children of the node selected by a root-relative path and
-- return a new root. An empty path selects the root node. Path validation
-- precedes target validation (a token has no editable children), which precedes
-- boundary validation. Successful edits rebuild ancestors using 'replaceAt'.
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
editNodeAt path edit root = do
  target <- first ChildEditPathError (elementAt path root)
  case target of
    Token _ -> Left CannotEditTokenChildren
    Node node -> do
      edited <- edit node
      first ChildEditPathError (replaceAt path (Node edited) root)

modifyChild
  :: Int
  -> (GreenElement kind -> Either PathError (GreenElement kind))
  -> GreenNode kind
  -> Either PathError (GreenNode kind)
modifyChild index edit node
  | index < 0 = Left (NegativeChildIndex index)
  | otherwise = greenNode (nodeKind node) <$> go index (nodeChildren node)
  where
    go _ [] = Left (ChildIndexOutOfBounds index)
    go 0 (child : children) = (: children) <$> edit child
    go n (child : children) = (child :) <$> go (n - 1) children

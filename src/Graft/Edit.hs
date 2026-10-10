-- | Persistent editing and lookup using paths of child indices.
module Graft.Edit
  ( Path
  , PathError (..)
  , elementAt
  , replaceAt
  , replaceChild
  ) where

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

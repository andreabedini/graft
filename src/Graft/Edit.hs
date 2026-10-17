-- | Persistent edits to green trees, selecting elements by child path. Each
-- edit returns a new tree and leaves the original usable; untouched subtrees
-- are shared between the two.
--
-- To make several edits in one region, or to edit what you found by
-- navigating, edit through a cursor instead ("Graft.Cursor").
--
-- >>> :set -XOverloadedStrings
-- >>> let tok = Token . greenToken 't'
-- >>> let tree = Node (greenNode 'l' [tok "a", tok ", ", tok "b"])
-- >>> renderElement <$> replaceAt [2] (tok "c") tree
-- Right "a, c"
-- >>> renderElement <$> spliceChildrenAt [] 3 3 [tok ", ", tok "d"] tree
-- Right "a, b, d"
-- >>> replaceAt [5] (tok "c") tree
-- Left (ChildIndexOutOfBounds 5)
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

-- | The zero-based index of a child at each level, from the root down. Every
-- child counts, tokens included, and @[]@ is the root itself. A path belongs
-- to one version of a tree: inserting or removing children shifts the paths
-- that follow.
type Path = [Int]

-- | Why a path doesn't lead to an element, reported for the first step that
-- fails. The index is the one at that step.
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

-- | The element at a path. Takes time proportional to the path's length.
elementAt :: Path -> GreenElement kind -> Either PathError (GreenElement kind)
elementAt [] root = Right root
elementAt (index : rest) root
  | index < 0 = Left (NegativeChildIndex index)
  | otherwise = case root of
      Token _ -> Left CannotDescendIntoToken
      Node node
        | index >= childCount node -> Left (ChildIndexOutOfBounds index)
        | otherwise -> elementAt rest (childAt node index)

-- | @replaceAt path replacement root@ replaces the element at @path@, which
-- may be @[]@ to replace the whole tree. The replacement can be of any kind,
-- node or token; Graft doesn't check it against a grammar.
--
-- Each ancestor on the path is rebuilt by copying its child array, so the
-- cost is the total width of the ancestors. Everything else is shared with
-- the original tree, which remains valid.
replaceAt :: Path -> GreenElement kind -> GreenElement kind -> Either PathError (GreenElement kind)
replaceAt [] replacement _ = Right replacement
replaceAt (index : rest) replacement root
  | index < 0 = Left (NegativeChildIndex index)
  | otherwise = case root of
      Token _ -> Left CannotDescendIntoToken
      Node node -> Node <$> modifyChild index (replaceAt rest replacement) node

-- | Replace one child of a node, as 'replaceAt' with a one-step path.
replaceChild :: Int -> GreenElement kind -> GreenNode kind -> Either PathError (GreenNode kind)
replaceChild index replacement = modifyChild index (const (Right replacement))

-- | Why a child edit failed. 'ChildEditPathError' wraps a path that leads
-- nowhere, 'CannotEditTokenChildren' means the target is a token, and the
-- others report a bad child position (or both positions of a reversed
-- range).
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

-- | 'spliceChildren' on the node at a path, returning the new tree. A bad
-- path is reported first, then a token target, then bad positions.
spliceChildrenAt
  :: Path -> Int -> Int -> [GreenElement kind] -> GreenElement kind
  -> Either ChildEditError (GreenElement kind)
spliceChildrenAt path start end replacements =
  editNodeAt path (spliceChildren start end replacements)

-- | 'insertChild' on the node at a path, returning the new tree.
insertChildAt
  :: Path -> Int -> GreenElement kind -> GreenElement kind
  -> Either ChildEditError (GreenElement kind)
insertChildAt path index element = editNodeAt path (insertChild index element)

-- | 'removeChild' on the node at a path, returning the new tree.
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

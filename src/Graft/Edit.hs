{-# LANGUAGE BangPatterns #-}

-- | Transformations of a tree that see the tree as it is and build the
-- result all at once. A rewrite visits a tree top-down and replaces elements
-- as it goes: every call sees the original tree, with its ranges and
-- ancestors, and each node above a replacement is rebuilt once. A node with
-- nothing replaced below it is returned as it is.
--
-- This module is meant to be imported qualified; its types are also
-- exported by "Graft":
--
-- > import Graft
-- > import qualified Graft.Edit as Edit
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Graft.Cursor as Cursor
-- >>> import qualified Graft.Edit as Edit
-- >>> import qualified Graft.Green as Green
-- >>> data Kind = Root | Name | Space deriving (Eq, Show)
-- >>> let tok kind = Token . Green.token kind
-- >>> let tree = Node (Green.node Root [tok Name "x", tok Space " ", tok Name "y"])
-- >>> let rename () cursor = if Cursor.kind cursor == Name then Replace (tok Name "z") else Descend
-- >>> Green.render (Edit.rewrite rename () tree)
-- "z z"
module Graft.Edit
  ( -- * Rewriting
    Change (..)
  , rewrite
  , rewriteM
  ) where

import Data.Functor.Identity (Identity (..))
import Graft.Cursor (Cursor)
import qualified Graft.Cursor as Cursor
import Graft.Green.Internal (GreenElement (..))
import qualified Graft.Green.Internal as Internal

-- | What a rewrite does with the element it is looking at. 'Descend' is not
-- "unchanged": it keeps the element's own form and goes on below it, where
-- replacements may still happen. 'Skip' is the answer for an unchanged
-- subtree. For a token, 'Descend' and 'Skip' are the same answer.
data Change ctx kind
  = Replace (GreenElement kind)
  -- ^ Put this element in place of the current one. Nothing below the
  -- current element is visited, and the replacement isn't visited either.
  | Descend
  -- ^ Keep the element's form and visit its children with the same context.
  | DescendWith ctx
  -- ^ Keep the element's form and visit its children with this context.
  | Skip
  -- ^ Keep the element and everything below it, unvisited.
  deriving (Eq, Show)

-- | Rewrite a tree top-down. The function is called on the root and then,
-- for each element it answers 'Descend' or 'DescendWith', on that element's
-- children in source order, with a cursor on the original tree and the
-- context from the nearest ancestor that answered 'DescendWith'. A node is
-- rebuilt, once, with one copy of its child array, exactly when something
-- below it was replaced; a node with no replacement below it is returned as
-- it is, so @rewrite (\\_ _ -> Skip)@ and @rewrite (\\_ _ -> Descend)@ give
-- back their argument. Answering 'Replace' with the original element counts
-- as a replacement.
--
-- The cost is proportional to the number of elements visited, which covers
-- the rebuilding, plus the calls. The function never sees a replacement,
-- its own or another's: to decide on the basis of a change, rewrite the
-- result again.
rewrite :: (ctx -> Cursor kind -> Change ctx kind) -> ctx -> GreenElement kind -> GreenElement kind
rewrite step ctx root = runIdentity (rewriteM (\context cursor -> Identity (step context cursor)) ctx root)
{-# INLINE rewrite #-}

-- | 'rewrite' with effects, which run in preorder: an element's before its
-- children's, and a child's before its next sibling's. An effect that stops
-- the computation, such as 'Left', stops the rewrite; in @StateT s (Either
-- e)@, say, a walk can collect the ranges it replaced and give up at the
-- first problem. Each rebuilt node is evaluated before it is returned
-- through the monad, so no rebuilding is left suspended in the result.
-- 'rewrite' is this function in t'Identity'.
rewriteM
  :: Monad m
  => (ctx -> Cursor kind -> m (Change ctx kind))
  -> ctx -> GreenElement kind -> m (GreenElement kind)
rewriteM step = \ctx root -> snd <$> element ctx (Cursor.fromGreen root)
  where
    -- Each result says whether it differs from the original, so a parent
    -- knows to rebuild without comparing trees.
    element ctx cursor = do
      change <- step ctx cursor
      case change of
        Replace replacement -> pure (True, replacement)
        Skip -> pure (False, Cursor.green cursor)
        Descend -> below ctx cursor
        DescendWith inner -> below inner cursor
    below ctx cursor = case original of
      Token _ -> pure (False, original)
      Node node -> do
        replaced <- children ctx (Cursor.children cursor) 0 []
        case replaced of
          [] -> pure (False, original)
          _ -> let !rebuilt = Internal.setChildren (reverse replaced) node
               in pure (True, Node rebuilt)
      where
        original = Cursor.green cursor
    -- The replaced children, newest first, with their positions.
    children _ [] !_ replaced = pure replaced
    children ctx (child : rest) !index replaced = do
      (changed, result) <- element ctx child
      children ctx rest (index + 1) (if changed then (index, result) : replaced else replaced)
{-# INLINE rewriteM #-}

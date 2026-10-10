{-# LANGUAGE TypeFamilies #-}

-- | Application-defined typed views over immutable syntax occurrences.
-- Views retain concrete text, trivia, paths and ranges through their cursor.
-- Casting is not parsing or grammar validation; missing fields can remain
-- optional even when a node's kind permits a view.
module Graft.Ast
  ( AstNode (..)
  , AstToken (..)
  , matchNode
  , matchToken
  , astChild
  , astChildren
  , astParent
  , astAncestors
  , astToken
  , astTokens
  ) where

import Data.Maybe (listToMaybe, mapMaybe)
import Graft.Cursor
import Graft.Green

-- | A typed node view, usually a newtype around a 'Cursor'. The associated
-- kind type is the application's existing syntax-kind type, with no integer
-- conversion or separate language tag required.
--
-- Instances must reject token cursors. If @castNode cursor = Just view@,
-- @nodeSyntax view@ must retain that exact occurrence in the same snapshot.
-- Casting a view's own syntax must succeed again. Instances must not detach,
-- rebuild, normalize or change the cursor while casting. Hiding application
-- wrapper constructors helps preserve these laws; the type class cannot enforce
-- them. 'matchNode' supplies the usual form-and-kind check without inspecting
-- children, allowing incomplete or malformed nodes to have typed views.
class AstNode ast where
  -- | Syntax-kind type used by this node view.
  type AstNodeKind ast
  -- | Try to view a node occurrence. Failure is represented by 'Nothing'.
  castNode :: Cursor (AstNodeKind ast) -> Maybe ast
  -- | The original syntax cursor, retaining all concrete text and context.
  nodeSyntax :: ast -> Cursor (AstNodeKind ast)

-- | A typed token view. Instances obey the same occurrence and round-trip
-- laws as 'AstNode', but must reject node cursors. Empty token text is valid.
-- 'matchToken' checks form and kind; interpreting spelling belongs to callers.
class AstToken ast where
  -- | Syntax-kind type used by this token view.
  type AstTokenKind ast
  -- | Try to view a token occurrence, without changing its text or context.
  castToken :: Cursor (AstTokenKind ast) -> Maybe ast
  -- | The original token cursor, including its snapshot and source range.
  tokenSyntax :: ast -> Cursor (AstTokenKind ast)

-- | @matchNode accepts wrap cursor@ checks node form and the kind predicate,
-- then passes the original cursor to the wrapper. It never traverses children.
-- Typical instance: @castNode = matchNode (== ExpressionKind) Expression@.
-- The wrapper must retain the cursor unchanged to satisfy the 'AstNode' laws.
matchNode :: (kind -> Bool) -> (Cursor kind -> ast) -> Cursor kind -> Maybe ast
matchNode accepts wrap cursor = case cursorGreen cursor of
  Node node | accepts (nodeKind node) -> Just (wrap cursor)
  _ -> Nothing

-- | Token counterpart of 'matchNode'. Rejects nodes even if their kind matches.
-- Allows empty or malformed token text; the predicate checks only the kind.
matchToken :: (kind -> Bool) -> (Cursor kind -> ast) -> Cursor kind -> Maybe ast
matchToken accepts wrap cursor = case cursorGreen cursor of
  Token token | accepts (tokenKind token) -> Just (wrap cursor)
  _ -> Nothing

-- | First matching immediate node child in source order, or 'Nothing'. Does
-- not search grandchildren. Stops when the first cast succeeds.
astChild :: AstNode ast => Cursor (AstNodeKind ast) -> Maybe ast
astChild = listToMaybe . astChildren

-- | Matching immediate node children, in source order. Tokens and failed casts
-- are skipped. A token focus returns an empty list. Results are lazy and keep
-- each occurrence's original cursor; repeated green subtrees remain distinct.
astChildren :: AstNode ast => Cursor (AstNodeKind ast) -> [ast]
astChildren = mapMaybe castNode . filter isNode . children

-- | Cast only the immediate parent to the requested node view. Does not skip
-- a mismatched parent to search higher ancestors. A root returns 'Nothing'.
-- Works from either a node or a token cursor.
astParent :: AstNode ast => Cursor (AstNodeKind ast) -> Maybe ast
astParent cursor = parent cursor >>= castNode

-- | Matching ancestors, nearest first, excluding the focus. Skips failed casts
-- and retains their original context. Results are lazy; the root returns @[]@.
astAncestors :: AstNode ast => Cursor (AstNodeKind ast) -> [ast]
astAncestors cursor = case parent cursor of
  Nothing -> []
  Just above -> mapMaybe castNode (ancestors above)

-- | First matching immediate token child, including empty tokens. Does not
-- search inside child nodes. Stops when the first cast succeeds.
astToken :: AstToken ast => Cursor (AstTokenKind ast) -> Maybe ast
astToken = listToMaybe . astTokens

-- | Matching immediate token children in source order, skipping nodes and
-- failed casts. A token focus returns @[]@ rather than itself. Results are lazy.
astTokens :: AstToken ast => Cursor (AstTokenKind ast) -> [ast]
astTokens = mapMaybe castToken . filter (not . isNode) . children

isNode :: Cursor kind -> Bool
isNode cursor = case cursorGreen cursor of
  Node _ -> True
  Token _ -> False

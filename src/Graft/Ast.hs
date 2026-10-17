{-# LANGUAGE TypeFamilies #-}

-- | Typed views of your grammar's nodes and tokens.
--
-- A view is a newtype over a cursor with a cast that checks the element's
-- kind. Accessors written with 'astChild', 'astToken' and friends then give
-- typed access to fields, and every view keeps its exact text, range and
-- context, so there is no separate AST to keep in sync with the source.
--
-- @
-- newtype Call = Call (Cursor Kind)
--
-- instance AstNode Call where
--   type AstNodeKind Call = Kind
--   castNode = matchNode (== CallKind) Call
--   nodeSyntax (Call cursor) = cursor
--
-- newtype Name = Name (Cursor Kind)
--
-- instance AstToken Name where
--   type AstTokenKind Name = Kind
--   castToken = matchToken (== NameKind) Name
--   tokenSyntax (Name cursor) = cursor
--
-- -- The function being called, if the call has one.
-- callee :: Call -> Maybe Name
-- callee = astToken . nodeSyntax
-- @
--
-- A cast checks only the kind, not the grammar, so a call with a missing
-- callee still casts and @callee@ returns 'Nothing'. Keep the view
-- constructors private so that views are only made by casting.
module Graft.Ast
  ( -- * Views
    AstNode (..)
  , AstToken (..)
  , matchNode
  , matchToken
    -- * Fields
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

-- | A typed view of a node, usually a newtype around a 'Cursor'.
--
-- Instances should satisfy these laws, which 'matchNode' does:
--
-- * 'castNode' rejects token cursors.
-- * If @castNode cursor == Just view@, then @nodeSyntax view@ is @cursor@,
--   unchanged.
-- * Casting @nodeSyntax view@ succeeds again.
class AstNode ast where
  -- | The kind type of the trees this view applies to.
  type AstNodeKind ast
  -- | View a node, or 'Nothing' if it isn't one of this view's kinds.
  castNode :: Cursor (AstNodeKind ast) -> Maybe ast
  -- | The cursor the view was made from.
  nodeSyntax :: ast -> Cursor (AstNodeKind ast)

-- | A typed view of a token, with the same laws as 'AstNode' except that
-- 'castToken' rejects node cursors. Interpreting the token's text, for
-- example reading a number, is up to the application.
class AstToken ast where
  -- | The kind type of the trees this view applies to.
  type AstTokenKind ast
  -- | View a token, or 'Nothing' if it isn't one of this view's kinds.
  castToken :: Cursor (AstTokenKind ast) -> Maybe ast
  -- | The cursor the view was made from.
  tokenSyntax :: ast -> Cursor (AstTokenKind ast)

-- | @matchNode accepts wrap@ is a 'castNode' that wraps any node whose kind
-- satisfies @accepts@, as in @castNode = matchNode (== CallKind) Call@. It
-- doesn't look at the node's children.
matchNode :: (kind -> Bool) -> (Cursor kind -> ast) -> Cursor kind -> Maybe ast
matchNode accepts wrap cursor = case cursorGreen cursor of
  Node node | accepts (nodeKind node) -> Just (wrap cursor)
  _ -> Nothing

-- | The 'castToken' counterpart of 'matchNode'. It checks only the kind, not
-- the text.
matchToken :: (kind -> Bool) -> (Cursor kind -> ast) -> Cursor kind -> Maybe ast
matchToken accepts wrap cursor = case cursorGreen cursor of
  Token token | accepts (tokenKind token) -> Just (wrap cursor)
  _ -> Nothing

-- | The first child node that casts to the view, or 'Nothing'. Only direct
-- children are searched.
astChild :: AstNode ast => Cursor (AstNodeKind ast) -> Maybe ast
astChild = listToMaybe . astChildren

-- | Every child node that casts to the view, in source order. The list is
-- lazy.
astChildren :: AstNode ast => Cursor (AstNodeKind ast) -> [ast]
astChildren = mapMaybe castNode . filter isNode . cursorChildren

-- | The parent, if it casts to the view. Ancestors further up are not
-- searched; use 'astAncestors' for that.
astParent :: AstNode ast => Cursor (AstNodeKind ast) -> Maybe ast
astParent cursor = cursorParent cursor >>= castNode

-- | Every ancestor that casts to the view, nearest first, not including the
-- cursor itself. The list is lazy.
astAncestors :: AstNode ast => Cursor (AstNodeKind ast) -> [ast]
astAncestors cursor = case cursorParent cursor of
  Nothing -> []
  Just above -> mapMaybe castNode (cursorAncestors above)

-- | The first child token that casts to the view, or 'Nothing'. Tokens inside
-- child nodes are not searched.
astToken :: AstToken ast => Cursor (AstTokenKind ast) -> Maybe ast
astToken = listToMaybe . astTokens

-- | Every child token that casts to the view, in source order. The list is
-- lazy.
astTokens :: AstToken ast => Cursor (AstTokenKind ast) -> [ast]
astTokens = mapMaybe castToken . filter (not . isNode) . cursorChildren

isNode :: Cursor kind -> Bool
isNode cursor = case cursorGreen cursor of
  Node _ -> True
  Token _ -> False

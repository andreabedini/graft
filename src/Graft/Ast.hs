{-# LANGUAGE TypeFamilies #-}

-- | Typed views of your grammar's nodes and tokens.
--
-- A view is a newtype over a cursor with a cast that checks the element's
-- kind. Accessors written with 'child', 'token' and friends then give
-- typed access to fields, and every view keeps its exact text, range and
-- context, so there is no separate AST to keep in sync with the source.
--
-- This module is meant to be imported qualified; its classes are also
-- exported by "Graft", so instances can be written without qualification:
--
-- > import Graft
-- > import qualified Graft.Ast as Ast
-- >
-- > newtype Call = Call (Cursor Kind)
-- >
-- > instance AstNode Call where
-- >   type AstNodeKind Call = Kind
-- >   castNode = Ast.matchNode (== CallKind) Call
-- >   nodeSyntax (Call cursor) = cursor
-- >
-- > newtype Name = Name (Cursor Kind)
-- >
-- > instance AstToken Name where
-- >   type AstTokenKind Name = Kind
-- >   castToken = Ast.matchToken (== NameKind) Name
-- >   tokenSyntax (Name cursor) = cursor
-- >
-- > -- The function being called, if the call has one.
-- > callee :: Call -> Maybe Name
-- > callee = Ast.token . nodeSyntax
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
  , child
  , children
  , parent
  , ancestors
  , token
  , tokens
  ) where

import Data.Maybe (listToMaybe, mapMaybe)
import Graft.Cursor (Cursor)
import qualified Graft.Cursor as Cursor
import Graft.Green.Internal (GreenElement (..), GreenNode (..), GreenToken (..))

-- | A typed view of a node, usually a newtype around a 'Cursor'.
--
-- Instances should satisfy these laws, which 'matchNode' does:
--
-- * 'castNode' rejects token cursors.
-- * Whether 'castNode' succeeds depends only on the focused element,
--   'Cursor.green', not on where it is: two cursors on equal elements are
--   both accepted or both rejected.
-- * If @castNode cursor@ is @Just view@, then @nodeSyntax view@ is that
--   cursor, unchanged, with the same path, range and element.
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
-- satisfies @accepts@, as in @castNode = Ast.matchNode (== CallKind) Call@. It
-- doesn't look at the node's children.
matchNode :: (kind -> Bool) -> (Cursor kind -> ast) -> Cursor kind -> Maybe ast
matchNode accepts wrap cursor = case Cursor.green cursor of
  Node (GreenNode nodeKind _ _ _) | accepts nodeKind -> Just (wrap cursor)
  _ -> Nothing

-- | The 'castToken' counterpart of 'matchNode'. It checks only the kind, not
-- the text.
matchToken :: (kind -> Bool) -> (Cursor kind -> ast) -> Cursor kind -> Maybe ast
matchToken accepts wrap cursor = case Cursor.green cursor of
  Token (GreenToken tokenKind _ _) | accepts tokenKind -> Just (wrap cursor)
  _ -> Nothing

-- | The first child node that casts to the view, or 'Nothing'. Only direct
-- children are searched.
child :: AstNode ast => Cursor (AstNodeKind ast) -> Maybe ast
child = listToMaybe . children

-- | Every child node that casts to the view, in source order. The list is
-- lazy.
children :: AstNode ast => Cursor (AstNodeKind ast) -> [ast]
children = mapMaybe castNode . filter isNode . Cursor.children

-- | The parent, if it casts to the view. Ancestors further up are not
-- searched; use 'ancestors' for that.
parent :: AstNode ast => Cursor (AstNodeKind ast) -> Maybe ast
parent cursor = Cursor.parent cursor >>= castNode

-- | Every ancestor that casts to the view, nearest first, not including the
-- cursor itself. The list is lazy.
ancestors :: AstNode ast => Cursor (AstNodeKind ast) -> [ast]
ancestors = mapMaybe castNode . Cursor.ancestors

-- | The first child token that casts to the view, or 'Nothing'. Tokens inside
-- child nodes are not searched.
token :: AstToken ast => Cursor (AstTokenKind ast) -> Maybe ast
token = listToMaybe . tokens

-- | Every child token that casts to the view, in source order. Tokens inside
-- child nodes are not included; 'Cursor.descendantTokens' finds those. The
-- list is lazy.
tokens :: AstToken ast => Cursor (AstTokenKind ast) -> [ast]
tokens = mapMaybe castToken . filter (not . isNode) . Cursor.children

isNode :: Cursor kind -> Bool
isNode cursor = case Cursor.green cursor of
  Node _ -> True
  Token _ -> False

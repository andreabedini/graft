{-# LANGUAGE TypeFamilies #-}

-- | Typed views of your grammar's nodes and tokens.
--
-- A view is a newtype over a cursor with a cast that checks the element's
-- kind. Accessors written with 'child', 'token' and friends then give
-- typed access to fields, and every view keeps its exact text, range and
-- context, so there is no separate AST to keep in sync with the source.
--
-- The classes follow the shape of @Exception@ in "Control.Exception", with
-- 'Cursor' in the place of @SomeException@: 'fromCursor'
-- is a cast that can fail, and 'toCursor' gives back the cursor. 'AstView'
-- holds the cast, and the empty classes 'AstNode' and 'AstToken' say
-- whether the view is of nodes or of tokens.
--
-- This module is meant to be imported qualified; its classes are also
-- exported by "Graft", so instances can be written without qualification:
--
-- > import Graft
-- > import qualified Graft.Ast as Ast
-- >
-- > newtype Call = Call (Cursor Kind)
-- >
-- > instance AstView Call where
-- >   type AstViewKind Call = Kind
-- >   fromCursor = Ast.matchNode (== CallKind) Call
-- >   toCursor (Call cursor) = cursor
-- >
-- > instance AstNode Call
-- >
-- > newtype Name = Name (Cursor Kind)
-- >
-- > instance AstView Name where
-- >   type AstViewKind Name = Kind
-- >   fromCursor = Ast.matchToken (== NameKind) Name
-- >   toCursor (Name cursor) = cursor
-- >
-- > instance AstToken Name
-- >
-- > -- The function being called, if the call has one.
-- > callee :: Call -> Maybe Name
-- > callee = Ast.token . toCursor
--
-- A cast checks only the kind, not the grammar, so a call with a missing
-- callee still casts and @callee@ returns 'Nothing'. Keep the view
-- constructors private so that views are only made by casting.
module Graft.Ast
  ( -- * Views
    AstView (..)
  , AstNode
  , AstToken
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

-- | A typed view of a node or a token, usually a newtype around a 'Cursor'.
-- Declare an 'AstNode' or 'AstToken' instance too, to say which it is.
--
-- Instances should satisfy these laws, which 'matchNode' and 'matchToken'
-- do:
--
-- * Whether 'fromCursor' succeeds depends only on the focused element,
--   'Cursor.green', not on where it is: two cursors on equal elements are
--   both accepted or both rejected.
-- * If @fromCursor cursor@ is @Just view@, then @toCursor view@ is that
--   cursor, unchanged, with the same path, range and element.
-- * Casting @toCursor view@ succeeds again.
class AstView view where
  -- | The kind type of the trees this view applies to.
  type AstViewKind view
  -- | View an element, or 'Nothing' if it isn't one of this view's kinds.
  fromCursor :: Cursor (AstViewKind view) -> Maybe view
  -- | The cursor the view was made from.
  toCursor :: view -> Cursor (AstViewKind view)

-- | A view of nodes. In addition to the 'AstView' laws, 'fromCursor' must
-- reject token cursors; 'matchNode' does. 'child', 'children', 'parent' and
-- 'ancestors' look for these views.
class AstView view => AstNode view

-- | A view of tokens. In addition to the 'AstView' laws, 'fromCursor' must
-- reject node cursors; 'matchToken' does. 'token' and 'tokens' look for these
-- views. Interpreting the token's text, for example reading a number, is up to
-- the application.
class AstView view => AstToken view

-- | @matchNode accepts wrap@ is a 'fromCursor' for an 'AstNode' view. It wraps
-- any node whose kind satisfies @accepts@, as in
-- @fromCursor = Ast.matchNode (== CallKind) Call@, and rejects tokens. It
-- looks at the kind only, not at the node's children.
matchNode :: (kind -> Bool) -> (Cursor kind -> view) -> Cursor kind -> Maybe view
matchNode accepts wrap cursor
  | not (Cursor.isToken cursor), accepts (Cursor.kind cursor) = Just (wrap cursor)
  | otherwise = Nothing

-- | The 'AstToken' counterpart of 'matchNode'. It rejects nodes and checks
-- only the kind, not the text.
matchToken :: (kind -> Bool) -> (Cursor kind -> view) -> Cursor kind -> Maybe view
matchToken accepts wrap cursor
  | Cursor.isToken cursor, accepts (Cursor.kind cursor) = Just (wrap cursor)
  | otherwise = Nothing

-- | The first child node that casts to the view, or 'Nothing'. Only direct
-- children are searched.
child :: AstNode view => Cursor (AstViewKind view) -> Maybe view
child = listToMaybe . children

-- | Every child node that casts to the view, in source order. The list is
-- lazy.
children :: AstNode view => Cursor (AstViewKind view) -> [view]
children = mapMaybe fromCursor . filter (not . Cursor.isToken) . Cursor.children

-- | The parent, if it casts to the view. Ancestors further up are not
-- searched; use 'ancestors' for that.
parent :: AstNode view => Cursor (AstViewKind view) -> Maybe view
parent cursor = Cursor.parent cursor >>= fromCursor

-- | Every ancestor that casts to the view, nearest first, not including the
-- cursor itself. The list is lazy.
ancestors :: AstNode view => Cursor (AstViewKind view) -> [view]
ancestors = mapMaybe fromCursor . Cursor.ancestors

-- | The first child token that casts to the view, or 'Nothing'. Tokens inside
-- child nodes are not searched.
token :: AstToken view => Cursor (AstViewKind view) -> Maybe view
token = listToMaybe . tokens

-- | Every child token that casts to the view, in source order. Tokens inside
-- child nodes are not included; 'Cursor.descendantTokens' finds those. The
-- list is lazy.
tokens :: AstToken view => Cursor (AstViewKind view) -> [view]
tokens = mapMaybe fromCursor . filter Cursor.isToken . Cursor.children

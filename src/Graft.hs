-- | Lossless, immutable concrete syntax trees, modelled on rust-analyzer's
-- Rowan.
--
-- A Graft tree keeps every byte of the source: whitespace, comments and
-- malformed fragments are ordinary tokens. You navigate the tree with
-- cursors, find syntax by byte offset, edit it, and print it back with the
-- untouched text exactly as it was. Lexing and parsing are up to you.
--
-- This module exports the library's types and classes. The functions live in
-- modules meant to be imported qualified:
--
-- > import Graft
-- > import qualified Graft.Ast as Ast
-- > import qualified Graft.Build as Build
-- > import qualified Graft.Builder as Builder
-- > import qualified Graft.Cursor as Cursor
-- > import qualified Graft.Green as Green
-- > import qualified Graft.LineIndex as LineIndex
-- > import qualified Graft.Range as Range
--
-- * "Graft.Green": immutable nodes and tokens with cached lengths, and edits
--   to a node's children.
-- * "Graft.Cursor": navigation, offset lookup and editing through a zipper.
-- * "Graft.Build" and "Graft.Builder": building trees from a parser's events,
--   in a monad or as a pure value.
-- * "Graft.Ast": typed views of your grammar's nodes and tokens.
-- * "Graft.Range": half-open byte ranges.
-- * "Graft.LineIndex": line and column positions in UTF-8 or UTF-16 units.
--
-- All lengths and offsets are t'TextLength', a count of UTF-8 bytes.
--
-- Importing "Graft" unqualified brings in constructors with common names,
-- such as 'Node' and 'Token', and the type synonym t'Path'. If they clash
-- with other imports, import only what you use, as in
-- @import Graft (GreenElement (..), TextLength)@, or import "Graft"
-- qualified too.
--
-- = Example
--
-- Build a tree, find the number at byte offset 5, replace it and render:
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Graft.Build as Build
-- >>> import qualified Graft.Cursor as Cursor
-- >>> import qualified Graft.Green as Green
-- >>> import qualified Graft.Range as Range
-- >>> data Kind = Root | Binding | Name | Number | Trivia deriving (Eq, Show)
-- >>> :{
-- let Right tree = Build.exec $ Build.node Root $ do
--       Build.node Binding $ do
--         Build.token Name "x"
--         Build.token Trivia "  = "
--         Build.token Number "1"
--       Build.token Trivia " -- keep\r\n"
-- :}
--
-- >>> Green.renderNode tree
-- "x  = 1 -- keep\r\n"
-- >>> let Just number = Cursor.covering (Range.fromLength 5 1) (Cursor.fromGreen (Node tree))
-- >>> Cursor.text number
-- "1"
-- >>> Green.render (Cursor.tree (Cursor.replace (Token (Green.token Number "42")) number))
-- "x  = 42 -- keep\r\n"
module Graft
  ( -- * Trees
    GreenNode
  , GreenToken
  , GreenElement (..)
  , TextLength (..)
  , ChildEditError (..)
    -- * Cursors
  , Cursor
  , Path
  , PathError (..)
  , TokenAtOffset (..)
  , WalkEvent (..)
    -- * Ranges and positions
  , TextRange
  , LineIndex
  , Utf8Position (..)
  , Utf16Position (..)
    -- * Building
  , Build
  , Builder
  , Checkpoint
  , BuilderError (..)
    -- * Typed views
  , AstNode (..)
  , AstToken (..)
  ) where

import Graft.Ast (AstNode (..), AstToken (..))
import Graft.Build (Build)
import Graft.Builder (Builder, BuilderError (..), Checkpoint)
import Graft.Cursor (Cursor, Path, PathError (..), TokenAtOffset (..), WalkEvent (..))
import Graft.Green (ChildEditError (..), GreenElement (..), GreenNode, GreenToken, TextLength (..))
import Graft.LineIndex (LineIndex, Utf16Position (..), Utf8Position (..))
import Graft.Range (TextRange)

-- | Lossless, immutable concrete syntax trees, modelled on rust-analyzer's
-- Rowan.
--
-- A Graft tree keeps every byte of the source: whitespace, comments and
-- malformed fragments are ordinary tokens. You navigate the tree with
-- cursors, find syntax by byte offset, edit it, and print it back with the
-- untouched text exactly as it was. Lexing and parsing are up to you.
--
-- This module re-exports the whole library:
--
-- * "Graft.Green": immutable nodes and tokens with cached lengths.
-- * "Graft.Cursor": navigation, offset lookup and editing through a zipper.
-- * "Graft.Builder": building trees from a parser's events.
-- * "Graft.Ast": typed views of your grammar's nodes and tokens.
-- * "Graft.Edit": editing green trees by child path.
-- * "Graft.Range": half-open byte ranges.
-- * "Graft.LineIndex": line, column and UTF-16 positions.
--
-- All lengths and offsets are 'TextLength', a count of UTF-8 bytes.
--
-- = Example
--
-- Build a tree, find the number at byte offset 5, replace it and render:
--
-- >>> :set -XOverloadedStrings
-- >>> data Kind = Root | Binding | Name | Number | Trivia deriving (Eq, Show)
-- >>> :{
-- let Right tree = execBuild $ buildNode Root $ do
--       buildNode Binding $ do
--         buildToken Name "x"
--         buildToken Trivia "  = "
--         buildToken Number "1"
--       buildToken Trivia " -- keep\r\n"
-- :}
--
-- >>> renderNode tree
-- "x  = 1 -- keep\r\n"
-- >>> let Just number = coveringElement (rangeFromLength 5 1) (rootCursor (Node tree))
-- >>> cursorText number
-- "1"
-- >>> renderElement (cursorTree (replaceFocus (Token (greenToken Number "42")) number))
-- "x  = 42 -- keep\r\n"
module Graft
  ( module Graft.Green
  , module Graft.Edit
  , module Graft.Range
  , module Graft.Cursor
  , module Graft.Builder
  , module Graft.Ast
  , module Graft.LineIndex
  ) where

import Graft.Ast
import Graft.Builder
import Graft.Cursor
import Graft.Edit
import Graft.Green
import Graft.LineIndex
import Graft.Range

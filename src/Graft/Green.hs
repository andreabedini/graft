-- | Immutable syntax without parents or absolute source positions.
--
-- Supply every source fragment as token text, including whitespace and comments.
-- Nodes and tokens are abstract so callers cannot invalidate cached lengths.
-- Children must form finite, acyclic trees (shared subtrees are allowed).
module Graft.Green
  ( GreenNode
  , GreenToken
  , GreenElement (..)
  , TextLength
  , greenNode
  , greenToken
  , nodeKind
  , tokenKind
  , elementKind
  , nodeChildren
  , tokenText
  , nodeTextLength
  , tokenTextLength
  , elementTextLength
  , renderNode
  , renderToken
  , renderElement
  ) where

import qualified Data.ByteString as BS
import qualified Data.Foldable as Foldable
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Numeric.Natural (Natural)

-- | A non-negative, unbounded count of UTF-8 bytes, including all trivia.
-- This is neither a character count nor a UTF-16 code-unit count.
type TextLength = Natural

-- | An internal node with ordered children and a cached total text length.
-- Equality is structural, including kinds, rather than pointer identity.
data GreenNode kind = GreenNode !kind !TextLength [GreenElement kind]
  deriving (Eq, Show)

-- | A leaf containing exact Unicode text and its cached UTF-8 byte length.
-- Empty text is allowed. No normalization is performed.
data GreenToken kind = GreenToken !kind !TextLength !Text
  deriving (Eq, Show)

-- | A node or token. Both can occur as children of a node.
data GreenElement kind
  = Node !(GreenNode kind)
  | Token !(GreenToken kind)
  deriving (Eq, Show)

-- | Construct a node, summing the cached lengths of its immediate children.
-- Takes time proportional to the number of children, without rendering them.
-- An empty child list is allowed; the supplied child values are reused.
greenNode :: kind -> [GreenElement kind] -> GreenNode kind
greenNode kind children =
  GreenNode kind (Foldable.foldl' (\n child -> n + elementTextLength child) 0 children) children

-- | Construct a token. Counting bytes currently encodes the text once, taking
-- time and temporary space proportional to its UTF-8 size.
greenToken :: kind -> Text -> GreenToken kind
greenToken kind text =
  GreenToken kind (fromIntegral (BS.length (Encoding.encodeUtf8 text))) text

-- | The caller-defined kind of a node.
nodeKind :: GreenNode kind -> kind
nodeKind (GreenNode kind _ _) = kind

-- | The caller-defined kind of a token.
tokenKind :: GreenToken kind -> kind
tokenKind (GreenToken kind _ _) = kind

-- | The caller-defined kind of either element.
elementKind :: GreenElement kind -> kind
elementKind (Node node) = nodeKind node
elementKind (Token token) = tokenKind token

-- | Immediate children in source order, including trivia tokens.
nodeChildren :: GreenNode kind -> [GreenElement kind]
nodeChildren (GreenNode _ _ children) = children

-- | Exact token text, including whatever trivia the caller supplied.
tokenText :: GreenToken kind -> Text
tokenText (GreenToken _ _ text) = text

-- | Read a node's cached UTF-8 byte length without traversing its children.
nodeTextLength :: GreenNode kind -> TextLength
nodeTextLength (GreenNode _ len _) = len

-- | Read a token's cached UTF-8 byte length without traversing its text.
tokenTextLength :: GreenToken kind -> TextLength
tokenTextLength (GreenToken _ len _) = len

-- | Read the cached UTF-8 byte length of either element.
elementTextLength :: GreenElement kind -> TextLength
elementTextLength (Node node) = nodeTextLength node
elementTextLength (Token token) = tokenTextLength token

-- | Concatenate token text in source order, preserving it exactly.
-- Uses a text builder, avoiding repeated strict-text concatenation.
-- Output must fit in a strict 'Text'; extremely deep trees may exhaust the stack.
renderNode :: GreenNode kind -> Text
renderNode = renderElement . Node

-- | Render a token by returning its exact text.
renderToken :: GreenToken kind -> Text
renderToken = tokenText

-- | Render either element losslessly. See 'renderNode' for limits.
renderElement :: GreenElement kind -> Text
renderElement = Lazy.toStrict . Builder.toLazyText . build
  where
    build (Token token) = Builder.fromText (tokenText token)
    build (Node node) = foldMap build (nodeChildren node)

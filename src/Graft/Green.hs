-- | Immutable syntax without parents or absolute source positions.
--
-- Supply every source fragment as token text, including whitespace and comments.
-- Nodes and tokens are abstract so callers cannot invalidate cached lengths.
-- Children must form finite, acyclic trees (shared subtrees are allowed).
module Graft.Green
  ( GreenNode
  , GreenToken
  , GreenElement (..)
  , TextLength (..)
  , textLengthOf
  , greenNode
  , greenToken
  , nodeKind
  , tokenKind
  , elementKind
  , nodeChildren
  , nodeChildCount
  , nodeChildAt
  , tokenText
  , nodeTextLength
  , tokenTextLength
  , elementTextLength
  , renderNode
  , renderToken
  , renderElement
  ) where

import Control.Monad.ST (runST)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Array as Array
import Data.Text.Internal (Text (..))
import Graft.Green.Internal

-- | The caller-defined kind of a node.
nodeKind :: GreenNode kind -> kind
nodeKind (GreenNode kind _ _ _) = kind

-- | The caller-defined kind of a token.
tokenKind :: GreenToken kind -> kind
tokenKind (GreenToken kind _ _) = kind

-- | The caller-defined kind of either element.
elementKind :: GreenElement kind -> kind
elementKind (Node node) = nodeKind node
elementKind (Token token) = tokenKind token

-- | Immediate children in source order, including trivia tokens. The list is
-- produced lazily from the node's array of children.
nodeChildren :: GreenNode kind -> [GreenElement kind]
nodeChildren (GreenNode _ _ children _) = toList children

-- | Number of immediate children, in constant time.
nodeChildCount :: GreenNode kind -> Int
nodeChildCount = childCount

-- | The immediate child at a zero-based index, in constant time.
nodeChildAt :: Int -> GreenNode kind -> Maybe (GreenElement kind)
nodeChildAt index node
  | index < 0 || index >= childCount node = Nothing
  | otherwise = Just (childAt node index)

-- | Exact token text, including whatever trivia the caller supplied.
tokenText :: GreenToken kind -> Text
tokenText (GreenToken _ _ text) = text

-- | Read a node's cached UTF-8 byte length without traversing its children.
nodeTextLength :: GreenNode kind -> TextLength
nodeTextLength (GreenNode _ len _ _) = len

-- | Read a token's cached UTF-8 byte length without traversing its text.
tokenTextLength :: GreenToken kind -> TextLength
tokenTextLength (GreenToken _ len _) = len

-- | Concatenate token text in source order, preserving it exactly. The
-- cached length sizes the result, so the text is copied once into a single
-- allocation. Output must fit in a strict 'Text'; shared subtrees count once
-- per occurrence. Extremely deep trees may exhaust the stack.
renderNode :: GreenNode kind -> Text
renderNode = renderElement . Node

-- | Render a token by returning its exact text.
renderToken :: GreenToken kind -> Text
renderToken = tokenText

-- | Render either element losslessly. See 'renderNode' for limits.
renderElement :: GreenElement kind -> Text
renderElement (Token token) = tokenText token
renderElement element
  | total == 0 = Text.empty
  | otherwise = runST $ do
      target <- Array.new bytes
      let write (TextLength offset) (Token (GreenToken _ (TextLength len) (Text source start _))) =
            Array.copyI len target offset source start
          write offset (Node node) = go 0
            where
              count = childCount node
              go index
                | index == count = pure ()
                | otherwise = do
                    write (offset + childStart node index) (childAt node index)
                    go (index + 1)
      write 0 element
      result <- Array.unsafeFreeze target
      pure (Text result 0 bytes)
  where
    total = elementTextLength element
    bytes = fromIntegral total

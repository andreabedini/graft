{-# LANGUAGE BangPatterns #-}

-- | Green trees: immutable nodes and tokens, tagged with your own kind type.
--
-- Every fragment of the source, whitespace and comments included, is the text
-- of some token, so rendering a tree gives back the source exactly. Green
-- values have no parents or positions, so a subtree can appear in several
-- places and in several versions of a tree. Use a cursor ("Graft.Cursor") to
-- see where an element is.
--
-- >>> :set -XOverloadedStrings
-- >>> data Kind = Call | Name | Punct | Space deriving (Eq, Show)
-- >>> let tok kind = Token . greenToken kind
-- >>> let call = greenNode Call [tok Name "f", tok Punct "(", tok Space " ", tok Punct ")"]
-- >>> renderNode call
-- "f( )"
-- >>> nodeTextLength call
-- 4
-- >>> nodeChildAt 1 call
-- Just (Token (greenToken Punct "("))
module Graft.Green
  ( -- * Trees
    GreenNode
  , GreenToken
  , GreenElement (..)
  , greenNode
  , greenToken
    -- * Lengths
  , TextLength (..)
  , textLengthOf
    -- * Inspection
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
    -- * Rendering
  , renderNode
  , renderToken
  , renderElement
  ) where

import Control.Monad.ST (ST, runST)
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

-- | The source text of a node: the text of its tokens, in order. The result
-- is written into one buffer sized from the cached length. A subtree that
-- appears in several places is rendered at each of them.
renderNode :: GreenNode kind -> Text
renderNode = renderElement . Node

-- | Render a token by returning its exact text.
renderToken :: GreenToken kind -> Text
renderToken = tokenText

-- | The source text of a node or token.
renderElement :: GreenElement kind -> Text
renderElement (Token token) = tokenText token
renderElement element
  | total == 0 = Text.empty
  | otherwise = runST $ do
      target <- Array.new bytes
      writeElement target 0 element
      result <- Array.unsafeFreeze target
      pure (Text result 0 bytes)
  where
    total = elementTextLength element
    bytes = fromIntegral total

writeElement :: Array.MArray s -> Int -> GreenElement kind -> ST s ()
writeElement target !offset (Token (GreenToken _ (TextLength len) (Text source start _))) =
  Array.copyI len target offset source start
writeElement target !offset (Node node) = writeChildren target offset node 0

-- The last child is written by a tail call, so a chain of single-child nodes
-- runs in constant stack.
writeChildren :: Array.MArray s -> Int -> GreenNode kind -> Int -> ST s ()
writeChildren target !offset node !index
  | index >= count = pure ()
  | index == count - 1 = writeElement target (childOffset index) (childAt node index)
  | otherwise = do
      writeElement target (childOffset index) (childAt node index)
      writeChildren target offset node (index + 1)
  where
    count = childCount node
    childOffset position = offset + fromIntegral (childStart node position)

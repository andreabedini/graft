{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

-- | Representation of green trees, for code that needs to work below the
-- public API. Nothing here is covered by the package's versioning policy, and
-- constructing values directly can break the cached lengths and offsets that
-- the rest of the library relies on. Prefer "Graft.Green".
module Graft.Green.Internal
  ( -- * Representation
    TextLength
  , GreenNode (..)
  , GreenToken (..)
  , GreenElement (..)
    -- * Construction
  , greenNode
  , greenNodeReversed
  , greenToken
    -- * Children
  , elementTextLength
  , childCount
  , childAt
  , childStart
  , firstChildEndingAtOrAfter
    -- * Unchecked updates
  , setChild
  , spliceChildrenUnchecked
  ) where

import Control.Monad.ST (ST, runST)
import Data.Foldable (toList)
import Data.Primitive.PrimArray
import Data.Primitive.SmallArray
import Data.Text (Text)
import qualified Data.Text.Foreign as Text
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)

-- | A non-negative count of UTF-8 bytes, including all trivia, in the same
-- 'Int' unit that "Data.Text" and "Data.ByteString" use for lengths. This is
-- neither a character count nor a UTF-16 code-unit count. Shared subtrees make
-- a logical length exceed the physical text size; sums are not checked for
-- overflow, which needs a logical length beyond @maxBound :: Int@.
type TextLength = Int

-- | An internal node: its kind, its cached total length, its children in
-- source order, and the start of each child relative to the node.
data GreenNode kind = GreenNode
  !kind
  !TextLength
  !(SmallArray (GreenElement kind))
  !(PrimArray Int)

-- | A leaf: its kind, its cached UTF-8 byte length and its exact text.
data GreenToken kind = GreenToken !kind !TextLength {-# UNPACK #-} !Text
  deriving (Eq)

-- | A node or token. Both can occur as children of a node.
data GreenElement kind
  = Node !(GreenNode kind)
  | Token !(GreenToken kind)
  deriving (Eq, Show)

-- | Structural equality. Physically identical nodes compare equal without
-- traversal, which assumes that equality on @kind@ is reflexive.
instance Eq kind => Eq (GreenNode kind) where
  left@(GreenNode leftKind leftLength leftChildren _)
    == right@(GreenNode rightKind rightLength rightChildren _) =
      isTrue# (reallyUnsafePtrEquality# left right)
        || (leftLength == rightLength && leftKind == rightKind && leftChildren == rightChildren)

-- | Shows the smart-constructor call that rebuilds the node.
instance Show kind => Show (GreenNode kind) where
  showsPrec precedence (GreenNode kind _ children _) = showParen (precedence > 10) $
    showString "greenNode " . showsPrec 11 kind . showChar ' ' . shows (toList children)

-- | Shows the smart-constructor call that rebuilds the token.
instance Show kind => Show (GreenToken kind) where
  showsPrec precedence (GreenToken kind _ text) = showParen (precedence > 10) $
    showString "greenToken " . showsPrec 11 kind . showChar ' ' . showsPrec 11 text

-- | Construct a node from children in source order, establishing its cached
-- length and child offsets in one pass over the immediate children.
greenNode :: kind -> [GreenElement kind] -> GreenNode kind
greenNode kind children = runST $ do
  let count = length children
  elements <- newSmallArray count missingChild
  starts <- newPrimArray count
  let fill !_ !offset [] = pure offset
      fill !index !offset (!child : rest) = do
        writeSmallArray elements index child
        writePrimArray starts index offset
        fill (index + 1) (offset + elementTextLength child) rest
  total <- fill 0 0 children
  freeze kind total elements starts

-- | Construct a node from @count@ children supplied in reverse source order,
-- as a builder accumulates them. The list must have exactly @count@ elements.
greenNodeReversed :: kind -> Int -> [GreenElement kind] -> GreenNode kind
greenNodeReversed kind count reversed = runST $ do
  elements <- newSmallArray count missingChild
  let place !_ [] = pure ()
      place !index (!child : rest) = writeSmallArray elements index child >> place (index - 1) rest
  place (count - 1) reversed
  starts <- newPrimArray count
  let measure !index !offset
        | index == count = pure offset
        | otherwise = do
            child <- readSmallArray elements index
            writePrimArray starts index offset
            measure (index + 1) (offset + elementTextLength child)
  total <- measure 0 0
  freeze kind total elements starts

freeze
  :: kind -> TextLength -> SmallMutableArray s (GreenElement kind) -> MutablePrimArray s Int
  -> ST s (GreenNode kind)
freeze kind total elements starts =
  GreenNode kind total <$> unsafeFreezeSmallArray elements <*> unsafeFreezePrimArray starts

missingChild :: a
missingChild = error "Graft.Green.Internal: uninitialised child"

-- | Construct a token. 'Text' stores UTF-8 internally, so reading the byte
-- length takes constant time and copies nothing.
greenToken :: kind -> Text -> GreenToken kind
greenToken kind text = GreenToken kind (Text.lengthWord8 text) text

-- | Read the cached UTF-8 byte length of either element.
elementTextLength :: GreenElement kind -> TextLength
elementTextLength (Node (GreenNode _ len _ _)) = len
elementTextLength (Token (GreenToken _ len _)) = len

-- | Number of immediate children.
childCount :: GreenNode kind -> Int
childCount (GreenNode _ _ children _) = sizeofSmallArray children

-- | The child at an index, which must lie in @[0, childCount node)@.
childAt :: GreenNode kind -> Int -> GreenElement kind
childAt (GreenNode _ _ children _) = indexSmallArray children

-- | Start of the child at an index relative to the node. The child count is
-- accepted and returns the node's length.
childStart :: GreenNode kind -> Int -> TextLength
childStart (GreenNode _ len _ starts) index
  | index >= sizeofPrimArray starts = len
  | otherwise = indexPrimArray starts index

-- | The smallest index whose child ends at or after a relative offset, or the
-- child count if there is none. Binary search over the cached child offsets.
firstChildEndingAtOrAfter :: GreenNode kind -> TextLength -> Int
firstChildEndingAtOrAfter node offset = go 0 (childCount node)
  where
    go low high
      | low >= high = low
      | childStart node (middle + 1) >= offset = go low middle
      | otherwise = go (middle + 1) high
      where
        middle = (low + high) `div` 2

-- | Replace the child at a valid index. The length is adjusted by the
-- difference between the old and new child; offsets are reused when the
-- lengths agree and shifted otherwise.
setChild :: Int -> GreenElement kind -> GreenNode kind -> GreenNode kind
setChild index !replacement node@(GreenNode kind len children starts) =
  GreenNode kind (len + delta) updated shifted
  where
    delta = elementTextLength replacement - elementTextLength (childAt node index)
    count = sizeofSmallArray children
    updated = runSmallArray $ do
      copy <- thawSmallArray children 0 count
      writeSmallArray copy index replacement
      pure copy
    shifted
      | delta == 0 = starts
      | otherwise = generatePrimArray count $ \position ->
          let start = indexPrimArray starts position
          in if position <= index then start else start + delta

-- | Replace the children in @[start, end)@, which must satisfy
-- @0 <= start <= end <= childCount node@. Offsets before the range are
-- copied, and offsets after it are shifted by the change in length.
spliceChildrenUnchecked :: Int -> Int -> [GreenElement kind] -> GreenNode kind -> GreenNode kind
spliceChildrenUnchecked start end replacements node@(GreenNode kind len children starts) = runST $ do
  let count = sizeofSmallArray children
      inserted = length replacements
      newCount = count - (end - start) + inserted
      suffix = count - end
  elements <- newSmallArray newCount missingChild
  offsets <- newPrimArray newCount
  copySmallArray elements 0 children 0 start
  copyPrimArray offsets 0 starts 0 start
  let place !_ !offset [] = pure offset
      place !index !offset (!child : rest) = do
        writeSmallArray elements index child
        writePrimArray offsets index offset
        place (index + 1) (offset + elementTextLength child) rest
  afterInserted <- place start (childStart node start) replacements
  let delta = afterInserted - childStart node end
      shift !position
        | position == suffix = pure ()
        | otherwise = do
            writePrimArray offsets (start + inserted + position)
              (indexPrimArray starts (end + position) + delta)
            shift (position + 1)
  copySmallArray elements (start + inserted) children end suffix
  shift 0
  freeze kind (len + delta) elements offsets

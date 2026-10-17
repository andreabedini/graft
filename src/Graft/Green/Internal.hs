{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}

-- | Representation of green trees, shared by the library's modules and not
-- exposed by the package. Constructing values directly can break the cached
-- lengths and offsets that the rest of the library relies on; "Graft.Green"
-- re-exports the safe interface.
module Graft.Green.Internal
  ( -- * Representation
    TextLength (..)
  , textLengthOf
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

import Control.DeepSeq (NFData (..))
import Control.Monad.ST (ST, runST)
import Data.Foldable (toList)
import qualified Data.Foldable as Foldable
import Data.Hashable (Hashable (..))
import Data.Primitive.PrimArray
import Data.Primitive.SmallArray
import Data.Text (Text)
import qualified Data.Text.Foreign as Text
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)

-- | A count of UTF-8 bytes, used for lengths and for offsets from the start
-- of a tree. It is neither a character count (as 'Data.Text.length' returns)
-- nor a UTF-16 code-unit count (as the Language Server Protocol uses), and
-- the newtype keeps those from being mixed up by accident. Numeric literals
-- and arithmetic work as for 'Int'; use the constructor or 'fromIntegral' to
-- convert deliberately.
--
-- Shared subtrees make a logical length exceed the physical text size; sums
-- are not checked for overflow, which needs a logical length beyond
-- @maxBound :: Int@.
newtype TextLength = TextLength Int
  deriving newtype (Eq, Ord, Show, Read, Bounded, Enum, Num, Real, Integral, NFData, Hashable)

-- | The UTF-8 byte length of a text, in constant time.
textLengthOf :: Text -> TextLength
textLengthOf = TextLength . Text.lengthWord8

-- | An interior node: a kind and its children in source order. Construct one
-- with 'greenNode'. A node caches its total length and the offset of each
-- child, so reading a length or a child takes constant time.
data GreenNode kind = GreenNode
  !kind
  !TextLength
  !(SmallArray (GreenElement kind))
  !(PrimArray Int)

-- | A leaf: a kind and the exact text of a source fragment, which may be
-- empty. Construct one with 'greenToken'. Text is stored as given, with no
-- normalization.
data GreenToken kind = GreenToken !kind !TextLength {-# UNPACK #-} !Text
  deriving (Eq)

-- | A node or a token; either can be a child of a node.
-- Tokens are unpacked into the element, which saves a box per token (about
-- 15% of a typical tree's heap). Nodes stay boxed so that cursors can share
-- the parent node instead of re-allocating it on every descent.
data GreenElement kind
  = Node !(GreenNode kind)
  | Token {-# UNPACK #-} !(GreenToken kind)
  deriving (Eq, Show)

-- | Structural equality. Physically identical nodes compare equal without
-- traversal, which assumes that equality on @kind@ is reflexive.
instance Eq kind => Eq (GreenNode kind) where
  left@(GreenNode leftKind leftLength leftChildren _)
    == right@(GreenNode rightKind rightLength rightChildren _) =
      isTrue# (reallyUnsafePtrEquality# left right)
        || (leftLength == rightLength && leftKind == rightKind && leftChildren == rightChildren)

instance NFData kind => NFData (GreenNode kind) where
  rnf (GreenNode kind _ children _) = rnf kind `seq` foldr (\child rest -> rnf child `seq` rest) () children

instance NFData kind => NFData (GreenToken kind) where
  rnf (GreenToken kind _ text) = rnf kind `seq` rnf text

instance NFData kind => NFData (GreenElement kind) where
  rnf (Node node) = rnf node
  rnf (Token token) = rnf token

-- | Hashes the kind, the length and every child, so hashing takes time
-- proportional to the size of the tree; hashes are not cached.
instance Hashable kind => Hashable (GreenNode kind) where
  hashWithSalt salt (GreenNode kind len children _) =
    Foldable.foldl' hashWithSalt (salt `hashWithSalt` kind `hashWithSalt` len) children

instance Hashable kind => Hashable (GreenToken kind) where
  hashWithSalt salt (GreenToken kind _ text) = salt `hashWithSalt` kind `hashWithSalt` text

instance Hashable kind => Hashable (GreenElement kind) where
  hashWithSalt salt (Node node) = salt `hashWithSalt` (0 :: Int) `hashWithSalt` node
  hashWithSalt salt (Token token) = salt `hashWithSalt` (1 :: Int) `hashWithSalt` token

-- | Maps every kind in the tree, reusing the cached lengths and offsets.
instance Functor GreenNode where
  fmap f (GreenNode kind len children starts) =
    GreenNode (f kind) len (mapSmallArray' (fmap f) children) starts

instance Functor GreenToken where
  fmap f (GreenToken kind len text) = GreenToken (f kind) len text

instance Functor GreenElement where
  fmap f (Node node) = Node (fmap f node)
  fmap f (Token token) = Token (fmap f token)

-- | Shows the smart-constructor call that rebuilds the node.
instance Show kind => Show (GreenNode kind) where
  showsPrec precedence (GreenNode kind _ children _) = showParen (precedence > 10) $
    showString "greenNode " . showsPrec 11 kind . showChar ' ' . shows (toList children)

-- | Shows the smart-constructor call that rebuilds the token.
instance Show kind => Show (GreenToken kind) where
  showsPrec precedence (GreenToken kind _ text) = showParen (precedence > 10) $
    showString "greenToken " . showsPrec 11 kind . showChar ' ' . showsPrec 11 text

-- | A node with a kind and its children in source order, which may be none.
-- Takes time proportional to the number of children; the children are
-- shared, not copied or traversed.
greenNode :: kind -> [GreenElement kind] -> GreenNode kind
greenNode kind children = runST $ do
  let count = length children
  elements <- newSmallArray count missingChild
  starts <- newPrimArray count
  let fill !_ !offset [] = pure offset
      fill !index !offset (!child : rest) = do
        writeSmallArray elements index child
        writePrimArray starts index (bytes offset)
        fill (index + 1) (offset + elementTextLength child) rest
  total <- fill 0 0 children
  freeze kind total elements starts

-- | Construct a node from the first @count@ elements of a list in reverse
-- source order, as a builder accumulates them, and return the rest of the
-- list. The list must have at least @count@ elements. The list is walked once.
greenNodeReversed :: kind -> Int -> [GreenElement kind] -> (GreenNode kind, [GreenElement kind])
greenNodeReversed kind count reversed = runST $ do
  elements <- newSmallArray count missingChild
  let place !index rest
        | index < 0 = pure rest
        | otherwise = case rest of
            [] -> error "Graft.Green.Internal.greenNodeReversed: too few elements"
            !child : older -> writeSmallArray elements index child >> place (index - 1) older
  remaining <- place (count - 1) reversed
  starts <- newPrimArray count
  let measure !index !offset
        | index == count = pure offset
        | otherwise = do
            child <- readSmallArray elements index
            writePrimArray starts index (bytes offset)
            measure (index + 1) (offset + elementTextLength child)
  total <- measure 0 0
  node <- freeze kind total elements starts
  pure (node, remaining)

freeze
  :: kind -> TextLength -> SmallMutableArray s (GreenElement kind) -> MutablePrimArray s Int
  -> ST s (GreenNode kind)
freeze kind total elements starts =
  GreenNode kind total <$> unsafeFreezeSmallArray elements <*> unsafeFreezePrimArray starts

bytes :: TextLength -> Int
bytes (TextLength count) = count

missingChild :: a
missingChild = error "Graft.Green.Internal: uninitialised child"

-- | Construct a token. 'Text' stores UTF-8 internally, so reading the byte
-- length takes constant time and copies nothing.
greenToken :: kind -> Text -> GreenToken kind
greenToken kind text = GreenToken kind (textLengthOf text) text

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
  | otherwise = TextLength (indexPrimArray starts index)

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
          in if position <= index then start else start + bytes delta

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
        writePrimArray offsets index (bytes offset)
        place (index + 1) (offset + elementTextLength child) rest
  afterInserted <- place start (childStart node start) replacements
  let delta = afterInserted - childStart node end
      shift !position
        | position == suffix = pure ()
        | otherwise = do
            writePrimArray offsets (start + inserted + position)
              (indexPrimArray starts (end + position) + bytes delta)
            shift (position + 1)
  copySmallArray elements (start + inserted) children end suffix
  shift 0
  freeze kind (len + delta) elements offsets

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
  , greenNodeFromArray
  , greenToken
    -- * Children
  , elementTextLength
  , childCount
  , childAt
  , childStart
  , firstChildEndingAtOrAfter
    -- * Folding
  , foldM
    -- * Child edits
  , ChildEditError (..)
  , spliceBoundsError
  , checkedLength
  , setChild
  , setChildren
  , patchChildren
  , spliceChildrenUnchecked
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Control.Monad.ST (ST, runST)
import Data.Functor.Identity (Identity (..))
import Data.Foldable (toList)
import qualified Data.List as List
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
-- Shared subtrees make a logical length exceed the physical text size.
-- Constructing or editing a node whose length would exceed
-- @maxBound :: Int@ fails with an error instead of wrapping around.
newtype TextLength = TextLength Int
  deriving newtype (Eq, Ord, Show, Read, Bounded, Enum, Num, Real, Integral, NFData, Hashable)

-- | The UTF-8 byte length of a text, in constant time.
textLengthOf :: Text -> TextLength
textLengthOf = TextLength . Text.lengthWord8

-- | An interior node: a kind and its children in source order. Construct one
-- with 'Graft.Green.node'. A node caches its total length and the offset of each
-- child, so reading a length or a child takes constant time.
data GreenNode kind = GreenNode
  !kind
  !TextLength
  !(SmallArray (GreenElement kind))
  !(PrimArray Int)

-- | A leaf: a kind and the exact text of a source fragment, which may be
-- empty. Construct one with 'Graft.Green.token'. Text is stored as given, with no
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
-- traversal, which assumes that equality on @kind@ is reflexive. Otherwise a
-- subtree shared in several places is compared at each of them, so comparing
-- two separately built trees takes time proportional to their size written
-- out in full.
instance Eq kind => Eq (GreenNode kind) where
  left@(GreenNode leftKind leftLength leftChildren _)
    == right@(GreenNode rightKind rightLength rightChildren _) =
      isTrue# (reallyUnsafePtrEquality# left right)
        || (leftLength == rightLength && leftKind == rightKind && leftChildren == rightChildren)

-- | Visits a subtree shared in several places at each of them.
instance NFData kind => NFData (GreenNode kind) where
  rnf node = rnf (Node node)

instance NFData kind => NFData (GreenToken kind) where
  rnf (GreenToken kind _ text) = rnf kind `seq` rnf text

-- | Forces every kind and every token's text with one walk of the tree.
instance NFData kind => NFData (GreenElement kind) where
  rnf tree = runIdentity (foldM (\() element -> Identity (force element)) () tree)
    where
      force (Node (GreenNode kind _ _ _)) = rnf kind
      force (Token token) = rnf token

-- | Hashes the kind and number of children of every node, and every token,
-- so hashing takes time proportional to the size of the tree written out in
-- full, with a shared subtree hashed at each place it appears; hashes are not
-- cached.
instance Hashable kind => Hashable (GreenNode kind) where
  hashWithSalt salt node = hashWithSalt salt (Node node)

instance Hashable kind => Hashable (GreenToken kind) where
  hashWithSalt salt (GreenToken kind _ text) = salt `hashWithSalt` kind `hashWithSalt` text

-- | One preorder fold over the tree. Each node contributes its kind and its
-- number of children, and a preorder sequence with the number of children of
-- each node determines the tree, so the nesting is part of the hash. Lengths
-- are left out: the tokens' text already determines them.
instance Hashable kind => Hashable (GreenElement kind) where
  hashWithSalt salt tree = runIdentity (foldM (\current element -> Identity (mix current element)) salt tree)
    where
      mix current (Node (GreenNode kind _ children _)) =
        current `hashWithSalt` (0 :: Int) `hashWithSalt` kind `hashWithSalt` sizeofSmallArray children
      mix current (Token token) = current `hashWithSalt` (1 :: Int) `hashWithSalt` token

-- | Maps every kind in the tree, reusing the cached lengths and offsets. A
-- subtree shared in several places is mapped at each of them, and the
-- results are not shared. Kinds are stored strictly, so @fmap f@ evaluates
-- each result of @f@, and the functor laws hold for functions that don't
-- return bottom.
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
    showString "Green.node " . showsPrec 11 kind . showChar ' ' . shows (toList children)

-- | Shows the smart-constructor call that rebuilds the token.
instance Show kind => Show (GreenToken kind) where
  showsPrec precedence (GreenToken kind _ text) = showParen (precedence > 10) $
    showString "Green.token " . showsPrec 11 kind . showChar ' ' . showsPrec 11 text

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
        fill (index + 1) (addLength offset (elementTextLength child)) rest
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
            measure (index + 1) (addLength offset (elementTextLength child))
  total <- measure 0 0
  node <- freeze kind total elements starts
  pure (node, remaining)

-- | A strict, monadic left fold over a tree's elements in preorder: each
-- element, then its children in source order. The accumulator is evaluated
-- to weak head normal form before each step. The loop runs over the child
-- arrays by index and folds the last child of each node by a tail call, so a
-- chain of single-child nodes runs in constant stack.
foldM :: Monad m => (b -> GreenElement kind -> m b) -> b -> GreenElement kind -> m b
foldM step = element
  where
    element !acc current = case current of
      Token _ -> step acc current
      Node (GreenNode _ _ elements _) ->
        step acc current >>= \ !entered -> children entered elements 0
    children !acc elements !index
      | index >= count = pure acc
      | index == count - 1 = element acc (indexSmallArray elements index)
      | otherwise =
          element acc (indexSmallArray elements index) >>= \ !next ->
            children next elements (index + 1)
      where
        count = sizeofSmallArray elements
{-# INLINE foldM #-}

-- | Construct a node from an array of children, measuring each child once.
greenNodeFromArray :: kind -> SmallArray (GreenElement kind) -> GreenNode kind
greenNodeFromArray kind children = runST $ do
  let count = sizeofSmallArray children
  starts <- newPrimArray count
  let measure !index !offset
        | index == count = pure offset
        | otherwise = do
            writePrimArray starts index (bytes offset)
            measure (index + 1) (addLength offset (elementTextLength (indexSmallArray children index)))
  total <- measure 0 0
  GreenNode kind total children <$> unsafeFreezePrimArray starts

freeze
  :: kind -> TextLength -> SmallMutableArray s (GreenElement kind) -> MutablePrimArray s Int
  -> ST s (GreenNode kind)
freeze kind total elements starts =
  GreenNode kind total <$> unsafeFreezeSmallArray elements <*> unsafeFreezePrimArray starts

bytes :: TextLength -> Int
bytes (TextLength count) = count

missingChild :: a
missingChild = error "Graft.Green.Internal: uninitialised child"

-- Lengths are sums of non-negative lengths, so a sum that comes out negative
-- has overflowed. Shared subtrees can reach that without the text existing,
-- and a wrapped length would make 'Graft.Green.render' size its buffer
-- wrongly, so overflow is an error.
checkedLength :: TextLength -> TextLength
checkedLength len
  | len < 0 = error "Graft.Green: a tree's length exceeds maxBound :: Int"
  | otherwise = len

addLength :: TextLength -> TextLength -> TextLength
addLength offset len = checkedLength (offset + len)

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

-- | Why a child edit failed. 'CannotEditTokenChildren' means the target is a
-- token; the others report a bad child position (or both positions of a
-- reversed range).
data ChildEditError
  = CannotEditTokenChildren
  | NegativeChildBoundary !Int
  | ReversedChildRange !Int !Int
  | ChildBoundaryOutOfBounds !Int
  deriving (Eq, Show)

instance Exception ChildEditError

instance NFData ChildEditError where
  rnf err = err `seq` ()

-- | Why @[start, end)@ is not a range of a node's children, if it isn't.
-- Errors are checked in this order: a negative start or end, then
-- @end < start@, then a start or end past the last child.
spliceBoundsError :: Int -> Int -> GreenNode kind -> Maybe ChildEditError
spliceBoundsError start end node
  | start < 0 = Just (NegativeChildBoundary start)
  | end < 0 = Just (NegativeChildBoundary end)
  | end < start = Just (ReversedChildRange start end)
  | start > count = Just (ChildBoundaryOutOfBounds start)
  | end > count = Just (ChildBoundaryOutOfBounds end)
  | otherwise = Nothing
  where
    count = childCount node

-- | Replace the child at a valid index. The length is adjusted by the
-- difference between the old and new child; offsets are reused when the
-- lengths agree and shifted otherwise.
setChild :: Int -> GreenElement kind -> GreenNode kind -> GreenNode kind
setChild index !replacement node@(GreenNode kind len children starts) =
  GreenNode kind (checkedLength (len + delta)) updated shifted
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

-- | Replace the children at valid indices, given in increasing order, with
-- one copy of the child array. Offsets before the first replacement are
-- copied, and the rest are recomputed.
setChildren :: [(Int, GreenElement kind)] -> GreenNode kind -> GreenNode kind
setChildren [] node = node
setChildren [(index, replacement)] node = setChild index replacement node
setChildren replacements@((first, _) : _) node@(GreenNode kind _ children starts) = runST $ do
  let count = sizeofSmallArray children
  elements <- thawSmallArray children 0 count
  mapM_ (\(index, !replacement) -> writeSmallArray elements index replacement) replacements
  offsets <- newPrimArray count
  copyPrimArray offsets 0 starts 0 first
  let measure !index !offset
        | index == count = pure offset
        | otherwise = do
            child <- readSmallArray elements index
            writePrimArray offsets index (bytes offset)
            measure (index + 1) (addLength offset (elementTextLength child))
  total <- measure first (childStart node first)
  freeze kind total elements offsets

-- | Replace several ranges of children at once, with one copy of the child
-- array: each @(start, end, elements)@ puts the elements in place of the
-- children in @[start, end)@, and an empty range inserts them at its
-- boundary. The ranges must be sorted, lie within @[0, childCount node]@
-- and not overlap. The untouched runs of children are copied as blocks and
-- keep their offsets, shifted by the change in length before them; only
-- the new elements are measured. When every patch puts as many bytes in as
-- many children as it takes out, the offsets are reused.
patchChildren :: [(Int, Int, [GreenElement kind])] -> GreenNode kind -> GreenNode kind
patchChildren [] node = node
patchChildren patches@((first, _, _) : _) node@(GreenNode kind len children starts)
  | sameLayout = runST $ do
      elements <- newSmallArray count missingChild
      copySmallArray elements 0 children 0 first
      let fill !source ((start, end, new) : rest) = do
            copySmallArray elements source children source (start - source)
            mapM_ (\(index, !child) -> writeSmallArray elements index child) (zip [start ..] new)
            fill end rest
          fill !source [] = copySmallArray elements source children source (count - source)
      fill first patches
      GreenNode kind len <$> unsafeFreezeSmallArray elements <*> pure starts
  | otherwise = patchLayout patches node
  where
    count = sizeofSmallArray children
    sameLayout = all unchanged patches
    unchanged (start, end, new) =
      length new == end - start
        && List.foldl' addLength 0 (map elementTextLength new) == childStart node end - childStart node start

-- The general case of 'patchChildren': a new offset array, with the
-- untouched runs shifted and the new elements measured.
patchLayout :: [(Int, Int, [GreenElement kind])] -> GreenNode kind -> GreenNode kind
patchLayout [] node = node
patchLayout patches@((first, _, _) : _) node@(GreenNode kind len children starts) = runST $ do
  let count = sizeofSmallArray children
      newCount = count + sum [length new - (end - start) | (start, end, new) <- patches]
  elements <- newSmallArray newCount missingChild
  offsets <- newPrimArray newCount
  copySmallArray elements 0 children 0 first
  copyPrimArray offsets 0 starts 0 first
  let -- Copy an untouched run and its offsets, shifted by the delta so far.
      keep !target !source !run !delta = do
        copySmallArray elements target children source run
        let shift !step
              | step == run = pure ()
              | otherwise = do
                  writePrimArray offsets (target + step) (indexPrimArray starts (source + step) + delta)
                  shift (step + 1)
        shift 0
      -- Write the new elements of a patch and measure them.
      place !target !offset [] = pure (target, offset)
      place !target !offset (!child : more) = do
        writeSmallArray elements target child
        writePrimArray offsets target (bytes offset)
        place (target + 1) (addLength offset (elementTextLength child)) more
      go !target !source !delta ((start, end, new) : rest) = do
        let run = start - source
        keep target source run delta
        let from = childStart node start + TextLength delta
        (target', to) <- place (target + run) from new
        go target' end (delta + bytes to - bytes from - bytes (childStart node end - childStart node start)) rest
      go !target !source !delta [] = do
        keep target source (count - source) delta
        pure delta
  delta <- go first first 0 patches
  freeze kind (checkedLength (len + TextLength delta)) elements offsets

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
        place (index + 1) (addLength offset (elementTextLength child)) rest
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
  freeze kind (checkedLength (len + delta)) elements offsets

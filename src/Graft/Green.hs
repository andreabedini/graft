{-# LANGUAGE BangPatterns #-}

-- | Green trees: immutable nodes and tokens, tagged with your own kind type.
--
-- Every fragment of the source, whitespace and comments included, is the text
-- of some token, so rendering a tree gives back the source exactly. Green
-- values have no parents or positions, so a subtree can appear in several
-- places and in several versions of a tree. Use a cursor ("Graft.Cursor") to
-- see where an element is, and to edit inside a tree.
--
-- This module is meant to be imported qualified; its types are also
-- exported by "Graft":
--
-- > import Graft
-- > import qualified Graft.Green as Green
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Graft.Green as Green
-- >>> data Kind = Call | Name | Punct | Space deriving (Eq, Show)
-- >>> let tok kind = Token . Green.token kind
-- >>> let call = Green.node Call [tok Name "f", tok Punct "(", tok Space " ", tok Punct ")"]
-- >>> Green.renderNode call
-- "f( )"
-- >>> Green.nodeLength call
-- 4
-- >>> Green.childAt 1 call
-- Just (Token (Green.token Punct "("))
-- >>> Green.renderNode <$> Green.replaceChild 2 (tok Name "x") call
-- Right "f(x)"
--
-- 'foldM', 'fold', 'map' and 'traverse' cover the whole tree, so most
-- whole-tree functions are one line:
--
-- >>> import qualified Data.Text as Text
-- >>> import Data.Functor.Identity (Identity (..))
-- >>> let count n element = case element of { Token _ -> n + 1; Node _ -> n }
-- >>> runIdentity (Green.foldM (\n element -> Identity (count n element)) (0 :: Int) (Node call))
-- 4
-- >>> Green.fold Green.text (const Text.concat) call
-- "f( )"
-- >>> let upper t = Green.token (Green.tokenKind t) (Text.toUpper (Green.text t))
-- >>> Green.renderNode (Green.map upper call)
-- "F( )"
-- >>> Green.renderNode <$> Green.traverse (\t -> if Green.text t == " " then Nothing else Just t) call
-- Nothing
module Graft.Green
  ( -- * Trees
    GreenNode
  , GreenToken
  , GreenElement (..)
  , node
  , token
    -- * Lengths
  , TextLength (..)
  , textLengthOf
  , length
  , nodeLength
  , tokenLength
    -- * Inspection
  , kind
  , nodeKind
  , tokenKind
  , children
  , childCount
  , childAt
  , text
    -- * Rendering
  , render
  , renderNode
    -- * Folding and traversing
  , foldM
  , fold
  , map
  , traverse
    -- * Editing children
  , ChildEditError (..)
  , replaceChild
  , spliceChildren
  , insertChild
  , removeChild
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Control.Monad.ST (ST, runST)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Array as Array
import qualified Data.Text.Internal as Text (Text (..))
import Data.Primitive.SmallArray (mapSmallArray')
import Graft.Green.Internal (GreenElement (..), GreenNode (..), GreenToken (..), TextLength (..), textLengthOf)
import qualified Graft.Green.Internal as Internal
import qualified Data.Traversable as Traversable
import qualified Data.List as List
import Prelude hiding (length, map, traverse)

-- | A node with a kind and its children in source order, which may be none.
-- Takes time proportional to the number of children; the children are
-- shared, not copied or traversed. A node whose length would exceed
-- @maxBound :: Int@, which only shared subtrees can reach, is an error.
node :: kind -> [GreenElement kind] -> GreenNode kind
node = Internal.greenNode

-- | A token with a kind and the exact text of a source fragment. Reading the
-- UTF-8 length of the text takes constant time.
token :: kind -> Text -> GreenToken kind
token = Internal.greenToken

-- | The length of a node or token in UTF-8 bytes, read from its cache.
length :: GreenElement kind -> TextLength
length = Internal.elementTextLength

-- | The length of a node in UTF-8 bytes, read from its cache.
nodeLength :: GreenNode kind -> TextLength
nodeLength (GreenNode _ len _ _) = len

-- | The length of a token in UTF-8 bytes, read from its cache.
tokenLength :: GreenToken kind -> TextLength
tokenLength (GreenToken _ len _) = len

-- | The kind of a node or token.
kind :: GreenElement kind -> kind
kind (Node element) = nodeKind element
kind (Token element) = tokenKind element

-- | The kind of a node.
nodeKind :: GreenNode kind -> kind
nodeKind (GreenNode nodeKind' _ _ _) = nodeKind'

-- | The kind of a token.
tokenKind :: GreenToken kind -> kind
tokenKind (GreenToken tokenKind' _ _) = tokenKind'

-- | The children of a node in source order, trivia included. The list is
-- produced lazily from the node's array of children.
children :: GreenNode kind -> [GreenElement kind]
children (GreenNode _ _ elements _) = toList elements

-- | The number of children, in constant time.
childCount :: GreenNode kind -> Int
childCount = Internal.childCount

-- | The child at a zero-based position, in constant time.
childAt :: Int -> GreenNode kind -> Maybe (GreenElement kind)
childAt index parent
  | index < 0 || index >= Internal.childCount parent = Nothing
  -- Read the element now, so the result doesn't keep the parent's array alive.
  | otherwise = Just $! Internal.childAt parent index

-- | The exact text of a token, as given to 'token'.
text :: GreenToken kind -> Text
text (GreenToken _ _ contents) = contents

-- | The source text of a node or token: the text of its tokens, in order.
-- The result is written into one buffer sized from the cached length. A
-- subtree that appears in several places is rendered at each of them.
render :: GreenElement kind -> Text
render (Token element) = text element
render element
  | total == 0 = Text.empty
  | otherwise = runST $ do
      target <- Array.new bytes
      writeElement target 0 element
      result <- Array.unsafeFreeze target
      pure (Text.Text result 0 bytes)
  where
    total = length element
    bytes = fromIntegral total

-- | The source text of a node, as 'render'.
renderNode :: GreenNode kind -> Text
renderNode = render . Node

-- | A strict left fold over a tree's elements in preorder, in any monad:
-- each element, then its children in source order, as @Graft.Cursor.descendants@
-- lists them but without positions.
--
-- The accumulator is evaluated to weak head normal form before each step.
-- The fold loops over each node's children by index and builds no list, so
-- when the step function is known at the call site it allocates nothing of
-- its own, in 'Data.Functor.Identity.Identity' as in @ST@. The last child of
-- each node is folded by a tail call, so a chain of single-child nodes runs
-- in constant stack. Use it for a single result: a count, a hash, a check,
-- or text written into a buffer. Track the offset in the accumulator if you
-- need positions. A subtree shared in several places is folded at each of
-- them.
foldM :: Monad m => (b -> GreenElement kind -> m b) -> b -> GreenElement kind -> m b
foldM = Internal.foldM
{-# INLINE foldM #-}

-- | Fold a tree bottom-up, like @Data.Tree.foldTree@. The first function
-- receives each token; the second receives the kind of each node and the
-- results for its children, in source order.
--
-- The list of results is lazy, so a function that does not need every
-- child does not visit the rest. Use this fold to build a result with the
-- shape of the tree, such as a syntax tree of your own; for a single value,
-- 'foldM' is several times faster because it builds no lists. A subtree
-- shared in several places is folded at each of them.
--
-- 'renderNode' is @fold text (const Text.concat)@, but faster:
-- 'renderNode' writes into one buffer, while that fold copies each byte
-- again at every enclosing node.
fold :: (GreenToken kind -> r) -> (kind -> [r] -> r) -> GreenNode kind -> r
fold onToken onNode = go
  where
    go (GreenNode nodeKind' _ elements _) = onNode nodeKind' (List.map element (toList elements))
    element (Token token') = onToken token'
    element (Node child) = go child

-- | Replace every token with the result of a function, and rebuild the
-- tree. The node kinds and the shape of the tree stay the same. The lengths
-- and offsets of every node are measured again, so the function can change
-- the text of a token freely, for example to copy it out of a large source
-- buffer with 'Text.copy'. Every node is rebuilt, and a subtree shared in
-- several places is rebuilt at each of them, so the result shares nothing
-- with the input.
--
-- @map f@ is @runIdentity . traverse (Identity . f)@, but two to three
-- times faster.
map :: (GreenToken kind -> GreenToken kind) -> GreenNode kind -> GreenNode kind
map f = go
  where
    go (GreenNode nodeKind' _ elements _) =
      Internal.greenNodeFromArray nodeKind' (mapSmallArray' element elements)
    element (Token token') = Token (f token')
    element (Node child) = Node (go child)

-- | Apply an action to every token, from left to right, and rebuild the
-- tree from the results. The node kinds and the shape of the tree stay the
-- same. The lengths and offsets of every node are measured again, so the
-- action can change the text of a token freely.
--
-- This is a van Laarhoven traversal, @Traversal' (GreenNode kind)
-- (GreenToken kind)@ in the terms of the lens and optics libraries, and it
-- satisfies the traversal laws. Some instances:
--
-- * With @Maybe@ or @Either@ it rewrites the tokens or fails.
-- * With @State@ it threads a value through the tokens in source order.
--
-- As with 'map', every node is rebuilt. For a pure function, 'map' is
-- faster; to fold without rebuilding, 'foldM' is faster than a @Const@
-- traversal.
traverse
  :: Applicative f
  => (GreenToken kind -> f (GreenToken kind))
  -> GreenNode kind -> f (GreenNode kind)
traverse action = go
  where
    go (GreenNode nodeKind' _ elements _) =
      Internal.greenNodeFromArray nodeKind' <$> Traversable.traverse element elements
    element (Token token') = Token <$> action token'
    element (Node child) = Node <$> go child

writeElement :: Array.MArray s -> Int -> GreenElement kind -> ST s ()
writeElement target !offset (Token (GreenToken _ (TextLength len) (Text.Text source start _))) =
  Array.copyI len target offset source start
writeElement target !offset (Node parent) = writeChildren target offset parent 0

-- The last child is written by a tail call, so a chain of single-child nodes
-- runs in constant stack.
writeChildren :: Array.MArray s -> Int -> GreenNode kind -> Int -> ST s ()
writeChildren target !offset parent !index
  | index >= count = pure ()
  | index == count - 1 = writeElement target (childOffset index) (Internal.childAt parent index)
  | otherwise = do
      writeElement target (childOffset index) (Internal.childAt parent index)
      writeChildren target offset parent (index + 1)
  where
    count = Internal.childCount parent
    childOffset position = offset + fromIntegral (Internal.childStart parent position)

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

-- | Replace the child at a position. Each edit returns a new node and leaves
-- the original usable; untouched children are shared between the two.
replaceChild :: Int -> GreenElement kind -> GreenNode kind -> Either ChildEditError (GreenNode kind)
replaceChild index replacement parent
  | index < 0 = Left (NegativeChildBoundary index)
  | index >= Internal.childCount parent = Left (ChildBoundaryOutOfBounds index)
  | otherwise = Right (Internal.setChild index replacement parent)

-- | @spliceChildren start end replacements node@ replaces the children at
-- positions @start@ up to but not including @end@. With @start == end@ it
-- inserts; with no replacements it deletes. Positions run from 0 to the
-- number of children. Nothing is added for you: include any separators or
-- whitespace in the replacements.
--
-- Errors are checked in this order: a negative start or end, then
-- @end < start@, then a start or end past the last child. Cost is
-- proportional to the number of children plus replacements.
spliceChildren
  :: Int -> Int -> [GreenElement kind] -> GreenNode kind
  -> Either ChildEditError (GreenNode kind)
spliceChildren start end replacements parent
  | start < 0 = Left (NegativeChildBoundary start)
  | end < 0 = Left (NegativeChildBoundary end)
  | end < start = Left (ReversedChildRange start end)
  | start > Internal.childCount parent = Left (ChildBoundaryOutOfBounds start)
  | end > Internal.childCount parent = Left (ChildBoundaryOutOfBounds end)
  | otherwise = Right (Internal.spliceChildrenUnchecked start end replacements parent)

-- | Insert a child before the given position; the number of children
-- appends. As @spliceChildren index index [element]@.
insertChild
  :: Int -> GreenElement kind -> GreenNode kind
  -> Either ChildEditError (GreenNode kind)
insertChild index element = spliceChildren index index [element]

-- | Remove the child at a position. Removing the last child leaves an empty
-- node of the same kind.
removeChild :: Int -> GreenNode kind -> Either ChildEditError (GreenNode kind)
removeChild index parent
  | index < 0 = Left (NegativeChildBoundary index)
  | index >= Internal.childCount parent = Left (ChildBoundaryOutOfBounds index)
  | otherwise = Right (Internal.spliceChildrenUnchecked index (index + 1) [] parent)

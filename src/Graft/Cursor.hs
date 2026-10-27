{-# LANGUAGE DeriveTraversable #-}

-- | Cursors: immutable zippers over green trees. A cursor focuses one
-- occurrence of a node or token in one tree and knows its parents, siblings
-- and absolute byte range. Navigation only reads the tree: a cursor does no
-- editing work, and nothing it holds is left to compute later. To change a
-- tree, describe the edits through cursors and make them with "Graft.Edit".
--
-- This module is meant to be imported qualified; its types are also
-- exported by "Graft":
--
-- > import Graft
-- > import qualified Graft.Cursor as Cursor
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Graft.Cursor as Cursor
-- >>> import qualified Graft.Green as Green
-- >>> import qualified Graft.Range as Range
-- >>> data Kind = Root | Call | Name | Punct deriving (Eq, Show)
-- >>> let tok kind = Token . Green.token kind
-- >>> let call = Node (Green.node Call [tok Name "f", tok Punct "(", tok Name "x", tok Punct ")"])
-- >>> let tree = Node (Green.node Root [call, tok Punct ";"])
-- >>> let Just x = Cursor.covering (Range.fromLength 2 1) (Cursor.fromGreen tree)
-- >>> (Cursor.path x, Cursor.text x, map Cursor.kind (Cursor.ancestors x))
-- ([0,2],"x",[Call,Root])
-- >>> fmap Cursor.text (Cursor.nextToken x)
-- Just ")"
--
-- Cursors have no equality of occurrences and expose no pointer identity;
-- compare 'path's within one tree instead.
module Graft.Cursor
  ( -- * Cursors
    Cursor
  , fromGreen
  , green
  , kind
  , isToken
  , range
  , text
  , path
  , Path
  , PathError (..)
    -- * Navigation
  , root
  , tree
  , at
  , parent
  , childAt
  , firstChild
  , lastChild
  , nextSibling
  , previousSibling
    -- * Traversal
  , children
  , ancestors
  , descendants
  , WalkEvent (..)
  , walk
    -- * Tokens
  , descendantTokens
  , firstToken
  , lastToken
  , nextToken
  , previousToken
    -- * Lookup by offset
  , TokenAtOffset (..)
  , tokenAt
  , covering
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Data.Text (Text)
import qualified Graft.Green as Green
import Graft.Green.Internal (GreenElement (..), GreenNode, TextLength)
import qualified Graft.Green.Internal as Internal
import Graft.Range (TextRange)
import qualified Graft.Range as Range

-- | The zero-based index of a child at each level, from the root down. Every
-- child counts, tokens included, and @[]@ is the root itself. A path belongs
-- to one version of a tree: inserting or removing children shifts the paths
-- that follow.
type Path = [Int]

-- | Why a path doesn't lead to an element, reported for the first step that
-- fails. The index is the one at that step.
data PathError
  = NegativeChildIndex !Int
  | ChildIndexOutOfBounds !Int
  | CannotDescendIntoToken
  deriving (Eq, Show)

instance Exception PathError

instance NFData PathError where
  rnf err = err `seq` ()

-- | A position in a tree: a focused node or token and the path back to the
-- root. Nodes and tokens share the cursor type; 'isToken' tells them apart.
-- There is no 'Eq' instance, because two equal subtrees in different
-- places are different occurrences; compare 'path's instead.
--
-- A cursor holds its parent cursor, and through it the whole tree, and
-- nothing else. Moving up costs nothing and allocates nothing.
data Cursor kind = Cursor
  { focusElement :: !(GreenElement kind)
  , focusOffset :: !TextLength
  -- ^ the absolute start of the focus
  , focusIndex :: !Int
  -- ^ the position of the focus among its parent's children; 0 at the root
  , focusParent :: !(Maybe (Cursor kind))
  }

-- | Forces the whole tree, which the focus and its ancestors are parts of,
-- in time proportional to its size.
instance NFData kind => NFData (Cursor kind) where
  rnf cursor = rnf (focusElement (root cursor))

-- | Shows the path, range and focus. Cursors have no public constructor, so
-- unlike the green types this output does not rebuild the value.
instance Show kind => Show (Cursor kind) where
  showsPrec precedence cursor = showParen (precedence > 10) $
    showString "Cursor " . showsPrec 11 (path cursor) . showChar ' '
      . showsPrec 11 (range cursor) . showChar ' ' . showsPrec 11 (green cursor)

-- | A cursor on the root of a tree, at offset zero. The root may be a token.
fromGreen :: GreenElement kind -> Cursor kind
fromGreen element = Cursor element 0 0 Nothing

-- | The green element at the focus.
green :: Cursor kind -> GreenElement kind
green = focusElement

-- | The kind of the focused element.
kind :: Cursor kind -> kind
kind = Green.kind . focusElement

-- | Whether the focus is a token rather than a node.
isToken :: Cursor kind -> Bool
isToken cursor = case focusElement cursor of
  Token _ -> True
  Node _ -> False

-- | The byte range of the focus in the whole tree, from the cached offset
-- and length.
range :: Cursor kind -> TextRange
range cursor = Range.fromLength (focusOffset cursor) (Internal.elementTextLength (focusElement cursor))

-- | The source text of the focus, rendered from its tokens.
text :: Cursor kind -> Text
text = Green.render . green

-- | The path from the root to the focus. Takes time proportional to the
-- depth, and builds a list of that length.
path :: Cursor kind -> Path
path cursor = go cursor []
  where
    go current steps = case focusParent current of
      Nothing -> steps
      Just up -> go up (focusIndex current : steps)

-- | The parent, or 'Nothing' at the root, in constant time.
parent :: Cursor kind -> Maybe (Cursor kind)
parent = focusParent

-- | A cursor on the root, in time proportional to the depth.
root :: Cursor kind -> Cursor kind
root cursor = maybe cursor root (focusParent cursor)

-- | The whole tree the cursor is in: the element at its 'root'.
tree :: Cursor kind -> GreenElement kind
tree = green . root

-- | Follow a path down from the focus. The first step that fails is reported.
at :: Path -> Cursor kind -> Either PathError (Cursor kind)
at [] cursor = Right cursor
at (index : rest) cursor = childAt index cursor >>= at rest

-- | The child at a zero-based position, in constant time.
childAt :: Int -> Cursor kind -> Either PathError (Cursor kind)
childAt index cursor
  | index < 0 = Left (NegativeChildIndex index)
  | otherwise = case focusElement cursor of
      Token _ -> Left CannotDescendIntoToken
      Node node
        | index >= Internal.childCount node -> Left (ChildIndexOutOfBounds index)
        | otherwise -> Right (child node cursor index)

-- The child at a valid index of the focused node.
child :: GreenNode kind -> Cursor kind -> Int -> Cursor kind
child node above index =
  Cursor (Internal.childAt node index) (focusOffset above + Internal.childStart node index) index (Just above)

childCount :: Cursor kind -> Int
childCount cursor = case focusElement cursor of
  Node node -> Internal.childCount node
  Token _ -> 0

-- | The first child, or 'Nothing' if there are none.
firstChild :: Cursor kind -> Maybe (Cursor kind)
firstChild = either (const Nothing) Just . childAt 0

-- | The last child, or 'Nothing' if there are none.
lastChild :: Cursor kind -> Maybe (Cursor kind)
lastChild cursor = either (const Nothing) Just (childAt (childCount cursor - 1) cursor)

-- | The next sibling, in constant time.
nextSibling :: Cursor kind -> Maybe (Cursor kind)
nextSibling = siblingAt 1

-- | The previous sibling, in constant time.
previousSibling :: Cursor kind -> Maybe (Cursor kind)
previousSibling = siblingAt (-1)

siblingAt :: Int -> Cursor kind -> Maybe (Cursor kind)
siblingAt step cursor = do
  above <- focusParent cursor
  case focusElement above of
    Node node
      | target >= 0, target < Internal.childCount node -> Just (child node above target)
    _ -> Nothing
  where
    target = focusIndex cursor + step

-- | The children, in source order. A token has none. The list is lazy and
-- costs a cursor per child consumed.
children :: Cursor kind -> [Cursor kind]
children cursor = case focusElement cursor of
  Token _ -> []
  Node node -> map (child node cursor) [0 .. Internal.childCount node - 1]

-- | Each ancestor of the focus, nearest first, up to the root. The focus is
-- not included, unlike Rowan's @ancestors@; 'descendants' does include it.
-- The ancestors already exist, so the list costs a cell per ancestor.
ancestors :: Cursor kind -> [Cursor kind]
ancestors = unfold parent . parent

-- | The focus and everything below it, in preorder. A subtree that appears
-- in several places is visited at each of them. The list is lazy, and
-- consuming all of it costs a cursor per element.
descendants :: Cursor kind -> [Cursor kind]
descendants cursor = go cursor []
  where
    go current rest = current : foldr go rest (children current)

-- | Entering or leaving an occurrence during a traversal.
data WalkEvent a = Enter a | Leave a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance NFData a => NFData (WalkEvent a) where
  rnf (Enter value) = rnf value
  rnf (Leave value) = rnf value

-- | A preorder traversal that also reports leaving each element, as a
-- formatter needs in order to close what it opened. Each element is entered,
-- its children are walked, and then it is left. The list is lazy.
walk :: Cursor kind -> [WalkEvent (Cursor kind)]
walk cursor = go cursor []
  where
    go current rest = Enter current : foldr go (Leave current : rest) (children current)

-- | Every token at or below the focus, in source order. A token focus gives
-- itself. The typed @Ast.tokens@ gives child tokens only. Between two
-- tokens the list passes over the elements between them.
descendantTokens :: Cursor kind -> [Cursor kind]
descendantTokens = filter isToken . descendants

-- | The first token at or below the focus. A token is its own first token.
-- Costs the elements visited until a token is found, which is the depth of
-- that token when no tokenless subtree comes before it.
firstToken :: Cursor kind -> Maybe (Cursor kind)
firstToken cursor
  | isToken cursor = Just cursor
  | otherwise = firstChild cursor >>= seek firstToken nextSibling

-- | The last token at or below the focus, at the cost of 'firstToken'.
lastToken :: Cursor kind -> Maybe (Cursor kind)
lastToken cursor
  | isToken cursor = Just cursor
  | otherwise = lastChild cursor >>= seek lastToken previousSibling

-- | The first token after the focus and everything below it, anywhere in the
-- tree. Costs up to the depth, plus any tokenless subtrees passed over; a
-- walk of the whole tree by repeated calls costs the size of the tree.
nextToken :: Cursor kind -> Maybe (Cursor kind)
nextToken cursor =
  (nextSibling cursor >>= seek firstToken nextSibling)
    <|> (parent cursor >>= nextToken)

-- | The last token before the focus, anywhere in the tree, at the cost of
-- 'nextToken'.
previousToken :: Cursor kind -> Maybe (Cursor kind)
previousToken cursor =
  (previousSibling cursor >>= seek lastToken previousSibling)
    <|> (parent cursor >>= previousToken)

-- | The tokens at an offset: none, one, or the two on either side of a
-- boundary, left first.
data TokenAtOffset a = NoToken | SingleToken a | BetweenTokens a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance NFData a => NFData (TokenAtOffset a) where
  rnf NoToken = ()
  rnf (SingleToken one) = rnf one
  rnf (BetweenTokens left right) = rnf left `seq` rnf right

-- | The tokens below the focus that touch an offset. An offset inside a token
-- gives that token; an offset between two tokens gives both, as an editor's
-- caret between two words touches both. The start and end of the text give
-- the first and last token. Empty tokens are skipped, and an offset outside
-- the focus gives 'NoToken'. Each level is binary-searched, and the empty
-- children at the offset are passed over.
tokenAt :: TextLength -> Cursor kind -> TokenAtOffset (Cursor kind)
tokenAt offset cursor = case take 2 (matching cursor) of
  [] -> NoToken
  [one] -> SingleToken one
  left : right : _ -> BetweenTokens left right
  where
    matching current
      | Range.isEmpty extent || offset < Range.start extent || offset > Range.end extent = []
      | otherwise = case focusElement current of
          Token _ -> [current]
          Node node ->
            let relative = offset - focusOffset current
                touching index = Internal.childStart node index <= relative
                candidates = takeWhile touching
                  [Internal.firstChildEndingAtOrAfter node relative .. Internal.childCount node - 1]
            in concatMap (matching . child node current) candidates
      where
        extent = range current

-- | The innermost element below the focus whose range contains the given
-- range, or 'Nothing' if the range lies outside the focus. When several
-- children contain it, as they can for an empty range on a boundary, the
-- leftmost is chosen. Each level is binary-searched.
covering :: TextRange -> Cursor kind -> Maybe (Cursor kind)
covering query cursor
  | not (query `Range.isSubrangeOf` range cursor) = Nothing
  | otherwise = Just (descend cursor)
  where
    descend current = case focusElement current of
      Node node
        | index < Internal.childCount node, Internal.childStart node index <= Range.start query - start ->
            descend (child node current index)
        where
          start = focusOffset current
          index = Internal.firstChildEndingAtOrAfter node (Range.end query - start)
      _ -> current

unfold :: (a -> Maybe a) -> Maybe a -> [a]
unfold _ Nothing = []
unfold step (Just value) = value : unfold step (step value)

seek :: (a -> Maybe b) -> (a -> Maybe a) -> a -> Maybe b
seek select step value = select value <|> (step value >>= seek select step)

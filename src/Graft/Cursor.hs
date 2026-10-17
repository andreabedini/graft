{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Cursors: immutable zippers over green trees. A cursor focuses one
-- occurrence of a node or token and knows its parents, siblings and absolute
-- byte range.
--
-- Edits through a cursor return a cursor on the edited tree, at the same
-- path, so a series of edits can keep navigating. Ancestors are rebuilt
-- lazily as the cursor moves up or sideways, and 'cursorTree' returns the
-- whole edited tree. Old cursors keep seeing the tree they were made from.
--
-- > -- Replace the text of every Name token, in one pass through one cursor.
-- > renameAll :: Text -> GreenElement Kind -> GreenElement Kind
-- > renameAll name root = maybe root (cursorTree . go) (cursorFirstToken (rootCursor root))
-- >   where
-- >     go cursor = maybe renamed go (cursorNextToken renamed)
-- >       where
-- >         renamed
-- >           | cursorKind cursor == Name = replaceFocus (Token (greenToken Name name)) cursor
-- >           | otherwise = cursor
--
-- Cursors have no equality of occurrences and expose no pointer identity;
-- compare 'cursorPath's within one tree instead.
module Graft.Cursor
  ( -- * Cursors
    Cursor
  , rootCursor
  , cursorGreen
  , cursorKind
  , cursorRange
  , cursorText
  , cursorPath
    -- * Navigation
  , cursorRoot
  , cursorAt
  , cursorParent
  , cursorChildAt
  , cursorFirstChild
  , cursorLastChild
  , cursorNextSibling
  , cursorPreviousSibling
    -- * Traversal
  , cursorChildren
  , cursorAncestors
  , cursorDescendants
  , WalkEvent (..)
  , cursorWalk
    -- * Tokens
  , cursorTokens
  , cursorFirstToken
  , cursorLastToken
  , cursorNextToken
  , cursorPreviousToken
    -- * Lookup by offset
  , TokenAtOffset (..)
  , tokenAtOffset
  , coveringElement
    -- * Editing
  , replaceFocus
  , modifyFocus
  , cursorTree
  , spliceChildrenFocus
  , insertChildFocus
  , removeChildFocus
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (..))
import Data.Text (Text)
import Graft.Edit (ChildEditError (..), Path, PathError (..))
import qualified Graft.Edit as Edit
import Graft.Green
import Graft.Green.Internal (childAt, childCount, childStart, firstChildEndingAtOrAfter, setChild)
import Graft.Range

-- The context of a focus: its parent as last rebuilt, the parent's absolute
-- offset, whether the parent differs from the child stored at its own index
-- in the grandparent, the grandparent context, and the focus index.
data Context kind
  = Top
  | Crumb !(GreenNode kind) !TextLength !Bool !(Context kind) !Int

-- | A position in a tree: a focused node or token and the path back to the
-- root. Nodes and tokens share the cursor type; match on 'cursorGreen' to
-- tell them apart. There is no 'Eq' instance, because two equal subtrees in
-- different places are different occurrences; compare 'cursorPath's instead.
-- The flag records that the focus has been replaced since the parent was
-- last rebuilt; the parent is rebuilt lazily on the way up or sideways.
data Cursor kind = Cursor !(GreenElement kind) !TextLength !Bool !(Context kind)

-- | Forces the focus and every ancestor in the cursor's context.
instance NFData kind => NFData (Cursor kind) where
  rnf (Cursor focus _ _ context) = rnf focus `seq` go context
    where
      go Top = ()
      go (Crumb parent _ _ above _) = rnf parent `seq` go above

instance Show kind => Show (Cursor kind) where
  showsPrec precedence cursor = showParen (precedence > 10) $
    showString "Cursor " . shows (cursorPath cursor) . showChar ' '
      . shows (cursorRange cursor) . showChar ' ' . shows (cursorGreen cursor)

-- | A cursor on the root of a tree, at offset zero. The root may be a token.
rootCursor :: GreenElement kind -> Cursor kind
rootCursor green = Cursor green 0 False Top

-- | The green element at the focus, without copying it.
cursorGreen :: Cursor kind -> GreenElement kind
cursorGreen (Cursor green _ _ _) = green

-- | The kind of the focused element.
cursorKind :: Cursor kind -> kind
cursorKind = elementKind . cursorGreen

-- | The byte range of the focus in the whole tree.
cursorRange :: Cursor kind -> TextRange
cursorRange (Cursor green offset _ _) = rangeFromLength offset (elementTextLength green)

-- | The source text of the focus.
cursorText :: Cursor kind -> Text
cursorText = renderElement . cursorGreen

-- | The path from the root to the focus. Takes time proportional to depth.
cursorPath :: Cursor kind -> Path
cursorPath (Cursor _ _ _ context) = go context []
  where
    go Top path = path
    go (Crumb _ _ _ above index) path = go above (index : path)

-- | The parent, or 'Nothing' at the root. Constant time, unless the focus was
-- replaced, in which case the parent is rebuilt by copying its child array.
cursorParent :: Cursor kind -> Maybe (Cursor kind)
cursorParent (Cursor _ _ _ Top) = Nothing
cursorParent (Cursor focus _ edited (Crumb parent start parentEdited above index)) =
  Just (Cursor (Node (rebuilt edited index focus parent)) start (edited || parentEdited) above)

{-# INLINE rebuilt #-}
rebuilt :: Bool -> Int -> GreenElement kind -> GreenNode kind -> GreenNode kind
rebuilt edited index focus parent
  | edited = setChild index focus parent
  | otherwise = parent

-- | A cursor on the root, rebuilding any ancestors of replaced foci.
cursorRoot :: Cursor kind -> Cursor kind
cursorRoot cursor@(Cursor _ _ _ Top) = cursor
cursorRoot (Cursor focus _ edited (Crumb parent start parentEdited above index)) =
  climb (rebuilt edited index focus parent) start (edited || parentEdited) above
  where
    climb !node !offset !nodeEdited Top = Cursor (Node node) offset nodeEdited Top
    climb !node _ !nodeEdited (Crumb grandparent offset grandparentEdited context position) =
      climb (rebuilt nodeEdited position (Node node) grandparent) offset
        (nodeEdited || grandparentEdited) context

-- | Follow a path down from the focus, failing as 'Graft.Edit.elementAt' does.
cursorAt :: Path -> Cursor kind -> Either PathError (Cursor kind)
cursorAt [] cursor = Right cursor
cursorAt (index : rest) cursor = cursorChildAt index cursor >>= cursorAt rest

-- | The child at a zero-based position, in constant time.
cursorChildAt :: Int -> Cursor kind -> Either PathError (Cursor kind)
cursorChildAt index (Cursor green offset edited context)
  | index < 0 = Left (NegativeChildIndex index)
  | otherwise = case green of
      Token _ -> Left CannotDescendIntoToken
      Node node
        | index >= childCount node -> Left (ChildIndexOutOfBounds index)
        | otherwise -> Right (childCursor node offset edited context index)

childCursor :: GreenNode kind -> TextLength -> Bool -> Context kind -> Int -> Cursor kind
childCursor node offset edited context index =
  Cursor (childAt node index) (offset + childStart node index) False
    (Crumb node offset edited context index)

-- | The first child, or 'Nothing' if there are none.
cursorFirstChild :: Cursor kind -> Maybe (Cursor kind)
cursorFirstChild = either (const Nothing) Just . cursorChildAt 0

-- | The last child, or 'Nothing' if there are none.
cursorLastChild :: Cursor kind -> Maybe (Cursor kind)
cursorLastChild cursor = case cursorGreen cursor of
  Node node -> either (const Nothing) Just (cursorChildAt (childCount node - 1) cursor)
  Token _ -> Nothing

-- | The next sibling. Constant time, unless the focus was replaced, in which
-- case the parent is rebuilt first.
cursorNextSibling :: Cursor kind -> Maybe (Cursor kind)
cursorNextSibling = siblingAt 1

-- | The previous sibling, at the same cost as 'cursorNextSibling'.
cursorPreviousSibling :: Cursor kind -> Maybe (Cursor kind)
cursorPreviousSibling = siblingAt (-1)

siblingAt :: Int -> Cursor kind -> Maybe (Cursor kind)
siblingAt _ (Cursor _ _ _ Top) = Nothing
siblingAt step (Cursor focus _ edited (Crumb parent start parentEdited above index))
  | target < 0 || target >= childCount parent = Nothing
  | otherwise = Just (childCursor (rebuilt edited index focus parent) start (edited || parentEdited) above target)
  where
    target = index + step

-- | The children, in source order. A token has none.
cursorChildren :: Cursor kind -> [Cursor kind]
cursorChildren (Cursor green offset edited context) = case green of
  Token _ -> []
  Node node -> map (childCursor node offset edited context) [0 .. childCount node - 1]

-- | The focus and then each ancestor, up to the root.
cursorAncestors :: Cursor kind -> [Cursor kind]
cursorAncestors = unfold cursorParent . Just

-- | The focus and everything below it, in preorder. A subtree that appears
-- in several places is visited at each of them. The list is lazy.
cursorDescendants :: Cursor kind -> [Cursor kind]
cursorDescendants cursor = go cursor []
  where
    go current rest = current : foldr go rest (cursorChildren current)

-- | Entering or leaving an occurrence during a traversal.
data WalkEvent a = Enter a | Leave a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance NFData a => NFData (WalkEvent a) where
  rnf (Enter value) = rnf value
  rnf (Leave value) = rnf value

-- | A preorder traversal that also reports leaving each element, as a
-- formatter needs in order to close what it opened. Each element is entered,
-- its children are walked, and then it is left. The list is lazy.
cursorWalk :: Cursor kind -> [WalkEvent (Cursor kind)]
cursorWalk cursor = go cursor []
  where
    go current rest = Enter current : foldr go (Leave current : rest) (cursorChildren current)

-- | Every token at or below the focus, in source order.
cursorTokens :: Cursor kind -> [Cursor kind]
cursorTokens = filter isToken . cursorDescendants
  where
    isToken cursor = case cursorGreen cursor of
      Token _ -> True
      Node _ -> False

-- | The first token at or below the focus. A token is its own first token.
cursorFirstToken :: Cursor kind -> Maybe (Cursor kind)
cursorFirstToken cursor = case cursorGreen cursor of
  Token _ -> Just cursor
  Node _ -> cursorFirstChild cursor >>= seek cursorFirstToken cursorNextSibling

-- | The last token at or below the focus.
cursorLastToken :: Cursor kind -> Maybe (Cursor kind)
cursorLastToken cursor = case cursorGreen cursor of
  Token _ -> Just cursor
  Node _ -> cursorLastChild cursor >>= seek cursorLastToken cursorPreviousSibling

-- | The first token after the focus and everything below it, anywhere in the
-- tree.
cursorNextToken :: Cursor kind -> Maybe (Cursor kind)
cursorNextToken cursor =
  (cursorNextSibling cursor >>= seek cursorFirstToken cursorNextSibling)
    <|> (cursorParent cursor >>= cursorNextToken)

-- | The last token before the focus, anywhere in the tree.
cursorPreviousToken :: Cursor kind -> Maybe (Cursor kind)
cursorPreviousToken cursor =
  (cursorPreviousSibling cursor >>= seek cursorLastToken cursorPreviousSibling)
    <|> (cursorParent cursor >>= cursorPreviousToken)

-- | The tokens at an offset: none, one, or the two on either side of a
-- boundary, left first.
data TokenAtOffset a = NoToken | SingleToken a | BetweenTokens a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance NFData a => NFData (TokenAtOffset a) where
  rnf NoToken = ()
  rnf (SingleToken token) = rnf token
  rnf (BetweenTokens left right) = rnf left `seq` rnf right

-- | The tokens below the focus that touch an offset. An offset inside a token
-- gives that token; an offset between two tokens gives both, as an editor's
-- caret between two words touches both. The start and end of the text give
-- the first and last token. Empty tokens are skipped, and an offset outside
-- the focus gives 'NoToken'. Each level is binary-searched.
tokenAtOffset :: TextLength -> Cursor kind -> TokenAtOffset (Cursor kind)
tokenAtOffset offset cursor = case take 2 (matching cursor) of
  [] -> NoToken
  [token] -> SingleToken token
  left : right : _ -> BetweenTokens left right
  where
    matching current@(Cursor green start edited context)
      | rangeIsEmpty range || offset < rangeStart range || offset > rangeEnd range = []
      | otherwise = case green of
          Token _ -> [current]
          Node node ->
            let relative = offset - start
                touching index = childStart node index <= relative
                candidates = takeWhile touching
                  [firstChildEndingAtOrAfter node relative .. childCount node - 1]
            in concatMap (matching . childCursor node start edited context) candidates
      where
        range = cursorRange current

-- | The innermost element below the focus whose range contains the given
-- range, or 'Nothing' if the range lies outside the focus. When several
-- children contain it, as they can for an empty range on a boundary, the
-- leftmost is chosen. Each level is binary-searched.
coveringElement :: TextRange -> Cursor kind -> Maybe (Cursor kind)
coveringElement query cursor
  | not (rangeContainsRange (cursorRange cursor) query) = Nothing
  | otherwise = Just (descend cursor)
  where
    descend current@(Cursor green start edited context) = case green of
      Node node
        | index < childCount node, childStart node index <= rangeStart query - start ->
            descend (childCursor node start edited context index)
        where
          index = firstChildEndingAtOrAfter node (rangeEnd query - start)
      _ -> current

-- | Replace the focus, returning a cursor at the same path in the edited
-- tree. The replacement can be of any kind, node or token. Ancestors are
-- rebuilt only when the cursor moves up or sideways, so a run of edits in one
-- region rebuilds each ancestor once. The original cursor still sees the
-- original tree. 'cursorTree' gives the edited tree.
replaceFocus :: GreenElement kind -> Cursor kind -> Cursor kind
replaceFocus replacement (Cursor _ offset _ context) = Cursor replacement offset True context

-- | Replace the focus with a function of it, as 'replaceFocus'.
modifyFocus :: (GreenElement kind -> GreenElement kind) -> Cursor kind -> Cursor kind
modifyFocus edit cursor = replaceFocus (edit (cursorGreen cursor)) cursor

-- | The whole tree the cursor is in, including every edit made through it.
cursorTree :: Cursor kind -> GreenElement kind
cursorTree = cursorGreen . cursorRoot

-- | 'Graft.Edit.spliceChildren' on the focused node, returning a cursor on
-- the edited node. A token focus gives 'CannotEditTokenChildren'.
spliceChildrenFocus
  :: Int -> Int -> [GreenElement kind] -> Cursor kind
  -> Either ChildEditError (Cursor kind)
spliceChildrenFocus start end replacements =
  editChildrenFocus (Edit.spliceChildren start end replacements)

-- | 'Graft.Edit.insertChild' on the focused node, as 'spliceChildrenFocus'.
insertChildFocus
  :: Int -> GreenElement kind -> Cursor kind
  -> Either ChildEditError (Cursor kind)
insertChildFocus index element = editChildrenFocus (Edit.insertChild index element)

-- | 'Graft.Edit.removeChild' on the focused node, as 'spliceChildrenFocus'.
removeChildFocus :: Int -> Cursor kind -> Either ChildEditError (Cursor kind)
removeChildFocus index = editChildrenFocus (Edit.removeChild index)

editChildrenFocus
  :: (GreenNode kind -> Either ChildEditError (GreenNode kind))
  -> Cursor kind -> Either ChildEditError (Cursor kind)
editChildrenFocus edit cursor = case cursorGreen cursor of
  Token _ -> Left CannotEditTokenChildren
  Node node -> do
    edited <- edit node
    pure (replaceFocus (Node edited) cursor)

unfold :: (a -> Maybe a) -> Maybe a -> [a]
unfold _ Nothing = []
unfold step (Just value) = value : unfold step (step value)

seek :: (a -> Maybe b) -> (a -> Maybe a) -> a -> Maybe b
seek select step value = select value <|> (step value >>= seek select step)

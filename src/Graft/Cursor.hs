{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}

-- | An immutable red-tree zipper over green elements. A cursor denotes an
-- occurrence in one tree snapshot, with parents, siblings and absolute offsets.
-- Edits through a cursor return a cursor on the edited snapshot, so a series
-- of edits can continue navigating; old cursors keep their snapshots.
-- No pointer identity is exposed.
module Graft.Cursor
  ( Cursor
  , rootCursor
  , cursorGreen
  , cursorKind
  , cursorRange
  , cursorPath
  , cursorRoot
  , cursorAt
  , cursorParent
  , cursorChildAt
  , cursorFirstChild
  , cursorLastChild
  , cursorNextSibling
  , cursorPreviousSibling
  , cursorChildren
  , cursorAncestors
  , cursorDescendants
  , cursorTokens
  , cursorFirstToken
  , cursorLastToken
  , cursorNextToken
  , cursorPreviousToken
  , TokenAtOffset (..)
  , tokenAtOffset
  , coveringElement
  , replaceFocus
  , modifyFocus
  , cursorTree
  , spliceChildrenFocus
  , insertChildFocus
  , removeChildFocus
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (..))
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

-- | A focus and its context. A node and a token use the same cursor type;
-- distinguish them by inspecting 'cursorGreen'. Equality of occurrences is
-- intentionally not supplied: compare paths within a known snapshot instead.
--
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

-- | Focus a green root at byte offset zero. A token root is allowed.
rootCursor :: GreenElement kind -> Cursor kind
rootCursor green = Cursor green 0 False Top

-- | The original green element at the focus, without copying it.
cursorGreen :: Cursor kind -> GreenElement kind
cursorGreen (Cursor green _ _ _) = green

-- | The caller-defined kind of the focused element.
cursorKind :: Cursor kind -> kind
cursorKind = elementKind . cursorGreen

-- | Absolute half-open byte range, including all token text and trivia.
cursorRange :: Cursor kind -> TextRange
cursorRange (Cursor green offset _ _) = rangeFromLength offset (elementTextLength green)

-- | Child path from the snapshot's root to this occurrence. Takes time
-- proportional to depth; counts both nodes and tokens.
cursorPath :: Cursor kind -> Path
cursorPath (Cursor _ _ _ context) = go context []
  where
    go Top path = path
    go (Crumb _ _ _ above index) path = go above (index : path)

-- | The immediate parent, or 'Nothing' at the root. Constant time unless
-- the focus was replaced, in which case the parent's child array is copied.
cursorParent :: Cursor kind -> Maybe (Cursor kind)
cursorParent (Cursor _ _ _ Top) = Nothing
cursorParent (Cursor focus _ edited (Crumb parent start parentEdited above index)) =
  Just (Cursor (Node (rebuilt edited index focus parent)) start (edited || parentEdited) above)

rebuilt :: Bool -> Int -> GreenElement kind -> GreenNode kind -> GreenNode kind
rebuilt edited index focus parent
  | edited = setChild index focus parent
  | otherwise = parent

-- | Ascend to the snapshot's root, rebuilding only ancestors of replaced foci.
cursorRoot :: Cursor kind -> Cursor kind
cursorRoot cursor = maybe cursor cursorRoot (cursorParent cursor)

-- | Follow a path relative to the current focus. Validation matches
-- 'Graft.Edit.elementAt', including negative-index precedence.
cursorAt :: Path -> Cursor kind -> Either PathError (Cursor kind)
cursorAt [] cursor = Right cursor
cursorAt (index : rest) cursor = cursorChildAt index cursor >>= cursorAt rest

-- | Focus a zero-based immediate child in constant time, using the parent's
-- cached child offsets.
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

-- | First immediate child, including tokens and zero-length elements.
cursorFirstChild :: Cursor kind -> Maybe (Cursor kind)
cursorFirstChild = either (const Nothing) Just . cursorChildAt 0

-- | Last immediate child, in constant time.
cursorLastChild :: Cursor kind -> Maybe (Cursor kind)
cursorLastChild cursor = case cursorGreen cursor of
  Node node -> either (const Nothing) Just (cursorChildAt (childCount node - 1) cursor)
  Token _ -> Nothing

-- | Following sibling, including empty elements. Constant time unless the
-- focus was replaced, in which case the parent is rebuilt first.
cursorNextSibling :: Cursor kind -> Maybe (Cursor kind)
cursorNextSibling = siblingAt 1

-- | Preceding sibling, including empty elements. Costs as 'cursorNextSibling'.
cursorPreviousSibling :: Cursor kind -> Maybe (Cursor kind)
cursorPreviousSibling = siblingAt (-1)

siblingAt :: Int -> Cursor kind -> Maybe (Cursor kind)
siblingAt _ (Cursor _ _ _ Top) = Nothing
siblingAt step (Cursor focus _ edited (Crumb parent start parentEdited above index))
  | target < 0 || target >= childCount parent = Nothing
  | otherwise = Just (childCursor (rebuilt edited index focus parent) start (edited || parentEdited) above target)
  where
    target = index + step

-- | Immediate children in source order. A token has no children.
cursorChildren :: Cursor kind -> [Cursor kind]
cursorChildren (Cursor green offset edited context) = case green of
  Token _ -> []
  Node node -> map (childCursor node offset edited context) [0 .. childCount node - 1]

-- | The focus followed by its parents, nearest first.
cursorAncestors :: Cursor kind -> [Cursor kind]
cursorAncestors = unfold cursorParent . Just

-- | Preorder traversal including the focus, every node, and every token.
-- Shared green values are visited once per occurrence. Results are lazy, and
-- the traversal takes time proportional to the number of occurrences.
cursorDescendants :: Cursor kind -> [Cursor kind]
cursorDescendants cursor = go cursor []
  where
    go current rest = current : foldr go rest (cursorChildren current)

-- | All token occurrences in this subtree in source order, including empty
-- tokens and trivia. Does not include tokens outside the focus.
cursorTokens :: Cursor kind -> [Cursor kind]
cursorTokens = filter isToken . cursorDescendants
  where
    isToken cursor = case cursorGreen cursor of
      Token _ -> True
      Node _ -> False

-- | First token in this subtree, skipping tokenless nodes. A token focus
-- returns itself, even when its text is empty.
cursorFirstToken :: Cursor kind -> Maybe (Cursor kind)
cursorFirstToken cursor = case cursorGreen cursor of
  Token _ -> Just cursor
  Node _ -> cursorFirstChild cursor >>= seek cursorFirstToken cursorNextSibling

-- | Last token in this subtree, skipping tokenless nodes.
cursorLastToken :: Cursor kind -> Maybe (Cursor kind)
cursorLastToken cursor = case cursorGreen cursor of
  Token _ -> Just cursor
  Node _ -> cursorLastChild cursor >>= seek cursorLastToken cursorPreviousSibling

-- | First token after the entire focused subtree in the snapshot, ascending
-- as needed. Includes empty tokens; from a token, moves to the next token.
cursorNextToken :: Cursor kind -> Maybe (Cursor kind)
cursorNextToken cursor =
  (cursorNextSibling cursor >>= seek cursorFirstToken cursorNextSibling)
    <|> (cursorParent cursor >>= cursorNextToken)

-- | Last token before the entire focused subtree in the snapshot.
cursorPreviousToken :: Cursor kind -> Maybe (Cursor kind)
cursorPreviousToken cursor =
  (cursorPreviousSibling cursor >>= seek cursorLastToken cursorPreviousSibling)
    <|> (cursorParent cursor >>= cursorPreviousToken)

-- | Offset lookup is unambiguous inside a token and may select both tokens
-- at a shared boundary. The pair is ordered left to right.
data TokenAtOffset a = NoToken | SingleToken a | BetweenTokens a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance NFData a => NFData (TokenAtOffset a) where
  rnf NoToken = ()
  rnf (SingleToken token) = rnf token
  rnf (BetweenTokens left right) = rnf left `seq` rnf right

-- | Find nonempty tokens touching an absolute byte offset within this focus.
-- Uses closed endpoint contact despite the half-open ranges: start and EOF
-- select the first and last nonempty token; a shared boundary selects both.
-- Empty tokens are ignored. An offset outside the focus returns 'NoToken'.
-- Offsets inside multibyte characters are accepted. Each level binary-searches
-- the cached child offsets, and the search stops after two tokens.
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

-- | Descend repeatedly into the first child whose range contains the query.
-- Returns the innermost element along that branch, or 'Nothing' if the query
-- lies outside the focus. At ambiguous empty ranges (including EOF), ties
-- favor the first covering child in source order, including empty elements.
-- Each level binary-searches the cached child offsets.
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

-- | Replace the focused element, keeping the cursor at the same path and
-- start offset. The result navigates the edited snapshot: ancestors are
-- rebuilt lazily when you move up or sideways, so a run of edits in one
-- region pays for each ancestor once. The original cursor and its snapshot
-- stay unchanged. Replacement may change kind or switch node/token form,
-- including at the root. Use 'cursorTree' to obtain the edited tree.
replaceFocus :: GreenElement kind -> Cursor kind -> Cursor kind
replaceFocus replacement (Cursor _ offset _ context) = Cursor replacement offset True context

-- | Apply a function to the focused element, as 'replaceFocus'.
modifyFocus :: (GreenElement kind -> GreenElement kind) -> Cursor kind -> Cursor kind
modifyFocus edit cursor = replaceFocus (edit (cursorGreen cursor)) cursor

-- | The whole tree that this cursor navigates, including every edit made
-- through it. Rebuilds the ancestors of replaced foci, sharing everything else.
cursorTree :: Cursor kind -> GreenElement kind
cursorTree = cursorGreen . cursorRoot

-- | Splice immediate children of a focused node, keeping the focus on that
-- node. Bounds and sharing follow 'Edit.spliceChildren'. A token focus returns
-- 'CannotEditTokenChildren' before checking boundaries. The original cursor
-- retains its snapshot; the result observes shifted paths and byte ranges.
-- No punctuation, whitespace or comments are inserted implicitly.
spliceChildrenFocus
  :: Int -> Int -> [GreenElement kind] -> Cursor kind
  -> Either ChildEditError (Cursor kind)
spliceChildrenFocus start end replacements =
  editChildrenFocus (Edit.spliceChildren start end replacements)

-- | Insert one child into a focused node. Its child count appends. Validation
-- and focus follow 'spliceChildrenFocus'.
insertChildFocus
  :: Int -> GreenElement kind -> Cursor kind
  -> Either ChildEditError (Cursor kind)
insertChildFocus index element = editChildrenFocus (Edit.insertChild index element)

-- | Remove an existing child of a focused node. Removing the last child
-- leaves an empty node. Follows 'spliceChildrenFocus'; the child count is out
-- of bounds for removal.
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

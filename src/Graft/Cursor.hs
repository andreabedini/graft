{-# LANGUAGE BangPatterns #-}

-- | An immutable red-tree zipper over green elements. A cursor denotes an
-- occurrence in one tree snapshot, with parents, siblings and absolute offsets.
-- Navigation does not edit or rebuild green nodes. No pointer identity is exposed.
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
  , spliceChildrenFocus
  , insertChildFocus
  , removeChildFocus
  ) where

import Control.Applicative ((<|>))
import Data.List (find)
import Graft.Edit (ChildEditError (..), Path, PathError (..))
import qualified Graft.Edit as Edit
import Graft.Green
import Graft.Range

-- The crumb retains the original parent, its absolute offset and context,
-- the child's index, reversed preceding siblings, and following siblings.
data Crumb kind = Crumb
  !(GreenNode kind) !TextLength !(Maybe (Crumb kind)) !Int
  [GreenElement kind] [GreenElement kind]

-- | A focus and its context. A node and a token use the same cursor type;
-- distinguish them by inspecting 'cursorGreen'. Equality of occurrences is
-- intentionally not supplied: compare paths within a known snapshot instead.
data Cursor kind = Cursor !(GreenElement kind) !TextLength !(Maybe (Crumb kind))

instance Show kind => Show (Cursor kind) where
  showsPrec precedence cursor = showParen (precedence > 10) $
    showString "Cursor " . shows (cursorPath cursor) . showChar ' '
      . shows (cursorRange cursor) . showChar ' ' . shows (cursorGreen cursor)

-- | Focus a green root at byte offset zero. A token root is allowed.
rootCursor :: GreenElement kind -> Cursor kind
rootCursor green = Cursor green 0 Nothing

-- | The original green element at the focus, without copying it.
cursorGreen :: Cursor kind -> GreenElement kind
cursorGreen (Cursor green _ _) = green

-- | The caller-defined kind of the focused element.
cursorKind :: Cursor kind -> kind
cursorKind = elementKind . cursorGreen

-- | Absolute half-open byte range, including all token text and trivia.
cursorRange :: Cursor kind -> TextRange
cursorRange (Cursor green offset _) = rangeFromLength offset (elementTextLength green)

-- | Child path from the snapshot's root to this occurrence. Takes time
-- proportional to depth; counts both nodes and tokens.
cursorPath :: Cursor kind -> Path
cursorPath (Cursor _ _ context) = go context []
  where
    go Nothing path = path
    go (Just (Crumb _ _ above index _ _)) path = go above (index : path)

-- | Restore the original parent in constant time. The root has no parent.
cursorParent :: Cursor kind -> Maybe (Cursor kind)
cursorParent (Cursor _ _ Nothing) = Nothing
cursorParent (Cursor _ _ (Just (Crumb node offset above _ _ _))) =
  Just (Cursor (Node node) offset above)

-- | Ascend to the snapshot's root without rebuilding it.
cursorRoot :: Cursor kind -> Cursor kind
cursorRoot cursor = maybe cursor cursorRoot (cursorParent cursor)

-- | Follow a path relative to the current focus. Validation matches
-- 'Graft.Edit.elementAt', including negative-index precedence.
cursorAt :: Path -> Cursor kind -> Either PathError (Cursor kind)
cursorAt [] cursor = Right cursor
cursorAt (index : rest) cursor = cursorChildAt index cursor >>= cursorAt rest

-- | Focus a zero-based immediate child. Scans preceding siblings and sums
-- their cached byte lengths; it does not traverse their descendants.
cursorChildAt :: Int -> Cursor kind -> Either PathError (Cursor kind)
cursorChildAt index (Cursor green offset above)
  | index < 0 = Left (NegativeChildIndex index)
  | otherwise = case green of
      Token _ -> Left CannotDescendIntoToken
      Node node -> go node index offset [] (nodeChildren node)
  where
    go _ _ _ _ [] = Left (ChildIndexOutOfBounds index)
    go node 0 !start before (child : after) =
      Right (Cursor child start (Just (Crumb node offset above index before after)))
    go node !remaining !start before (child : after) =
      go node (remaining - 1) (start + elementTextLength child) (child : before) after

-- | First immediate child, including tokens and zero-length elements.
cursorFirstChild :: Cursor kind -> Maybe (Cursor kind)
cursorFirstChild = either (const Nothing) Just . cursorChildAt 0

-- | Last immediate child. Scans the child list once.
cursorLastChild :: Cursor kind -> Maybe (Cursor kind)
cursorLastChild cursor = go <$> cursorFirstChild cursor
  where
    go child = maybe child go (cursorNextSibling child)

-- | Following sibling in constant time, including empty elements.
cursorNextSibling :: Cursor kind -> Maybe (Cursor kind)
cursorNextSibling (Cursor green offset (Just (Crumb node start above index before (next : after)))) =
  Just (Cursor next (offset + elementTextLength green)
    (Just (Crumb node start above (index + 1) (green : before) after)))
cursorNextSibling _ = Nothing

-- | Preceding sibling in constant time, including empty elements.
cursorPreviousSibling :: Cursor kind -> Maybe (Cursor kind)
cursorPreviousSibling (Cursor green offset (Just (Crumb node start above index (previous : before) after))) =
  Just (Cursor previous (offset - elementTextLength previous)
    (Just (Crumb node start above (index - 1) before (green : after))))
cursorPreviousSibling _ = Nothing

-- | Immediate children in source order. A token has no children.
cursorChildren :: Cursor kind -> [Cursor kind]
cursorChildren = unfold cursorNextSibling . cursorFirstChild

-- | The focus followed by its parents, nearest first.
cursorAncestors :: Cursor kind -> [Cursor kind]
cursorAncestors = unfold cursorParent . Just

-- | Preorder traversal including the focus, every node, and every token.
-- Shared green values are visited once per occurrence. Results are lazy.
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
  deriving (Eq, Show)

-- | Find nonempty tokens touching an absolute byte offset within this focus.
-- Uses closed endpoint contact despite the half-open ranges: start and EOF
-- select the first and last nonempty token; a shared boundary selects both.
-- Empty tokens are ignored. An offset outside the focus returns 'NoToken'.
-- Offsets inside multibyte characters are accepted. Unrelated subtrees are
-- pruned using their cached ranges, and the search stops after two tokens.
tokenAtOffset :: TextLength -> Cursor kind -> TokenAtOffset (Cursor kind)
tokenAtOffset offset cursor = case take 2 (matching cursor) of
  [] -> NoToken
  [token] -> SingleToken token
  left : right : _ -> BetweenTokens left right
  where
    matching current
      | rangeIsEmpty range || offset < rangeStart range || offset > rangeEnd range = []
      | otherwise = case cursorGreen current of
          Token _ -> [current]
          Node _ -> concatMap matching (takeWhile startsBefore (cursorChildren current))
      where
        range = cursorRange current
    startsBefore child = rangeStart (cursorRange child) <= offset

-- | Descend repeatedly into the first child whose range contains the query.
-- Returns the innermost element along that branch, or 'Nothing' if the query
-- lies outside the focus. At ambiguous empty ranges (including EOF), ties
-- favor the first covering child in source order, including empty elements.
coveringElement :: TextRange -> Cursor kind -> Maybe (Cursor kind)
coveringElement query cursor
  | not (rangeContainsRange (cursorRange cursor) query) = Nothing
  | otherwise = case find (\child -> rangeContainsRange (cursorRange child) query) (cursorChildren cursor) of
      Nothing -> Just cursor
      Just child -> coveringElement query child

-- | Replace the focused element and return a new green root. Only ancestors
-- and their child-list prefixes are rebuilt; other green elements and list
-- suffixes are reused. The cursor and its original snapshot stay unchanged.
-- Replacement may change kind or switch node/token form, including at the root.
-- Cost follows the sum of ancestor child counts. Start a new cursor to navigate
-- the result; offsets of later occurrences may have changed.
replaceFocus :: GreenElement kind -> Cursor kind -> GreenElement kind
replaceFocus replacement (Cursor _ _ context) = rebuild replacement context
  where
    rebuild edited Nothing = edited
    rebuild edited (Just (Crumb node _ above _ before after)) =
      rebuild (Node (greenNode (nodeKind node) (reverse before ++ edited : after))) above

-- | Splice immediate children of a focused node and return a new green root.
-- Bounds and sharing follow 'Edit.spliceChildren'. A token focus returns
-- 'CannotEditTokenChildren' before checking boundaries. The cursor retains its
-- original snapshot; start a new cursor on the result to observe shifted paths
-- and byte ranges. No punctuation, whitespace or comments are inserted implicitly.
spliceChildrenFocus
  :: Int -> Int -> [GreenElement kind] -> Cursor kind
  -> Either ChildEditError (GreenElement kind)
spliceChildrenFocus start end replacements =
  editChildrenFocus (Edit.spliceChildren start end replacements)

-- | Insert one child into a focused node. Its child count appends. Returns a
-- new root with the same validation and snapshot rules as 'spliceChildrenFocus'.
insertChildFocus
  :: Int -> GreenElement kind -> Cursor kind
  -> Either ChildEditError (GreenElement kind)
insertChildFocus index element = editChildrenFocus (Edit.insertChild index element)

-- | Remove an existing child of a focused node, returning a new root. Removing
-- the last child leaves an empty node. Follows 'spliceChildrenFocus' snapshot
-- rules; the child count is out of bounds for removal.
removeChildFocus :: Int -> Cursor kind -> Either ChildEditError (GreenElement kind)
removeChildFocus index = editChildrenFocus (Edit.removeChild index)

editChildrenFocus
  :: (GreenNode kind -> Either ChildEditError (GreenNode kind))
  -> Cursor kind -> Either ChildEditError (GreenElement kind)
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

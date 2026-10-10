-- | An immutable red-tree zipper over green elements. A cursor denotes an
-- occurrence in one tree snapshot, with parents, siblings and absolute offsets.
-- Navigation does not edit or rebuild green nodes. No pointer identity is exposed.
module Graft.Cursor
  ( Cursor
  , newCursor
  , cursorGreen
  , cursorKind
  , cursorRange
  , cursorPath
  , cursorRoot
  , cursorAt
  , parent
  , childAt
  , firstChild
  , lastChild
  , nextSibling
  , previousSibling
  , children
  , ancestors
  , descendants
  , tokens
  , firstToken
  , lastToken
  , nextToken
  , previousToken
  , TokenAtOffset (..)
  , tokenAtOffset
  , coveringElement
  , replaceFocus
  ) where

import Control.Applicative ((<|>))
import Data.List (find)
import Graft.Edit (Path, PathError (..))
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
newCursor :: GreenElement kind -> Cursor kind
newCursor green = Cursor green 0 Nothing

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
parent :: Cursor kind -> Maybe (Cursor kind)
parent (Cursor _ _ Nothing) = Nothing
parent (Cursor _ _ (Just (Crumb node offset above _ _ _))) =
  Just (Cursor (Node node) offset above)

-- | Ascend to the snapshot's root without rebuilding it.
cursorRoot :: Cursor kind -> Cursor kind
cursorRoot cursor = maybe cursor cursorRoot (parent cursor)

-- | Follow a path relative to the current focus. Validation matches
-- 'Graft.Edit.elementAt', including negative-index precedence.
cursorAt :: Path -> Cursor kind -> Either PathError (Cursor kind)
cursorAt [] cursor = Right cursor
cursorAt (index : rest) cursor = childAt index cursor >>= cursorAt rest

-- | Focus a zero-based immediate child. Scans preceding siblings and sums
-- their cached byte lengths; it does not traverse their descendants.
childAt :: Int -> Cursor kind -> Either PathError (Cursor kind)
childAt index (Cursor green offset above)
  | index < 0 = Left (NegativeChildIndex index)
  | otherwise = case green of
      Token _ -> Left CannotDescendIntoToken
      Node node -> go node index offset [] (nodeChildren node)
  where
    go _ _ _ _ [] = Left (ChildIndexOutOfBounds index)
    go node 0 start before (child : after) =
      Right (Cursor child start (Just (Crumb node offset above index before after)))
    go node remaining start before (child : after) =
      go node (remaining - 1) (start + elementTextLength child) (child : before) after

-- | First immediate child, including tokens and zero-length elements.
firstChild :: Cursor kind -> Maybe (Cursor kind)
firstChild = either (const Nothing) Just . childAt 0

-- | Last immediate child. Scans the child list once.
lastChild :: Cursor kind -> Maybe (Cursor kind)
lastChild cursor = firstChild cursor >>= Just . go
  where
    go child = maybe child go (nextSibling child)

-- | Following sibling in constant time, including empty elements.
nextSibling :: Cursor kind -> Maybe (Cursor kind)
nextSibling (Cursor green offset (Just (Crumb node start above index before (next : after)))) =
  Just (Cursor next (offset + elementTextLength green)
    (Just (Crumb node start above (index + 1) (green : before) after)))
nextSibling _ = Nothing

-- | Preceding sibling in constant time, including empty elements.
previousSibling :: Cursor kind -> Maybe (Cursor kind)
previousSibling (Cursor green offset (Just (Crumb node start above index (previous : before) after))) =
  Just (Cursor previous (offset - elementTextLength previous)
    (Just (Crumb node start above (index - 1) before (green : after))))
previousSibling _ = Nothing

-- | Immediate children in source order. A token has no children.
children :: Cursor kind -> [Cursor kind]
children = unfold nextSibling . firstChild

-- | The focus followed by its parents, nearest first.
ancestors :: Cursor kind -> [Cursor kind]
ancestors = unfold parent . Just

-- | Preorder traversal including the focus, every node, and every token.
-- Shared green values are visited once per occurrence. Results are lazy.
descendants :: Cursor kind -> [Cursor kind]
descendants cursor = cursor : concatMap descendants (children cursor)

-- | All token occurrences in this subtree in source order, including empty
-- tokens and trivia. Does not include tokens outside the focus.
tokens :: Cursor kind -> [Cursor kind]
tokens = filter isToken . descendants
  where
    isToken cursor = case cursorGreen cursor of
      Token _ -> True
      Node _ -> False

-- | First token in this subtree, skipping tokenless nodes. A token focus
-- returns itself, even when its text is empty.
firstToken :: Cursor kind -> Maybe (Cursor kind)
firstToken cursor = case cursorGreen cursor of
  Token _ -> Just cursor
  Node _ -> firstChild cursor >>= seek firstToken nextSibling

-- | Last token in this subtree, skipping tokenless nodes.
lastToken :: Cursor kind -> Maybe (Cursor kind)
lastToken cursor = case cursorGreen cursor of
  Token _ -> Just cursor
  Node _ -> lastChild cursor >>= seek lastToken previousSibling

-- | First token after the entire focused subtree in the snapshot, ascending
-- as needed. Includes empty tokens; from a token, moves to the next token.
nextToken :: Cursor kind -> Maybe (Cursor kind)
nextToken cursor =
  (nextSibling cursor >>= seek firstToken nextSibling)
    <|> (parent cursor >>= nextToken)

-- | Last token before the entire focused subtree in the snapshot.
previousToken :: Cursor kind -> Maybe (Cursor kind)
previousToken cursor =
  (previousSibling cursor >>= seek lastToken previousSibling)
    <|> (parent cursor >>= previousToken)

-- | Offset lookup is unambiguous inside a token and may select both tokens
-- at a shared boundary. The pair is ordered left to right.
data TokenAtOffset a = NoToken | SingleToken a | BetweenTokens a a
  deriving (Eq, Show)

-- | Find nonempty tokens touching an absolute byte offset within this focus.
-- Uses closed endpoint contact despite the half-open ranges: start and EOF
-- select the first and last nonempty token; a shared boundary selects both.
-- Empty tokens are ignored. An offset outside the focus returns 'NoToken'.
-- Offsets inside multibyte characters are accepted. Unrelated subtrees are
-- pruned using their cached ranges; relevant child lists are scanned.
tokenAtOffset :: TextLength -> Cursor kind -> TokenAtOffset (Cursor kind)
tokenAtOffset offset cursor = case matching cursor of
  [] -> NoToken
  [token] -> SingleToken token
  left : right : _ -> BetweenTokens left right
  where
    matching current
      | rangeIsEmpty range || offset < rangeStart range || offset > rangeEnd range = []
      | otherwise = case cursorGreen current of
          Token _ -> [current]
          Node _ -> concatMap matching (children current)
      where
        range = cursorRange current

-- | Descend repeatedly into the first child whose range contains the query.
-- Returns the innermost element along that branch, or 'Nothing' if the query
-- lies outside the focus. At ambiguous empty ranges (including EOF), ties
-- favor the first covering child in source order, including empty elements.
coveringElement :: TextRange -> Cursor kind -> Maybe (Cursor kind)
coveringElement query cursor
  | not (rangeContainsRange (cursorRange cursor) query) = Nothing
  | otherwise = case find (\child -> rangeContainsRange (cursorRange child) query) (children cursor) of
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

unfold :: (a -> Maybe a) -> Maybe a -> [a]
unfold _ Nothing = []
unfold step (Just value) = value : unfold step (step value)

seek :: (a -> Maybe b) -> (a -> Maybe a) -> a -> Maybe b
seek select step value = select value <|> (step value >>= seek select step)

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Cursors: immutable zippers over green trees. A cursor focuses one
-- occurrence of a node or token and knows its parents, siblings and absolute
-- byte range.
--
-- Edits through a cursor return a cursor on the edited tree, at the same
-- path, so a series of edits can keep navigating. The edits are recorded
-- in the ancestors as the cursor moves, each edited node is built once when
-- it is read, and 'tree' returns the whole edited tree. Old cursors keep
-- seeing the tree they were made from.
--
-- This module is meant to be imported qualified; its types are also
-- exported by "Graft":
--
-- > import Graft
-- > import qualified Graft.Cursor as Cursor
-- > import qualified Graft.Green as Green
-- >
-- > -- Replace the text of every Name token, in one pass through one cursor.
-- > -- Each edited node is copied once, by Cursor.tree (see 'replace').
-- > renameAll :: Text -> GreenElement Kind -> GreenElement Kind
-- > renameAll name root = maybe root (Cursor.tree . go) (Cursor.firstToken (Cursor.fromGreen root))
-- >   where
-- >     go cursor = maybe renamed go (Cursor.nextToken renamed)
-- >       where
-- >         renamed
-- >           | Cursor.kind cursor == Name = Cursor.replace (Token (Green.token Name name)) cursor
-- >           | otherwise = cursor
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
    -- * Editing
  , replace
  , modify
  , tree
  , spliceChildren
  , insertChild
  , removeChild
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Graft.Green (ChildEditError (..))
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
-- A cursor holds its ancestors, so moving up costs nothing until an edit has
-- to be recorded in them; see 'replace'.
data Cursor kind = Cursor
  -- The focus, lazy when its children are pending; how to read its children;
  -- its offset, length and index; whether the focus differs from the child at
  -- that index in the parent the cursor was made from; that parent; and the
  -- parent holding the focus, which is that parent or, after 'replace', a
  -- lazy rebuild of it, shared by every cursor that moves through it.
  { focusElement :: GreenElement kind
  , focusChildren :: !(Children kind)
  , focusOffset :: !TextLength
  , focusLength :: !TextLength
  , focusIndex :: !Int
  , focusEdited :: !Bool
  , madeFrom :: !(Maybe (Cursor kind))
  , holder :: Maybe (Cursor kind)
  }

-- How to read the children of the focus: from the focus itself, or from a
-- node and the children replaced since, which the focus is built from.
data Children kind
  = Built
  | Pending !(GreenNode kind) !(IntMap (Entry kind))

-- A replaced child: its length, how to read its children, and the element.
data Entry kind = Entry !TextLength !(Children kind) (GreenElement kind)

-- | Forces the whole tree and every ancestor of the focus, building any
-- edits pending in them, in time proportional to the size of the tree. Each
-- ancestor's element is part of the tree at the root, so the tree is forced
-- once.
instance NFData kind => NFData (Cursor kind) where
  rnf cursor = rnf (focusElement (root cursor)) `seq` spine cursor
    where
      spine current = focusElement current `seq` maybe () spine (holder current)

-- | Shows the path, range and focus. Cursors have no public constructor, so
-- unlike the green types this output does not rebuild the value.
instance Show kind => Show (Cursor kind) where
  showsPrec precedence cursor = showParen (precedence > 10) $
    showString "Cursor " . showsPrec 11 (path cursor) . showChar ' '
      . showsPrec 11 (range cursor) . showChar ' ' . showsPrec 11 (green cursor)

-- | A cursor on the root of a tree, at offset zero. The root may be a token.
fromGreen :: GreenElement kind -> Cursor kind
fromGreen element = built element 0 0 False Nothing Nothing

-- A cursor whose focus has no pending edits.
built
  :: GreenElement kind -> TextLength -> Int -> Bool -> Maybe (Cursor kind) -> Maybe (Cursor kind)
  -> Cursor kind
built element offset index edited made above =
  Cursor element Built offset (Internal.elementTextLength element) index edited made above

-- | The green element at the focus. When edits have been made below the
-- focus, the first call builds it, copying the child array of each node on
-- the way down to them once; see 'replace'.
green :: Cursor kind -> GreenElement kind
green = focusElement

-- | The kind of the focused element, without building it.
kind :: Cursor kind -> kind
kind cursor = case focusChildren cursor of
  Pending node _ -> Green.nodeKind node
  Built -> Green.kind (focusElement cursor)

-- | Whether the focus is a token rather than a node, without building it.
isToken :: Cursor kind -> Bool
isToken cursor = case focusChildren cursor of
  Pending _ _ -> False
  Built -> case focusElement cursor of
    Token _ -> True
    Node _ -> False

-- | The byte range of the focus in the whole tree, without building it.
range :: Cursor kind -> TextRange
range cursor = Range.fromLength (focusOffset cursor) (focusLength cursor)

-- | The source text of the focus.
text :: Cursor kind -> Text
text = Green.render . green

-- | The path from the root to the focus. Takes time proportional to depth.
path :: Cursor kind -> Path
path cursor = go cursor []
  where
    go current steps = case madeFrom current of
      Nothing -> steps
      Just above -> go above (focusIndex current : steps)

-- | The parent, or 'Nothing' at the root, in constant time. After 'replace',
-- the first move up or sideways records the edit in the parent, in time
-- logarithmic in the number of its children replaced so far, and every
-- cursor that moves through that parent shares the result.
parent :: Cursor kind -> Maybe (Cursor kind)
parent = holder

-- | A cursor on the root, recording any edits in each ancestor on the way,
-- as 'parent' does. Takes time proportional to depth.
root :: Cursor kind -> Cursor kind
root cursor = maybe cursor root (holder cursor)

-- | Follow a path down from the focus. The first step that fails is reported.
at :: Path -> Cursor kind -> Either PathError (Cursor kind)
at [] cursor = Right cursor
at (index : rest) cursor = childAt index cursor >>= at rest

-- | The child at a zero-based position. Constant time, unless the focus has
-- replaced children, when it takes time proportional to their number.
childAt :: Int -> Cursor kind -> Either PathError (Cursor kind)
childAt index cursor
  | index < 0 = Left (NegativeChildIndex index)
  | otherwise = case focusChildren cursor of
      Built -> case focusElement cursor of
        Token _ -> Left CannotDescendIntoToken
        Node node
          | index >= Internal.childCount node -> Left (ChildIndexOutOfBounds index)
          | otherwise -> Right (childCursor node cursor index)
      Pending _ _ -> case childEntry cursor index of
        Just entry -> Right (entryCursor cursor index (focusOffset cursor + childStartAt cursor index) entry)
        Nothing -> Left (ChildIndexOutOfBounds index)

-- The child at a valid index, if the focus is a node with no pending edits.
childCursor :: GreenNode kind -> Cursor kind -> Int -> Cursor kind
childCursor node above index =
  built (Internal.childAt node index) (focusOffset above + Internal.childStart node index) index False made made
  where
    made = Just above

entryCursor :: Cursor kind -> Int -> TextLength -> Entry kind -> Cursor kind
entryCursor above index offset (Entry len pending element) =
  Cursor element pending offset len index False made made
  where
    made = Just above

-- The child at an index, or Nothing if there is none.
childEntry :: Cursor kind -> Int -> Maybe (Entry kind)
childEntry cursor index
  | index < 0 = Nothing
  | otherwise = case focusChildren cursor of
      Pending node replaced
        | index < Internal.childCount node ->
            Just (fromMaybe (original node) (IntMap.lookup index replaced))
      Built
        | Node node <- focusElement cursor, index < Internal.childCount node -> Just (original node)
      _ -> Nothing
  where
    original node = originalEntry node index

originalEntry :: GreenNode kind -> Int -> Entry kind
originalEntry node index = Entry (Internal.elementTextLength element) Built element
  where
    element = Internal.childAt node index

-- The start of the child at a valid index, relative to the focus.
childStartAt :: Cursor kind -> Int -> TextLength
childStartAt cursor index = case focusChildren cursor of
  Pending node replaced ->
    IntMap.foldlWithKey' (shift node) (Internal.childStart node index) (fst (IntMap.split index replaced))
  Built -> case focusElement cursor of
    Node node -> Internal.childStart node index
    Token _ -> 0
  where
    shift node total position (Entry len _ _) =
      total + len - Internal.elementTextLength (Internal.childAt node position)

childCount :: Cursor kind -> Int
childCount cursor = case focusChildren cursor of
  Pending node _ -> Internal.childCount node
  Built -> case focusElement cursor of
    Node node -> Internal.childCount node
    Token _ -> 0

-- | The first child, or 'Nothing' if there are none.
firstChild :: Cursor kind -> Maybe (Cursor kind)
firstChild = either (const Nothing) Just . childAt 0

-- | The last child, or 'Nothing' if there are none.
lastChild :: Cursor kind -> Maybe (Cursor kind)
lastChild cursor = either (const Nothing) Just (childAt (childCount cursor - 1) cursor)

-- | The next sibling. Constant time, apart from recording an edit, as for
-- 'parent'.
nextSibling :: Cursor kind -> Maybe (Cursor kind)
nextSibling = siblingAt 1

-- | The previous sibling, at the same cost as 'nextSibling'.
previousSibling :: Cursor kind -> Maybe (Cursor kind)
previousSibling = siblingAt (-1)

siblingAt :: Int -> Cursor kind -> Maybe (Cursor kind)
siblingAt step cursor = case holder cursor of
  Nothing -> Nothing
  made@(Just above) -> case focusChildren above of
    Built
      | Node node <- focusElement above, target >= 0, target < Internal.childCount node ->
          let element = Internal.childAt node target
          in Just (built element (offset (Internal.elementTextLength element)) target False made made)
      | otherwise -> Nothing
    Pending _ _ -> case childEntry above target of
      Just entry@(Entry len _ _) -> Just (entryCursor above target (offset len) entry)
      Nothing -> Nothing
  where
    target = focusIndex cursor + step
    offset len
      | step > 0 = focusOffset cursor + focusLength cursor
      | otherwise = focusOffset cursor - len

-- | The children, in source order. A token has none.
children :: Cursor kind -> [Cursor kind]
children cursor = case (focusChildren cursor, focusElement cursor) of
  (Built, Token _) -> []
  (Built, Node node) -> map (childCursor node cursor) [0 .. Internal.childCount node - 1]
  (Pending _ _, _) -> go 0 (focusOffset cursor)
  where
    go index offset = case childEntry cursor index of
      Nothing -> []
      Just entry@(Entry len _ _) -> entryCursor cursor index offset entry : go (index + 1) (offset + len)

-- | Each ancestor of the focus, nearest first, up to the root. The focus is
-- not included, unlike Rowan's @ancestors@; 'descendants' does include it.
ancestors :: Cursor kind -> [Cursor kind]
ancestors = unfold parent . parent

-- | The focus and everything below it, in preorder. A subtree that appears
-- in several places is visited at each of them. The list is lazy.
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
-- itself. The typed @Ast.tokens@ gives child tokens only.
descendantTokens :: Cursor kind -> [Cursor kind]
descendantTokens = filter isToken . descendants

-- | The first token at or below the focus. A token is its own first token.
firstToken :: Cursor kind -> Maybe (Cursor kind)
firstToken cursor
  | isToken cursor = Just cursor
  | otherwise = firstChild cursor >>= seek firstToken nextSibling

-- | The last token at or below the focus.
lastToken :: Cursor kind -> Maybe (Cursor kind)
lastToken cursor
  | isToken cursor = Just cursor
  | otherwise = lastChild cursor >>= seek lastToken previousSibling

-- | The first token after the focus and everything below it, anywhere in the
-- tree.
nextToken :: Cursor kind -> Maybe (Cursor kind)
nextToken cursor =
  (nextSibling cursor >>= seek firstToken nextSibling)
    <|> (parent cursor >>= nextToken)

-- | The last token before the focus, anywhere in the tree.
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
-- the focus gives 'NoToken'. Each level is binary-searched.
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
            in concatMap (matching . childCursor node current) candidates
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
            descend (childCursor node current index)
        where
          start = focusOffset current
          index = Internal.firstChildEndingAtOrAfter node (Range.end query - start)
      _ -> current

-- | Replace the focus, returning a cursor at the same path in the edited
-- tree. The replacement can be of any kind, node or token. The original
-- cursor still sees the original tree. 'tree' gives the edited tree.
--
-- Nothing is copied as the cursor moves. The first move up or sideways from
-- the returned cursor records the replacement in the parent, in time
-- logarithmic in the number of the parent's children replaced so far; the
-- parent's first move records it in the grandparent, and so on. Every cursor
-- that moves through an ancestor shares what was recorded there, so a walk
-- that edits many tokens, and reads the 'ancestors' of each, records each
-- edit in each ancestor once.
--
-- An edited node is built when its 'green' element is first read, as
-- 'text', 'modify', 'tokenAt', 'covering' and the child edits do: its child
-- array is copied once, with all its replacements, and every cursor on that
-- version of the node shares the result. 'kind', 'isToken', 'range' and the
-- casts in "Graft.Ast" don't build it. 'tree' builds the nodes from the
-- edits to the root in one pass. A walk that edits many tokens and then takes
-- the 'tree' copies each edited node once.
--
-- The returned cursor, and every cursor moved from it, keeps the version of
-- each edited node from before the edit in memory, and with it the replaced
-- elements.
replace :: GreenElement kind -> Cursor kind -> Cursor kind
replace replacement cursor = replaced
  where
    replaced = built replacement (focusOffset cursor) (focusIndex cursor) True (madeFrom cursor) above
    above = case madeFrom cursor of
      Nothing -> Nothing
      Just made -> Just (withChild replaced made)

-- The parent as made, with the current version of a child recorded among
-- its replaced children. The node itself is built lazily.
withChild :: Cursor kind -> Cursor kind -> Cursor kind
withChild child above = case focusChildren above of
  Pending node pending -> record node pending
  Built -> case focusElement above of
    Node node -> record node IntMap.empty
    Token _ -> error "Graft.Cursor: internal error: a token holds a child"
  where
    index = focusIndex child
    record node pending = rebuilt
      where
        previous = case IntMap.lookup index pending of
          Just (Entry len _ _) -> len
          Nothing -> Internal.elementTextLength (Internal.childAt node index)
        updated = IntMap.insert index (Entry (focusLength child) (focusChildren child) (focusElement child)) pending
        element
          | IntMap.null pending = Internal.setChild index (focusElement child) node
          | otherwise = Internal.setChildren
              [(position, value) | (position, Entry _ _ value) <- IntMap.toAscList updated] node
        rebuilt = Cursor (Node element) (Pending node updated) (focusOffset above)
          (Internal.checkedLength (focusLength above + focusLength child - previous))
          (focusIndex above) True (madeFrom above) grandparent
        grandparent = case madeFrom above of
          Nothing -> Nothing
          Just made -> Just (withChild rebuilt made)

-- | Replace the focus with a function of it, as 'replace'.
modify :: (GreenElement kind -> GreenElement kind) -> Cursor kind -> Cursor kind
modify edit cursor = replace (edit (green cursor)) cursor

-- | The whole tree the cursor is in, including every edit made through it.
-- Each node between the edits and the root is copied once, with all its
-- replaced children, and each call copies them again: keep the result to
-- read it more than once.
tree :: Cursor kind -> GreenElement kind
tree cursor = case madeFrom cursor of
  Nothing -> focusElement cursor
  Just above
    | focusEdited cursor -> treeWith above (focusIndex cursor) (focusElement cursor)
    | otherwise -> tree above

-- The tree above a parent whose child at an index is now the given element.
-- The element is built first, so each node is built from the bottom up.
treeWith :: Cursor kind -> Int -> GreenElement kind -> GreenElement kind
treeWith above index !element = case madeFrom above of
  Nothing -> edited
  Just made -> treeWith made (focusIndex above) edited
  where
    edited = Node $ case focusChildren above of
      Pending node pending -> Internal.setChildren
        (IntMap.toAscList (IntMap.insert index element (IntMap.map entryElement pending))) node
      Built -> case focusElement above of
        Node node -> Internal.setChild index element node
        Token _ -> error "Graft.Cursor: internal error: a token holds a child"
    entryElement (Entry _ _ value) = value

-- | 'Graft.Green.spliceChildren' on the focused node, returning a cursor on
-- the edited node. A token focus gives 'CannotEditTokenChildren'.
spliceChildren
  :: Int -> Int -> [GreenElement kind] -> Cursor kind
  -> Either ChildEditError (Cursor kind)
spliceChildren start end replacements =
  editChildren (Green.spliceChildren start end replacements)

-- | 'Graft.Green.insertChild' on the focused node, as 'spliceChildren'.
insertChild
  :: Int -> GreenElement kind -> Cursor kind
  -> Either ChildEditError (Cursor kind)
insertChild index element = editChildren (Green.insertChild index element)

-- | 'Graft.Green.removeChild' on the focused node, as 'spliceChildren'.
removeChild :: Int -> Cursor kind -> Either ChildEditError (Cursor kind)
removeChild index = editChildren (Green.removeChild index)

editChildren
  :: (GreenNode kind -> Either ChildEditError (GreenNode kind))
  -> Cursor kind -> Either ChildEditError (Cursor kind)
editChildren edit cursor = case green cursor of
  Token _ -> Left CannotEditTokenChildren
  Node node -> do
    edited <- edit node
    pure (replace (Node edited) cursor)

unfold :: (a -> Maybe a) -> Maybe a -> [a]
unfold _ Nothing = []
unfold step (Just value) = value : unfold step (step value)

seek :: (a -> Maybe b) -> (a -> Maybe a) -> a -> Maybe b
seek select step value = select value <|> (step value >>= seek select step)

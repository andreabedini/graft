{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Cursors: immutable zippers over green trees. A cursor focuses one
-- occurrence of a node or token and knows its parents, siblings and absolute
-- byte range.
--
-- Edits through a cursor return a cursor on the edited tree, at the same
-- path, so a series of edits can keep navigating. Ancestors are rebuilt
-- lazily as the cursor moves up or sideways, and 'tree' returns the whole
-- edited tree. Old cursors keep seeing the tree they were made from.
--
-- This module is meant to be imported qualified; its types are also
-- exported by "Graft":
--
-- > import Graft
-- > import qualified Graft.Cursor as Cursor
-- > import qualified Graft.Green as Green
-- >
-- > -- Replace the text of every Name token, in one pass through one cursor.
-- > -- Each step after a rename copies the parent's children (see 'replace').
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
  = NegativeChildIndex Int
  | ChildIndexOutOfBounds Int
  | CannotDescendIntoToken
  deriving (Eq, Show)

instance Exception PathError

instance NFData PathError where
  rnf (NegativeChildIndex _) = ()
  rnf (ChildIndexOutOfBounds _) = ()
  rnf CannotDescendIntoToken = ()

-- The context of a focus: its parent as last rebuilt, the parent's absolute
-- offset, whether the parent differs from the child stored at its own index
-- in the grandparent, the grandparent context, and the focus index.
data Context kind
  = Top
  | Crumb !(GreenNode kind) !TextLength !Bool !(Context kind) !Int

-- | A position in a tree: a focused node or token and the path back to the
-- root. Nodes and tokens share the cursor type; match on 'green' to tell them
-- apart. There is no 'Eq' instance, because two equal subtrees in different
-- places are different occurrences; compare 'path's instead.
-- The flag records that the focus has been replaced since the parent was
-- last rebuilt; the parent is rebuilt lazily on the way up or sideways.
data Cursor kind = Cursor !(GreenElement kind) !TextLength !Bool !(Context kind)

-- | Forces the focus and every ancestor in the cursor's context.
instance NFData kind => NFData (Cursor kind) where
  rnf (Cursor focus _ _ context) = rnf focus `seq` go context
    where
      go Top = ()
      go (Crumb above _ _ outer _) = rnf above `seq` go outer

-- | Shows the path, range and focus. Cursors have no public constructor, so
-- unlike the green types this output does not rebuild the value.
instance Show kind => Show (Cursor kind) where
  showsPrec precedence cursor = showParen (precedence > 10) $
    showString "Cursor " . showsPrec 11 (path cursor) . showChar ' '
      . showsPrec 11 (range cursor) . showChar ' ' . showsPrec 11 (green cursor)

-- | A cursor on the root of a tree, at offset zero. The root may be a token.
fromGreen :: GreenElement kind -> Cursor kind
fromGreen element = Cursor element 0 False Top

-- | The green element at the focus, without copying it.
green :: Cursor kind -> GreenElement kind
green (Cursor focus _ _ _) = focus

-- | The kind of the focused element.
kind :: Cursor kind -> kind
kind = Green.kind . green

-- | The byte range of the focus in the whole tree.
range :: Cursor kind -> TextRange
range (Cursor focus offset _ _) = Range.fromLength offset (Green.length focus)

-- | The source text of the focus.
text :: Cursor kind -> Text
text = Green.render . green

-- | The path from the root to the focus. Takes time proportional to depth.
path :: Cursor kind -> Path
path (Cursor _ _ _ context) = go context []
  where
    go Top steps = steps
    go (Crumb _ _ _ above index) steps = go above (index : steps)

-- | The parent, or 'Nothing' at the root. Constant time, unless the focus was
-- replaced, in which case the parent is rebuilt by copying its child array.
parent :: Cursor kind -> Maybe (Cursor kind)
parent (Cursor _ _ _ Top) = Nothing
parent (Cursor focus _ edited (Crumb above start aboveEdited outer index)) =
  Just (Cursor (Node (rebuilt edited index focus above)) start (edited || aboveEdited) outer)

{-# INLINE rebuilt #-}
rebuilt :: Bool -> Int -> GreenElement kind -> GreenNode kind -> GreenNode kind
rebuilt edited index focus above
  | edited = Internal.setChild index focus above
  | otherwise = above

-- | A cursor on the root, rebuilding any ancestors of replaced foci.
root :: Cursor kind -> Cursor kind
root cursor@(Cursor _ _ _ Top) = cursor
root (Cursor focus _ edited (Crumb above start aboveEdited outer index)) =
  climb (rebuilt edited index focus above) start (edited || aboveEdited) outer
  where
    climb !node !offset !nodeEdited Top = Cursor (Node node) offset nodeEdited Top
    climb !node _ !nodeEdited (Crumb grandparent offset grandparentEdited context position) =
      climb (rebuilt nodeEdited position (Node node) grandparent) offset
        (nodeEdited || grandparentEdited) context

-- | Follow a path down from the focus. The first step that fails is reported.
at :: Path -> Cursor kind -> Either PathError (Cursor kind)
at [] cursor = Right cursor
at (index : rest) cursor = childAt index cursor >>= at rest

-- | The child at a zero-based position, in constant time.
childAt :: Int -> Cursor kind -> Either PathError (Cursor kind)
childAt index (Cursor focus offset edited context)
  | index < 0 = Left (NegativeChildIndex index)
  | otherwise = case focus of
      Token _ -> Left CannotDescendIntoToken
      Node node
        | index >= Internal.childCount node -> Left (ChildIndexOutOfBounds index)
        | otherwise -> Right (childCursor node offset edited context index)

childCursor :: GreenNode kind -> TextLength -> Bool -> Context kind -> Int -> Cursor kind
childCursor node offset edited context index =
  Cursor (Internal.childAt node index) (offset + Internal.childStart node index) False
    (Crumb node offset edited context index)

-- | The first child, or 'Nothing' if there are none.
firstChild :: Cursor kind -> Maybe (Cursor kind)
firstChild = either (const Nothing) Just . childAt 0

-- | The last child, or 'Nothing' if there are none.
lastChild :: Cursor kind -> Maybe (Cursor kind)
lastChild cursor = case green cursor of
  Node node -> either (const Nothing) Just (childAt (Internal.childCount node - 1) cursor)
  Token _ -> Nothing

-- | The next sibling. Constant time, unless the focus was replaced, in which
-- case the parent is rebuilt first.
nextSibling :: Cursor kind -> Maybe (Cursor kind)
nextSibling = siblingAt 1

-- | The previous sibling, at the same cost as 'nextSibling'.
previousSibling :: Cursor kind -> Maybe (Cursor kind)
previousSibling = siblingAt (-1)

siblingAt :: Int -> Cursor kind -> Maybe (Cursor kind)
siblingAt _ (Cursor _ _ _ Top) = Nothing
siblingAt step (Cursor focus _ edited (Crumb above start aboveEdited outer index))
  | target < 0 || target >= Internal.childCount above = Nothing
  | otherwise = Just (childCursor (rebuilt edited index focus above) start (edited || aboveEdited) outer target)
  where
    target = index + step

-- | The children, in source order. A token has none.
children :: Cursor kind -> [Cursor kind]
children (Cursor focus offset edited context) = case focus of
  Token _ -> []
  Node node -> map (childCursor node offset edited context) [0 .. Internal.childCount node - 1]

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
  where
    isToken cursor = case green cursor of
      Token _ -> True
      Node _ -> False

-- | The first token at or below the focus. A token is its own first token.
firstToken :: Cursor kind -> Maybe (Cursor kind)
firstToken cursor = case green cursor of
  Token _ -> Just cursor
  Node _ -> firstChild cursor >>= seek firstToken nextSibling

-- | The last token at or below the focus.
lastToken :: Cursor kind -> Maybe (Cursor kind)
lastToken cursor = case green cursor of
  Token _ -> Just cursor
  Node _ -> lastChild cursor >>= seek lastToken previousSibling

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
    matching current@(Cursor focus start edited context)
      | Range.isEmpty extent || offset < Range.start extent || offset > Range.end extent = []
      | otherwise = case focus of
          Token _ -> [current]
          Node node ->
            let relative = offset - start
                touching index = Internal.childStart node index <= relative
                candidates = takeWhile touching
                  [Internal.firstChildEndingAtOrAfter node relative .. Internal.childCount node - 1]
            in concatMap (matching . childCursor node start edited context) candidates
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
    descend current@(Cursor focus start edited context) = case focus of
      Node node
        | index < Internal.childCount node, Internal.childStart node index <= Range.start query - start ->
            descend (childCursor node start edited context index)
        where
          index = Internal.firstChildEndingAtOrAfter node (Range.end query - start)
      _ -> current

-- | Replace the focus, returning a cursor at the same path in the edited
-- tree. The replacement can be of any kind, node or token. The original
-- cursor still sees the original tree. 'tree' gives the edited tree.
--
-- Nothing is rebuilt until the cursor moves up or sideways; then the parent
-- is rebuilt by copying its child array. Editing @k@ children of one node,
-- moving between them, therefore takes time proportional to @k@ times the
-- node's width. To change many children of one node, edit the node once
-- instead, with 'modify' or 'spliceChildren'.
replace :: GreenElement kind -> Cursor kind -> Cursor kind
replace replacement (Cursor _ offset _ context) = Cursor replacement offset True context

-- | Replace the focus with a function of it, as 'replace'.
modify :: (GreenElement kind -> GreenElement kind) -> Cursor kind -> Cursor kind
modify edit cursor = replace (edit (green cursor)) cursor

-- | The whole tree the cursor is in, including every edit made through it.
tree :: Cursor kind -> GreenElement kind
tree = green . root

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

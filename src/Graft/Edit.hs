{-# LANGUAGE BangPatterns #-}

-- | Edits that are described against a tree as it is and made all at once.
-- A plan applies a list of edits, each referring to the base tree through a
-- cursor, in one pass; a rewrite visits a tree top-down and replaces
-- elements as it goes. Both read only the original tree, rebuild each node
-- above a change once, and share everything else with the base. A plan
-- also gives the text edits that take the base's source to the result's.
--
-- This module is meant to be imported qualified; its types are also
-- exported by "Graft":
--
-- > import Graft
-- > import qualified Graft.Edit as Edit
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Graft.Cursor as Cursor
-- >>> import qualified Graft.Edit as Edit
-- >>> import qualified Graft.Green as Green
-- >>> import qualified Graft.Range as Range
-- >>> data Kind = Root | Name | Space deriving (Eq, Show)
-- >>> let tok kind = Token . Green.token kind
-- >>> let tree = Node (Green.node Root [tok Name "x", tok Space " ", tok Name "y"])
-- >>> let Right y = Cursor.childAt 2 (Cursor.fromGreen tree)
-- >>> let edits = [Edit.replace y (tok Name "z"), Edit.insertBefore y [tok Name "w", tok Space " "]]
-- >>> Green.render <$> Edit.apply tree edits
-- Right "x w z"
-- >>> map (\e -> (Range.start (Edit.editRange e), Range.end (Edit.editRange e), Edit.editText e)) <$> Edit.textEdits tree edits
-- Right [(2,3,"w z")]
-- >>> let rename () cursor = if Cursor.kind cursor == Name then Replace (tok Name "z") else Descend
-- >>> Green.render (Edit.rewrite rename () tree)
-- "z z"
module Graft.Edit
  ( -- * Edit plans
    Edit
  , replace
  , splice
  , insertBefore
  , insertAfter
  , remove
  , EditError (..)
  , apply
    -- * Text edits
  , TextEdit (..)
  , textEdits
    -- * Rewriting
  , Change (..)
  , rewrite
  , rewriteM
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Control.Monad (foldM)
import Data.Functor.Identity (Identity (..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortBy, unsnoc)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import Graft.Cursor (Cursor, Path)
import qualified Graft.Cursor as Cursor
import qualified Graft.Green as Green
import Graft.Green.Internal (ChildEditError (..), GreenElement (..), GreenNode, TextLength)
import qualified Graft.Green.Internal as Internal
import Graft.Range (TextRange)
import qualified Graft.Range as Range

-- | One change to a base tree, made with 'replace', 'splice',
-- 'insertBefore', 'insertAfter' or 'remove' and applied by 'apply'. It
-- refers to the tree its cursor was made from, and keeps that tree alive.
data Edit kind
  = Replacement (Cursor kind) (GreenElement kind)
  | Splice (Cursor kind) !Int !Int [GreenElement kind]
  | InsertBefore (Cursor kind) [GreenElement kind]
  | InsertAfter (Cursor kind) [GreenElement kind]
  | Remove (Cursor kind)

-- | Shows the edit with the path of its cursor in place of the cursor.
instance Show kind => Show (Edit kind) where
  showsPrec precedence edit = showParen (precedence > 10) $ case edit of
    Replacement cursor element ->
      showString "Edit.replace " . at cursor . showChar ' ' . showsPrec 11 element
    Splice cursor start end elements ->
      showString "Edit.splice " . at cursor . showChar ' ' . showsPrec 11 start . showChar ' '
        . showsPrec 11 end . showChar ' ' . showsPrec 11 elements
    InsertBefore cursor elements ->
      showString "Edit.insertBefore " . at cursor . showChar ' ' . showsPrec 11 elements
    InsertAfter cursor elements ->
      showString "Edit.insertAfter " . at cursor . showChar ' ' . showsPrec 11 elements
    Remove cursor -> showString "Edit.remove " . at cursor
    where
      at cursor = showsPrec 11 (Cursor.path cursor)

-- | Replace the element at the cursor. The replacement may be a node or a
-- token.
replace :: Cursor kind -> GreenElement kind -> Edit kind
replace = Replacement

-- | @splice cursor start end elements@ replaces the children at positions
-- @start@ up to but not including @end@ of the node at the cursor with the
-- elements, as 'Graft.Green.spliceChildren' does: with @start == end@ it
-- inserts, and with no elements it removes. The positions are positions
-- among the children of the base node. Nothing is added for you: include
-- any separators or whitespace in the elements.
splice :: Cursor kind -> Int -> Int -> [GreenElement kind] -> Edit kind
splice = Splice

-- | Insert elements before the element at the cursor, among its siblings.
-- On the root, 'apply' reports 'EditAtRoot'.
insertBefore :: Cursor kind -> [GreenElement kind] -> Edit kind
insertBefore = InsertBefore

-- | Insert elements after the element at the cursor, among its siblings.
-- On the root, 'apply' reports 'EditAtRoot'.
insertAfter :: Cursor kind -> [GreenElement kind] -> Edit kind
insertAfter = InsertAfter

-- | Remove the element at the cursor from its parent. On the root, 'apply'
-- reports 'EditAtRoot'.
remove :: Cursor kind -> Edit kind
remove = Remove

-- | Why a plan was not applied. Each constructor names the edits concerned
-- by their zero-based positions in the list given to 'apply'.
data EditError
  = EditFromAnotherTree !Int
  -- ^ The edit's cursor is in a tree that is not equal to the base.
  | EditAtRoot !Int
  -- ^ 'insertBefore', 'insertAfter' or 'remove' on the root, which has no
  -- siblings.
  | InvalidSplice !Int !ChildEditError
  -- ^ The splice's positions are not valid for the base node, or its
  -- cursor is on a token.
  | ConflictingEdits !Int !Int
  -- ^ Two edits that cannot both be made; see 'apply'.
  deriving (Eq, Show)

instance Exception EditError

instance NFData EditError where
  rnf err = err `seq` ()

-- | Apply a list of edits to the tree they were made on, all at once.
--
-- Every cursor and every child position refers to the base: replacing the
-- children at positions 2 and 7 replaces those two original children,
-- whatever happens to the lengths or to the children between them. At
-- each node with an edit, the new children are, for each boundary @i@
-- from 0 to the number of children: the insertions at @i@ (splices with
-- the empty range @[i, i)@), in the order of the list; then the base
-- child @i@, which is the elements of the splice that starts at @i@, or
-- nothing if an earlier splice consumed it, or its replacement, or the
-- child itself, rebuilt below if an edit lies below it and shared
-- otherwise. So an insertion at the start of a splice precedes the
-- splice's elements and one at its end follows them. Apart from the order
-- of insertions at one boundary of one node, the result does not depend
-- on the order of the list.
--
-- A plan is rejected with 'ConflictingEdits' when a replacement and any
-- other edit apply at or below the same element; when a splice removes a
-- child, or part of one, that another edit applies to or below (an
-- insertion strictly inside a splice's range is such an edit, while one
-- at either end is not); or when two splices of one node have ranges
-- @[a, b)@ and @[c, d)@ with @a < d@ and @c < b@. Splice positions are
-- checked against the base node as 'Graft.Green.spliceChildren' checks
-- them.
--
-- Every edit's cursor must be in a tree equal to the base, as a cursor
-- made from it is. The check compares the trees, which costs nothing
-- beyond a pointer comparison for a cursor made from the base itself and
-- up to the size of the tree otherwise; this is why @Eq kind@ is needed.
--
-- The result shares with the base every subtree that no edit applies to
-- or below, and @apply base []@ is @base@. Each node above an edit is
-- built once. Evaluating the result to @Right@ completes the validation
-- and the construction of every rebuilt node; replacement elements and
-- the shared parts of the base are as evaluated as they were given.
apply :: Eq kind => GreenElement kind -> [Edit kind] -> Either EditError (GreenElement kind)
apply base [] = Right base
apply base edits = planOf base edits >>= \plan -> rebuild plan base

-- | A change to the base's source text: the bytes in a range of the
-- original, replaced by new text. Ranges are UTF-8 byte offsets in the
-- base's text; "Graft.LineIndex" converts them to positions for an editor.
data TextEdit = TextEdit
  { editRange :: !TextRange
  , editText :: !Text
  }
  deriving (Eq, Show)

instance NFData TextEdit where
  rnf (TextEdit range new) = rnf range `seq` rnf new

-- | The text edits that take the base's source to the source of
-- @'apply' base edits@, for the same plan and with the same errors. They
-- are sorted by start, their ranges neither overlap nor touch, and every
-- range is read in the original text, so patching the original with them,
-- in any order, renders the result of 'apply'. Edits at one offset, such
-- as insertions at the boundaries of an empty element, are merged into one
-- in the order of the result.
textEdits :: Eq kind => GreenElement kind -> [Edit kind] -> Either EditError [TextEdit]
textEdits _ [] = Right []
textEdits base edits = planOf base edits >>= \plan -> coalesce <$> collect plan 0 base

-- The edits of a plan, grouped by the path of the element they apply to.
data Plan kind = Plan
  { planFirst :: !Int
  -- ^ the lowest position of an edit at or below this element
  , planReplacements :: [(Int, GreenElement kind)]
  -- ^ replacements of this element, newest first
  , planSplices :: [Splicing kind]
  -- ^ splices of this node's children, newest first
  , planBelow :: !(IntMap (Plan kind))
  -- ^ the plans of its children, by index
  }

-- A splice as planned: its position in the list, its range and its elements.
data Splicing kind = Splicing !Int !Int !Int [GreenElement kind]

data Op kind = OpReplace (GreenElement kind) | OpSplice !Int !Int [GreenElement kind]

emptyPlan :: Plan kind
emptyPlan = Plan maxBound [] [] IntMap.empty

planOf :: Eq kind => GreenElement kind -> [Edit kind] -> Either EditError (Plan kind)
planOf base = foldM step emptyPlan . zip [0 ..]
  where
    step plan (position, edit) = do
      (path, op) <- resolve base position edit
      pure (insertPlan position path op plan)

-- Where an edit applies in the base and what it does there, once its
-- cursor is known to be in a tree equal to the base.
resolve :: Eq kind => GreenElement kind -> Int -> Edit kind -> Either EditError (Path, Op kind)
resolve base position edit
  | Cursor.tree cursor /= base = Left (EditFromAnotherTree position)
  | otherwise = case edit of
      Replacement _ element -> Right (path, OpReplace element)
      Splice _ start end elements -> Right (path, OpSplice start end elements)
      InsertBefore _ elements -> sibling (\index -> OpSplice index index elements)
      InsertAfter _ elements -> sibling (\index -> OpSplice (index + 1) (index + 1) elements)
      Remove _ -> sibling (\index -> OpSplice index (index + 1) [])
  where
    cursor = case edit of
      Replacement c _ -> c
      Splice c _ _ _ -> c
      InsertBefore c _ -> c
      InsertAfter c _ -> c
      Remove c -> c
    path = Cursor.path cursor
    sibling op = case unsnoc path of
      Nothing -> Left (EditAtRoot position)
      Just (parentPath, index) -> Right (parentPath, op index)

insertPlan :: Int -> Path -> Op kind -> Plan kind -> Plan kind
insertPlan position path op plan = case path of
  [] -> case op of
    OpReplace element -> touched {planReplacements = (position, element) : planReplacements plan}
    OpSplice start end elements ->
      touched {planSplices = Splicing position start end elements : planSplices plan}
  index : rest ->
    touched {planBelow = IntMap.alter (Just . insertPlan position rest op . fromMaybe emptyPlan) index (planBelow plan)}
  where
    touched = plan {planFirst = min position (planFirst plan)}

-- The replacement of an element, if its plan is one replacement and nothing
-- else; a replacement with anything else at or below the element conflicts.
replacementOf :: Plan kind -> Either EditError (Maybe (GreenElement kind))
replacementOf plan = case planReplacements plan of
  [] -> Right Nothing
  [(position, element)]
    | null others -> Right (Just element)
    | otherwise -> conflict position (minimum others)
  (position, _) : (other, _) : _ -> conflict position other
  where
    others = [p | Splicing p _ _ _ <- planSplices plan] ++ map planFirst (IntMap.elems (planBelow plan))

conflict :: Int -> Int -> Either EditError a
conflict first second = Left (ConflictingEdits (min first second) (max first second))

-- A splice on a token is reported for the first splice in the list; a plan
-- below a token can't be made, since cursors don't descend into tokens.
tokenError :: Plan kind -> EditError
tokenError plan = case reverse (planSplices plan) of
  Splicing position _ _ _ : _ -> InvalidSplice position CannotEditTokenChildren
  [] -> error "Graft.Edit: internal error: a plan below a token"

-- The children of a rebuilt node, as the merge lays them out.
data Slot kind
  = Original !Int
  -- ^ the base child at this index, shared
  | Replaced !Int !Int [GreenElement kind]
  -- ^ the base children in this range, replaced by these elements; an
  -- empty range is an insertion at its boundary
  | Nested !Int (Plan kind)
  -- ^ the base child at this index, rebuilt by its plan

-- Lay out a node's children under its plan, checking the splices against
-- the node, against each other, and against the edits below.
slots :: Plan kind -> GreenNode kind -> Either EditError [Slot kind]
slots plan node = do
  splices <- traverse check (reverse (planSplices plan))
  let ordered = sortBy (comparing key) splices
  sweep Nothing ordered
  layout 0 ordered
  where
    count = Internal.childCount node
    key (Splicing position start end _) = (start, end, position)
    check splicing@(Splicing position start end _) =
      maybe (Right splicing) (Left . InvalidSplice position) (Internal.spliceBoundsError start end node)
    -- Two splices conflict when a < d and c < b. In (start, end) order, a
    -- splice conflicts with an earlier one exactly when it starts before
    -- the furthest end of the non-empty splices so far; empty splices
    -- cover nothing.
    sweep _ [] = Right ()
    sweep covering (Splicing position start end _ : rest) = case covering of
      Just (coverEnd, coverPosition) | start < coverEnd -> conflict coverPosition position
      _ | end > start && maybe True ((end >) . fst) covering -> sweep (Just (end, position)) rest
        | otherwise -> sweep covering rest
    -- The slots from a boundary on: the insertions there, then the child,
    -- which a splice starting there consumes along with the following ones.
    layout index pending = do
      let (insertions, rest) = span (\(Splicing _ start end _) -> start == index && end == index) pending
          inserted = [Replaced index index elements | Splicing _ _ _ elements <- insertions]
      following <- case rest of
        _ | index == count -> Right []
        Splicing position start end elements : more | start == index -> do
          mapM_ (conflict position . planFirst)
            [below | (child, below) <- IntMap.toList (planBelow plan), start <= child, child < end]
          (Replaced start end elements :) <$> layout end more
        _ -> (slot :) <$> layout (index + 1) rest
          where
            slot = maybe (Original index) (Nested index) (IntMap.lookup index (planBelow plan))
      pure (inserted ++ following)

-- The element under a plan, built from the bottom up: each rebuilt node is
-- evaluated before its parent is laid out.
rebuild :: Plan kind -> GreenElement kind -> Either EditError (GreenElement kind)
rebuild plan element = do
  replaced <- replacementOf plan
  case replaced of
    Just replacement -> Right replacement
    Nothing -> case element of
      Token _ -> Left (tokenError plan)
      Node node -> do
        laid <- slots plan node
        children <- concat <$> traverse (elements node) laid
        let !rebuilt = Internal.greenNode (Green.nodeKind node) children
        Right (Node rebuilt)
  where
    elements node slot = case slot of
      Original index -> Right [Internal.childAt node index]
      Replaced _ _ new -> Right new
      Nested index below -> (: []) <$> rebuild below (Internal.childAt node index)

-- The text edits under a plan, in the order of the result, with ranges in
-- the base's text.
collect :: Plan kind -> TextLength -> GreenElement kind -> Either EditError [TextEdit]
collect plan offset element = do
  replaced <- replacementOf plan
  case replaced of
    Just replacement -> Right [TextEdit (Range.fromLength offset (Green.length element)) (Green.render replacement)]
    Nothing -> case element of
      Token _ -> Left (tokenError plan)
      Node node -> do
        laid <- slots plan node
        concat <$> traverse (edits node) laid
  where
    edits node slot = case slot of
      Original _ -> Right []
      Replaced start end new ->
        let from = offset + Internal.childStart node start
            to = offset + Internal.childStart node end
        in Right [TextEdit (Range.fromLength from (to - from)) (Text.concat (map Green.render new))]
      Nested index below -> collect below (offset + Internal.childStart node index) (Internal.childAt node index)

-- Merge each run of edits in which one ends where the next starts, so that
-- no two edits share an offset. The texts of a run are concatenated once.
coalesce :: [TextEdit] -> [TextEdit]
coalesce [] = []
coalesce (TextEdit range new : rest) = go (Range.start range) (Range.end range) [new] rest
  where
    go start end pieces (TextEdit next text : more)
      | Range.start next == end = go start (Range.end next) (text : pieces) more
    go start end pieces more =
      TextEdit (Range.fromLength start (end - start)) (Text.concat (reverse pieces)) : coalesce more

-- | What a rewrite does with the element it is looking at. 'Descend' is not
-- "unchanged": it keeps the element's own form and goes on below it, where
-- replacements may still happen. 'Skip' is the answer for an unchanged
-- subtree. For a token, 'Descend' and 'Skip' are the same answer.
data Change ctx kind
  = Replace (GreenElement kind)
  -- ^ Put this element in place of the current one. Nothing below the
  -- current element is visited, and the replacement isn't visited either.
  | Descend
  -- ^ Keep the element's form and visit its children with the same context.
  | DescendWith ctx
  -- ^ Keep the element's form and visit its children with this context.
  | Skip
  -- ^ Keep the element and everything below it, unvisited.
  deriving (Eq, Show)

-- | Rewrite a tree top-down. The function is called on the root and then,
-- for each element it answers 'Descend' or 'DescendWith', on that element's
-- children in source order, with a cursor on the original tree and the
-- context from the nearest ancestor that answered 'DescendWith'. A node is
-- rebuilt, once, with one copy of its child array, exactly when something
-- below it was replaced; a node with no replacement below it is returned as
-- it is, so @rewrite (\\_ _ -> Skip)@ and @rewrite (\\_ _ -> Descend)@ give
-- back their argument. Answering 'Replace' with the original element counts
-- as a replacement.
--
-- The cost is proportional to the number of elements visited, which covers
-- the rebuilding, plus the calls. The function never sees a replacement,
-- its own or another's: to decide on the basis of a change, rewrite the
-- result again.
rewrite :: (ctx -> Cursor kind -> Change ctx kind) -> ctx -> GreenElement kind -> GreenElement kind
rewrite step ctx root = runIdentity (rewriteM (\context cursor -> Identity (step context cursor)) ctx root)
{-# INLINE rewrite #-}

-- | 'rewrite' with effects, which run in preorder: an element's before its
-- children's, and a child's before its next sibling's. An effect that stops
-- the computation, such as 'Left', stops the rewrite; in @StateT s (Either
-- e)@, say, a walk can collect the ranges it replaced and give up at the
-- first problem. Each rebuilt node is evaluated before it is returned
-- through the monad, so no rebuilding is left suspended in the result.
-- 'rewrite' is this function in t'Identity'.
rewriteM
  :: Monad m
  => (ctx -> Cursor kind -> m (Change ctx kind))
  -> ctx -> GreenElement kind -> m (GreenElement kind)
rewriteM step = \ctx root -> snd <$> element ctx (Cursor.fromGreen root)
  where
    -- Each result says whether it differs from the original, so a parent
    -- knows to rebuild without comparing trees.
    element ctx cursor = do
      change <- step ctx cursor
      case change of
        Replace replacement -> pure (True, replacement)
        Skip -> pure (False, Cursor.green cursor)
        Descend -> below ctx cursor
        DescendWith inner -> below inner cursor
    below ctx cursor = case original of
      Token _ -> pure (False, original)
      Node node -> do
        replaced <- children ctx (Cursor.children cursor) 0 []
        case replaced of
          [] -> pure (False, original)
          _ -> let !rebuilt = Internal.setChildren (reverse replaced) node
               in pure (True, Node rebuilt)
      where
        original = Cursor.green cursor
    -- The replaced children, newest first, with their positions.
    children _ [] !_ replaced = pure replaced
    children ctx (child : rest) !index replaced = do
      (changed, result) <- element ctx child
      children ctx rest (index + 1) (if changed then (index, result) : replaced else replaced)
{-# INLINE rewriteM #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- Workloads and observations shared by timing and residency runs.
module Fixtures
  ( Kind (..), Prepared (..), PreparedCursor (..), Named (..), Numbered (..), Grouped (..)
  , wideSource, wideTree, deepTree, chainTree, chainSource, documentTree, documentSource
  , forceElement, checked, selected, cached, cursorEnd, siblingSum
  , buildTokens, buildMonadic, buildReuse, buildWrapped, editMany, snapshots, renameWalk, renamePasses, renameVisits
  , checkFixtures
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Data.Hashable (Hashable (..))
import Control.Monad (unless)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Graft
import qualified Graft.Ast as Ast
import qualified Graft.Build as Build
import qualified Graft.Builder as Builder
import qualified Graft.Cursor as Cursor
import qualified Graft.Green as Green
import qualified Graft.Range as Range

data Kind = Root | Group | Name | Number | Trivia
  deriving (Eq, Show, Enum)

instance Hashable Kind where
  hashWithSalt salt = hashWithSalt salt . fromEnum

instance NFData Kind where
  rnf kind = kind `seq` ()

-- These wrappers force fixtures before the timer starts.
newtype Prepared = Prepared { preparedTree :: GreenElement Kind }

instance NFData Prepared where
  rnf (Prepared tree) = rnf tree

newtype PreparedCursor = PreparedCursor { preparedCursor :: Cursor Kind }

instance NFData PreparedCursor where
  rnf (PreparedCursor cursor) = rnf cursor

forceElement :: GreenElement Kind -> ()
forceElement = rnf

newtype Named = Named (Cursor Kind)
instance AstView Named where
  type AstViewKind Named = Kind
  fromCursor = Ast.matchToken (== Name) Named
  toCursor (Named cursor) = cursor
instance AstToken Named

newtype Numbered = Numbered (Cursor Kind)
instance AstView Numbered where
  type AstViewKind Numbered = Kind
  fromCursor = Ast.matchToken (== Number) Numbered
  toCursor (Numbered cursor) = cursor
instance AstToken Numbered

newtype Grouped = Grouped (Cursor Kind)
instance AstView Grouped where
  type AstViewKind Grouped = Kind
  fromCursor = Ast.matchNode (== Group) Grouped
  toCursor (Grouped cursor) = cursor
instance AstNode Grouped

wideSource :: Int -> [Text]
wideSource count = [Text.pack ("名" ++ show index ++ " ") | index <- [1 .. count]]

wideTree :: [Text] -> GreenNode Kind
wideTree = Green.node Root . wideElements

wideElements :: [Text] -> [GreenElement Kind]
wideElements [] = []
wideElements [text] = [Token (Green.token Number text)]
wideElements (text : rest) = Token (Green.token Name text) : wideElements rest

deepTree :: Int -> GreenNode Kind
deepTree depth = List.foldl' (\node _ -> Green.node Group [Node node]) leaf [1 .. depth]
  where leaf = Green.node Group [Token (Green.token Number "001")]

-- A left-nested operator chain such as @((0 + 1) + 2) + 3@: both deep and
-- wide, so whole-tree traversals pay for every level on every token.
chainTree :: Int -> GreenNode Kind
chainTree count = List.foldl' step leaf [1 .. count]
  where
    leaf = Green.node Group [Token (Green.token Number "0")]
    step left index = Green.node Group
      [Node left, Token (Green.token Trivia " + "), Token (Green.token Number (Text.pack (show index)))]

chainSource :: Int -> Text
chainSource count = Text.concat ("0" : [Text.pack (" + " ++ show index) | index <- [1 .. count]])

lineText :: Int -> [Text]
lineText index = [Text.pack ("名" ++ show index), "  = ", "001", " -- keep", "\r\n"]

documentSource :: Int -> Text
documentSource count = Text.concat (concatMap lineText [1 .. count])

documentTree :: Int -> GreenNode Kind
documentTree count = Green.node Root [Node (line index) | index <- [1 .. count]]
  where
    line index = Green.node Group (zipWith (\kind text -> Token (Green.token kind text))
      [Name, Trivia, Number, Trivia, Trivia] (lineText index))

checked :: Show err => Either err a -> a
checked = either (error . show) id

selected :: Maybe a -> a
selected = maybe (error "benchmark fixture has no selected element") id

cached :: GreenNode Kind -> TextLength
cached = Green.nodeLength

cursorEnd :: Cursor Kind -> TextLength
cursorEnd = Range.end . Cursor.range

-- A strict consumer: each adjacent sibling and range contributes to the result.
siblingSum :: Cursor Kind -> TextLength
siblingSum root = go 0 (Cursor.firstChild root)
  where
    go total Nothing = total
    go total (Just cursor) = let next = total + cursorEnd cursor
      in next `seq` go next (Cursor.nextSibling cursor)

buildTokens :: [Text] -> GreenNode Kind
buildTokens texts = checked (Builder.finish (checked (Builder.finishNode emitted)))
  where
    emitted = go (Builder.startNode Root Builder.empty) texts
    go builder [] = builder
    go builder [text] = Builder.token Number text builder
    go builder (text : rest) = let next = Builder.token Name text builder
      in next `seq` go next rest

buildMonadic :: [Text] -> GreenNode Kind
buildMonadic texts = checked (Build.exec (Build.node Root (go texts)))
  where
    go [] = pure ()
    go [text] = Build.token Number text
    go (text : rest) = Build.token Name text >> go rest

buildReuse :: GreenNode Kind -> GreenNode Kind
buildReuse node = checked (Builder.finish (checked (Builder.finishNode emitted)))
  where emitted = List.foldl' (flip Builder.element) (Builder.startNode Root Builder.empty) (Green.children node)

buildWrapped :: GreenNode Kind -> GreenNode Kind
buildWrapped node = checked (Builder.finish closed)
  where
    opened = Builder.startNode Root Builder.empty
    marker = Builder.checkpoint opened
    emitted = List.foldl' (flip Builder.element) opened (Green.children node)
    wrapped = checked (Builder.startNodeAt marker Group emitted)
    closed = checked (Builder.finishNode (checked (Builder.finishNode wrapped)))

-- Each edit depends on the preceding snapshot. Distinct text prevents no-op
-- updates. Force the cache on every step without walking shared descendants.
editMany :: Int -> Path -> GreenElement Kind -> GreenElement Kind
editMany count path root = List.foldl' step root [1 .. count]
  where
    step previous index =
      let updated = Cursor.tree (Cursor.replace (Token (Green.token Number (Text.pack (show index))))
            (checked (Cursor.at path (Cursor.fromGreen previous))))
      in Green.length updated `seq` updated

-- Rename every Name token through one cursor, as in the Graft.Cursor
-- example, and return the cursor on the last token. With ancestors, every
-- token also reads the ranges of its ancestors first, as a rename that
-- checks scopes would.
renameWalk :: Bool -> GreenElement Kind -> Maybe (Cursor Kind)
renameWalk ancestors input = fmap List.last (renameVisits ancestors (Cursor.fromGreen input))

-- Every cursor that a renaming walk from the root visits, after any edit.
renameVisits :: Bool -> Cursor Kind -> Maybe [Cursor Kind]
renameVisits ancestors start = go <$> Cursor.firstToken (Cursor.root start)
  where
    go cursor = renamed : maybe [] go (Cursor.nextToken renamed)
      where
        renamed
          | (not ancestors || ancestorEnds cursor > 0) && Cursor.kind cursor == Name =
              Cursor.replace (Token (Green.token Name "renamed")) cursor
          | otherwise = cursor
    ancestorEnds = List.foldl' (\total ancestor -> total + cursorEnd ancestor) 0 . Cursor.ancestors

-- The renaming walk repeated through one chain of cursors, each pass
-- starting from the root of the last cursor of the previous one.
renamePasses :: Int -> GreenElement Kind -> Cursor Kind
renamePasses count input = List.foldl' pass (Cursor.fromGreen input) [1 .. count]
  where
    pass cursor _ = let next = maybe cursor List.last (renameVisits False cursor)
      in cursorEnd next `seq` next

snapshots :: Int -> Path -> GreenElement Kind -> [GreenElement Kind]
snapshots count path root = scanl step root [1 .. count]
  where
    step previous index =
      let updated = Cursor.tree (Cursor.replace (Token (Green.token Number (Text.pack (show index))))
            (checked (Cursor.at path (Cursor.fromGreen previous))))
      in Green.length updated `seq` updated

-- Independent source checks guard the workload, not timing thresholds.
checkFixtures :: IO ()
checkFixtures = do
  mapM_ checkWide [100, 1000, 10000]
  mapM_ checkDeep [100, 1000, 10000]
  mapM_ checkChain [100, 1000, 10000]
  let document = Node (documentTree 1000)
      before = documentSource 1000
      path = [499, 2]
      cursor = checked (Cursor.at path (Cursor.fromGreen document))
      updated = editMany 100 path document
      expected = Text.concat [documentSource 499, Text.concat
        ["名500", "  = ", "100", " -- keep", "\r\n"],
        Text.concat (concatMap lineText [501 .. 1000])]
  assert "document source" (Green.render document == before)
  assert "repeated exact edits" (Green.render updated == expected)
  assert "focus/path edit agreement" (Cursor.tree (Cursor.replace (Token (Green.token Number "100")) cursor) == updated)
  assert "original snapshot preserved" (Green.render document == before)
  assert "typed groups" (length (Ast.children (Cursor.fromGreen document) :: [Grouped]) == 1000)
  assert "history count" (length (snapshots 100 path document) == 101)
  putStrLn "Benchmark fixtures and observations: PASS"
  where
    assert label valid = unless valid (error ("benchmark check failed: " ++ label))
    checkWide count = do
      let texts = wideSource count
          node = wideTree texts
          root = Cursor.fromGreen (Node node)
          insertion = Token (Green.token Number "NEW")
          inserted = checked (Green.spliceChildren (count `div` 2) (count `div` 2) [insertion] node)
      _ <- evaluate (forceElement (Node node))
      assert "wide exact source" (Green.renderNode node == Text.concat texts)
      assert "builder tokens" (buildTokens texts == node)
      assert "builder reuse" (buildReuse node == node)
      assert "monadic builder" (buildMonadic texts == node)
      assert "checkpoint source" (Green.renderNode (buildWrapped node) == Text.concat texts)
      assert "wide UTF-8 cache" (Green.nodeLength node == sum
        [4 + fromIntegral (length (show index)) | index <- [1 .. count]])
      assert "last offset" (cursorEnd (checked (Cursor.childAt (count - 1) root)) == Green.nodeLength node)
      assert "sibling observation" (siblingSum root == sum (map cursorEnd (Cursor.children root)))
      assert "typed last token" (cursorEnd (toCursor (selected (Ast.token root :: Maybe Numbered))) == Green.nodeLength node)
      assert "splice exact source" (Green.renderNode inserted == Text.concat
        [Text.concat (take (count `div` 2) texts), "NEW", Text.concat (drop (count `div` 2) texts)])
    checkChain count = do
      let node = chainTree count
          root = Cursor.fromGreen (Node node)
      _ <- evaluate (forceElement (Node node))
      assert "chain source" (Green.renderNode node == chainSource count)
      assert "chain tokens" (length (Cursor.descendantTokens root) == 2 * count + 1)
      assert "chain first offset" (fmap cursorEnd (singleToken (Cursor.tokenAt 0 root)) == Just 1)
    singleToken (SingleToken cursor) = Just cursor
    singleToken _ = Nothing
    checkDeep depth = do
      let node = deepTree depth
          root = Cursor.fromGreen (Node node)
          path = replicate (depth + 1) 0
          leaf = checked (Cursor.at path root)
      _ <- evaluate (forceElement (Node node))
      assert "deep source" (Green.renderNode node == "001")
      assert "deep path" (Cursor.path leaf == path)
      assert "deep cached range" (cursorEnd leaf == 3)
      assert "deep edit" (Green.render (editMany 2 path (Node node)) == "2")

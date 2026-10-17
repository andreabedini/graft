{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- Workloads and observations shared by timing and residency runs.
module Fixtures
  ( Kind (..), Prepared (..), PreparedCursor (..), Named (..), Numbered (..), Grouped (..)
  , wideSource, wideTree, deepTree, chainTree, chainSource, documentTree, documentSource
  , forceElement, checked, selected, cached, cursorEnd, siblingSum
  , buildTokens, buildReuse, buildCheckpoint, editMany, snapshots
  , checkFixtures
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Control.Monad (unless)
import qualified Data.Text as Text
import Data.Text (Text)
import Graft
import qualified Data.List as List

data Kind = Root | Group | Name | Number | Trivia
  deriving (Eq, Show)

-- This benchmark-only wrapper forces fixtures before the timer starts.
-- It deliberately adds no NFData instance to the library's public types.
newtype Prepared = Prepared { preparedTree :: GreenElement Kind }

instance NFData Prepared where
  rnf (Prepared tree) = forceElement tree

newtype PreparedCursor = PreparedCursor { preparedCursor :: Cursor Kind }

instance NFData PreparedCursor where
  rnf (PreparedCursor cursor) = forceElement (cursorGreen (cursorRoot cursor)) `seq`
    rnf (cursorPath cursor) `seq` rnf (cursorEnd cursor)

forceElement :: GreenElement Kind -> ()
forceElement element = elementKind element `seq` elementTextLength element `seq`
  case element of
    Token token -> rnf (tokenText token)
    Node node -> foldr (\child rest -> forceElement child `seq` rest) () (nodeChildren node)

newtype Named = Named (Cursor Kind)
instance AstToken Named where
  type AstTokenKind Named = Kind
  castToken = matchToken (== Name) Named
  tokenSyntax (Named cursor) = cursor

newtype Numbered = Numbered (Cursor Kind)
instance AstToken Numbered where
  type AstTokenKind Numbered = Kind
  castToken = matchToken (== Number) Numbered
  tokenSyntax (Numbered cursor) = cursor

newtype Grouped = Grouped (Cursor Kind)
instance AstNode Grouped where
  type AstNodeKind Grouped = Kind
  castNode = matchNode (== Group) Grouped
  nodeSyntax (Grouped cursor) = cursor

wideSource :: Int -> [Text]
wideSource count = [Text.pack ("名" ++ show index ++ " ") | index <- [1 .. count]]

wideTree :: [Text] -> GreenNode Kind
wideTree = greenNode Root . wideElements

wideElements :: [Text] -> [GreenElement Kind]
wideElements [] = []
wideElements [text] = [Token (greenToken Number text)]
wideElements (text : rest) = Token (greenToken Name text) : wideElements rest

deepTree :: Int -> GreenNode Kind
deepTree depth = List.foldl' (\node _ -> greenNode Group [Node node]) leaf [1 .. depth]
  where leaf = greenNode Group [Token (greenToken Number "001")]

-- A left-nested operator chain such as @((0 + 1) + 2) + 3@: both deep and
-- wide, so whole-tree traversals pay for every level on every token.
chainTree :: Int -> GreenNode Kind
chainTree count = List.foldl' step leaf [1 .. count]
  where
    leaf = greenNode Group [Token (greenToken Number "0")]
    step left index = greenNode Group
      [Node left, Token (greenToken Trivia " + "), Token (greenToken Number (Text.pack (show index)))]

chainSource :: Int -> Text
chainSource count = Text.concat ("0" : [Text.pack (" + " ++ show index) | index <- [1 .. count]])

lineText :: Int -> [Text]
lineText index = [Text.pack ("名" ++ show index), "  = ", "001", " -- keep", "\r\n"]

documentSource :: Int -> Text
documentSource count = Text.concat (concatMap lineText [1 .. count])

documentTree :: Int -> GreenNode Kind
documentTree count = greenNode Root [Node (line index) | index <- [1 .. count]]
  where
    line index = greenNode Group (zipWith (\kind text -> Token (greenToken kind text))
      [Name, Trivia, Number, Trivia, Trivia] (lineText index))

checked :: Show err => Either err a -> a
checked = either (error . show) id

selected :: Maybe a -> a
selected = maybe (error "benchmark fixture has no selected element") id

cached :: GreenNode Kind -> TextLength
cached = nodeTextLength

cursorEnd :: Cursor Kind -> TextLength
cursorEnd = rangeEnd . cursorRange

-- A strict consumer: each adjacent sibling and range contributes to the result.
siblingSum :: Cursor Kind -> TextLength
siblingSum root = go 0 (cursorFirstChild root)
  where
    go total Nothing = total
    go total (Just cursor) = let next = total + cursorEnd cursor
      in next `seq` go next (cursorNextSibling cursor)

buildTokens :: [Text] -> GreenNode Kind
buildTokens texts = checked (runBuilder (checked (finishNode emitted)))
  where
    emitted = go (startNode Root emptyBuilder) texts
    go builder [] = builder
    go builder [text] = emitToken Number text builder
    go builder (text : rest) = let next = emitToken Name text builder
      in next `seq` go next rest

buildReuse :: GreenNode Kind -> GreenNode Kind
buildReuse node = checked (runBuilder (checked (finishNode emitted)))
  where emitted = List.foldl' (flip emitElement) (startNode Root emptyBuilder) (nodeChildren node)

buildCheckpoint :: GreenNode Kind -> GreenNode Kind
buildCheckpoint node = checked (runBuilder closed)
  where
    opened = startNode Root emptyBuilder
    marker = checkpoint opened
    emitted = List.foldl' (flip emitElement) opened (nodeChildren node)
    wrapped = checked (startNodeAt marker Group emitted)
    closed = checked (finishNode (checked (finishNode wrapped)))

-- Each edit depends on the preceding snapshot. Distinct text prevents no-op
-- updates. Force the cache on every step without walking shared descendants.
editMany :: Int -> Path -> GreenElement Kind -> GreenElement Kind
editMany count path root = List.foldl' step root [1 .. count]
  where
    step previous index =
      let updated = checked (replaceAt path (Token (greenToken Number (Text.pack (show index)))) previous)
      in elementTextLength updated `seq` updated

snapshots :: Int -> Path -> GreenElement Kind -> [GreenElement Kind]
snapshots count path root = scanl step root [1 .. count]
  where
    step previous index =
      let updated = checked (replaceAt path (Token (greenToken Number (Text.pack (show index)))) previous)
      in elementTextLength updated `seq` updated

-- Independent source checks guard the workload, not timing thresholds.
checkFixtures :: IO ()
checkFixtures = do
  mapM_ checkWide [100, 1000, 10000]
  mapM_ checkDeep [100, 1000, 10000]
  mapM_ checkChain [100, 1000, 10000]
  let document = Node (documentTree 1000)
      before = documentSource 1000
      path = [499, 2]
      cursor = checked (cursorAt path (rootCursor document))
      updated = editMany 100 path document
      expected = Text.concat [documentSource 499, Text.concat
        ["名500", "  = ", "100", " -- keep", "\r\n"],
        Text.concat (concatMap lineText [501 .. 1000])]
  assert "document source" (renderElement document == before)
  assert "repeated exact edits" (renderElement updated == expected)
  assert "focus/path edit agreement" (replaceFocus (Token (greenToken Number "100")) cursor == updated)
  assert "original snapshot preserved" (renderElement document == before)
  assert "typed groups" (length (astChildren (rootCursor document) :: [Grouped]) == 1000)
  assert "history count" (length (snapshots 100 path document) == 101)
  putStrLn "Benchmark fixtures and observations: PASS"
  where
    assert label valid = unless valid (error ("benchmark check failed: " ++ label))
    checkWide count = do
      let texts = wideSource count
          node = wideTree texts
          root = rootCursor (Node node)
          insertion = Token (greenToken Number "NEW")
          inserted = checked (spliceChildren (count `div` 2) (count `div` 2) [insertion] node)
      _ <- evaluate (forceElement (Node node))
      assert "wide exact source" (renderNode node == Text.concat texts)
      assert "builder tokens" (buildTokens texts == node)
      assert "builder reuse" (buildReuse node == node)
      assert "checkpoint source" (renderNode (buildCheckpoint node) == Text.concat texts)
      assert "wide UTF-8 cache" (nodeTextLength node == sum
        [4 + length (show index) | index <- [1 .. count]])
      assert "last offset" (cursorEnd (checked (cursorChildAt (count - 1) root)) == nodeTextLength node)
      assert "sibling observation" (siblingSum root == sum (map cursorEnd (cursorChildren root)))
      assert "typed last token" (cursorEnd (tokenSyntax (selected (astToken root :: Maybe Numbered))) == nodeTextLength node)
      assert "splice exact source" (renderNode inserted == Text.concat
        [Text.concat (take (count `div` 2) texts), "NEW", Text.concat (drop (count `div` 2) texts)])
    checkChain count = do
      let node = chainTree count
          root = rootCursor (Node node)
      _ <- evaluate (forceElement (Node node))
      assert "chain source" (renderNode node == chainSource count)
      assert "chain tokens" (length (cursorTokens root) == 2 * count + 1)
      assert "chain first offset" (fmap cursorEnd (singleToken (tokenAtOffset 0 root)) == Just 1)
    singleToken (SingleToken cursor) = Just cursor
    singleToken _ = Nothing
    checkDeep depth = do
      let node = deepTree depth
          root = rootCursor (Node node)
          path = replicate (depth + 1) 0
          leaf = checked (cursorAt path root)
      _ <- evaluate (forceElement (Node node))
      assert "deep source" (renderNode node == "001")
      assert "deep path" (cursorPath leaf == path)
      assert "deep cached range" (cursorEnd leaf == 3)
      assert "deep edit" (renderElement (editMany 2 path (Node node)) == "2")

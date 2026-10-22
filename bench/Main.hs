{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.DeepSeq (rnf)
import Data.Hashable (hash)
import qualified Data.List as List
import qualified Data.Text as Text
import Fixtures
import Graft
import qualified Graft.Ast as Ast
import qualified Graft.Cursor as Cursor
import qualified Graft.Green as Green
import qualified Graft.Range as Range
import Residency (runResidency)
import System.Environment (getArgs)
import Test.Tasty.Bench
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--check"] -> checkFixtures
    ["--residency", scenario, size, edits] ->
      case (readMaybe size, readMaybe edits) of
        (Just documentLines, Just editCount) -> runResidency scenario documentLines editCount
        _ -> error "residency size and edits must be integers"
    _ -> defaultMain
      [ bgroup "token" [tokenBench bytes | bytes <- [32, 4096]]
      , bgroup "wide" (map wideBench [100, 1000, 10000])
      , bgroup "deep" (map deepBench [100, 1000, 10000])
      , bgroup "chain" (map chainBench [100, 1000, 10000])
      , documentBench
      ]

tokenBench :: Int -> Benchmark
tokenBench bytes = env (pure (Text.replicate bytes (Text.singleton 'a'),
    Text.replicate (bytes `div` 4) (Text.singleton '🙂'))) $ \ ~(ascii, unicode) ->
  bgroup (show bytes)
    [ bench "ascii" $ whnf (Green.tokenLength . Green.token Name) ascii
    , bench "unicode" $ whnf (Green.tokenLength . Green.token Name) unicode
    ]

wideBench :: Int -> Benchmark
wideBench size = bgroup (show size)
  [ env (pure (wideSource size)) $ \texts -> bgroup "construct"
      [ bench "direct" $ whnf (cached . wideTree) texts
      , bench "builder-tokens" $ whnf (cached . buildTokens) texts
      , bench "builder-monad" $ whnf (cached . buildMonadic) texts
      ]
  , env (pure (Prepared (Node (wideTree (wideSource size))))) $ \prepared ->
      let tree = preparedTree prepared
          node = case tree of Node value -> value; Token _ -> error "expected node"
          root = Cursor.fromGreen tree
          replacement = Token (Green.token Number (Text.singleton '2'))
          endAt index input = cursorEnd (checked (Cursor.childAt index (Cursor.fromGreen input)))
          replace index input = Green.length (Cursor.tree (Cursor.replace replacement
            (checked (Cursor.childAt index (Cursor.fromGreen input)))))
          -- Replace every child through one cursor, moving to the next sibling
          -- after each edit, as a codemod would.
          editEach input = Green.length (Cursor.tree (go (checked (Cursor.childAt 0 (Cursor.fromGreen input)))))
            where
              go cursor = let edited = Cursor.replace replacement cursor
                in maybe edited go (Cursor.nextSibling edited)
          -- The same result from one edit of the parent.
          editParent input = Green.length (Cursor.tree (Cursor.modify rebuild (Cursor.fromGreen input)))
            where
              rebuild (Node parent) = Node (Green.node (Green.nodeKind parent) (map (const replacement) (Green.children parent)))
              rebuild token = token
      in bgroup "prepared"
        [ bench "render" $ nf Green.render tree
        , bench "hash" $ whnf hash tree
        , bench "rnf" $ whnf rnf tree
        , bench "builder-reuse" $ whnf (cached . buildReuse) node
        , bench "builder-checkpoint" $ whnf (cached . buildWrapped) node
        , bench "child-first" $ whnf (endAt 0) tree
        , bench "child-middle" $ whnf (endAt (size `div` 2)) tree
        , bench "child-last" $ whnf (endAt (size - 1)) tree
        , bench "siblings" $ whnf (siblingSum . Cursor.fromGreen) tree
        , bench "offset-first" $ whnf (offsetEnd 1 . Cursor.fromGreen) tree
        , bench "offset-last" $ whnf (offsetEnd (Green.nodeLength node - 1) . Cursor.fromGreen) tree
        , bench "cover-last" $ whnf (cursorEnd . selected . Cursor.covering
            (Range.fromLength (Green.nodeLength node - 1) 1) . Cursor.fromGreen) tree
        , bench "replace-first" $ whnf (replace 0) tree
        , bench "replace-middle" $ whnf (replace (size `div` 2)) tree
        , bench "replace-last" $ whnf (replace (size - 1)) tree
        , bench "splice-middle" $ whnf (cached . checked . Green.spliceChildren
            (size `div` 2) (size `div` 2) [replacement]) node
        , bench "edit-each-child" $ whnf editEach tree
        , bench "edit-parent-once" $ whnf editParent tree
        , bench "typed-first" $ whnf (maybe 0 (cursorEnd . tokenSyntax) .
            (Ast.token :: Cursor Kind -> Maybe Named)) root
        , bench "typed-last" $ whnf (maybe 0 (cursorEnd . tokenSyntax) .
            (Ast.token :: Cursor Kind -> Maybe Numbered)) root
        , bench "typed-all" $ whnf (List.foldl' (\total view -> total + cursorEnd (tokenSyntax view)) 0 .
            (Ast.tokens :: Cursor Kind -> [Named])) root
        ]
  ]

deepBench :: Int -> Benchmark
deepBench depth = bgroup (show depth)
  [ bench "construct" $ whnf (cached . deepTree) depth
  , env (pure (Prepared (Node (deepTree depth)))) $ \prepared ->
      let tree = preparedTree prepared
          path = replicate (depth + 1) 0
          focused input = checked (Cursor.at path (Cursor.fromGreen input))
          replacement = Token (Green.token Number (Text.singleton '2'))
      in bgroup "prepared"
        [ bench "render" $ nf Green.render tree
        , bench "hash" $ whnf hash tree
        , bench "rnf" $ whnf rnf tree
        , bench "descend" $ whnf (cursorEnd . focused) tree
        , bench "replace-path" $ whnf (Green.length . Cursor.tree . Cursor.replace replacement
            . checked . (`Cursor.at` Cursor.fromGreen tree)) path
        , bench "offset" $ whnf (offsetEnd 1 . Cursor.fromGreen) tree
        , bench "focus-and-replace" $ whnf (Green.length . Cursor.tree . Cursor.replace replacement . focused) tree
        ]
  , env (pure (PreparedCursor (checked (Cursor.at (replicate (depth + 1) 0)
      (Cursor.fromGreen (Node (deepTree depth))))))) $ \prepared -> bgroup "focused"
      [ bench "replace" $ whnf (Green.length . Cursor.tree . Cursor.replace
          (Token (Green.token Number (Text.singleton '2')))) (preparedCursor prepared)
      , bench "root" $ whnf (cursorEnd . Cursor.root) (preparedCursor prepared)
      ]
  ]

chainBench :: Int -> Benchmark
chainBench count = env (pure (Prepared (Node (chainTree count)))) $ \prepared ->
  let tree = preparedTree prepared
      end = Green.length tree
  in bgroup (show count)
    [ bench "render" $ nf Green.render tree
    , bench "hash" $ whnf hash tree
    , bench "rnf" $ whnf rnf tree
    , bench "tokens" $ whnf (List.foldl' (\total cursor -> total + cursorEnd cursor) 0 . Cursor.descendantTokens . Cursor.fromGreen) tree
    , bench "descendants" $ whnf (length . Cursor.descendants . Cursor.fromGreen) tree
    , bench "offset-first" $ whnf (offsetEnd 1 . Cursor.fromGreen) tree
    , bench "offset-last" $ whnf (offsetEnd (end - 1) . Cursor.fromGreen) tree
    ]

documentBench :: Benchmark
documentBench = env (pure (Prepared (Node (documentTree 1000)))) $ \prepared ->
  let tree = preparedTree prepared
      path = [499, 2]
      root = Cursor.fromGreen tree
  in bgroup "document-1000"
    [ bench "construct" $ whnf (cached . documentTree) 1000
    , bench "render" $ nf Green.render tree
    , bench "hash" $ whnf hash tree
    , bench "rnf" $ whnf rnf tree
    , bench "tokens" $ whnf (List.foldl' (\total cursor -> total + cursorEnd cursor) 0 . Cursor.descendantTokens . Cursor.fromGreen) tree
    , bench "typed-first-group" $ whnf (maybe 0 (cursorEnd . nodeSyntax) .
        (Ast.child :: Cursor Kind -> Maybe Grouped)) root
    , bench "typed-all-groups" $ whnf (List.foldl' (\total view -> total + cursorEnd (nodeSyntax view)) 0 .
        (Ast.children :: Cursor Kind -> [Grouped])) root
    , bench "100-edits-latest" $ whnf (Green.length . editMany 100 path) tree
    , bench "100-edits-history" $ whnf (List.foldl' (\total snapshot -> total + Green.length snapshot) 0 .
        snapshots 100 path) tree
    , bench "rename-all" $ whnf (Green.length . renameAll False) tree
    , bench "rename-all-ancestors" $ whnf (Green.length . renameAll True) tree
    ]
  where
    renameAll ancestors input = maybe input Cursor.tree (renameWalk ancestors input)

offsetEnd :: TextLength -> Cursor Kind -> TextLength
offsetEnd offset root = case Cursor.tokenAt offset root of
  NoToken -> error "fixture offset has no token"
  SingleToken cursor -> cursorEnd cursor
  BetweenTokens left right -> cursorEnd left + cursorEnd right

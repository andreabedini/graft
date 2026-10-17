{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.Text as Text
import Fixtures
import Graft
import qualified Data.List as List
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
        (Just lineCount, Just editCount) -> runResidency scenario lineCount editCount
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
    [ bench "ascii" $ whnf (tokenTextLength . greenToken Name) ascii
    , bench "unicode" $ whnf (tokenTextLength . greenToken Name) unicode
    ]

wideBench :: Int -> Benchmark
wideBench size = bgroup (show size)
  [ env (pure (wideSource size)) $ \texts -> bgroup "construct"
      [ bench "direct" $ whnf (cached . wideTree) texts
      , bench "builder-tokens" $ whnf (cached . buildTokens) texts
      ]
  , env (pure (Prepared (Node (wideTree (wideSource size))))) $ \prepared ->
      let tree = preparedTree prepared
          node = case tree of Node value -> value; Token _ -> error "expected node"
          root = rootCursor tree
          replacement = Token (greenToken Number (Text.singleton '2'))
          endAt index input = cursorEnd (checked (cursorChildAt index (rootCursor input)))
          replace index input = elementTextLength (checked (replaceAt [index] replacement input))
      in bgroup "prepared"
        [ bench "render" $ nf renderElement tree
        , bench "builder-reuse" $ whnf (cached . buildReuse) node
        , bench "builder-checkpoint" $ whnf (cached . buildCheckpoint) node
        , bench "child-first" $ whnf (endAt 0) tree
        , bench "child-middle" $ whnf (endAt (size `div` 2)) tree
        , bench "child-last" $ whnf (endAt (size - 1)) tree
        , bench "siblings" $ whnf (siblingSum . rootCursor) tree
        , bench "offset-first" $ whnf (offsetEnd 1 . rootCursor) tree
        , bench "offset-last" $ whnf (offsetEnd (nodeTextLength node - 1) . rootCursor) tree
        , bench "cover-last" $ whnf (cursorEnd . selected . coveringElement
            (rangeFromLength (nodeTextLength node - 1) 1) . rootCursor) tree
        , bench "replace-first" $ whnf (replace 0) tree
        , bench "replace-middle" $ whnf (replace (size `div` 2)) tree
        , bench "replace-last" $ whnf (replace (size - 1)) tree
        , bench "splice-middle" $ whnf (cached . checked . spliceChildren
            (size `div` 2) (size `div` 2) [replacement]) node
        , bench "typed-first" $ whnf (maybe 0 (cursorEnd . tokenSyntax) .
            (astToken :: Cursor Kind -> Maybe Named)) root
        , bench "typed-last" $ whnf (maybe 0 (cursorEnd . tokenSyntax) .
            (astToken :: Cursor Kind -> Maybe Numbered)) root
        , bench "typed-all" $ whnf (List.foldl' (\total view -> total + cursorEnd (tokenSyntax view)) 0 .
            (astTokens :: Cursor Kind -> [Named])) root
        ]
  ]

deepBench :: Int -> Benchmark
deepBench depth = bgroup (show depth)
  [ bench "construct" $ whnf (cached . deepTree) depth
  , env (pure (Prepared (Node (deepTree depth)))) $ \prepared ->
      let tree = preparedTree prepared
          path = replicate (depth + 1) 0
          focused input = checked (cursorAt path (rootCursor input))
          replacement = Token (greenToken Number (Text.singleton '2'))
      in bgroup "prepared"
        [ bench "render" $ nf renderElement tree
        , bench "descend" $ whnf (cursorEnd . focused) tree
        , bench "replace-path" $ whnf (elementTextLength . checked . replaceAt path replacement) tree
        , bench "offset" $ whnf (offsetEnd 1 . rootCursor) tree
        , bench "focus-and-replace" $ whnf (elementTextLength . cursorTree . replaceFocus replacement . focused) tree
        ]
  , env (pure (PreparedCursor (checked (cursorAt (replicate (depth + 1) 0)
      (rootCursor (Node (deepTree depth))))))) $ \prepared -> bgroup "focused"
      [ bench "replace" $ whnf (elementTextLength . cursorTree . replaceFocus
          (Token (greenToken Number (Text.singleton '2')))) (preparedCursor prepared)
      , bench "root" $ whnf (cursorEnd . cursorRoot) (preparedCursor prepared)
      ]
  ]

chainBench :: Int -> Benchmark
chainBench count = env (pure (Prepared (Node (chainTree count)))) $ \prepared ->
  let tree = preparedTree prepared
      end = elementTextLength tree
  in bgroup (show count)
    [ bench "tokens" $ whnf (List.foldl' (\total cursor -> total + cursorEnd cursor) 0 . cursorTokens . rootCursor) tree
    , bench "descendants" $ whnf (length . cursorDescendants . rootCursor) tree
    , bench "offset-first" $ whnf (offsetEnd 1 . rootCursor) tree
    , bench "offset-last" $ whnf (offsetEnd (end - 1) . rootCursor) tree
    ]

documentBench :: Benchmark
documentBench = env (pure (Prepared (Node (documentTree 1000)))) $ \prepared ->
  let tree = preparedTree prepared
      path = [499, 2]
      root = rootCursor tree
  in bgroup "document-1000"
    [ bench "construct" $ whnf (cached . documentTree) 1000
    , bench "render" $ nf renderElement tree
    , bench "tokens" $ whnf (List.foldl' (\total cursor -> total + cursorEnd cursor) 0 . cursorTokens . rootCursor) tree
    , bench "typed-first-group" $ whnf (maybe 0 (cursorEnd . nodeSyntax) .
        (astChild :: Cursor Kind -> Maybe Grouped)) root
    , bench "typed-all-groups" $ whnf (List.foldl' (\total view -> total + cursorEnd (nodeSyntax view)) 0 .
        (astChildren :: Cursor Kind -> [Grouped])) root
    , bench "100-edits-latest" $ whnf (elementTextLength . editMany 100 path) tree
    , bench "100-edits-history" $ whnf (List.foldl' (\total snapshot -> total + elementTextLength snapshot) 0 .
        snapshots 100 path) tree
    ]

offsetEnd :: TextLength -> Cursor Kind -> TextLength
offsetEnd offset root = case tokenAtOffset offset root of
  NoToken -> error "fixture offset has no token"
  SingleToken cursor -> cursorEnd cursor
  BetweenTokens left right -> cursorEnd left + cursorEnd right

-- Checks the examples in the Haddock documentation. GHC finds the library's
-- dependencies through the environment file that cabal writes at the end of
-- a build (write-ghc-environment-files in cabal.project), so build once
-- before the first run:
--
-- > cabal build all -fdoctest
-- > cabal test graft-doctest -fdoctest
module Main (main) where

import Control.Monad (when)
import Data.List (isPrefixOf)
import System.Directory (listDirectory)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Test.DocTest (doctest)

main :: IO ()
main = do
  files <- listDirectory "."
  when (not (any (".ghc.environment." `isPrefixOf`) files)) $ do
    hPutStrLn stderr "No GHC environment file in the package directory."
    hPutStrLn stderr "Run `cabal build all -fdoctest` once, then run the doctests again."
    exitFailure
  doctest
    [ "-isrc"
    , "src/Graft.hs"
    , "src/Graft/Ast.hs"
    , "src/Graft/Build.hs"
    , "src/Graft/Builder.hs"
    , "src/Graft/Cursor.hs"
    , "src/Graft/Green.hs"
    , "src/Graft/LineIndex.hs"
    , "src/Graft/Range.hs"
    ]

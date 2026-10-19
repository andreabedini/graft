-- Checks the examples in the Haddock documentation. GHC finds the library's
-- dependencies through the environment file that cabal writes at the end of
-- a build (write-ghc-environment-files in cabal.project), so build once
-- before the first run:
--
-- > cabal build all -fdoctest
-- > cabal test graft-doctest -fdoctest
--
-- Outside the repository, where cabal.project is absent, pass
-- --write-ghc-environment-files=always to the build.
module Main (main) where

import Control.Monad (unless)
import Data.Version (showVersion)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Info (arch, fullCompilerVersion, os)
import Test.DocTest (doctest)

main :: IO ()
main = do
  -- doctest runs this GHC, so it needs this GHC's environment file.
  let environment = ".ghc.environment." ++ arch ++ "-" ++ os ++ "-" ++ showVersion fullCompilerVersion
  present <- doesFileExist environment
  unless present $ do
    hPutStrLn stderr ("No " ++ environment ++ " in the package directory. Build once with")
    hPutStrLn stderr "  cabal build all -fdoctest --write-ghc-environment-files=always"
    hPutStrLn stderr "using the same compiler, then run the doctests again."
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

module Residency (runResidency) where

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.IORef (newIORef, readIORef)
import Fixtures
import Graft
import qualified Data.List as List
import GHC.Stats
import System.Mem (performMajorGC)

data Retained = Roots [GreenElement Kind] | Focus (Cursor Kind)

-- Force all snapshot roots, but avoid revisiting unchanged descendants for
-- every edit. Smart constructors already force their immediate cache sums.
observe :: Retained -> TextLength
observe (Roots roots) = List.foldl' (\total root -> total + elementTextLength root) 0 roots
observe (Focus cursor) = cursorEnd cursor

-- Keep fixture creation out of the caller: only this result survives its GC.
{-# NOINLINE prepare #-}
prepare :: String -> Int -> Int -> IO Retained
prepare scenario size edits = do
  let root = Node (documentTree size)
      path = [size `div` 2, 2]
  case scenario of
    "shared" -> do
      let line = case nodeChildren (documentTree 1) of
            first : _ -> first
            [] -> error "shared fixture has no line"
          shared = Node (greenNode Root (replicate size line))
      _ <- evaluate (forceElement shared)
      pure (Roots [shared])
    _ -> do
      _ <- evaluate (forceElement root)
      case scenario of
        "tree" -> pure (Roots [root])
        "latest" -> evaluated (Roots [editMany edits path root])
        "history" -> evaluated (Roots (snapshots edits path root))
        "cursor" -> pure (Focus (checked (cursorAt path (rootCursor root))))
        "detached-token" -> pure (Roots [cursorGreen (checked (cursorAt path (rootCursor root)))])
        _ -> error "scenario must be tree, shared, latest, history, cursor or detached-token"
  where
    evaluated retained = evaluate (observe retained) >> pure retained

-- One fresh process per scenario. A live IORef, read after measurement, keeps
-- precisely the requested result reachable across the full collection.
runResidency :: String -> Int -> Int -> IO ()
runResidency scenario size edits = do
  unless (size > 0 && edits >= 0) (error "size must be positive and edits nonnegative")
  enabled <- getRTSStatsEnabled
  unless enabled (error "RTS statistics are required: pass +RTS -T")
  performMajorGC
  before <- getRTSStats
  retained <- prepare scenario size edits
  _ <- evaluate (observe retained)
  keep <- newIORef retained
  performMajorGC
  after <- getRTSStats
  stillLive <- readIORef keep
  checksum <- evaluate (observe stillLive)
  let liveBefore = gcdetails_live_bytes (gc before)
      liveAfter = gcdetails_live_bytes (gc after)
      delta = if liveAfter >= liveBefore then liveAfter - liveBefore else 0
  putStrLn "Scenario,Lines,Edits,Live bytes,Baseline live bytes,Live delta bytes,Allocated bytes,Checksum"
  putStrLn (concat [scenario, ",", show size, ",", show edits, ",", show liveAfter,
    ",", show liveBefore, ",", show delta, ",", show (allocated_bytes after - allocated_bytes before),
    ",", show checksum])

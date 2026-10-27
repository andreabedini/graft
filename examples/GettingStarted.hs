{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text.IO as Text
import Graft
import qualified Graft.Build as Build
import qualified Graft.Cursor as Cursor
import qualified Graft.Edit as Edit
import qualified Graft.Green as Green
import qualified Graft.Range as Range

data Kind = Root | Binding | Name | Number | Trivia
  deriving (Eq, Show)

-- A parser would emit these events; here they are written by hand.
parsed :: Either BuilderError (GreenNode Kind)
parsed = Build.exec $ Build.node Root $ do
  Build.node Binding $ do
    Build.token Name "x"
    Build.token Trivia "  = "
    Build.token Number "1"
  Build.token Trivia " -- keep\r\n"

main :: IO ()
main = do
  tree <- either (fail . show) pure parsed
  Text.putStr (Green.renderNode tree)
  case Cursor.covering (Range.fromLength 5 1) (Cursor.fromGreen (Node tree)) of
    Just number -> do
      let edits = [Edit.replace number (Token (Green.token Number "42"))]
      edited <- either (fail . show) pure (Edit.apply (Node tree) edits)
      Text.putStr (Green.render edited)
    Nothing -> fail "nothing at offset 5"

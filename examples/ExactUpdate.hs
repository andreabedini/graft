{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- Run with: cabal run graft-example
-- The tree is supplied by an application, without a lexer or parser in Graft.
module Main (main) where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Graft
import qualified Graft.Ast as Ast
import qualified Graft.Cursor as Cursor
import qualified Graft.Green as Green
import Text.Read (readMaybe)

data Kind = Root | Expr | Name | Number | Trivia deriving (Eq, Show)

-- Constructors stay private so application code obtains views by casting.
newtype Expression = Expression (Cursor Kind)
instance AstView Expression where
  type AstViewKind Expression = Kind
  fromCursor = Ast.matchNode (== Expr) Expression
  toCursor (Expression cursor) = cursor
instance AstNode Expression

newtype NumberLiteral = NumberLiteral (Cursor Kind)
instance AstView NumberLiteral where
  type AstViewKind NumberLiteral = Kind
  fromCursor = Ast.matchToken (== Number) NumberLiteral
  toCursor (NumberLiteral cursor) = cursor
instance AstToken NumberLiteral

expressionNumber :: Expression -> Maybe NumberLiteral
expressionNumber = Ast.token . toCursor

-- Reading the semantic value and choosing new spelling are application policy.
-- A valid token kind alone does not guarantee valid numeric text.
numberValue :: NumberLiteral -> Maybe Integer
numberValue = readMaybe . Text.unpack . Green.render . Cursor.green . toCursor

setNumberValue :: Integer -> NumberLiteral -> Maybe (GreenElement Kind)
setNumberValue value literal = do
  oldValue <- numberValue literal
  let focus = toCursor literal
  pure $ if oldValue == value
    then Cursor.tree focus -- Retain spelling such as 001.
    else Cursor.tree (Cursor.replace (Token (Green.token Number (Text.pack (show value)))) focus)

updateValue :: Integer -> GreenElement Kind -> Maybe (GreenElement Kind)
updateValue value root = do
  expression <- Ast.child (Cursor.fromGreen root)
  literal <- expressionNumber expression
  setNumberValue value literal

original :: GreenElement Kind
original = Node (Green.node Root
  [ Token (Green.token Name "x")
  , Token (Green.token Trivia "  = ")
  , Node (Green.node Expr [Token (Green.token Number "001")])
  , Token (Green.token Trivia " -- keep\r\n")
  ])

main :: IO ()
main = mapM_ display [Just original, updateValue 1 original, updateValue 2 original]
  where
    display Nothing = fail "Missing expression, missing number, or invalid numeric text"
    display (Just root) = TextIO.putStr (Green.render root)

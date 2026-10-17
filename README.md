# Graft

Lossless, immutable concrete syntax trees for Haskell, modelled on
rust-analyzer's [Rowan](https://github.com/rust-analyzer/rowan).

A Graft tree keeps every byte of the source: whitespace, comments and malformed
fragments are ordinary tokens. You can navigate the tree, find the syntax at a
byte offset, edit it and print it back, and everything you didn't touch comes
out exactly as it went in. That makes it a foundation for formatters,
refactoring and codemod tools, and language servers. You supply the lexer and
parser; Graft supplies the tree.

Graft is experimental. The API may change before 1.0.

## Getting started

Add `graft` to your `build-depends`. This program builds a tree, finds the
number at byte offset 5, replaces it, and prints the source before and after:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text.IO as Text
import Graft

data Kind = Root | Binding | Name | Number | Trivia
  deriving (Eq, Show)

-- A parser would emit these events; here they are written by hand.
parsed :: Either BuilderError (GreenNode Kind)
parsed = execBuild $ buildNode Root $ do
  buildNode Binding $ do
    buildToken Name "x"
    buildToken Trivia "  = "
    buildToken Number "1"
  buildToken Trivia " -- keep\r\n"

main :: IO ()
main = do
  tree <- either (fail . show) pure parsed
  Text.putStr (renderNode tree)
  case coveringElement (rangeFromLength 5 1) (rootCursor (Node tree)) of
    Just number -> do
      let edited = replaceFocus (Token (greenToken Number "42")) number
      Text.putStr (renderElement (cursorTree edited))
    Nothing -> fail "nothing at offset 5"
```

It prints:

```text
x  = 1 -- keep
x  = 42 -- keep
```

The double space, the comment and the CRLF line ending survive the edit.

## How it fits together

- **Green trees** (`Graft.Green`) are immutable nodes and tokens tagged with
  your own syntax-kind type. Each node caches its length in UTF-8 bytes and the
  offset of each child, so length and child lookups don't traverse anything.
  Equal subtrees can be shared.
- **Cursors** (`Graft.Cursor`) are zippers over a green tree. They know their
  parent, siblings and absolute byte range. You can find tokens by offset, and
  edit through a cursor and keep navigating the edited tree; old cursors keep
  seeing the old tree.
- **Builders** (`Graft.Builder`) turn a parser's start/token/finish events into a
  tree. Checkpoints let a parser wrap elements it has already emitted, as an
  operator-precedence parser needs to.
- **Typed views** (`Graft.Ast`) are newtypes over cursors that give your
  grammar's nodes typed accessors, without building a separate AST.
- **Child edits** (`Graft.Edit`) replace, insert and remove the children of a
  green node; to edit inside a tree, go through a cursor. **Byte ranges**
  (`Graft.Range`) describe where things are.
- **Line index** (`Graft.LineIndex`) converts byte offsets to and from line and
  column positions, including the UTF-16 columns used by the Language Server
  Protocol.

Import `Graft` for everything, or the individual modules. All offsets and
lengths are `TextLength`, a count of UTF-8 bytes, never characters.

## Learn more

- The [Haddock documentation](https://hackage.haskell.org/package/graft) covers
  every function's contract, cost and edge cases.
- [`examples/ExactUpdate.hs`](examples/ExactUpdate.hs) uses typed views to
  change a number's value while keeping its spelling (`001` stays `001` if the
  value doesn't change).
- [`docs/architecture.md`](docs/architecture.md) explains the design, how it
  differs from Rowan, and what is left to do.
- [`docs/performance.md`](docs/performance.md) records benchmark results, and
  [`bench/README.md`](bench/README.md) explains how to reproduce them.
- [`CONTRIBUTING.md`](CONTRIBUTING.md) describes how to build and test Graft.

## License

[MIT](LICENSE). Graft is a port of ideas from Rowan, which is MIT or Apache-2.0;
see [NOTICE](NOTICE) and [licenses/ROWAN-MIT](licenses/ROWAN-MIT).

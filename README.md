# Graft

Lossless, immutable concrete syntax trees for Haskell, modelled on
rust-analyzer's [Rowan](https://github.com/rust-analyzer/rowan).

A Graft tree keeps every byte of the source: whitespace, comments and malformed
fragments are ordinary tokens. You can navigate the tree, find the syntax at a
byte offset, edit it and print it back, and everything you didn't touch comes
out exactly as it went in. That makes it a base for formatters, refactoring
and codemod tools, and language servers. You supply the lexer and
parser; Graft supplies the tree.

Graft is experimental. The API may change before 1.0.

## Getting started

Add `graft` to your `build-depends`. If your Hackage index doesn't have the
release you want, take it from its tag in `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/andreabedini/graft
  tag: v0.1.0.0
```

This program builds a tree, finds the number at byte offset 5, replaces it,
and prints the source before and after:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text.IO as Text
import Graft
import qualified Graft.Build as Build
import qualified Graft.Cursor as Cursor
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
      let edited = Cursor.replace (Token (Green.token Number "42")) number
      Text.putStr (Green.render (Cursor.tree edited))
    Nothing -> fail "nothing at offset 5"
```

It prints:

```text
x  = 1 -- keep
x  = 42 -- keep
```

The double space, the comment and the CRLF line ending survive the edit.

`Cursor.covering` returns the innermost element that contains a range, here
the one byte at offset 5. `Cursor.tokenAt 5` would return both tokens that
touch offset 5, `"  = "` and `"1"`, as an editor's caret between them touches
both. `Cursor.replace` returns a cursor on the edited tree, and `Cursor.tree`
gives the whole edited tree.

## How it fits together

- **Green trees** (`Graft.Green`) are immutable nodes and tokens tagged with
  your own syntax-kind type. Each node caches its length in UTF-8 bytes and the
  offset of each child, so length and child lookups don't traverse anything.
  Green values have no parents or positions, so equal subtrees can be shared,
  within a tree and between versions of it. `Green.foldM`, `Green.fold`,
  `Green.map` and `Green.traverse` cover a whole tree.
- **Cursors** (`Graft.Cursor`) are zippers over a green tree. They know their
  parent, siblings and absolute byte range, and find tokens by offset. An edit
  through a cursor returns a cursor on the edited tree, so you can keep
  navigating and editing; old cursors keep seeing the old tree.
- **Builders** (`Graft.Build`, `Graft.Builder`) turn a parser's start/token/finish
  events into a tree, in a monad or as a pure value. Checkpoints let a parser
  wrap elements it has already emitted, as an operator-precedence parser needs
  to.
- **Typed views** (`Graft.Ast`) are newtypes over cursors that give your
  grammar's nodes typed accessors, without building a separate AST.
- **Byte ranges** (`Graft.Range`) describe where things are.
- **Line index** (`Graft.LineIndex`) converts byte offsets to and from line and
  column positions, including the UTF-16 columns used by the Language Server
  Protocol.

`Graft` exports the types and classes. The functions have short names meant
for qualified import, as in the example: `Cursor.parent`, `Green.node`,
`Range.start`. Several, such as `Green.length` and `Green.map`, clash with the
Prelude. All offsets and lengths are `TextLength`, a count of UTF-8 bytes,
never characters.

## Learn more

- The [API documentation](https://andreabedini.github.io/graft/) of the latest
  release gives each function's contract and cost. The introductions to
  `Graft.Build`, `Graft.Cursor`, `Graft.Ast` and `Graft.LineIndex` each start
  with an example: wrapping an operand at a checkpoint, renaming every matching
  token in one pass, a typed view with a field, and converting Language Server
  Protocol positions.
- [`examples/ExactUpdate.hs`](https://github.com/andreabedini/graft/blob/main/examples/ExactUpdate.hs)
  uses typed views to change a number's value while keeping its spelling
  (`001` stays `001` if the value doesn't change).
- [`docs/architecture.md`](https://github.com/andreabedini/graft/blob/main/docs/architecture.md)
  explains the design, how it differs from Rowan, and what is left to do.
- [`docs/performance.md`](https://github.com/andreabedini/graft/blob/main/docs/performance.md)
  records benchmark results, and
  [`bench/README.md`](https://github.com/andreabedini/graft/blob/main/bench/README.md)
  explains how to reproduce them.
- [`CONTRIBUTING.md`](https://github.com/andreabedini/graft/blob/main/CONTRIBUTING.md)
  describes how to build and test Graft, and how releases are made.

## License

[MIT](https://github.com/andreabedini/graft/blob/main/LICENSE). Graft is a port of ideas from Rowan, which is MIT or Apache-2.0;
see [NOTICE](https://github.com/andreabedini/graft/blob/main/NOTICE) and [licenses/ROWAN-MIT](https://github.com/andreabedini/graft/blob/main/licenses/ROWAN-MIT).

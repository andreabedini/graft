# Graft

Graft is an idiomatic Haskell port of [Rowan](https://github.com/rust-analyzer/rowan)
for immutable, lossless concrete syntax trees. Its intended use is bidirectional
parsing and printing, with semantic updates that preserve untouched source text.
Lexing, parsing, grammar validation, and pretty-printing policy belong to callers.

This first milestone implements the **green tree** core: ordered nodes and exact
token text, cached UTF-8 byte lengths, lossless rendering, path lookup, and
persistent subtree replacement. The API is experimental. Red trees (zippers),
source ranges, builders, checkpoints, interning, and typed AST views are future
work.

## Build and test

Tested locally with GHC 9.12.2 and cabal-install 3.16.1.0. The dependency bounds
also permit GHC 9.6 through 9.10; those versions have not yet been tested.

```sh
cabal build all
cabal test all --test-show-details=direct
cabal haddock all
```

The tests use QuickCheck and a separate model tree. They cover rendering, UTF-8
lengths at every subtree, different token boundaries, edits at valid paths,
unchanged branches, identity edits, and invalid paths. Deterministic regressions
cover empty nodes/tokens, trivia, CRLF, Unicode, NUL, and large logical lengths
in shared trees. A failed property makes the test executable exit unsuccessfully.

## Preserve formatting while changing a subtree

The following complete program prints the original and the edited source. The
double space, trailing comment, and CRLF line ending survive the replacement.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Graft

data Kind = Root | Expr | Name | Number | Trivia deriving (Eq, Show)

tok :: Kind -> Text -> GreenElement Kind
tok kind = Token . greenToken kind

original :: GreenNode Kind
original = greenNode Root
  [ tok Name "x"
  , tok Trivia "  = "
  , Node (greenNode Expr [tok Number "1"])
  , tok Trivia " -- keep\r\n"
  ]

main :: IO ()
main = do
  TextIO.putStr (renderNode original)
  let replacement = Node (greenNode Expr [tok Number "2"])
  case replaceAt [2] replacement (Node original) of
    Left err -> fail (show err)
    Right edited -> TextIO.putStr (renderElement edited)
```

Every source fragment must be supplied in token text, including whitespace,
comments, punctuation, and malformed syntax. Graft neither inserts separators
nor normalizes text. Losslessness applies to Unicode `Text`; retaining invalid
UTF-8 byte sequences is outside this representation's contract.

## Core API

Import `Graft`, or use `Graft.Green` and `Graft.Edit` separately. `GreenNode kind`
and `GreenToken kind` have hidden constructors. `GreenElement kind` exposes
`Node` and `Token` for pattern matching. `kind` is the caller's syntax-kind type.

| Operation | Contract |
| --- | --- |
| `greenNode`, `greenToken` | Construct values and establish cached lengths. Empty values are allowed. |
| `nodeKind`, `tokenKind`, `elementKind` | Read caller-defined syntax kinds. |
| `nodeChildren`, `tokenText` | Read ordered children or exact leaf text. |
| `nodeTextLength`, `tokenTextLength`, `elementTextLength` | Read cached UTF-8 byte counts (`TextLength`, an alias for `Natural`). |
| `renderNode`, `renderToken`, `renderElement` | Concatenate leaf text in source order. |
| `elementAt path root` | Look up an element using zero-based child indices. |
| `replaceAt path replacement root` | Replace an element, rebuilding its ancestors. `[]` replaces the root. |
| `replaceChild index replacement node` | Replace an immediate child while retaining the root node and its kind. |

Paths count both nodes and tokens. Lookup and editing return `Either PathError`:
`NegativeChildIndex`, `ChildIndexOutOfBounds`, or `CannotDescendIntoToken`.
Negative indices take precedence over token descent at the same step. Paths
refer to one tree version; paths inside a replaced subtree may become invalid.
Replacement may change kind or switch between node and token. Callers enforce
their grammar's constraints.

## Architecture and remaining work

Graft retains Rowan's separation of immutable syntax from contextual navigation.
Its initial representation deliberately differs:

| Rowan | Graft today |
| --- | --- |
| Compact reference-counted allocations | Ordinary Haskell values managed by the garbage collector |
| Raw integer syntax tags | A caller-defined type parameter |
| Fixed-width byte lengths and cached child offsets | Unbounded byte lengths; child offsets are not yet stored |
| Leading/trailing trivia APIs | Trivia supplied as ordinary token children |
| Cursor-based replacement | Explicit child paths and structured errors |
| Builder cache for deduplication | Explicit value reuse; no interning |

The [architecture note](docs/architecture.typ) explains the decisions, costs,
upstream sources reviewed, and next steps. Build its PDF with Typst:

```sh
typst compile docs/architecture.typ docs/architecture.pdf
```

Next milestones are red zippers with navigation and UTF-8 source ranges;
Rowan-style builders and checkpoints; persistent child insertion/removal/splicing;
typed AST views that retain concrete syntax; and benchmarks before selecting an
interner or changing the representation. Attached trivia and compatibility with
Rowan's current API require further design. Semantic update policy remains a
layer above the concrete tree.

## License

[MIT](LICENSE). Rowan attribution and the preserved upstream permission notice
are in [NOTICE](NOTICE) and [licenses/ROWAN-MIT](licenses/ROWAN-MIT).

# Graft

Graft is an idiomatic Haskell port of [Rowan](https://github.com/rust-analyzer/rowan)
for immutable, lossless concrete syntax trees. Its intended use is bidirectional
parsing and printing, with semantic updates that preserve untouched source text.
Lexing, parsing, grammar validation, and pretty-printing policy belong to callers.

The **green tree** core provides ordered nodes and exact
token text, cached UTF-8 byte lengths, lossless rendering, path lookup, and
persistent subtree replacement. The second milestone adds immutable **red
cursors** (zippers), contextual navigation, UTF-8 source ranges, boundary-aware
lookup, and replacement through a focus. The third milestone adds pure builders
with checkpoints for wrapping previously emitted elements. The API is
experimental. Child splicing, interning, and typed AST views remain future work.

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
Cursor properties check every occurrence against the model, navigation round
trips, token order, offsets after edits, scoped offset lookup, and covering ranges.
Regressions distinguish shared occurrences and test boundaries and empty elements.
Builder properties compare streamed construction with an independent tree model.
They cover checkpoint wrapping, later emission, repeated wrapping, saved builder
branches, subtree reuse, and construction errors. Regressions cover checkpoint
lifetime, empty values, and reuse of large cached lengths.

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

Import `Graft`, or use `Graft.Green`, `Graft.Edit`, `Graft.Range`, `Graft.Cursor`,
and `Graft.Builder` separately. `GreenNode kind`
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

## Navigate and edit through a cursor

A `Cursor kind` focuses either a node or a token in one immutable tree snapshot.
Its `TextRange` is a half-open interval of UTF-8 byte offsets.
`newCursor` starts at byte offset zero. Paths identify occurrences within that
snapshot; equal green subtrees can have different paths and source ranges.
`cursorAt` follows a path relative to the current focus.

Using `original` and `Kind` from the program above:

```haskell
replaceExpression :: Either PathError (TextRange, GreenElement Kind)
replaceExpression = do
  focus <- cursorAt [2] (newCursor (Node original))
  let replacement = Node (greenNode Expr [tok Number "2"])
  pure (cursorRange focus, replaceFocus replacement focus)
```

The returned range covers the original expression at UTF-8 byte offsets `[5,6)`.
`replaceFocus` returns a new green root. The old cursor keeps its original tree
and range. Start a new cursor on the result to navigate the edited snapshot.

| Operation | Contract |
| --- | --- |
| `cursorGreen`, `cursorKind`, `cursorRange`, `cursorPath` | Read the focus and its absolute byte range and root-relative path. |
| `cursorRoot`, `parent` | Restore the original root or immediate parent. |
| `childAt`, `firstChild`, `lastChild`, `children` | Navigate immediate children, counting nodes, tokens, and empty elements. |
| `nextSibling`, `previousSibling` | Move between immediate siblings. |
| `ancestors` | Focus followed by parents, nearest first. |
| `descendants` | Preorder traversal including the focus and every node/token occurrence. |
| `tokens`, `firstToken`, `lastToken` | Token occurrences within the focus, including empty tokens. Tokenless nodes are skipped. |
| `nextToken`, `previousToken` | Tokens after or before the entire focused subtree in the snapshot. |
| `replaceFocus replacement focus` | Rebuild ancestors to produce a new green root, retaining untouched subtrees. |

Failed `childAt` and `cursorAt` return `PathError`. Optional navigation returns
`Nothing` at a missing neighbor. Cursors expose no occurrence equality or pointer
identity; compare paths only when the tree snapshot is known to be the same.

### Source ranges and boundary lookup

`TextRange` is an abstract half-open UTF-8 byte interval. `textRange start end`
rejects reversed endpoints; `rangeFromLength start length` cannot overflow.
`rangeStart`, `rangeEnd`, `rangeLength`, and `rangeIsEmpty` inspect the interval.
Offsets need not lie at Unicode character boundaries.

`rangeContains offset range` excludes the end and contains no offsets for an
empty range. `rangeContainsRange outer inner` checks endpoint containment,
including an empty inner range at either endpoint.

`tokenAtOffset offset focus` uses absolute offsets and returns `NoToken`,
`SingleToken`, or `BetweenTokens` (ordered left to right). It selects nonempty
tokens touching the offset, so a shared boundary returns both neighbors and
the end of file (EOF) selects the last nonempty token. Empty tokens are ignored by this lookup
but remain navigable. Outside the focus, the result is `NoToken`.

`coveringElement range focus` repeatedly chooses the first covering child in
source order. It returns the innermost element along that branch, or `Nothing`
outside the focus. An empty query at a shared boundary favors the first covering
child, including empty nodes/tokens; it does not necessarily select the deepest
element among all possible branches.

## Build a tree with delayed wrapping

`GreenNodeBuilder kind` is a pure value for constructing a tree incrementally.
A `Checkpoint` marks a position in its pending elements. It lets a parser open
a node around elements that it already emitted.

Using `Kind` from the first example, this builds `1 + 2` inside an expression:

```haskell
buildExpression :: Either BuilderError (GreenNode Kind)
buildExpression = do
  let initial = startNode Root newBuilder
      marker = checkpoint initial
      first = builderToken Number "1" initial
  expression <- startNodeAt marker Expr first
  let rest = builderToken Number "2" (builderToken Trivia " + " expression)
  expressionDone <- finishNode rest
  rootDone <- finishNode expressionDone
  finishBuilder rootDone
```

`renderNode <$> buildExpression` returns `Right "1 + 2"`, with byte length 5.
The expression contains both the token before `startNodeAt` and the tokens after
it. Graft creates no punctuation or whitespace. The caller supplies every token.

| Operation | Contract |
| --- | --- |
| `newBuilder` | Start with no open nodes or pending elements. |
| `startNode kind builder` | Open a node at the current end. |
| `builderToken kind text builder` | Append exact token text. |
| `builderElement element builder` | Append an existing green node or token, reusing its value and cached length. |
| `finishNode builder` | Close the innermost node and replace its pending children with that node. |
| `checkpoint builder` | Mark the current end, counting pending elements rather than bytes. |
| `startNodeAt marker kind builder` | Open a node around the suffix after the marker, including later emissions until it closes. |
| `finishBuilder builder` | Return a single completed root node after all open nodes close. |

Keep an old builder value to continue construction along independent branches.
Adding an existing green element traverses none of its descendants. This lets
syntax-preserving transformations reuse untouched subtrees.

### Construction errors and checkpoint lifetime

`finishNode`, `startNodeAt`, and `finishBuilder` return `Either BuilderError`.
`finishNode` returns `NoOpenNode` if there is no node to close. `finishBuilder`
first reports `UnclosedNodes`, with the number of open nodes. Once balanced, it
reports `EmptyBuilder`, `MultipleRoots` with the pending count, or `TokenRoot`.
It never closes nodes implicitly. Failed operations leave the input value available.

`startNodeAt` accepts a marker at the current end, even for an initially empty
suffix. It rejects a position beyond that end with `CheckpointOutOfBounds`.
It rejects a position before the innermost open node with
`CheckpointBeforeCurrentNode`. Both errors carry the marker position followed by
the current count or the current node's starting position. With no open node,
it can wrap a suffix at the top level.

Checkpoints follow Rowan's positional semantics. They record neither builder
ownership nor history. Closing a node collapses its children into one pending
element. A marker inside those children can become out of bounds. Later emission
can make that numeric position valid again. A marker from another builder can
also pass the bounds tests. Use markers with their corresponding construction
history. A marker before an expression can be reused for repeated wrapping,
such as building a chain of left-associative operators.

## Architecture and remaining work

Graft retains Rowan's separation of immutable syntax from contextual navigation.
Its initial representation deliberately differs:

| Rowan | Graft today |
| --- | --- |
| Compact reference-counted allocations | Ordinary Haskell values managed by the garbage collector |
| Raw integer syntax tags | A caller-defined type parameter |
| Fixed-width byte lengths and cached child offsets | Unbounded byte lengths; cursors cache focus offsets while green children have no offset cache |
| Leading/trailing trivia APIs | Trivia supplied as ordinary token children |
| Separate node/token cursors with reference counting | One immutable element zipper; persistent replacement by focus or path |
| Mutable builder with positional checkpoints | Pure persistent builder with the same positional wrapping and structured errors |
| Builder cache for deduplication | Explicit value reuse; no interning |

The [architecture note](docs/architecture.typ) explains the decisions, costs,
upstream sources reviewed, and next steps. Build its PDF with Typst:

```sh
typst compile docs/architecture.typ docs/architecture.pdf
```

The next milestone is persistent child insertion, removal, and splicing.
Later work includes typed AST views that retain concrete syntax, and benchmarks
before selecting an interner or changing the representation. Attached trivia and compatibility with
Rowan's current API require further design. Semantic update policy remains a
layer above the concrete tree.

## License

[MIT](LICENSE). Rowan attribution and the preserved upstream permission notice
are in [NOTICE](NOTICE) and [licenses/ROWAN-MIT](licenses/ROWAN-MIT).

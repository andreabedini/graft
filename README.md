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
experimental. The fourth milestone adds child insertion, removal, and splicing
through green nodes, paths, and cursor focuses. The fifth milestone adds typed
node and token views that retain their original syntax cursors. The sixth
milestone establishes a benchmark baseline for runtime, allocation, and retained
memory. Interning remains future work.

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
Child-edit properties check exact source, caches, unchanged occurrences, shifted
paths and ranges, insertion/removal inverses, and validation order. Regressions
cover extreme indices and empty elements that shift paths without shifting text.
Typed-view properties check casting laws, retained occurrences, immediate fields,
parents and ancestors, and exact-preserving edits. Regressions cover malformed
syntax, repeated shared occurrences, lazy first-match queries, and large ranges.

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
`Graft.Builder`, and `Graft.Ast` separately. `GreenNode kind`
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

## Insert, remove, or splice children

A child splice replaces a range of immediate children with a list of elements.
`spliceChildren start end replacements node` uses a half-open range `[start,end)`.
The boundaries count both nodes and tokens, including empty elements. Both
boundaries can equal the child count. Equal boundaries insert, and an empty
replacement list deletes the selected children. The parent node keeps its kind.

Using `original` from the first example, append two children to its expression:

```haskell
extendExpression :: Either ChildEditError (GreenElement Kind)
extendExpression = spliceChildrenAt [2] 1 1
  [tok Trivia " + ", tok Number "2"] (Node original)
```

The result renders as `x  = 1 + 2 -- keep\r\n`. The existing spacing, comment,
and line ending survive. The original root still renders as `x  = 1 -- keep\r\n`.
New syntax and its separators come from the supplied token text.

| Operation | Contract |
| --- | --- |
| `spliceChildren start end replacements node` | Replace immediate children in `[start,end)` and return a new node. |
| `insertChild index element node` | Insert before a child. The child count appends, and zero inserts into an empty node. |
| `removeChild index node` | Remove an existing child. The child count is out of bounds. |
| `spliceChildrenAt path start end replacements root` | Splice children of the path-selected node and return a new root. `[]` selects the root. |
| `insertChildAt path index element root`, `removeChildAt path index root` | Insert or remove a child of the path-selected node. |
| `spliceChildrenFocus start end replacements focus` | Splice children of a node focus and return a new green root. |
| `insertChildFocus index element focus`, `removeChildFocus index focus` | Insert or remove a child of the focused node. |

The indices are relative to the selected node, even when it lies inside a larger
tree. Removing all children leaves an empty node. Removing a token retains its
sibling trivia. Include that trivia in the splice range when it must also go.
Graft performs no grammar validation or formatting repair.

All child edits return `Either ChildEditError`. Path variants first report
`ChildEditPathError`, which contains the original `PathError`. Selecting a token
then reports `CannotEditTokenChildren`, before checking any child boundaries.
Boundary validation checks negative start, negative end, reversed range, start
bounds, and end bounds in that order. The errors are `NegativeChildBoundary`,
`ReversedChildRange`, and `ChildBoundaryOutOfBounds`. They carry the offending
position, or both positions for a reversed range. Removal checks an existing
child index without adding one, so `maxBound :: Int` cannot overflow.

Edits reuse untouched green elements and the following child-list suffix.
Cached lengths are recomputed from immediate children, without reading their
descendant text. Path and cursor variants rebuild ancestors. The original root
and cursor retain their original snapshot. Start a new cursor on the returned
root to observe the edited tree.

Adding or removing children can shift paths as well as source ranges. Inserting
an empty node shifts later child indices even though no byte offsets change.
Paths from the old snapshot must be reconsidered after an edit. These operations
apply one edit to one snapshot, without tracking occurrences across versions.

## View concrete syntax through typed fields

An abstract syntax tree (AST) view wraps a syntax cursor. It retains the
original token text, trivia, path, and byte range. Enable `TypeFamilies` in your
application module to define the associated syntax-kind types.

Using `Kind` from the first example:

```haskell
newtype Expression = Expression (Cursor Kind)
instance AstNode Expression where
  type AstNodeKind Expression = Kind
  castNode = matchNode (== Expr) Expression
  nodeSyntax (Expression cursor) = cursor

newtype NumberLiteral = NumberLiteral (Cursor Kind)
instance AstToken NumberLiteral where
  type AstTokenKind NumberLiteral = Kind
  castToken = matchToken (== Number) NumberLiteral
  tokenSyntax (NumberLiteral cursor) = cursor

expressionNumber :: Expression -> Maybe NumberLiteral
expressionNumber = astToken . nodeSyntax

setNumberText :: Text -> NumberLiteral -> GreenElement Kind
setNumberText text literal =
  replaceFocus (Token (greenToken Number text)) (tokenSyntax literal)
```

`astChild (newCursor (Node original)) :: Maybe Expression` selects the expression.
`expressionNumber` selects its first immediate number token. `setNumberText`
returns a new green root, preserving surrounding tokens and the old view.
The application decides the replacement spelling and validates its semantic value.

| Operation | Contract |
| --- | --- |
| `castNode`, `castToken` | Try to obtain an application view. Failure returns `Nothing`. |
| `nodeSyntax`, `tokenSyntax` | Return the view's original cursor, with all concrete syntax and context. |
| `matchNode accepts wrap`, `matchToken accepts wrap` | Check node/token form and the kind predicate before wrapping the original cursor. |
| `astChild`, `astChildren` | First or all matching immediate node children in source order. Tokens are skipped. |
| `astToken`, `astTokens` | First or all matching immediate token children, including empty tokens. Nodes are skipped. |
| `astParent` | Cast only the immediate parent. A mismatched parent returns `Nothing`. |
| `astAncestors` | All matching ancestors, nearest first, excluding the focus. |

Field helpers do not search grandchildren. A token focus has no child fields.
Missing or mismatched fields return `Nothing` or an empty list. List results are
lazy, and first-match queries stop at the first successful cast. Duplicate fields
remain in source order. Grammar-specific accessors decide whether duplicates are errors.

Instances must reject the opposite element form and retain the supplied cursor
unchanged after a successful cast. Casting a view's own syntax must succeed again.
The type class cannot enforce these laws. Hide your wrapper constructors and use
`matchNode` or `matchToken` for the usual form-and-kind check. That check accepts
incomplete nodes or malformed token spelling. Interpret values and validate fields
in application code, with explicit failure for missing or invalid data.

A view retains its snapshot and ancestors, so it also keeps the original root
reachable. Start a new cursor and cast again after editing. Views expose no
occurrence equality or handle that tracks an element across tree versions.

The complete [exact-update example](examples/ExactUpdate.hs) interprets a number
as an integer. An unchanged value retains the spelling `001`. A changed value
uses a new number token while preserving the surrounding spacing, comment, and
CRLF line ending. Missing fields and invalid numeric text return `Nothing`.
The application supplies the tree and its numeric interpretation policy.

```sh
cabal build all
cabal exec -- runghc -package=graft examples/ExactUpdate.hs
```

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
| Generic range bounds for green child splicing | Validated half-open child indices with node/path/focus entry points |
| `AstNode` trait, language conversion, and `AstChildren` iterator | `AstNode`/`AstToken` classes with associated kind types and lazy lists |

The [architecture note](docs/architecture.typ) explains the decisions, costs,
upstream sources reviewed, and next steps. Build its PDF with Typst:

```sh
typst compile docs/architecture.typ docs/architecture.pdf
```

The [benchmark report](docs/performance.md) records two passes over 89 cases and
isolated measurements of retained trees, snapshots, and cursors. The
[run guide](bench/README.md) gives reproduction and comparison commands.
Wide child lookup and ancestor reconstruction are candidates for measured
representation experiments. Location handles, attached trivia, and compatibility
with Rowan's current API require further design. Semantic update policy remains
a layer above the concrete tree.

## License

[MIT](LICENSE). Rowan attribution and the preserved upstream permission notice
are in [NOTICE](NOTICE) and [licenses/ROWAN-MIT](licenses/ROWAN-MIT).

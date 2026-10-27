# Graft architecture

This note explains how Graft is put together and why, and where it departs from
Rowan. The Haddock documentation is the reference for individual functions.

## Scope

Graft is the tree layer of a concrete-syntax toolchain. A concrete syntax tree
keeps the source's exact spelling, including whitespace and comments (trivia),
where an abstract syntax tree keeps only its meaning. Graft stores such trees,
navigates them, edits them persistently and prints them back. Lexing, parsing,
grammar validation and formatting policy belong to the application.

## Modules and names

Each module covers one concept, and its functions have short names meant for
qualified import: `Cursor.parent`, `Green.node`, `Range.start`, `Build.token`.
The same short name can appear in several modules, such as `children` in
`Graft.Green`, `Graft.Cursor` and `Graft.Ast`, each acting on its own module's
type. The `Graft` module exports only the types and classes, so that they can
be used unqualified alongside the qualified functions.

Edits inside a tree go through `Graft.Edit`: `Graft.Green` edits the children
of a single node, a plan applies a list of edits described through cursors to
the tree they were made on, and a rewrite visits a tree top-down and replaces
elements as it goes. Cursors themselves only read.

## Green trees

A green token is a leaf with a syntax kind and exact `Text`. A green node has a
kind and an ordered list of children, each a node or a token. The kind is a
type parameter, so an application uses its own enumeration with no conversion
to integers. Empty nodes and tokens are valid, and the same subtree may appear
any number of times.

Green values have no parents and no absolute positions, so a subtree can be
shared between trees and between versions of one tree. Their constructors are
hidden, and the smart constructors `Green.node` and `Green.token` establish the
cached lengths. The representation lives in `Graft.Green.Internal`, which the
package does not expose.

All lengths and offsets are `TextLength`, a newtype over `Int` counting UTF-8
bytes. `Text` is stored as UTF-8, so a token's byte length is read in constant
time. The newtype keeps byte offsets apart from character counts and from the
UTF-16 positions that language clients send; `Graft.LineIndex` converts between
them.

Rendering writes every token's text in source order into one buffer sized from
the cached length. Graft never inserts separators or normalizes text: every
byte of output came from some token. Valid Unicode round-trips exactly; invalid
UTF-8 is outside the contract.

`Green.foldM` is a strict left fold over a tree's elements in preorder, in any
monad. It loops over each node's child array by index, builds no list, and
folds a node's last child by a tail call, so a chain of single-child nodes
runs in constant stack. Rendering, `rnf` and hashing are written with it. It
has no event for leaving a node, because that would lose the tail call: an
earlier version with enter and leave events rendered the 10,000-level chain
2.4 times slower. `Cursor.walk` reports leaving, at the cost of a cursor per
element. `Green.fold` is the lazy bottom-up fold, for results shaped like the
tree; `Green.map` and `Green.traverse` rebuild a tree from new tokens and
measure every node again.

Trivia is ordinary tokens. Rowan attaches leading and trailing trivia to tokens;
Graft doesn't, so that there is one kind of leaf and no rule about which token
owns a comment. Applications that want ownership can express it with node
structure.

## Representation and costs

A node stores its kind, its total length, its children in a `SmallArray`, and
the start of each child relative to the node in a `PrimArray Int`. This is the
same information Rowan keeps: the child offsets make child access constant time
and let offset lookup binary-search each level instead of scanning. A token's
kind, length and text are stored directly in the element that holds it, which
saves a box per token. Nodes keep their own box, so that a cursor can point at
a parent node without re-allocating it.

| Operation | Cost |
| --- | --- |
| Length of a node or token | Constant |
| Child by index, first or last child, sibling, parent | Constant |
| Path or root of a cursor | Linear in the depth |
| Token or element at an offset | Logarithmic in the width of each level, times depth, plus any empty children at the offset |
| Building a node | Linear in its number of children |
| Replacing a child of one node | Linear in the parent's width: the array is copied, and the length is adjusted by the difference |
| Splicing children of one node | Linear in the parent's width plus the replacements |
| `Edit.apply` | Linear in the edits, their paths, and the widths of the nodes above them; the trees are compared when a cursor isn't from the base itself |
| `Edit.rewrite` | Linear in the elements visited, which covers rebuilding the nodes above the replacements |
| `Edit.textEdits` | As `Edit.apply`, plus rendering the replacements |
| Rendering | Linear in the elements and the output |

No edit traverses descendants it doesn't change. Equality is structural
but returns immediately for physically identical nodes, so comparing two
versions that share most of their structure is cheap when the shared parts are
the same values.

Graft has no node cache. Rowan's builder interns identical nodes and tokens to
save memory; Graft only shares what the application shares, for example by
emitting existing green elements into a builder. Interning remains an open
question that needs measurements from real parsers.

## Cursors

A cursor is a zipper: the focused element, its absolute offset, its index in
the parent, and the parent's cursor, and so on up to the root. Moving down, up
or sideways is constant time, and the cursor computes absolute ranges from the
cached child offsets. Navigation never performs transformation work: a
cursor's fields are values, and there is no edit state that an observer could
force. A cursor retains its parent chain and, through it, the whole tree, and
nothing else. Lists such as `children` and `descendants` are lazy, cost a
cursor per element consumed, and buy early exit; one step of `descendants`
can cost up to the depth, returning from a finished branch.

Cursors are read-only. An earlier design let a cursor record edits as it
moved and build the edited nodes when they were read. The cost of an
observation then depended on which thunk it forced, the documentation had to
list which observers built an edited node, and three successive changes each
found an observer that was accidentally quadratic. Edits now live in
`Graft.Edit`, and `Cursor.tree` is the whole tree the cursor is in.

Rowan has separate node and token cursors, and offers mutable red trees
(`clone_for_update`) so that edits are visible through every handle. Graft has
one cursor type for both and no mutation. Rowan's `ancestors` starts with the
node itself; Graft's `Cursor.ancestors` and `Ast.ancestors` start with the
parent, while `Cursor.descendants` starts with the focus.

## Edits

`Graft.Edit` makes changes that are described against a tree as it is and
made all at once. It has two entry points, for two workloads.

A plan is a list of edits, each made with a cursor on the base tree:
`Edit.replace` an element, `Edit.splice` a range of a node's children, or
`Edit.insertBefore`, `Edit.insertAfter` and `Edit.remove` a child among its
siblings. `Edit.apply` makes them together. Every cursor and child position
refers to the base, so an edit never shifts another: replacing children 2 and
7 replaces those two original children, whatever happens to the lengths or the
children between them. At each node with an edit, the new children are, for
each boundary from the first to the last, the insertions at that boundary in
the order of the list, then the child, which is the elements of a splice that
starts there, or nothing if an earlier splice consumed it, or its replacement,
or the child itself, rebuilt if an edit lies below it and shared otherwise.
Apart from the order of insertions at one boundary, the result does not depend
on the order of the list.

A plan is rejected when a replacement and any other edit apply at or below the
same element; when a splice consumes a child that another edit applies to or
below, which an insertion strictly inside the splice's range does and one at
either end does not; or when two splices of one node have ranges `[a, b)` and
`[c, d)` with `a < d` and `c < b`. Each error names the edits by their
positions in the list. Every edit's cursor must be in a tree equal to the
base. That is compatibility, not provenance: `apply` cannot tell where a
cursor came from, but equality of the whole tree guarantees that its path,
range and context mean the same thing in the base, which equality of the
target alone would not. For a cursor made from the base the comparison is a
pointer test; a cursor from an equal tree built separately is accepted after
a structural comparison, which can cost the size of the tree, and that is why
`apply` needs `Eq` on the kind. Evaluating the result to `Right` completes the
validation and every rebuilt node; replacement elements and the shared parts
of the base are as evaluated as they were given.

`Edit.textEdits` gives the same plan as changes to the source text, with
UTF-8 byte ranges in the base's text, for `Graft.LineIndex` to convert for an
editor. It runs the same merge as `apply`, so insertions at boundaries that
share an offset, as the boundaries of an empty element do, come out in the
order of the result, and it merges edits that touch, so that no two share an
offset. Patching the original text with them renders the result of `apply`.

A rewrite, `Edit.rewrite` or `Edit.rewriteM`, visits a tree top-down and
calls a function on each element with a cursor on the original tree and a
context passed down from the ancestors. The function answers `Replace` with a
new element, below which nothing is visited; `Descend`, which keeps the
element's form and visits its children; `DescendWith` a new context; or
`Skip`, which keeps the subtree unvisited. A node is rebuilt, once, exactly
when a child reports a replacement, by a flag carried up and never by
comparing trees, so `Replace` with the original element counts; a node with
no replacement below it is returned as it is. `rewriteM` needs a monad rather
than an applicative because the answer decides what is visited next; in
`Either` it stops at the first failure, and in `StateT` it can collect the
ranges it replaced, which are ranges in the original. It forces each rebuilt
node before returning it through the monad. A rewrite suits a traversal that
may change many elements, such as a rename that reads each token's ancestors;
a plan suits a sparse set of targets found by analysis. The two share only
the array copying underneath.

Neither lets a later decision observe an earlier change inside one operation.
When a second change depends on the first, apply the first and work on the
result; the intermediate tree is paid for because it was asked for. The
rebuilt nodes are the only new allocation that outlives an operation: a
plan's trie, the paths, and the cursors a rewrite visits are garbage when it
returns. There is no occurrence identity across versions: Graft can't tell you
where an element of the old tree went in the new one.

## Lookup by offset

`TextRange` is a half-open interval of byte offsets with validated endpoints.
Offsets need not lie on character boundaries.

`Cursor.tokenAt` treats token ranges as closed when deciding what an offset
touches. An offset inside a token returns that token. An offset on the boundary
between two tokens returns both, left first, as an editor needs at a cursor
position between two words. The start and end of the text return the first and
last token. Empty tokens are skipped by this lookup, though navigation still
visits them.

`Cursor.covering` returns the innermost element whose range contains a query
range, descending into the first covering child at each level. An empty query
on a boundary therefore picks the leftmost candidate, including empty
elements. These rules are Graft's own and may differ from Rowan's tie-breaking.

## Builders

A builder holds the elements emitted so far, in reverse order, and a stack of
open nodes with the position where each started. Closing a node collects the
elements emitted since its start into a node. A checkpoint is a position in
that sequence, counted in elements; `Builder.startNodeAt` opens a node at an earlier
checkpoint, so a parser can wrap an operand after seeing the operator that
follows it.

Checkpoints are positions, not handles, exactly as in Rowan: a checkpoint
taken inside a node that has since closed may be out of range, or may become
valid again after more elements are emitted. The builder checks only that the
position lies within the current node.

Rowan's builder asserts on misuse. Graft's pure builder returns
`Either BuilderError` from every operation that can fail, and `Builder.finish`
checks that every node was closed and that exactly one root node remains. The
`Build` monad in `Graft.Build` threads the builder and the errors, and its
`Build.node` and `Build.nodeAt` bracket a node, so a parser that opens and
closes nodes only through them can't leave one open. `Build.save` and
`Build.restore` give backtracking parsers the persistence of the pure builder;
restoring inside a bracketed body can unbalance it, so restore at the level
where you saved.

## Typed views

`AstView` is the class for application-defined views. A view is usually a
newtype over a cursor. `fromCursor` casts a cursor to the view and can fail;
`toCursor` gives the cursor back. The class has the same shape as `Exception`,
with `Cursor` in the place of `SomeException`. It has an associated kind type,
which replaces Rowan's language parameter. Views add typed accessors without a
second tree, so every view keeps its exact text, trivia, range and context.

The empty subclasses `AstNode` and `AstToken` say whether a view is of nodes or
of tokens. `Ast.child`, `Ast.children`, `Ast.parent` and `Ast.ancestors` need
an `AstNode` view; `Ast.token` and `Ast.tokens` need an `AstToken` view. Each
view therefore has two instances: the `AstView` instance with the cast, and an
empty `AstNode` or `AstToken` instance.

The set of views is open. Graft can't know an application's kinds, and an
application can define more than one view for the same kind, or add views in a
package other than the one that defines the kind type. A type class allows
both. Within one application the kind type is an ordinary closed type, so a
cast that matches on the kind gets the usual exhaustiveness checks.

A cast checks the element's form and kind, not the grammar. A view of an
expression with a missing operand still exists, and its accessor for that
operand returns `Nothing`. Required fields, duplicates and token spelling are
the application's to check. The classes have laws (a cast looks only at the
element, not its position; it keeps the cursor; casting a view's cursor
succeeds again; a node view rejects tokens and a token view rejects nodes) that
the type system can't enforce; `Ast.matchNode` and `Ast.matchToken` implement
the usual `fromCursor`.

`examples/ExactUpdate.hs` uses views for a semantic update: it reads a number's
value, keeps the original token when the value is unchanged, so `001` stays
`001`, and otherwise replaces only that token.

## Line and column positions

`Graft.LineIndex` records where each line starts in a text and converts byte
offsets to and from zero-based lines and columns. Columns can be counted in
UTF-8 bytes or in UTF-16 code units, the Language Server Protocol's default,
and each unit has its own position type, so a column in one can't be passed
where the other is expected.
Lines end at `\n`, `\r\n` or a lone `\r`, as in that protocol, and columns past
the end of a line clamp to it. An offset or column inside a character, in
either unit, is rejected rather than rounded, so a converted offset is always a
character boundary. An index describes one text and must be rebuilt
after an edit.

## Testing

The tests compare the library with independent models that share none of its
code: a plain rose tree for the green layer and the cursors, a merge written
over lists and a predicate of the conflict rules for the plans, and a
generated decision table for the rewrites. A doctest suite checks the examples
in the Haddock documentation;
[CONTRIBUTING.md](../CONTRIBUTING.md) describes both. The benchmark suite
and its latest results are in [performance.md](performance.md).

## Relation to Rowan

Graft started from a study of Rowan at revision
`b6f1cb6494a99a2264186d110b1b170989774814` (version 0.17.0), retrieved on
10 October 2026. The sources studied were [green nodes][node] and
[tokens][token], [cursors][cursor], [AST support][ast], the [builder][builder]
and [node cache][node-cache], and Rowan's [green][green], [text][syntax-text]
and [formatting][tidy] tests.

| Rowan | Graft |
| --- | --- |
| Reference-counted thin allocations | Ordinary values managed by the garbage collector |
| `u16` syntax kinds and a language trait | A type parameter for the application's own kind type |
| `TextSize`, a `u32` byte count | `TextLength`, a newtype over `Int` |
| Children with cached relative offsets | The same, in a `SmallArray` and a `PrimArray` |
| Leading and trailing trivia on tokens | Trivia as ordinary tokens |
| Separate node and token cursors, optionally mutable | One read-only cursor type; edits are plans and rewrites applied to a tree |
| Mutable builder that asserts on misuse | Pure builder with structured errors, and a `Build` monad |
| Node cache for interning | No interning |
| `AstNode` and `AstToken` traits, `AstChildren` iterator, `SyntaxNodePtr` | An `AstView` class with `AstNode` and `AstToken` subclasses, and lazy lists; no location pointers |

## Open questions

- Interning: whether a node cache pays for itself depends on duplicate rates in
  real parser output. The first user, haskott (a Haskell reimplementation of
  Ott), will provide the measurements.
- Location handles: an equivalent of Rowan's `SyntaxNodePtr`, which finds an
  element again in a new version of the tree, needs rules for empty and nested
  elements that share a range. Relocating a cursor from the base into the
  result of `Edit.apply` is the same question.
- Editing sessions: an operation in which a later decision sees an earlier
  edit. Not planned until a workload needs it, and then with a cost analysis
  fitted to its representation, which is the test of whether it should exist.
- Trivia ownership, and how close to Rowan's API Graft should stay.
- Incremental reparsing: Graft can splice a reparsed subtree into a tree, but
  has no support for deciding which subtree to reparse after a text edit. This
  is a likely follow-up.

[node]: https://github.com/rust-analyzer/rowan/blob/b6f1cb6494a99a2264186d110b1b170989774814/src/green/node.rs
[token]: https://github.com/rust-analyzer/rowan/blob/b6f1cb6494a99a2264186d110b1b170989774814/src/green/token.rs
[cursor]: https://github.com/rust-analyzer/rowan/blob/b6f1cb6494a99a2264186d110b1b170989774814/src/cursor.rs
[ast]: https://github.com/rust-analyzer/rowan/blob/b6f1cb6494a99a2264186d110b1b170989774814/src/ast.rs
[builder]: https://github.com/rust-analyzer/rowan/blob/b6f1cb6494a99a2264186d110b1b170989774814/src/green/builder.rs
[node-cache]: https://github.com/rust-analyzer/rowan/blob/b6f1cb6494a99a2264186d110b1b170989774814/src/green/node_cache.rs
[green]: https://github.com/rust-analyzer/rowan/blob/b6f1cb6494a99a2264186d110b1b170989774814/src/green.rs
[syntax-text]: https://github.com/rust-analyzer/rowan/blob/b6f1cb6494a99a2264186d110b1b170989774814/src/syntax_text.rs
[tidy]: https://github.com/rust-analyzer/rowan/blob/b6f1cb6494a99a2264186d110b1b170989774814/tests/tidy.rs

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

Edits inside a tree go through a cursor. `Graft.Green` edits only the
children of a single node, and `Graft.Cursor` rebuilds the ancestors; there is
no separate set of edits addressed by path.

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
| Child by index, first or last child, sibling | Constant |
| Token or element at an offset | Logarithmic in the width of each level, times depth |
| Building a node | Linear in its number of children |
| Replacing a child | Linear in the parent's width: the array is copied, and the length is adjusted by the difference |
| Splicing children | Linear in the parent's width plus the replacements |
| Rendering | Linear in the output |

No operation traverses descendants it doesn't change. Equality is structural
but returns immediately for physically identical nodes, so comparing two
versions that share most of their structure is cheap when the shared parts are
the same values.

Graft has no node cache. Rowan's builder interns identical nodes and tokens to
save memory; Graft only shares what the application shares, for example by
emitting existing green elements into a builder. Interning remains an open
question that needs measurements from real parsers.

## Cursors

A cursor is a zipper. It holds the focused element, its absolute offset, and a
chain of contexts: for each ancestor, the parent node, the parent's offset, and
the focus's index in it. Moving down, up or sideways is constant time, and the
cursor computes absolute ranges from the cached child offsets.

A cursor also records whether its focus has been replaced. `Cursor.replace`
doesn't rebuild anything; it returns a cursor with the new focus and that flag
set. The parent is rebuilt when the cursor next moves up or sideways, and the
rebuilt parent is marked as replaced in turn. So a run of edits in one region,
such as renaming every token in a list, rebuilds each ancestor once rather than
once per edit, and the cursor stays usable for navigation throughout.
`Cursor.tree` climbs to the root and returns the edited tree.

Edits are persistent. The cursor you started with still sees the original tree,
and so does every cursor derived from it before the edit. Paths and byte ranges
in the edited tree can differ from those in the original; a cursor derived from
the edited cursor sees the new ones. There is no occurrence identity across
versions: Graft can't tell you where an element of the old tree went in the new
one.

Rowan has separate node and token cursors, and offers mutable red trees
(`clone_for_update`) so that edits are visible through every handle. Graft has one cursor type for both and
no mutation; returning a cursor from each edit gives the same ability to keep
working after an edit, without shared mutable state.

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
`Build.node` and `Build.nodeAt` bracket a node so that it can't be left open.
`Build.save` and `Build.restore` give backtracking parsers the persistence of
the pure builder.

## Typed views

`AstNode` and `AstToken` are classes for application-defined views. A view is
usually a newtype over a cursor with a fallible cast; it adds typed accessors
without building a second tree, so every view keeps its exact text, trivia,
range and context. Each class has an associated kind type, replacing Rowan's
language parameter.

A cast checks the element's form and kind, not the grammar. A view of an
expression with a missing operand still exists, and its accessor for that
operand returns `Nothing`. Required fields, duplicates and token spelling are
the application's to check. The classes have laws (a cast keeps the cursor,
and casting a view's syntax succeeds again) that the type system can't
enforce; `Ast.matchNode` and `Ast.matchToken` implement the usual instance.

`examples/ExactUpdate.hs` uses views for a semantic update: it reads a number's
value, keeps the original token when the value is unchanged, so `001` stays
`001`, and otherwise replaces only that token.

## Line and column positions

`Graft.LineIndex` records where each line starts in a text and converts byte
offsets to and from zero-based lines and columns. Columns can be counted in
UTF-8 bytes or in UTF-16 code units, the Language Server Protocol's default.
Lines end at `\n`, `\r\n` or a lone `\r`, as in that protocol, and columns past
the end of a line clamp to it. An index describes one text and must be rebuilt
after an edit.

## Testing

The tests compare the library with independent models that share none of its
code, and a doctest suite checks the examples in the Haddock documentation;
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
| Separate node and token cursors, optionally mutable | One immutable cursor type; edits return a new cursor |
| Mutable builder that asserts on misuse | Pure builder with structured errors, and a `Build` monad |
| Node cache for interning | No interning |
| `AstNode` trait, `AstChildren` iterator, `SyntaxNodePtr` | `AstNode` and `AstToken` classes and lazy lists; no location pointers |

## Open questions

- Interning: whether a node cache pays for itself depends on duplicate rates in
  real parser output. The first user, haskott (a Haskell reimplementation of
  Ott), will provide the measurements.
- Location handles: an equivalent of Rowan's `SyntaxNodePtr`, which finds an
  element again in a new version of the tree, needs rules for empty and nested
  elements that share a range.
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

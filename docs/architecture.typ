#set document(title: "Graft: green tree architecture", author: "Andrea Bedini")
#set page(paper: "a4", numbering: "1")
#set text(font: "Libertinus Serif", size: 11pt)
#set heading(numbering: "1.")

= Graft: green tree architecture

Graft provides concrete syntax infrastructure for bidirectional parsing,
printing, and semantic updates that preserve untouched text. A concrete syntax
tree retains the source spelling and trivia (whitespace and comments), rather
than just the semantic structure of an abstract syntax tree (AST). Parsers and
lexers are supplied by applications.

== The immutable core

A green token is a leaf with a syntax kind, exact Unicode text, and a cached
UTF-8 byte length. A green node has a syntax kind, ordered child elements, and
their cached total length. A green element is either a node or a token. The
type parameter `kind` is the application's own syntax-kind type. Empty nodes
and tokens are valid; repeated references to the same subtree are valid too.

Green values contain no parents or absolute positions. `GreenNode` and
`GreenToken` have hidden constructors; smart constructors establish the length
invariants. `TextLength` is an alias for `Natural`, a non-negative unbounded
integer. Reading a cache never walks the children or token text. Construction
requires finite, acyclic input, as ordinary Haskell recursion can otherwise
define non-terminating values.

Rendering concatenates leaves in source order through a text builder. All
source fragments must appear in token text: Graft does not synthesize spaces,
discard comments, or normalize Unicode or line endings. Valid Unicode text
round-trips exactly; arbitrary invalid UTF-8 bytes are outside the contract.

== Why this representation differs from Rowan

Rowan uses compact reference-counted allocations, integer syntax tags,
fixed-width lengths, and child offsets. Graft starts with ordinary algebraic
data types and child lists, relying on Haskell's garbage collector and value
reuse. Parameterized kinds allow application enums without a conversion layer.
Unbounded byte counts prevent overflow even when a small shared graph has an
enormous logical text size. This does not mean that enormous text can be rendered:
the result must still fit in a strict `Text`.

Whitespace and comments are ordinary child tokens initially. Rowan's current
leading/trailing trivia representation is not ported yet. This choice preserves
exact text with one leaf model, while leaving trivia ownership and compatibility
as explicit future decisions. Graft supplies structural equality, not a public
pointer-identity operation. Independently constructed equal trees are not
automatically deduplicated.

== Persistent updates and their costs

A `Path` is a list of zero-based child indices, counting nodes and tokens. The
empty path selects the root. `elementAt` looks up the selected element;
`replaceAt` replaces it and returns a new root. Invalid steps return a
`PathError` without a partial edit. Replacement may change kind or element form;
grammar validation belongs to the application. Paths inside a replaced subtree
must be reconsidered against the new tree version.

Replacement rebuilds ancestor nodes and child-list prefixes along the path.
Sibling elements, descendant subtrees outside the edit, and list suffixes are
reused. Ancestor lengths are recomputed by summing immediate child caches, so no
untouched descendant text is traversed. The original root remains available.

Node construction costs a traversal of its immediate children. Token construction
currently allocates a temporary UTF-8 encoding to count bytes. Path lookup scans
list prefixes. Replacement cost follows the sum of ancestor child counts.
Rendering traverses every token occurrence and allocates the output; sharing
cannot eliminate repeated source occurrences. Deep recursion may exhaust the
stack. No performance or memory advantage over Rowan is claimed without
benchmarks.

== Evidence and upstream study

The first milestone tests rendering and byte counts against an independent model
tree, verifies every cache, checks the exact changed source span, and checks
unaffected branches. Generated text includes Unicode scalar values, combining
characters, CRLF, and NUL. Coverage checks exercise nested edits and node/token
targets and replacements. Regressions cover empty values and a shared graph
whose logical length exceeds a machine word.

The implementation study used Rowan revision
`b6f1cb6494a99a2264186d110b1b170989774814`, retrieved on 10 October 2026;
its manifest identifies version 0.17.0. The inspected sources were:

#let upstream = "https://github.com/rust-analyzer/rowan/blob/b6f1cb6494a99a2264186d110b1b170989774814/"
- #link(upstream + "src/green/node.rs")[Green nodes] and
  #link(upstream + "src/green/token.rs")[tokens]: lengths, child storage,
  rendering, and trivia.
- #link(upstream + "src/cursor.rs")[Cursors]: contextual navigation and
  replacement through ancestors.
- #link(upstream + "src/green/builder.rs")[Builders] and
  #link(upstream + "src/green/node_cache.rs")[cache]: checkpoints and interning.
- #link(upstream + "src/green.rs")[Green tests],
  #link(upstream + "src/syntax_text.rs")[text tests], and
  #link(upstream + "tests/tidy.rs")[formatting test]. Green tests concern Rust
  thread traits and sizes; text tests exercise empty and differently chunked
  strings. The README also points to rust-analyzer integration tests.

== Remaining work

- Red zippers: contextual parent/sibling navigation, traversals, UTF-8 source
  ranges, and well-defined behavior at empty elements and token boundaries.
- Rowan-style builders and checkpoints with validation of balanced construction.
- Persistent child insertion, removal, and splicing.
- Typed AST views that keep access to the concrete tokens and trivia.
- Explicit trivia ownership and the desired Rowan compatibility boundary.
- Benchmarks of wide/deep trees, repeated edits, rendering, and memory residency
  before choosing interning, vectors, additional offset caches, or other layouts.

Bidirectional semantic updates will need a separate correspondence between AST
views and concrete syntax. The green core supplies exact text and persistent
edits; it cannot decide where new comments belong, which spelling to prefer, or
how to reconcile a changed semantic value with retained syntax.

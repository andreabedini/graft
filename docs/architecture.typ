#set document(title: "Graft: concrete syntax architecture", author: "Andrea Bedini")
#set page(paper: "a4", numbering: "1")
#set text(font: "Libertinus Serif", size: 11pt)
#set heading(numbering: "1.")

= Graft: concrete syntax architecture

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

== Red cursors and source ranges

The second milestone adds an immutable red cursor: a zipper focusing a node or
token occurrence with its context. `Cursor kind` contains a green focus, its
absolute byte offset, and parent breadcrumbs. Each breadcrumb retains the
original parent node and offset, its context, the child's index, preceding
siblings in reverse order, and following siblings. This is an ordinary algebraic
data type with no unsafe code, mutable references, or separate allocation layer.

`newCursor` starts at offset zero. Child navigation computes offsets from cached
sibling lengths. Parent and adjacent sibling moves restore context in constant
time; child indexing and last-child navigation scan lists. `cursorPath` and
`cursorRoot` walk the ancestor chain. Traversals are lazy and visit shared green
values once per occurrence. Token navigation includes empty tokens and skips
tokenless nodes; moving to the next or previous token from a node crosses the
entire focused subtree.

A cursor belongs to its original snapshot. `replaceFocus` rebuilds ancestors
and returns a new green root, preserving the old cursor and snapshot. Navigating
the result requires a new cursor; later offsets may have changed. A live cursor
retains its original ancestors and therefore the original green root. Cursors
do not expose pointer identity or occurrence equality. Paths distinguish repeated
green values only within a known snapshot.

`TextRange` is an abstract half-open UTF-8 byte interval with validated endpoints.
`textRange` takes start and end, rejecting a reversed interval;
`rangeFromLength` takes start and length. Byte offsets may lie inside multibyte
characters. Ordinary point containment excludes the end, while range containment
accepts an empty query at either endpoint.

Offset lookup uses closed endpoint contact: `tokenAtOffset` returns a single
nonempty token in its interior, both neighbors at a shared boundary, or no token
outside the focus. Empty tokens remain navigable but do not participate in offset
lookup. The end of file (EOF) selects the last nonempty token. Cached ranges prune unrelated
subtrees, so lookup need not render or enumerate the whole tree.

`coveringElement` repeatedly descends into the first child whose range contains
the query. At ambiguous empty ranges, this deterministic choice favors the first
covering child in source order, including empty elements. It returns the
innermost element on that branch, not a globally deepest candidate. Out-of-focus
queries return no element. These boundary rules are part of Graft's contract;
they do not promise Rowan's exact tie-breaking behavior.

== Evidence and upstream study

The first milestone tests rendering and byte counts against an independent model
tree, verifies every cache, checks the exact changed source span, and checks
unaffected branches. Generated text includes Unicode scalar values, combining
characters, CRLF, and NUL. Coverage checks exercise nested edits and node/token
targets and replacements. Regressions cover empty values and a shared graph
whose logical length exceeds a machine word.

Cursor properties compare every occurrence and range with the independent model,
check parent/sibling round trips and token ordering, and verify unaffected ranges
after replacements grow or shrink. Scoped offset lookup and covering queries are
checked against model positions. Explicit cases cover tokenless nodes, empty
tokens, multibyte interiors, shared-boundary ambiguity, EOF, repeated shared
subtrees at distinct offsets, and lookup in a shared graph beyond machine-word
length without rendering it.

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

- The next milestone: Rowan-style builders and checkpoints with validation of
  balanced construction.
- Persistent child insertion, removal, and splicing.
- Typed AST views that keep access to the concrete tokens and trivia.
- Explicit trivia ownership and the desired Rowan compatibility boundary.
- Benchmarks of wide/deep trees, repeated edits, rendering, and memory residency
  before choosing interning, vectors, additional offset caches, or other layouts.

Bidirectional semantic updates will need a separate correspondence between AST
views and concrete syntax. The green core supplies exact text and persistent
edits; it cannot decide where new comments belong, which spelling to prefer, or
how to reconcile a changed semantic value with retained syntax.

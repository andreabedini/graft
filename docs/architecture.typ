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

== Child insertion, removal, and splicing

The fourth milestone adds `spliceChildren`, which replaces a half-open range
of immediate children with a list of green elements. Its integer boundaries
count both nodes and tokens, including empty elements. They range from zero
through the child count, inclusive. Equal boundaries insert. An empty
replacement deletes. `insertChild` inserts one element, allowing append at the
child count. `removeChild` requires an existing child and avoids incrementing
the supplied index, so extreme indices cannot overflow.

The original parent kind survives, even when all its children are removed.
Untouched elements and the following list suffix are reused. Construction scans
immediate child caches to establish the new total length. Descendant text needs
no traversal. Callers supply all new punctuation and whitespace. Trivia outside
the selected child range remains untouched. No grammar or formatting repair occurs.

Path variants select a node in a green root, edit its children, and rebuild its
ancestors. Cursor variants edit the focused node and rebuild through its context.
Both return a new green root and retain the original snapshot. A token cannot be
a child-edit target. An empty path selects the root node, and removing all
children retains that root as an empty node.

`ChildEditError` separates path errors, token targets, and invalid boundaries.
Path validation precedes token-target validation, which precedes boundary
validation. Boundaries are checked in this order: negative start, negative end,
reversed range, start bounds, then end bounds. Errors return no partial edit.
Rowan accepts generic range bounds for green splicing and first collects the
children into a vector. Graft uses one explicit half-open convention, rebuilds
list prefixes, and retains the following suffix. Path and focus helpers connect
this node edit to persistent root reconstruction.

Edits can shift paths as well as byte ranges. Empty inserted elements shift
indices without adding text. Removed occurrences have no counterpart in the
new snapshot. Applications must reconsider paths after editing and start a new
cursor to observe the result. This milestone supplies individual structural
edits without occurrence tracking or a batch-edit protocol across versions.

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

== Pure builders and positional checkpoints

The third milestone adds `GreenNodeBuilder kind`, a pure value for incremental
construction. It stores pending elements in reverse source order, their cached
count, and a stack of open node kinds and starting positions. A `Checkpoint`
records a position in this pending forest, counting elements rather than bytes.
`startNode` opens a node at the current end. `builderToken` appends exact text.
`builderElement` appends an existing green element and reuses its cached length.

`finishNode` removes the pending suffix belonging to the innermost node. It
reverses that suffix into source order, constructs the node, and replaces the
suffix with one pending element. Descendants need no traversal. The builder
uses ordinary lists and values, with no mutation or interning. Keeping an old
builder allows independent construction branches, while retaining its pending
trees and open node stack.

`startNodeAt` opens a node at a checkpoint. It includes the elements already
emitted after that position and further elements emitted before the node closes.
This supports delayed wrapping when a parser recognizes an operator after
emitting its left operand. Repeated wrapping at the same position supports
left-associative expressions. A marker at the end can start an empty node.

Graft follows Rowan's positional validation: the marker cannot exceed the
pending count or precede the innermost open node. It records no ownership or
history. Closing a node collapses positions. Later emissions can make an old
position valid again, and an unrelated builder's marker can pass the same tests.
Applications must use checkpoints with their corresponding construction history.

Errors use `Either BuilderError`, replacing Rowan's assertions. `finishNode`
rejects a builder with no open node. `finishBuilder` first rejects unclosed
nodes, then requires exactly one completed root node. This explicit balance
check is stricter than Rowan's final forest check. Finishing never closes nodes
implicitly, and a failed operation leaves the input value available.

Opening nodes, marking positions, and appending existing elements take constant
time. Closing a node costs a traversal of its immediate children. Token creation
retains the green core's UTF-8 counting cost. Finishing checks the open stack
to report its depth. Saved builders retain their earlier construction state,
so clients can release old values when branching is no longer needed.

== Typed AST views and semantic updates

The fifth milestone adds `AstNode` and `AstToken`, Haskell type classes for
application-defined views over node and token occurrences. Each class associates
a view with the application's existing syntax-kind type. A view is usually a
newtype around a cursor, with a fallible cast and a syntax accessor. This retains
concrete text, trivia, paths, byte ranges, and the original snapshot without
constructing a separate semantic tree.

`matchNode` and `matchToken` check element form and a kind predicate, then pass
the unchanged cursor to the application's wrapper. Instances must reject the
opposite form. A successful cast must retain the exact occurrence and snapshot,
and casting the resulting view's syntax must succeed again. These are instance
laws, not properties that Haskell's type system enforces. Private constructors
and the supplied helpers support the usual implementation.

Typed fields select matching immediate node or token children in source order.
They do not search grandchildren. List results are lazy, and first-match queries
stop at the first successful cast. `astParent` casts only the immediate parent.
`astAncestors` returns matching ancestors nearest first, excluding the focus.
Empty tokens remain visible, and repeated shared green values retain distinct
paths and ranges. A view retains its ancestors and therefore its original root.

Casting checks a view's admissibility, not a grammar. An expression with missing
operands can still have an expression view. Field accessors return optional
values or lists, leaving required fields, duplicate handling, and token spelling
validation to the application. No parser or lexer belongs to this layer.

The application example `examples/ExactUpdate.hs` reads an integer from a typed
number token. Its setter retains the original token when the value is unchanged,
including the spelling `001`. A changed value uses a new number token and
rebuilds ancestors through the retained cursor. Surrounding spacing, comment,
and CRLF spelling remain exact. Missing fields or invalid numeric text return
no update. This is one explicit semantic policy, not a general bidirectional
update protocol supplied by Graft.

Rowan's node trait and Graft's node class both supply typed syntax wrappers.
Graft directly associates views with kind types, without Rowan's language-tag
conversion, and adds a corresponding token class. Lazy lists replace a dedicated
typed-child iterator. Match helpers check form in addition to kind because
Graft uses one cursor type for nodes and tokens. There is no separate kind-only
cast query or equivalent of Rowan's location-based syntax/AST pointers yet.
Location handles need a design that addresses ambiguous empty and nested ranges.

The usual newtype cast requires no tree traversal beyond the supplied predicate.
Typed navigation scans candidate child lists or ancestor chains, with work
determined by the user's cast implementation. Views perform no copying or
normalization of source text. The baseline measures Graft only. It establishes
no allocation or runtime equivalence with Rowan.

== Performance baseline

The sixth milestone records two benchmark passes over 89 cases, using GHC
9.12.2 with optimisation. Cases cover ASCII and Unicode token construction,
wide nodes, deep unary trees, and a document with 1,000 line nodes and five
tokens per line. Widths and depths reach 10,000. Prepared trees are fully
evaluated before timing. Result consumers force rendered text, cached lengths,
or sums of visited source ranges. Edited roots need no traversal of reused
descendants to establish their caches.

At 10,000 children, first-child lookup takes about 40 nanoseconds, while
last-child lookup takes 210–218 microseconds. Replacing the first child takes
50–60 microseconds because the new root still sums every child cache.
These measurements identify wide navigation and ancestor reconstruction as
candidates for controlled experiments. They do not select a new layout.

Separate processes measure live heap after full garbage collection.
A kibibyte is 1,024 bytes, and a mebibyte is 1,048,576 bytes.
For 10,000 lines, retaining the latest edited root uses about 5.6 mebibytes,
while retaining the original and 100 edited roots uses 17.1 mebibytes.
A middle token cursor retains about 5.7 mebibytes. Retaining only its green
token uses about 32 kibibytes. These totals subtract startup live memory but include fixture
and runtime overhead.

The measurement host uses an Intel Xeon w3-2435 with one selected CPU and
a powersave governor. Some cases reach the three-second timing limit before
meeting the requested precision. Results support broad cost comparisons, not
fine rankings. `docs/performance.md` records the environment, raw CSV files,
uncertainty, method, and conclusions. `bench/README.md` gives reproduction
commands. This baseline supplies no duplicate-rate evidence that establishes
a need for interning, and it makes no speed or memory comparison with Rowan.

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

Builder properties stream start, token, and finish events from an independent
tree model. They compare shape, exact source, and every cached byte length.
Generated cases check retroactive and repeated wrapping, emissions after opening
a wrapper, independent builder branches, reuse of existing elements, checkpoint
bounds, and balance at every close. Regressions cover invalid root shapes,
empty construction, top-level wrapping, markers from unrelated builders, revived
positions, and reuse of large shared trees without rendering them.

Child-edit properties share the green and cursor tests' independent model.
They compare shape, exact source, all caches, and every new occurrence range.
Untouched occurrences are checked at their shifted paths. Properties also cover
direct/path/focus agreement, insertion/removal inverses, empty and identity
splices, validation order, invalid paths, and token targets. Regressions cover
empty elements, retained trivia, Unicode, extreme indices, and large cached
lengths without rendering shared trees.

Typed-view properties compare casts and syntax round trips with model occurrences,
including their green focus, root, path, and byte range. They check immediate
node/token fields, parent and ancestor filtering, token replacement, and child
insertion through typed views. New trees are checked against independent source
and range oracles. Regressions cover incomplete fields, opposite forms with the
same kind, shared occurrences, first-match laziness, and large cached ranges.

The implementation study used Rowan revision
`b6f1cb6494a99a2264186d110b1b170989774814`, retrieved on 10 October 2026;
its manifest identifies version 0.17.0. The inspected sources were:

#let upstream = "https://github.com/rust-analyzer/rowan/blob/b6f1cb6494a99a2264186d110b1b170989774814/"
- #link(upstream + "src/green/node.rs")[Green nodes] and
  #link(upstream + "src/green/token.rs")[tokens]: lengths, child storage,
  rendering, trivia, and green child insertion/removal/splicing.
- #link(upstream + "src/cursor.rs")[Cursors]: contextual navigation and
  replacement through ancestors.
- #link(upstream + "src/ast.rs")[AST support]: the node trait, typed child
  filtering, and location-based syntax and AST pointers.
- #link(upstream + "src/green/builder.rs")[Builders] and
  #link(upstream + "src/green/node_cache.rs")[cache]: the pending forest, open-node
  stack, checkpoint validation, suffix wrapping, and interning.
- #link(upstream + "src/green.rs")[Green tests],
  #link(upstream + "src/syntax_text.rs")[text tests], and
  #link(upstream + "tests/tidy.rs")[formatting test]. Green tests concern Rust
  thread traits and sizes; text tests exercise empty and differently chunked
  strings. The README also points to rust-analyzer integration tests.

== Remaining work

- Use the measured baseline for focused experiments on wide child navigation
  and ancestor reconstruction, before choosing interning, vectors, additional
  offset caches, or other layouts.
- Location handles and occurrence tracking across snapshots.
- Explicit trivia ownership and the desired Rowan compatibility boundary.

General bidirectional semantic updates still need an application correspondence
between semantic values and concrete syntax. Typed views expose the retained
syntax, and persistent edits reconstruct roots. They cannot decide where new
comments belong, which spelling to prefer, or how to reconcile every changed
semantic value with retained syntax.

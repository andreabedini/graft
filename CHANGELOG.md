# Revision history for graft

## 0.1.0.0 -- unreleased

* Initial immutable green nodes and tokens, with caller-defined kinds, cached
  UTF-8 byte lengths, and lossless rendering.
* Persistent path lookup, subtree and token replacement, and structured path
  errors. Unchanged subtrees are reused.
* QuickCheck properties for exact rendering, byte lengths, replacement, and
  preservation of unrelated branches; Unicode and empty-tree regressions.
* Immutable red cursors with contextual parent/child/sibling navigation,
  traversals, token navigation, and persistent focus replacement.
* Abstract half-open UTF-8 byte ranges, boundary-aware token lookup, and
  deterministic covering-element lookup.
* Model properties and regressions for occurrence ranges, shifted offsets,
  empty elements, shared subtrees, and large logical source ranges.
* Pure persistent green tree builders with token emission, reuse of existing
  green elements, and positional checkpoints for retroactive wrapping.
* Structured errors for unbalanced construction, invalid roots, and checkpoint
  bounds, with model properties and checkpoint lifetime regressions.
* Persistent half-open child splicing, insertion, and removal on green nodes,
  path-selected nodes, and cursor focuses, with structured child-edit errors.
* Shared-model properties for unchanged occurrences, shifted paths and byte
  ranges, identity edits, inverse edits, and child-edit validation order.
* Cursor-backed typed node/token views with associated syntax-kind types,
  form-and-kind cast helpers, and lazy typed field and ancestor navigation.
* Model properties for typed casting laws, occurrences, fields, and edits, with
  malformed-syntax and lazy-query regressions. An exact-update example preserves
  existing number spelling when the semantic value does not change.
* Reproducible benchmarks for construction, rendering, wide/deep navigation,
  builders, typed fields, and repeated edits, with allocation statistics.
* Isolated live-heap measurements for shared trees, snapshot histories, and
  retained cursors; raw baseline results and a reproduction guide.

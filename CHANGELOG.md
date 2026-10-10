# Revision history for graft

## 0.1.0.0 -- unreleased

* Initial immutable green nodes and tokens, with caller-defined kinds, cached
  UTF-8 byte lengths, and lossless rendering.
* Persistent path lookup, subtree and token replacement, and structured path
  errors. Unchanged subtrees are reused.
* QuickCheck properties for exact rendering, byte lengths, replacement, and
  preservation of unrelated branches; Unicode and empty-tree regressions.

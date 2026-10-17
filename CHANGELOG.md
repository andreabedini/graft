# Revision history for graft

## 0.1.0.0 -- unreleased

Initial release.

* Immutable, lossless green trees with caller-defined syntax kinds, cached
  UTF-8 byte lengths and child offsets, and structural sharing.
* Cursors (zippers) with parent, child, sibling and token navigation, absolute
  byte ranges, offset and range lookup, and edits that return a cursor on the
  edited tree.
* Persistent replacement, splicing, insertion and removal of a node's
  children, or anywhere in a tree through a cursor, with structured errors.
* Short function names meant for qualified import (`Cursor.parent`,
  `Green.node`); the `Graft` module exports the types and classes.
* Builders for parsers, with checkpoints for wrapping already-emitted elements,
  as a pure value or in the `Build` monad.
* Typed AST views over cursors.
* A line index for line, column and UTF-16 positions.
* `NFData`, `Hashable` and `Exception` instances, and `Functor` over kinds for
  green trees. `Show` prints the calls that rebuild abstract values.

# Revision history for graft

## 0.1.0.0 -- unreleased

Initial release.

* Immutable, lossless green trees with caller-defined syntax kinds, cached
  UTF-8 byte lengths and child offsets, and structural sharing.
* Cursors (zippers) with parent, child, sibling and token navigation, absolute
  byte ranges, offset and range lookup, and edits that return a cursor on the
  edited tree.
* Persistent replacement, splicing, insertion and removal by path or through a
  cursor, with structured errors.
* Builders for parsers, with checkpoints for wrapping already-emitted elements,
  as a pure value or in the `Build` monad.
* Typed AST views over cursors.
* A line index for line, column and UTF-16 positions.

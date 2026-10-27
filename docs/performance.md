# Graft performance

A dated log of Graft's benchmark measurements, oldest first. The first three
entries are full runs of `bench/run.py`, whose raw results are kept under
`bench/results/`; the [run guide](../bench/README.md) explains how to
reproduce them. Later entries are before-and-after passes of the cases that a
change affected. The last section says what the numbers suggest about the
remaining costs.

## Method

The full runs measured these workloads ([source](../bench/Main.hs)):

| Workload | Shape |
| --- | --- |
| Token | ASCII or emoji text of 32 or 4,096 UTF-8 bytes |
| Wide | One node with 100, 1,000 or 10,000 tokens, each with distinct Unicode text |
| Deep | 100, 1,000 or 10,000 nested single-child nodes above one `001` token |
| Chain | A left-nested operator chain `((0 + 1) + 2) + …` with 100, 1,000 or 10,000 operators, so both deep and wide |
| Document | 1,000 line nodes of five tokens each: name, spacing, number, comment, CRLF |

The cases cover construction (directly, with the pure builder and with the
`Build` monad), rendering, child access, sibling iteration, offset and range
lookup, replacement and splicing, typed fields, whole-tree traversal and 100
successive edits. Inputs are fully evaluated before timing, and each case
consumes its result: rendered text, a cached length, or a strict sum of visited
ranges. `bench/Fixtures.hs` checks every workload against independently built
text before measuring (`graft-bench --check`). Cases added since then are
described in the entries that use them.

Each timing pass asks `tasty-bench` for a relative standard deviation of 2%,
with a three-second limit per case, and the suite runs twice. Times are CPU
time including garbage collection. Allocation is per operation. Separate
processes measure residency: the live heap after a full collection, minus the
live heap at startup.

All three runs used GHC 9.12.2 with `-O2` on an Intel Xeon w3-2435 under Fedora
Linux, pinned to CPU 2 with the `powersave` governor. Other activity on the
machine was not controlled.

| Run | Code measured | Results |
| --- | --- | --- |
| 2026-10-10 | List children, before the 2026-10-17 changes | [timing 1](../bench/results/2026-10-10/timing-1.csv), [timing 2](../bench/results/2026-10-10/timing-2.csv), [residency](../bench/results/2026-10-10/residency.csv), [environment](../bench/results/2026-10-10/environment.json) |
| 2026-10-17 | Array children, cursor edits, single-buffer rendering | [timing 1](../bench/results/2026-10-17/timing-1.csv), [timing 2](../bench/results/2026-10-17/timing-2.csv), [residency](../bench/results/2026-10-17/residency.csv), [environment](../bench/results/2026-10-17/environment.json) |
| 2026-10-17b | As above, plus a single-pass builder and unpacked tokens | [timing 1](../bench/results/2026-10-17b/timing-1.csv), [timing 2](../bench/results/2026-10-17b/timing-2.csv), [residency](../bench/results/2026-10-17b/residency.csv), [environment](../bench/results/2026-10-17b/environment.json) |

Each environment record includes SHA-256 hashes of the sources and the
executable. The 2026-10-17 run measured the commit "perf: render without
per-node closures and climb to the root strictly", and 2026-10-17b measured
"docs: plainer Haddock with examples in every module".

The 2026-10-17b run took place while the machine was busy with other work
(a load average of about 6.6), and 38 and 40 of its 107 cases missed the
requested precision, with up to 72% spread. Its timings are therefore not
used in the tables below, except where noted; its residency figures are, since
live heap doesn't depend on load.

The tables give the smaller and larger mean of the two passes, so a range is
not a confidence interval. In 2026-10-17, 6 of 107 cases in the first pass and
1 in the second ended at the time limit before reaching the requested
precision, with at most 16% spread (twice the standard deviation over the
mean). The 2026-10-10 run missed in 43 and 29 of its 89 cases, with up to 42%.
Treat differences of less than about 20% as noise.

## Results

Times are in microseconds.

### Wide node, 10,000 children

| Case | 2026-10-10 | 2026-10-17 |
| --- | ---: | ---: |
| First child | 0.039–0.041 | 0.003 |
| Last child | 210–218 | 0.004 |
| Offset lookup, last token | 729–812 | 0.056–0.058 |
| Offset lookup, first token | — | 0.054–0.056 |
| Covering range, last token | 609–750 | 0.035–0.036 |
| Replace first child | 50.1–60.3 | 9.6–10.1 |
| Replace last child | 216–231 | 8.8–9.0 |
| Splice in the middle | 116–137 | 12.7–13.0 |
| Iterate all siblings | 551–582 | 312–326 |
| Last typed token | 576–642 | 302–319 |
| Render | 337–352 | 41.9–42.3 |
| Construct directly | 483–518 | 233–239 |
| Construct with the builder | 949–1,020 | 476–483 |
| Construct with `Build` | — | 478–488 |

Child access and lookups are now independent of width. Replacement still
copies the parent's child array, so it is linear in the width, but it no
longer re-sums every child's length. Rendering copies each token once into a
buffer sized from the cached length.

### Deep and chain trees, 10,000 levels

| Case | 2026-10-10 | 2026-10-17 |
| --- | ---: | ---: |
| Deep: construct | 166–185 | 164–166 |
| Deep: descend to the leaf | 475–511 | 267–269 |
| Deep: render three bytes | 192–207 | 42.5–43.2 |
| Deep: offset lookup | 1,237–1,384 | 590–605 |
| Deep: replace at a path (descend, replace, rebuild) | 795–990 | 790–797 |
| Deep: replace through a prepared cursor | 398–419 | 170–171 |
| Deep: prepared cursor to root | 75.2–186 | 22.9–23.0 |
| Chain: all tokens | — | 1,769–1,824 |
| Chain: offset lookup, first token | — | 565–572 |

Deep cases are bounded by depth, which the array representation doesn't
change. Construction allocates more per level than before (1.2 MiB rather than
0.8 MiB), because each node now holds two small arrays.

The chain workload is new. Before the 2026-10-17 changes, whole-tree
traversal was quadratic in depth. A separate measurement of the 2026-10-10 code
took 346 ms and allocated 1.8 GB to visit every token of a 4,000-operator
chain; the 10,000-operator chain now takes 1.8 ms.

### Document, 1,000 lines

| Case | 2026-10-10 | 2026-10-17 |
| --- | ---: | ---: |
| Construct | 273–312 | 136–137 |
| Render 22,893 bytes | 189–213 | 23.4–23.6 |
| All token ranges | 619–668 | 252–254 |
| All typed line nodes | 71.2–74.5 | 36.7 |
| 100 successive edits | 770–774 | 65.0–65.4 |

### Retained memory

Live heap in MiB after a full collection, minus startup. Edited cases apply 100
edits to the middle line.

| Retained result | 2026-10-10, 1,000 lines | 2026-10-17, 1,000 lines | 2026-10-10, 10,000 lines | 2026-10-17, 10,000 lines |
| --- | ---: | ---: | ---: | ---: |
| Original tree | 0.558 | 0.505 | 5.571 | 5.037 |
| One line subtree, reused for every line | 0.025 | 0.017 | 0.231 | 0.154 |
| Latest edited tree | 0.589 | 0.505 | 5.601 | 5.037 |
| All 101 versions | 1.768 | 1.319 | 17.080 | 12.923 |
| A cursor on one middle token | 0.600 | 0.505 | 5.716 | 5.037 |
| Only that cursor's green token | 0.032 | 0.001 | 0.032 | 0.001 |

A document of 10,000 lines (238,894 bytes of source, 50,000 tokens) takes about
105 bytes of heap per token, or 22 per source byte. Keeping every version costs
less than before because an edit copies one child array per ancestor instead of
rebuilding list prefixes. A cursor keeps its whole tree alive. Building and
editing the 10,000-line document allocates 19 MiB in total, down from 59 MiB.

### Later changes (2026-10-17b)

Two changes followed the 2026-10-17 run:

- **Unpacked tokens.** A token's fields are stored directly in the `Token`
  constructor, saving a box per token. The heap figures for 10,000 lines become:

  | Retained result | 2026-10-17 | 2026-10-17b |
  | --- | ---: | ---: |
  | Original tree | 5.037 | 4.274 |
  | Latest edited tree | 5.037 | 4.274 |
  | All 101 versions | 12.923 | 12.159 |

  That is about 90 bytes per token, or 19 per source byte. An interleaved
  comparison of the two builds on the same machine found the timings within
  noise, except that offset lookup became about 10 ns slower. Unpacking nodes
  as well saved only a further 3% of heap and made replacements about 45%
  slower, because cursors had to re-allocate the parent node on every
  descent, so nodes stay boxed.
- **Single-pass `Builder.finishNode`.** Closing a node now fills the node while walking
  the pending elements, instead of splitting the list first and copying it.
  Even on the busy machine, building 10,000 tokens took 158–176 µs, against
  476–483 µs in the 2026-10-17 run.

### Recorded edits (2026-10-19)

Cursor edits are now recorded in the ancestors and built when read, instead
of copying the parent on every move away from an edit. These are single
passes of the affected cases on one machine, before and after the change,
with GHC 9.12.2, `--stdev 3` and a 32 MiB allocation area, not runs of
`bench/run.py`. `rename-all` renames each of the 1,000 `Name` tokens of the
1,000-line document through one cursor, as the `Graft.Cursor` example does;
`rename-all-ancestors` also reads the ranges of every token's ancestors, as a
rename that checks scopes would. `edit-each-child` replaces every child of the
wide node through one cursor.

| Case | Before | After |
| --- | ---: | ---: |
| `rename-all`, 1,000 lines | 1.96 ms, 17 MB allocated | 540 µs, 2.5 MB |
| `rename-all-ancestors`, 1,000 lines | 8.25 ms, 80 MB | 738 µs, 3.1 MB |
| `edit-each-child`, 100 children | 22.8 µs | 15.5 µs |
| `edit-each-child`, 1,000 children | 1.61 ms, 16 MB | 210 µs, 1.0 MB |
| `edit-each-child`, 10,000 children | 149 ms, 1.5 GB | 2.78 ms, 11 MB |
| One replacement at depth 10,000, then the tree | 189 µs, 1.0 MB | 222 µs, 1.0 MB |
| 100 single edits of the document, each followed by the tree | 101 µs | 99.4 µs |
| Iterate all siblings, 10,000 children | 320 µs | 293 µs |
| Child by index from a fresh cursor | 3.6–6.3 ns | 39–53 ns |
| Prepared cursor at depth 10,000 to the root | 23.2 µs | 64.6 µs |

Before, each step away from an edited child copied the parent's child array,
so a run of edits was quadratic in the width, and every climb from below an
edit that was then thrown away, such as `Cursor.ancestors` on each token,
copied the ancestors again. Single edits cost the same as before. A cursor is
now larger, so taking one child of a fresh cursor, which GHC used to reduce to
almost nothing, allocates, and walking up a very deep cursor is slower.

| Retained result, 1,000 lines | Before | After |
| --- | ---: | ---: |
| The tree after renaming every `Name` | 0.428 MB, 18.8 MB allocated | 0.428 MB, 3.7 MB allocated |
| The cursor on the last token after renaming | 0.429 MB | 0.765 MB |
| The last cursor after 10 renaming passes through one chain of cursors | 0.429 MB | 0.902 MB |
| The last cursor after 100 passes | 0.429 MB | 0.902 MB |
| Every cursor that one renaming walk visits | 17.3 MB | 1.81 MB |

At 10,000 lines the renamed tree takes 4.24 MB before and 4.28 MB after,
with 908 MB and 37 MB allocated, the cursor 4.24 MB before and 7.64 MB
after, and every visited cursor 893 MB before and 18.7 MB after. A cursor
keeps the version of each edited node from before the edits, but repeated
passes don't accumulate older versions: the residency after 10 and 100
passes is the same. Keeping every cursor of a walk costs far less than
before, when each cursor that had left an edited line held its own copy of
the document's child array.

### Typed casts on edited cursors (2026-10-26)

A typed walk that edits as it goes was still quadratic after the previous
change. `Ast.matchNode` and `Ast.matchToken` told nodes from tokens by
matching on `Cursor.green`, which builds a node that has been edited through
the cursor. So `Ast.parent` or `Ast.ancestors` on each token of a renaming
walk rebuilt every edited ancestor after each edit, at a cost proportional to
its width. The casts now use `Cursor.kind` and the new `Cursor.isToken`,
which build nothing.

`rename-all-groups` renames every `Name` in the 1,000-line document and,
before each rename, reads the ranges of the token's ancestors that cast to a
typed view of line (`Group`) nodes. As in the previous entry, these are single
passes on one machine with GHC 9.12.2, `--stdev 3` and a 32 MiB allocation
area, not runs of `bench/run.py`.

| Case | Before | After |
| --- | ---: | ---: |
| `rename-all-groups`, 1,000 lines | 13.5 ms, 65 MB allocated | 669 µs, 3.5 MB |

The typed walk now costs the same as `rename-all-ancestors`, which reads
every ancestor through `Cursor.ancestors`: 658 µs and 3.5 MB in the same run.

### Read-only cursors and edit plans (2026-10-27)

Cursors lost their edits and shrank to four fields, and `Graft.Edit` took
over editing: `apply` makes a plan of edits against the tree they refer to,
and `rewrite` visits a tree top-down and replaces as it goes
([architecture](architecture.md)). The cases that edit were rewritten over
the new API, so the "before" and "after" columns time the same task done
the way each design does it: `edit-each-child` was a cursor walk and is a
rewrite; `replace-*`, `replace-path`, the focused `replace` and the 100
document edits were `Cursor.replace` and `Cursor.tree` and are plans of one
edit; `rename-all*` renamed through one cursor and are rewrites that read
the same things before each rename. New cases: `plan-each-child`, the same
change as `edit-each-child` as a plan with one edit per child;
`apply-sparse-k`, a plan replacing the name of `k` lines spread over the
document, and `text-edits-1000` for its largest plan; and
`apply-from-equal-tree`, one edit whose cursor is in an equal document built
separately, so that `apply` compares the trees instead of recognising the
base.

Both columns are single passes on one machine in one sitting, with GHC
9.12.2, `-O2`, `--stdev 3`, a 32 MiB allocation area and the process pinned
to CPU 2, not runs of `bench/run.py`; the "before" build is the `main`
revision `7a36b16`. The plan cases were timed a second time after two fixes
that the first pass prompted: `apply` had laid out every untouched child of
a rebuilt node through lists, at 470 bytes and 80 ns per child, and now
copies the untouched runs of the child array as blocks, shifts their offsets
and measures only the new elements, reusing the offsets when no patch
changes the layout. Times are in microseconds.

| Case | Before | After |
| --- | ---: | ---: |
| Wide, 10,000: child by index from a fresh cursor | 0.038–0.042 | 0.030 |
| Wide, 10,000: iterate all siblings | 273 | 48.6 |
| Wide, 10,000: all typed tokens | 464 | 405 |
| Wide, 10,000: replace the first, middle, last child | 13.6, 13.3, 10.5 | 17.5, 15.3, 13.5 |
| Wide, 100 / 1,000 / 10,000: `edit-each-child` | 17.2 / 167 / 2,272 | 4.79 / 47.1 / 590 |
| Wide, 10,000: `plan-each-child` | — | 3,281 |
| Wide, 10,000: `edit-parent-once` | 349 | 347 |
| Deep, 10,000: descend to the leaf | 357 | 243 |
| Deep, 10,000: prepared cursor to the root | 46.1 | 30.9 |
| Deep, 10,000: replace at a path | 550 | 2,036 |
| Deep, 10,000: replace through a prepared cursor | 223 | 1,824 |
| Chain, 10,000: all tokens | 2,133 | 1,879 |
| Document: all token ranges | 328 | 281 |
| Document: all typed line nodes | 47.8 | 39.4 |
| Document: 100 successive edits, latest / all versions | 84.3 / 90.4 | 113 / 109 |
| Document: `rename-all` / `-ancestors` / `-groups` | 595 / 807 / 964 | 300 / 504 / 454 |
| Document: `apply-sparse` 1 / 10 / 100 / 1,000 | — | 1.86 / 5.64 / 38.4 / 411 |
| Document: `text-edits-1000` | — | 507 |
| Document: `apply-from-equal-tree` | — | 97.5 |

Allocation moved with the times: `rename-all` from 3.05 MB to 1.57 MB,
`edit-each-child` at 10,000 from 11.3 MB to 2.88 MB, a replacement in the
wide node from 160 KB to 161 KB, and the 100 document edits from 0.93 MB to
1.06 MB.

| Retained result, 1,000 lines | Before | After |
| --- | ---: | ---: |
| The original tree | 0.449 MB | 0.449 MB |
| A cursor on a middle token | 0.450 MB | 0.449 MB |
| The tree after renaming every `Name` | 0.425 MB | 0.377 MB |
| A cursor on that tree's last token (before: the cursor that renamed) | 0.738 MB | 0.378 MB |
| The same after 10 and after 100 renaming passes | 0.899 MB | 0.377 MB |
| Every cursor a renaming walk visits (before: tokens; after: every element) | 1.78 MB | 0.929 MB |

At 10,000 lines the renamed tree takes 4.27 MB before and 3.76 MB after, and
the cursor 7.39 MB before and 3.76 MB after. A cursor now retains the tree it
is in and nothing else. The renamed tree is smaller after because the
rewrite's replacement token is one shared value where the walk built one per
token; that is the fixture, not the library.

What the pass found:

- Navigation is faster and smaller across the board, as the four-field
  cursor predicted: a fresh child costs what it did before 2026-10-19,
  sibling iteration over 10,000 children is five times faster, and deep
  descents and typed walks gained 15–30%.
- A rewrite does the renaming walk in half the time and half the allocation
  of the editing cursor, and a dense change through a rewrite is five times
  faster than the same change as a plan with one edit per child, which pays
  for a trie entry and a path per edit. That is the division of labour the
  design intended.
- A plan of one edit costs a fixed 0.3 µs more than the old cursor edit for
  resolving the cursor, building the trie and validating, and the same array
  copy after that. On the 1,000-line document that is 1.9 µs per edit
  against 0.84. Through a 10,000-level chain it is 2.0 ms against 0.55: each
  level costs a trie node, a layout and a one-child patch where the cursor
  copied one child. A plan of a single edit could walk the cursor's parent
  chain instead of building a trie; this log notes it as the candidate if a
  real tree is that deep.
- Comparing an equal tree built separately costs 97 µs on the 1,000-line
  document, fifty times the edit it validates. The pointer test is what
  makes the normal case cheap, and it is an optimisation, not a bound.

## What the numbers suggest

- Memory per token is the largest remaining cost. Each token occupies an array
  slot, an offset and a `Token` holding its text, and nothing deduplicates
  identical tokens such as `;` or a single space. A node cache
  like Rowan's could share them. Its value depends on duplicate rates in real
  parser output, which haven't been measured.
- Edits in very wide nodes copy the whole child array. A chunked or
  finger-tree child list would make them logarithmic, at a cost to access and
  memory in the common case.
- A plan of one edit on a very deep path pays per level for machinery that a
  single edit doesn't need; walking the cursor's parent chain would remove
  it.
- Traversals through cursors allocate a cursor per occurrence. `Green.foldM`
  avoids that when positions aren't needed; this log doesn't yet record it.

These results describe Graft alone and make no comparison with Rowan.

# Graft performance

This report records Graft's benchmark results and compares them with the first
baseline. The [run guide](../bench/README.md) explains how to reproduce them.

## Method

The benchmark suite ([source](../bench/Main.hs)) runs these workloads:

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
text before measuring (`graft-bench --check`).

Each timing pass asks `tasty-bench` for a relative standard deviation of 2%,
with a three-second limit per case, and the suite runs twice. Times are CPU
time including garbage collection. Allocation is per operation. Separate
processes measure residency: the live heap after a full collection, minus the
live heap at startup.

Both runs used GHC 9.12.2 with `-O2` on an Intel Xeon w3-2435 under Fedora
Linux, pinned to CPU 2 with the `powersave` governor. Other activity on the
machine was not controlled.

| Run | Code measured | Results |
| --- | --- | --- |
| 2026-10-10 | List children, before the review changes | [timing 1](../bench/results/2026-10-10/timing-1.csv), [timing 2](../bench/results/2026-10-10/timing-2.csv), [residency](../bench/results/2026-10-10/residency.csv), [environment](../bench/results/2026-10-10/environment.json) |
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

The chain workload is new. Before the review, whole-tree traversal was
quadratic in depth. A separate measurement of the code before the review
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
- **Single-pass `finishNode`.** Closing a node now fills the node while walking
  the pending elements, instead of splitting the list first and copying it.
  Even on the busy machine, building 10,000 tokens took 158–176 µs, against
  476–483 µs in the 2026-10-17 run.

## What the numbers suggest

- Memory per token is the largest remaining cost. Each token occupies an array
  slot, an offset and a `Token` holding its text, and nothing deduplicates
  identical tokens such as `;` or a single space. A node cache
  like Rowan's could share them. Its value depends on duplicate rates in real
  parser output, which haven't been measured.
- Edits in very wide nodes copy the whole child array. A chunked or
  finger-tree child list would make them logarithmic, at a cost to access and
  memory in the common case.
- Deep traversals allocate a cursor per occurrence. A traversal that reads only
  green elements, or a fold, would avoid that when ranges aren't needed.

These results describe Graft alone and make no comparison with Rowan.

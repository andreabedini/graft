# Graft performance baseline

This report records the first measured baseline for Graft. It covers the existing
list representation and immutable cursors. It introduces no interning or library
optimisations. The [run guide](../bench/README.md) gives reproduction commands.

## Environment and method

The baseline uses revision `6ddf6b12` on 10 October 2026. The library and benchmark
component compile with GHC 9.12.2, Cabal 3.16.1.0, and `-O2`. The benchmark uses
`tasty-bench` 0.3.5, `text` 2.1.2, and `deepseq` 1.5.1.0. The machine runs Fedora
Linux on an Intel Xeon w3-2435. Each measurement process runs on CPU 2.
The CPU governor is `powersave`. Other host activity remains uncontrolled.

Each timing pass requests 2% relative standard deviation, with a three-second
limit per case. The runtime collects allocation statistics and uses a 32 MiB
allocation area. The suite runs twice, sequentially. Times measure CPU execution,
including garbage collection, rather than elapsed wall time.

Prepared environments force input text and green trees before timing. Rendering
forces the strict `Text` result. Navigation consumes ranges or a strict sum over
visited ranges. Construction and editing consume cached root lengths. Green
smart constructors force the immediate cache sums, which force the rebuilt
ancestors. This measures completed edits without traversing unchanged descendants.
The deep focus cases prepare their selected cursor before timing.

The [benchmark source](../bench/Main.hs) defines these workloads:

| Workload | Shape and observations |
| --- | --- |
| Token | ASCII or emoji text with 32 or 4,096 UTF-8 bytes; token construction |
| Wide | 100, 1,000, or 10,000 immediate tokens, each with distinct Unicode text |
| Deep | 100, 1,000, or 10,000 unary wrapper nodes above one `001` token |
| Document | 1,000 line nodes with five tokens per line: name, spacing, number, comment, CRLF |

Wide cases measure direct and builder construction, subtree reuse, checkpoint
wrapping, rendering, indexed children, adjacent siblings, offset lookup, covering
ranges, replacement at three positions, insertion, and typed token fields.
Deep cases measure construction, rendering, descent, offset lookup, path and
cursor replacement, and return to the root. Document cases measure construction,
rendering, token traversal, typed node fields, and 100 successive edits.

The document edits replace one middle number token with successive decimal
values. The timing case `100-edits-latest` observes the final root's cache.
The timing case `100-edits-history` sums every snapshot's cache as a stream.
It does not retain the whole history across a garbage collection. The separate
residency case `history` keeps all 101 roots. Fixed trivia text can share through
ordinary compiler evaluation.
The wide fixture uses distinct spellings to avoid assuming automatic deduplication.
Construction includes Graft's token byte counting. Source generation for wide
construction is outside timing. Document construction includes name generation.

The fixture checks compare rendered source with independently assembled text,
verify known UTF-8 lengths, and check exact changed text after repeated edits.
They also check retained originals, path/focus agreement, builder output, typed
fields, sibling observations, and the full deep paths. The separate property
suite remains the correctness check for general inputs.

## Reading the raw measurements

Each timing CSV contains the mean in picoseconds, twice the estimated standard
deviation, allocated bytes per operation, copied bytes per operation, and peak
process memory. One picosecond is one trillionth of a second. Peak memory is a
cumulative runtime measurement across the process, not live bytes per operation.
The allocation estimates include the benchmark's observation of the result.

Residency cases run in separate processes and force their selected results.
A live reference retains the result through a full garbage collection, then
the process reads it again. The live-byte difference subtracts startup live
memory. It includes fixture and runtime overhead. Residency is not operating-system
resident memory, and total allocation includes the discarded intermediate trees.
The shared case repeats one line subtree. The ordinary document uses distinct
names, so this comparison demonstrates explicit reuse rather than an interner.

Times depend on the compiler, machine, runtime configuration, and tree shape.
A timing limit can end a measurement before its target deviation is reached.
The raw deviation columns expose this uncertainty. These results establish a
baseline for Graft and make no performance comparison with Rowan.

## Measured results

Both passes completed all 89 timing cases. Their raw files are
[pass one](../bench/results/2026-10-10/timing-1.csv) and
[pass two](../bench/results/2026-10-10/timing-2.csv). The
[environment record](../bench/results/2026-10-10/environment.json) identifies
the executable and sources by SHA-256 hashes.

The tables show the smaller and larger mean from the two passes. These ranges
are not confidence intervals. A microsecond (µs) is one millionth of a second.
A kibibyte (KiB) is 1,024 bytes. A mebibyte (MiB) is 1,048,576 bytes.
Allocation ranges also use the two reported estimates.

| Wide children | First child, µs | Last child, µs | Replace first, µs | Replace last, µs |
| ---: | ---: | ---: | ---: | ---: |
| 100 | 0.032–0.038 | 0.882–0.907 | 0.371–0.395 | 0.852–0.941 |
| 1,000 | 0.032–0.038 | 9.847–10.470 | 3.809–3.835 | 10.055–10.492 |
| 10,000 | 0.039–0.041 | 209.792–217.663 | 50.055–60.333 | 215.742–230.584 |

| Selected case | Time, µs | Allocated, KiB |
| --- | ---: | ---: |
| Wide 10,000, direct construction | 483–518 | 2,446 |
| Wide 10,000, builder tokens | 949–1,020 | 3,936–3,961 |
| Wide 10,000, builder subtree reuse | 723–732 | 2,147 |
| Wide 10,000, rendering | 337–352 | 1,420 |
| Wide 10,000, last child | 210–218 | 862 |
| Wide 10,000, last-token offset lookup | 729–812 | 3,413 |
| Wide 10,000, middle insertion | 116–137 | 415 |
| Wide 10,000, first typed token | 0.062–0.066 | 0.37 |
| Wide 10,000, last typed token | 576–642 | 2,936 |
| Deep 10,000, rendering three bytes | 192–207 | 464 |
| Deep 10,000, path replacement | 795–990 | 3,486 |
| Deep 10,000, replacement with prepared cursor | 398–419 | 1,891–1,929 |
| Document 1,000, construction | 273–312 | 1,513 |
| Document 1,000, rendering 22,893 bytes | 189–213 | 751 |
| Document 1,000, all token ranges | 619–668 | 2,944 |
| Document 1,000, all typed line fields | 71–75 | 398 |
| Document 1,000, 100 successive edits | 770–774 | 3,574 |

The requested precision was not reached in 43 cases during pass one and 29 cases
during pass two. The largest reported twice-standard-deviation values were 42%
and 29% of the mean. Returning from a prepared 10,000-level cursor to its root
also varied from 75 to 186 µs between passes. These variations limit fine comparisons.

The [residency measurements](../bench/results/2026-10-10/residency.csv) report
live heap after a full collection. The table subtracts startup live memory.
Each row uses its own process, and these cases run once per policy and size.
Edited policies apply 100 edits. History includes the original root.

| Retained result | 1,000 lines, MiB | 10,000 lines, MiB |
| --- | ---: | ---: |
| Original tree | 0.558 | 5.571 |
| One explicitly reused line subtree | 0.025 | 0.231 |
| Latest edited root | 0.589 | 5.601 |
| All 101 snapshots | 1.768 | 17.080 |
| One middle token cursor | 0.600 | 5.716 |
| Only that cursor's green token | 0.032 | 0.032 |

For 10,000 lines, constructing and editing while retaining only the latest root
allocates about 59 MiB in total. The final live heap is about 5.6 MiB.
Allocation and retention measure different costs. Releasing earlier snapshots
allows collection of their rebuilt list prefixes while preserving reused syntax.

## Consequences for further work

Wide indexed navigation is a clear candidate for investigation. The first child
remains cheap while lookup near the end grows with the child list.
Offset and typed-field searches also scan candidate siblings and allocate cursors.
Compare an indexed child representation or a direct green search against this
baseline before changing the public representation.

Replacement at the first child still scans all immediate caches. Its cost grows
from about 0.4 µs to 50–60 µs across these widths. Replacement near the end also
rebuilds a longer list prefix. Experiments can separate incremental cache updates
from changes to child storage, with the existing property suite checking both.

Prepared cursors avoid repeated deep descent. Their replacement measurements
are about half the corresponding path-replacement times in this fixture.
The successful 10,000-level checks establish a tested case, not a general stack
safety guarantee. Rendering still traverses all wrapper nodes, even for three
bytes of text.

Explicit subtree reuse reduces retained memory, and latest-only editing does
not retain the whole history. A cursor still keeps its original root alive.
Long-lived application caches can retain green tokens when they need only text,
or release cursors when their snapshots are no longer needed.

The baseline does not establish a need for an interner. It supplies no measured
duplicate rate from a real parser workload. Interning experiments first need
representative repeated syntax and a comparison of construction time, retained
memory, and cache lifetime. Start with the visible costs of wide navigation and
ancestor reconstruction, and retain these measurements for future comparisons.

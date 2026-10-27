# Run the benchmarks

Run these commands from the package root. The benchmark component adds
`tasty-bench` to the library's dependencies.
Python 3 runs the measurement script. Linux CPU pinning also requires `taskset`.

Build the library and benchmarks with optimisation:

```sh
cabal build bench:graft-bench --enable-benchmarks -O2 --ghc-options=-Werror
```

Check the workloads before measuring them:

```sh
graft_bench=$(cabal list-bin bench:graft-bench --enable-benchmarks -O2)
"$graft_bench" --check
```

Run two passes and save the raw data in a new directory:

```sh
python3 bench/run.py --output bench/results/"$(date +%F)" --runs 2 --timeout 3s --stdev 2
```

If Linux permits CPU 2 in your process's affinity, add `--cpu 2` to pin each
measurement process. Close other CPU-intensive applications before measuring.
The script rejects an existing output directory to preserve earlier results.
It records source and executable hashes, the parent revision, compiler versions,
CPU information, and runtime options in `environment.json`.
Revision metadata is available in a jj checkout. Unpacked source distributions
record source hashes without requiring jj.

The output contains timing CSV files, full timing logs, and `residency.csv`.
Each timing pass runs all cases sequentially with CPU time and allocation
statistics. The runtime uses a 32 MiB allocation area for these passes.
Each residency case runs in a fresh process with the default allocation area.
Residency means live heap bytes after a full garbage collection.

Inspect or filter individual timing cases:

```sh
"$graft_bench" --list-tests
"$graft_bench" --pattern 'wide.10000' --timeout 3s --stdev 2 +RTS -T -A32m -RTS
```

Compare a new run with an earlier CSV file on the same machine:

```sh
"$graft_bench" --baseline bench/results/2026-10-17/timing-1.csv --timeout 3s --stdev 2 +RTS -T -A32m -RTS
```

Measure what one retained result keeps alive:

```sh
"$graft_bench" --residency history 10000 100 +RTS -T -RTS
```

The arguments are a scenario, the number of document lines and the number of
edits. Lines must be positive and edits nonnegative. The scenarios are:

- `tree`: the original document.
- `shared`: one line subtree repeated for every line.
- `latest`: only the root after the edits.
- `history`: the original and every edited root.
- `cursor`: one cursor on a middle token.
- `detached-token`: only that cursor's green token.
- `renamed-tree`: the tree after renaming every `Name` token in one rewrite.
- `renamed-cursor`: a cursor on the last token of that tree.
- `renamed-passes`: the tree after as many renaming passes as there are
  edits, each rewriting the previous pass's result.
- `walk-cursors`: every cursor that one renaming rewrite visits.

Only `latest`, `history` and `renamed-passes` use the edit count; the others
ignore it.

The CSV reports absolute live bytes, startup live bytes, their nonnegative
difference, total allocation since startup collection, and an observed checksum.
These measurements include runtime and fixture overhead. They are not a census
of tree allocations or a measure of operating-system resident memory.

The [baseline report](../docs/performance.md) describes the workloads, measured
results, and limitations. Rebuild and rerun the fixture checks and property suite
after changing a workload or representation. Compare repeated runs before drawing
conclusions from small timing differences.

# Run the benchmarks

Run these commands from the package root. The benchmark component adds
`deepseq` and `tasty-bench` dependencies. The library's dependencies stay unchanged.
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
python3 bench/run.py --output /tmp/graft-benchmark --runs 2 --timeout 3s --stdev 2
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
"$graft_bench" --baseline /tmp/graft-benchmark/timing-1.csv --timeout 3s --stdev 2 +RTS -T -A32m -RTS
```

Measure a specific retention policy:

```sh
"$graft_bench" --residency history 10000 100 +RTS -T -RTS
```

The two numbers specify document lines and successive edits. Sizes must be
positive, and edit counts must be nonnegative. Policies are `tree`, `shared`,
`latest`, `history`, `cursor`, and `detached-token`. `tree` retains the original
document. `shared` repeats one existing line subtree. `latest` retains only the
final edited root. `history` retains the original and every edited root.
`cursor` retains one middle token cursor. `detached-token` retains only that
cursor's green token. Only `latest` and `history` use the edit count.

The CSV reports absolute live bytes, startup live bytes, their nonnegative
difference, total allocation since startup collection, and an observed checksum.
These measurements include runtime and fixture overhead. They are not a census
of tree allocations or a measure of operating-system resident memory.

The [baseline report](../docs/performance.md) describes the workloads, measured
results, and limitations. Rebuild and rerun the fixture checks and property suite
after changing a workload or representation. Compare repeated runs before drawing
conclusions from small timing differences.

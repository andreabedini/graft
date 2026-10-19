#!/usr/bin/env python3
"""Run an already-built benchmark, preserving raw results and its environment."""

import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import platform
import subprocess
import sys


def output(*command):
    return subprocess.check_output(command, text=True).strip()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--cpu", type=int, help="pin child processes to this Linux CPU")
    parser.add_argument("--runs", type=int, default=2)
    parser.add_argument("--timeout", default="3s")
    parser.add_argument("--stdev", type=float, default=2)
    args = parser.parse_args()
    if args.runs < 1 or args.stdev <= 0:
        parser.error("runs and stdev must be positive")
    root = Path(__file__).resolve().parent.parent
    os.chdir(root)
    if args.cpu is not None:
        if not hasattr(os, "sched_getaffinity") or args.cpu not in os.sched_getaffinity(0):
            parser.error("requested CPU is outside this process's Linux affinity")
    binary = Path(output("cabal", "list-bin", "bench:graft-bench", "--enable-benchmarks", "-O2"))
    if not binary.is_file():
        parser.error("build bench:graft-bench with --enable-benchmarks -O2 first")
    # Refuse to overwrite an earlier measurement.
    args.output.mkdir(parents=True, exist_ok=False)
    args.output = args.output.resolve()
    prefix = [] if args.cpu is None else ["taskset", "-c", str(args.cpu)]
    timing_options = ["--stdev", str(args.stdev), "--timeout", args.timeout,
                      "--time-mode", "cpu", "--color", "never", "+RTS", "-T", "-A32m", "-RTS"]
    sources = [root / "graft.cabal", *sorted((root / "src").rglob("*.hs")),
               *sorted((root / "bench").glob("*.hs")), root / "bench/run.py"]
    has_jj = (root / ".jj").is_dir()
    metadata = {
        "started_at": datetime.now(timezone.utc).isoformat(),
        "platform": platform.platform(), "ghc": output("ghc", "--numeric-version"),
        "cabal": output("cabal", "--numeric-version"),
        "parent_revision": output("jj", "--ignore-working-copy", "log", "-r", "@-", "--no-graph",
                                  "-T", "commit_id ++ \"\\n\"") if has_jj else None,
        "working_copy": output("jj", "--ignore-working-copy", "diff", "--stat") if has_jj else None,
        "binary_sha256": hashlib.sha256(binary.read_bytes()).hexdigest(),
        "source_sha256": {str(path.relative_to(root)): hashlib.sha256(path.read_bytes()).hexdigest()
                          for path in sources},
        "cpu": args.cpu, "affinity": sorted(os.sched_getaffinity(0))
        if hasattr(os, "sched_getaffinity") else None,
        "lscpu": output("lscpu") if sys.platform.startswith("linux") else None,
        "timing_options": timing_options, "runs": args.runs,
        "residency_options": ["+RTS", "-T", "-RTS"],
        "build_command": "cabal build bench:graft-bench --enable-benchmarks -O2 --ghc-options=-Werror",
    }
    if args.cpu is not None:
        governor = Path(f"/sys/devices/system/cpu/cpu{args.cpu}/cpufreq/scaling_governor")
        metadata["governor"] = governor.read_text().strip() if governor.exists() else None
    (args.output / "environment.json").write_text(json.dumps(metadata, indent=2) + "\n")
    subprocess.run(prefix + [str(binary), "--check"], check=True)
    for run in range(1, args.runs + 1):
        csv_path = args.output / f"timing-{run}.csv"
        command = prefix + [str(binary), "--csv", str(csv_path)] + timing_options
        print(f"Timing run {run}: {csv_path}", flush=True)
        with (args.output / f"timing-{run}.log").open("w") as log:
            subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, check=True)
    rows = []
    for size in (1000, 10000):
        for edits in (0, 100):
            scenarios = ("tree", "shared", "cursor", "detached-token", "renamed-tree", "renamed-cursor", "walk-cursors") if edits == 0 else ("latest", "history", "renamed-passes")
            for scenario in scenarios:
                command = prefix + [str(binary), "--residency", scenario, str(size), str(edits),
                                    "+RTS", "-T", "-RTS"]
                lines = output(*command).splitlines()
                if not rows:
                    rows.append(lines[0])
                rows.append(lines[1])
    (args.output / "residency.csv").write_text("\n".join(rows) + "\n")
    print(f"Benchmark results: {args.output}", flush=True)


if __name__ == "__main__":
    main()

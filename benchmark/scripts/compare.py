#!/usr/bin/env python3
"""Compare two hyperfine benchmark results."""
import json
import sys

THRESHOLD = 0.05  # 5% regression threshold


def compare(baseline_path, current_path):
    with open(baseline_path) as f:
        baseline = {r['command']: r['mean'] for r in json.load(f)['results']}
    with open(current_path) as f:
        current = {r['command']: r['mean'] for r in json.load(f)['results']}

    print("| Benchmark | Baseline | Current | Change | Status |")
    print("|-----------|----------|---------|--------|--------|")

    for name in sorted(baseline.keys()):
        if name not in current:
            continue
        base_ms = baseline[name] * 1000
        curr_ms = current[name] * 1000
        change = (curr_ms - base_ms) / base_ms
        status = "REGRESS" if change > THRESHOLD else "IMPROVE" if change < -THRESHOLD else "OK"
        print(f"| {name} | {base_ms:.1f}ms | {curr_ms:.1f}ms | {change:+.1%} | {status} |")


if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: compare.py <baseline.json> <current.json>")
        sys.exit(1)
    compare(sys.argv[1], sys.argv[2])

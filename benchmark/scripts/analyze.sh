#!/bin/bash
# Full analysis output
# Usage: ./benchmark/scripts/analyze.sh examples/aoc2018_day05.santa [baseline.json]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$(dirname "$SCRIPT_DIR")")"

FILE="$1"
BASELINE="${2:-$SCRIPT_DIR/../results/baseline.json}"

if [ -z "$FILE" ]; then
    echo "Usage: analyze.sh <file.santa> [baseline.json]"
    exit 1
fi

echo "=========================================="
echo "FULL ANALYSIS: $FILE"
echo "=========================================="

echo ""
echo "=== 1. CORRECTNESS ==="
cargo test -p lang --quiet 2>&1 | tail -10 || echo "Tests failed"

echo ""
echo "=== 2. SINGLE RUN TIMING ==="
cargo build --release -p santa-cli --manifest-path "$PROJECT_ROOT/Cargo.toml" 2>/dev/null
"$PROJECT_ROOT/target/release/santa-cli" "$FILE"

echo ""
echo "=== 3. PROFILE (generating flamegraph.svg) ==="
cargo build --profile profiling --features profile -p santa-cli --manifest-path "$PROJECT_ROOT/Cargo.toml" 2>/dev/null
"$PROJECT_ROOT/target/profiling/santa-cli" -p "$FILE" 2>&1 || echo "Profiling failed"

echo ""
echo "=== 4. BENCHMARK COMPARISON ==="
if [ -f "$BASELINE" ]; then
    hyperfine --warmup 2 --runs 5 --export-json /tmp/current.json \
        --command-name "$(basename "$FILE" .santa)" "$PROJECT_ROOT/target/release/santa-cli $FILE" 2>/dev/null
    python3 "$SCRIPT_DIR/compare.py" "$BASELINE" /tmp/current.json 2>/dev/null || echo "No baseline match for this file"
    rm -f /tmp/current.json
else
    echo "No baseline found at $BASELINE"
fi

echo ""
echo "=========================================="
echo "ANALYSIS COMPLETE"
echo "=========================================="

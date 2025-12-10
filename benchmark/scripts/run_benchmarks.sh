#!/bin/bash
# Run hyperfine benchmarks on selected AOC solutions
# Outputs JSON for comparison

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$(dirname "$SCRIPT_DIR")")"
RESULTS_DIR="$SCRIPT_DIR/../results"

WARMUP=3
RUNS=10
OUTPUT="$RESULTS_DIR/$(date +%Y%m%d_%H%M%S).json"

echo "Building release binary..."
cargo build --release -p santa-cli --manifest-path "$PROJECT_ROOT/Cargo.toml"

CLI="$PROJECT_ROOT/target/release/santa-cli"
EXAMPLES="$PROJECT_ROOT/examples"

echo "Running benchmarks..."
hyperfine --warmup "$WARMUP" --runs "$RUNS" --export-json "$OUTPUT" \
  --command-name 'aoc2018_day01' "$CLI $EXAMPLES/aoc2018_day01.santa" \
  --command-name 'aoc2018_day05' "$CLI $EXAMPLES/aoc2018_day05.santa" \
  --command-name 'aoc2022_day01' "$CLI $EXAMPLES/aoc2022_day01.santa" \
  --command-name 'aoc2022_day05' "$CLI $EXAMPLES/aoc2022_day05.santa" \
  --command-name 'aoc2023_day01' "$CLI $EXAMPLES/aoc2023_day01.santa"

echo ""
echo "Results saved to: $OUTPUT"

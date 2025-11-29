#!/bin/bash

# AoC 2022 Benchmark Script
# Compares Blitzen VM performance against a baseline santa-cli binary

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

# Default settings
WARMUP=5
RUNS=20
TIMEOUT=120
IGNORE_FAILURES=false

usage() {
    echo "Usage: $0 <baseline-binary> [OPTIONS]"
    echo ""
    echo "Arguments:"
    echo "  baseline-binary       Path to the baseline santa-cli binary"
    echo ""
    echo "Options:"
    echo "  -w, --warmup N        Number of warmup runs (default: $WARMUP)"
    echo "  -r, --runs N          Number of benchmark runs (default: $RUNS)"
    echo "  -t, --timeout N       Time limit per benchmark in seconds (default: $TIMEOUT)"
    echo "  -i, --ignore-failures Continue on benchmark failures"
    echo "  -h, --help            Show this help message"
    echo ""
    echo "Example:"
    echo "  $0 /path/to/other/santa-cli"
    echo "  $0 /path/to/other/santa-cli --warmup 5 --runs 20"
    exit 0
}

# Parse arguments
if [[ $# -lt 1 ]]; then
    usage
fi

BASELINE=""
while [[ $# -gt 0 ]]; do
    case $1 in
        -w|--warmup)
            WARMUP="$2"
            shift 2
            ;;
        -r|--runs)
            RUNS="$2"
            shift 2
            ;;
        -t|--timeout)
            TIMEOUT="$2"
            shift 2
            ;;
        -i|--ignore-failures)
            IGNORE_FAILURES=true
            shift
            ;;
        -h|--help)
            usage
            ;;
        -*)
            echo "Unknown option: $1"
            usage
            ;;
        *)
            if [[ -z "$BASELINE" ]]; then
                BASELINE="$1"
            else
                echo "Unexpected argument: $1"
                usage
            fi
            shift
            ;;
    esac
done

if [[ -z "$BASELINE" ]]; then
    echo -e "${RED}Error: baseline binary path is required${NC}"
    usage
fi

if [[ ! -f "$BASELINE" ]]; then
    echo -e "${RED}Error: baseline binary not found: $BASELINE${NC}"
    exit 1
fi

# Convert to absolute path
BASELINE="$(cd "$(dirname "$BASELINE")" && pwd)/$(basename "$BASELINE")"

if ! command -v hyperfine &> /dev/null; then
    echo -e "${RED}Error: hyperfine is not installed${NC}"
    echo "Install with: brew install hyperfine"
    exit 1
fi

# Build Blitzen
echo -e "${BOLD}${BLUE}Building Blitzen (release)...${NC}"
if ! cargo build --release -p santa-cli --manifest-path "$PROJECT_ROOT/Cargo.toml" 2>/dev/null; then
    echo -e "${RED}Failed to build${NC}"
    exit 1
fi

BLITZEN="$PROJECT_ROOT/target/release/santa-cli"
if [[ ! -f "$BLITZEN" ]]; then
    echo -e "${RED}Error: Blitzen binary not found at $BLITZEN${NC}"
    exit 1
fi

echo -e "${GREEN}Build successful${NC}"
echo ""

# Print config
echo -e "${BOLD}Benchmark Configuration${NC}"
echo "  Blitzen:  $BLITZEN"
echo "  Baseline: $BASELINE"
echo "  Warmup:   $WARMUP runs"
echo "  Runs:     $RUNS"
echo "  Timeout:  ${TIMEOUT}s per benchmark"
echo "  Ignore failures: $IGNORE_FAILURES"
echo ""

# Create results directory
RESULTS_DIR="$PROJECT_ROOT/benchmark-results"
mkdir -p "$RESULTS_DIR"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
RESULTS_FILE="$RESULTS_DIR/aoc2022_$TIMESTAMP.md"

echo "# AoC 2022 Benchmark Results" > "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"
echo "Date: $(date)" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"
echo "| Day | Blitzen | Baseline | Ratio |" >> "$RESULTS_FILE"
echo "|-----|---------|----------|-------|" >> "$RESULTS_FILE"

# Run benchmarks
echo -e "${BOLD}${BLUE}Running benchmarks...${NC}"
echo ""

cd "$SCRIPT_DIR"

for day in $(seq -w 1 25); do
    SANTA_FILE="$SCRIPT_DIR/aoc2022_day${day}.santa"

    if [[ ! -f "$SANTA_FILE" ]]; then
        continue
    fi

    # Run hyperfine and capture JSON output
    JSON_FILE=$(mktemp)

    HYPERFINE_OPTS=(
        --warmup "$WARMUP"
        --runs "$RUNS"
        --time-limit "$TIMEOUT"
        --export-json "$JSON_FILE"
        --shell=none
    )

    if [[ "$IGNORE_FAILURES" == "true" ]]; then
        HYPERFINE_OPTS+=(--ignore-failure)
    fi

    HYPERFINE_OUTPUT=$(hyperfine \
        "${HYPERFINE_OPTS[@]}" \
        -n "Blitzen" "$BLITZEN $SANTA_FILE" \
        -n "Baseline" "$BASELINE $SANTA_FILE" \
        2>&1) || true

    # Extract times from JSON
    BLITZEN_TIME=$(jq -r '.results[] | select(.command | contains("Blitzen")) | .mean' "$JSON_FILE" 2>/dev/null || echo "N/A")
    BASELINE_TIME=$(jq -r '.results[] | select(.command | contains("Baseline")) | .mean' "$JSON_FILE" 2>/dev/null || echo "N/A")

    # Print compact single-line result
    if [[ "$BLITZEN_TIME" != "N/A" && "$BASELINE_TIME" != "N/A" ]]; then
        RATIO=$(echo "scale=2; $BLITZEN_TIME / $BASELINE_TIME" | bc)
        BLITZEN_MS=$(echo "scale=1; $BLITZEN_TIME * 1000" | bc)
        BASELINE_MS=$(echo "scale=1; $BASELINE_TIME * 1000" | bc)

        # Color the ratio based on performance
        if (( $(echo "$RATIO < 1" | bc -l) )); then
            RATIO_COLOR="${GREEN}"
            INDICATOR="▲"
        elif (( $(echo "$RATIO > 1.1" | bc -l) )); then
            RATIO_COLOR="${RED}"
            INDICATOR="▼"
        else
            RATIO_COLOR="${NC}"
            INDICATOR="≈"
        fi

        printf "${BOLD}Day %s${NC}  Blitzen: %7.1fms  Baseline: %7.1fms  ${RATIO_COLOR}%s %.2fx${NC}\n" \
            "$day" "$BLITZEN_MS" "$BASELINE_MS" "$INDICATOR" "$RATIO"

        echo "| Day $day | ${BLITZEN_MS}ms | ${BASELINE_MS}ms | ${RATIO}x |" >> "$RESULTS_FILE"
    else
        printf "${BOLD}Day %s${NC}  ${RED}Failed${NC}\n" "$day"

        # Check for failures and print error details
        if [[ "$IGNORE_FAILURES" == "true" && "$HYPERFINE_OUTPUT" == *"Ignoring non-zero exit code"* ]]; then
            BLITZEN_ERROR=$("$BLITZEN" "$SANTA_FILE" 2>&1) || {
                echo -e "  ${RED}Blitzen:${NC} $(echo "$BLITZEN_ERROR" | head -1)"
            }
            BASELINE_ERROR=$("$BASELINE" "$SANTA_FILE" 2>&1) || {
                echo -e "  ${RED}Baseline:${NC} $(echo "$BASELINE_ERROR" | head -1)"
            }
        fi
    fi

    rm -f "$JSON_FILE"
done

echo "" >> "$RESULTS_FILE"

# Print summary
echo -e "${BOLD}${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${BOLD}                     BENCHMARK COMPLETE${NC}"
echo -e "${BOLD}${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo ""
echo -e "Results saved to: ${GREEN}$RESULTS_FILE${NC}"
echo ""
cat "$RESULTS_FILE"

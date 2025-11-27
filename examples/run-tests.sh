#!/bin/bash

# Advent of Code Examples Test Runner
# Runs all .santa example files in test mode and reports results

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Default settings
INCLUDE_PATTERN=""
EXCLUDE_PATTERN=""
TIMEOUT=60  # Default 60 second timeout per test

# Parse command line arguments
usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -i, --include PATTERN   Only run tests matching PATTERN (glob)"
    echo "  -e, --exclude PATTERN   Skip tests matching PATTERN (glob)"
    echo "  -t, --timeout SECONDS   Timeout per test (default: 60)"
    echo "  -h, --help              Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                           # Run all tests"
    echo "  $0 -i 'aoc2022_*'            # Run only 2022 tests"
    echo "  $0 -e 'aoc2018_*'            # Skip 2018 tests"
    echo "  $0 -i 'aoc2023_day01'        # Run specific test"
    echo "  $0 -e 'aoc2022_day14' -e 'aoc2022_day17'  # Exclude multiple"
    echo "  $0 -t 30                     # 30 second timeout"
    exit 0
}

# Arrays for multiple exclude patterns
declare -a EXCLUDE_PATTERNS=()

while [[ $# -gt 0 ]]; do
    case $1 in
        -i|--include)
            INCLUDE_PATTERN="$2"
            shift 2
            ;;
        -e|--exclude)
            EXCLUDE_PATTERNS+=("$2")
            shift 2
            ;;
        -t|--timeout)
            TIMEOUT="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        *)
            echo "Unknown option: $1"
            usage
            ;;
    esac
done

# Function to check if a name matches any exclude pattern
is_excluded() {
    local name="$1"
    for pattern in "${EXCLUDE_PATTERNS[@]}"; do
        if [[ "$name" == $pattern ]]; then
            return 0  # true, is excluded
        fi
    done
    return 1  # false, not excluded
}

# Results tracking
declare -a PASSED_TESTS=()
declare -a FAILED_TESTS=()
declare -a SKIPPED_TESTS=()
declare -a ERROR_TESTS=()
declare -a TIMEOUT_TESTS=()

# Build the CLI first
echo -e "${BOLD}${BLUE}Building santa-cli...${NC}"
if ! cargo build --release -p santa-cli --manifest-path "$PROJECT_ROOT/Cargo.toml" 2>/dev/null; then
    echo -e "${RED}Failed to build CLI${NC}"
    exit 1
fi

CLI="$PROJECT_ROOT/target/release/santa-cli"
if [ ! -f "$CLI" ]; then
    echo -e "${RED}CLI binary not found at $CLI${NC}"
    exit 1
fi

echo -e "${GREEN}Build successful${NC}"

# Show filter info if any
if [ -n "$INCLUDE_PATTERN" ]; then
    echo -e "${CYAN}Include pattern: $INCLUDE_PATTERN${NC}"
fi
if [ ${#EXCLUDE_PATTERNS[@]} -gt 0 ]; then
    echo -e "${CYAN}Exclude patterns: ${EXCLUDE_PATTERNS[*]}${NC}"
fi
echo ""

# Run tests
echo -e "${BOLD}${BLUE}Running example tests...${NC}\n"

cd "$SCRIPT_DIR"

TOTAL=0
FILTERED=0
for santa_file in *.santa; do
    [ -f "$santa_file" ] || continue

    # Extract base name for display
    base_name="${santa_file%.santa}"

    # Check include pattern
    if [ -n "$INCLUDE_PATTERN" ]; then
        if [[ ! "$base_name" == $INCLUDE_PATTERN ]]; then
            FILTERED=$((FILTERED + 1))
            continue
        fi
    fi

    # Check exclude patterns
    if is_excluded "$base_name"; then
        FILTERED=$((FILTERED + 1))
        continue
    fi

    TOTAL=$((TOTAL + 1))
    printf "  %-30s " "$base_name"

    # Run the test with timeout and capture output
    exit_code=0
    output=$(timeout "$TIMEOUT" "$CLI" -t "$santa_file" 2>&1) || exit_code=$?

    case $exit_code in
        0)
            echo -e "${GREEN}PASS${NC}"
            PASSED_TESTS+=("$base_name")
            ;;
        3)
            echo -e "${RED}FAIL${NC}"
            FAILED_TESTS+=("$base_name|$output")
            ;;
        2)
            echo -e "${YELLOW}ERROR${NC}"
            ERROR_TESTS+=("$base_name|$output")
            ;;
        124)
            echo -e "${RED}TIMEOUT${NC}"
            TIMEOUT_TESTS+=("$base_name|Exceeded ${TIMEOUT}s timeout")
            ;;
        *)
            echo -e "${YELLOW}SKIP${NC}"
            SKIPPED_TESTS+=("$base_name|$output")
            ;;
    esac
done

# Print summary
echo ""
echo -e "${BOLD}${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${BOLD}                          TEST SUMMARY${NC}"
echo -e "${BOLD}${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo ""

PASSED=${#PASSED_TESTS[@]}
FAILED=${#FAILED_TESTS[@]}
ERRORS=${#ERROR_TESTS[@]}
SKIPPED=${#SKIPPED_TESTS[@]}
TIMEOUTS=${#TIMEOUT_TESTS[@]}

echo -e "  ${GREEN}Passed:${NC}   $PASSED"
echo -e "  ${RED}Failed:${NC}   $FAILED"
echo -e "  ${YELLOW}Errors:${NC}   $ERRORS"
echo -e "  ${RED}Timeouts:${NC} $TIMEOUTS"
echo -e "  ${YELLOW}Skipped:${NC}  $SKIPPED"
echo -e "  ${CYAN}Total:${NC}    $TOTAL"
if [ $FILTERED -gt 0 ]; then
    echo -e "  ${CYAN}Filtered:${NC} $FILTERED"
fi
echo ""

# Print details for failures
if [ ${#FAILED_TESTS[@]} -gt 0 ]; then
    echo -e "${BOLD}${RED}Failed Tests:${NC}"
    echo -e "${RED}───────────────────────────────────────────────────────────────${NC}"
    for entry in "${FAILED_TESTS[@]}"; do
        name="${entry%%|*}"
        output="${entry#*|}"
        echo -e "\n  ${BOLD}$name${NC}"
        echo "$output" | sed 's/^/    /'
    done
    echo ""
fi

# Print details for timeouts
if [ ${#TIMEOUT_TESTS[@]} -gt 0 ]; then
    echo -e "${BOLD}${RED}Timeout Tests:${NC}"
    echo -e "${RED}───────────────────────────────────────────────────────────────${NC}"
    for entry in "${TIMEOUT_TESTS[@]}"; do
        name="${entry%%|*}"
        output="${entry#*|}"
        echo -e "\n  ${BOLD}$name${NC}"
        echo "$output" | sed 's/^/    /'
    done
    echo ""
fi

# Print details for errors
if [ ${#ERROR_TESTS[@]} -gt 0 ]; then
    echo -e "${BOLD}${YELLOW}Runtime Errors:${NC}"
    echo -e "${YELLOW}───────────────────────────────────────────────────────────────${NC}"
    for entry in "${ERROR_TESTS[@]}"; do
        name="${entry%%|*}"
        output="${entry#*|}"
        echo -e "\n  ${BOLD}$name${NC}"
        echo "$output" | sed 's/^/    /'
    done
    echo ""
fi

# Final status line
echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
if [ $FAILED -eq 0 ] && [ $ERRORS -eq 0 ] && [ $TIMEOUTS -eq 0 ]; then
    echo -e "${BOLD}${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${BOLD}${RED}Some tests failed or had errors${NC}"
    exit 1
fi

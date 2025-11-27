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

# Results tracking
declare -a PASSED_TESTS=()
declare -a FAILED_TESTS=()
declare -a SKIPPED_TESTS=()
declare -a ERROR_TESTS=()

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

echo -e "${GREEN}Build successful${NC}\n"

# Run tests
echo -e "${BOLD}${BLUE}Running example tests...${NC}\n"

cd "$SCRIPT_DIR"

TOTAL=0
for santa_file in *.santa; do
    [ -f "$santa_file" ] || continue
    TOTAL=$((TOTAL + 1))

    # Extract base name for display
    base_name="${santa_file%.santa}"
    printf "  %-30s " "$base_name"

    # Run the test and capture output
    exit_code=0
    output=$("$CLI" -t "$santa_file" 2>&1) || exit_code=$?

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

echo -e "  ${GREEN}Passed:${NC}  $PASSED"
echo -e "  ${RED}Failed:${NC}  $FAILED"
echo -e "  ${YELLOW}Errors:${NC}  $ERRORS"
echo -e "  ${YELLOW}Skipped:${NC} $SKIPPED"
echo -e "  ${CYAN}Total:${NC}   $TOTAL"
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
if [ $FAILED -eq 0 ] && [ $ERRORS -eq 0 ]; then
    echo -e "${BOLD}${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${BOLD}${RED}Some tests failed or had errors${NC}"
    exit 1
fi

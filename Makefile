.DEFAULT_GOAL := help

.PHONY: *

help: ## Display this help message
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n"} /^[a-zA-Z\/_%-]+:.*?##/ { printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)

##@ Development

build: ## Build debug binary
	cargo build -p santa-cli

release: ## Build release binary
	cargo build --release -p santa-cli

fmt: ## Format code with rustfmt
	cargo fmt

repl: ## Start interactive REPL
	cargo run -p santa-cli -- -r

run: ## Run a script (FILE=path/to/script.santa)
	cargo run -p santa-cli -- $(FILE)

run-test: ## Run a script in test mode (FILE=path)
	cargo run -p santa-cli -- -t $(FILE)

##@ Testing/Linting

can-release: lint test ## Run all CI checks (lint + test)

lint: ## Run rustfmt check and clippy
	cargo fmt --check
	cargo clippy -- -D warnings

test: ## Run all tests
	cargo test

##@ Benchmarking

bench: ## Run criterion benchmarks
	cargo bench -p benchmarks

bench/run: ## Run hyperfine benchmarks
	./benchmark/scripts/run_benchmarks.sh

bench/compare: ## Compare results (BASELINE=file CURRENT=file)
	python3 benchmark/scripts/compare.py $(BASELINE) $(CURRENT)

test-examples: ## Run example test suite
	./examples/run-tests.sh

##@ Profiling

profile: ## Run with CPU profiling (FILE=path) - outputs flamegraph.svg
	cargo build --profile profiling --features profile -p santa-cli
	./target/profiling/santa-cli -p $(FILE)

##@ Analysis

analyze: ## Full analysis (FILE=path) - tests + timing + profile
	@echo "=========================================="
	@echo "ANALYSIS: $(FILE)"
	@echo "=========================================="
	@echo ""
	@echo "--- CORRECTNESS (tests) ---"
	@cargo test -p lang --quiet 2>&1 | tail -5
	@echo ""
	@echo "--- TIMING (single run) ---"
	@cargo build --release -p santa-cli 2>/dev/null
	@./target/release/santa-cli $(FILE) 2>&1 | grep -E "(Part|ms|Error)" || true
	@echo ""
	@echo "--- PROFILE (generating flamegraph.svg) ---"
	@cargo build --profile profiling --features profile -p santa-cli 2>/dev/null
	@./target/profiling/santa-cli -p $(FILE) 2>&1 || true
	@echo ""
	@echo "=========================================="

##@ Maintenance

clean: ## Clean build artifacts
	cargo clean

install: ## Install release binary to ~/.cargo/bin
	cargo install --path runtime/cli

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

test-examples: ## Run example test suite
	./examples/run-tests.sh

##@ Maintenance

clean: ## Clean build artifacts
	cargo clean

install: ## Install release binary to ~/.cargo/bin
	cargo install --path runtime/cli

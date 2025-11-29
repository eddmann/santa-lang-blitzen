.PHONY: build release test check fmt clippy bench clean run repl

# Default target
all: check test build

# Build debug binary
build:
	cargo build -p santa-cli

# Build release binary
release:
	cargo build --release -p santa-cli

# Run all tests
test:
	cargo test

# Run all checks (fmt, clippy, test)
check: fmt-check clippy test

# Format code
fmt:
	cargo fmt

# Check formatting without modifying
fmt-check:
	cargo fmt --check

# Run clippy lints
clippy:
	cargo clippy -- -D warnings

# Run criterion benchmarks
bench:
	cargo bench -p benchmarks

# Run example test suite
test-examples:
	./examples/run-tests.sh

# Clean build artifacts
clean:
	cargo clean

# Run a santa script (usage: make run FILE=examples/aoc2022_day01.santa)
run:
	cargo run -p santa-cli -- $(FILE)

# Run a santa script in test mode
run-test:
	cargo run -p santa-cli -- -t $(FILE)

# Start REPL
repl:
	cargo run -p santa-cli -- -r

# Install release binary to ~/.cargo/bin
install:
	cargo install --path runtime/cli

# Help
help:
	@echo "Available targets:"
	@echo "  build         - Build debug binary"
	@echo "  release       - Build release binary"
	@echo "  test          - Run all tests"
	@echo "  check         - Run fmt-check, clippy, and test"
	@echo "  fmt           - Format code with rustfmt"
	@echo "  fmt-check     - Check formatting without modifying"
	@echo "  clippy        - Run clippy lints"
	@echo "  bench         - Run criterion benchmarks"
	@echo "  test-examples - Run example test suite"
	@echo "  clean         - Clean build artifacts"
	@echo "  run           - Run a script (FILE=path/to/script.santa)"
	@echo "  run-test      - Run a script in test mode"
	@echo "  repl          - Start interactive REPL"
	@echo "  install       - Install release binary to ~/.cargo/bin"

## santa-lang Implementation

This is **Blitzen**, a santa-lang reindeer implementation. santa-lang is a functional programming language designed for solving Advent of Code puzzles. Multiple implementations exist to explore different execution models.

## Project Overview

- **Blitzen**: Bytecode virtual machine written in Rust
- Stack-based architecture: Source → Lexer → Parser → Compiler → Blitzen VM (FrostByte bytecode)
- Batteries-included standard library for AoC patterns

## Makefile

**Always use Makefile targets.** Never run build tools directly.

- Run `make help` to see all available targets
- `make fmt` for code formatting
- `make test` for running tests
- `make can-release` before submitting a PR (runs lint + all tests)

This ensures consistent, reproducible builds across all environments.

## Setup

```bash
# Requires Rust 1.91.1 (see rust-toolchain.toml)
make build              # Debug build
make release            # Release build (LTO, strip)
make install            # Install to ~/.cargo/bin
```

## Common Commands

```bash
make help               # Show available targets
make fmt                # Format code (rustfmt)
make lint               # rustfmt check + clippy -D warnings
make test               # Run all unit tests
make test-examples      # Run 100+ AoC example files
make can-release        # Run before submitting PR (lint + all tests)
make repl               # Interactive REPL
make run FILE=<path>    # Execute script
make run-test FILE=<path>  # Run tests in script
make bench              # Criterion micro-benchmarks
make profile FILE=<path>   # CPU profiling with flamegraph
```

## Code Conventions

- **Edition**: Rust 2024
- **Toolchain**: 1.91.1 (rust-toolchain.toml)
- **Formatting**: `max_width = 100` (rustfmt.toml)
- **Linting**: `clippy -D warnings`
- **Testing**: `expect_test` for snapshot testing
- **Structure**: `lang/` (lexer, parser, vm, runner) + `runtime/cli/` + `benchmarks/`

## Tests & CI

- **CI** (`test.yml`): Runs `make can-release` on ubuntu-24.04
- **Build** (`build.yml`): Multi-platform builds (linux/macos, amd64/arm64), Docker
- Auto-updates `draft-release` branch after tests pass

## PR & Workflow Rules

- **Branches**: `main` for development, `draft-release` auto-updated
- **CI gates**: Must pass lint + test
- **Release**: release-drafter generates notes from PR labels

## Security & Gotchas

- **Custom im-rs fork**: Uses `https://github.com/eddmann/im-rs.git` with pool feature
- **Profile feature**: CPU profiling requires `--features profile`
- **Test timeout**: Example runner defaults to 60s; some tests marked `@slow`
- **Cross-compilation**: ARM64 Linux requires manual linker setup in CI

## Related Implementations

Other santa-lang reindeer (for cross-reference and consistency checks):

| Codename | Type | Language | Local Path | Repository |
|----------|------|----------|------------|------------|
| **Comet** | Tree-walking interpreter | Rust | `~/Projects/santa-lang-comet` | `github.com/eddmann/santa-lang-comet` |
| **Blitzen** | Bytecode VM | Rust | `~/Projects/santa-lang-blitzen` | `github.com/eddmann/santa-lang-blitzen` |
| **Dasher** | LLVM native compiler | Rust | `~/Projects/santa-lang-dasher` | `github.com/eddmann/santa-lang-dasher` |
| **Donner** | JVM bytecode compiler | Kotlin | `~/Projects/santa-lang-donner` | `github.com/eddmann/santa-lang-donner` |
| **Prancer** | Tree-walking interpreter | TypeScript | `~/Projects/santa-lang-prancer` | `github.com/eddmann/santa-lang-prancer` |

Language specification and documentation: `~/Projects/santa-lang` or `github.com/eddmann/santa-lang`

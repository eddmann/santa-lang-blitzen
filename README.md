<p align="center"><a href="https://eddmann.com/santa-lang/"><img src="./docs/logo.png" alt="santa-lang" width="400px" /></a></p>

# santa-lang Blitzen

Bytecode virtual machine implementation of [santa-lang](https://eddmann.com/santa-lang/), written in Rust.

## Overview

santa-lang is a functional, expression-oriented programming language designed for solving Advent of Code puzzles. This implementation uses a bytecode compilation approach, compiling to FrostByte bytecode and executing on a stack-based virtual machine.

Key language features:

- First-class functions and closures with tail-call optimization
- Pipeline and composition operators for expressive data flow
- Persistent immutable data structures
- Lazy sequences and infinite ranges
- Pattern matching with guards
- [Rich built-in function library](https://eddmann.com/santa-lang/builtins/)
- AoC runner with automatic input fetching

## Architecture

```
Source Code → Lexer → Parser → Compiler → Blitzen VM
                                  ↓
                            FrostByte Bytecode
```

| Component      | Description                                         |
| -------------- | --------------------------------------------------- |
| **Lexer**      | Tokenizes source into keywords, operators, literals |
| **Parser**     | Builds an Abstract Syntax Tree (AST)                |
| **Compiler**   | Translates AST to FrostByte bytecode                |
| **Blitzen VM** | Stack-based virtual machine that executes bytecode  |

The **FrostByte** bytecode format includes instructions for stack manipulation, variable access, arithmetic, control flow, collection operations, and function calls.

For detailed implementation internals, see [ARCHITECTURE.md](docs/ARCHITECTURE.md).

## Installation

### Docker

```bash
docker pull ghcr.io/eddmann/santa-lang-blitzen:cli-latest
docker run --rm ghcr.io/eddmann/santa-lang-blitzen:cli-latest --help
```

### Release Binaries

Download pre-built binaries from [GitHub Releases](https://github.com/eddmann/santa-lang-blitzen/releases):

| Platform              | Artifact                                       |
| --------------------- | ---------------------------------------------- |
| Linux (x86_64)        | `santa-lang-blitzen-cli-{version}-linux-amd64` |
| Linux (ARM64)         | `santa-lang-blitzen-cli-{version}-linux-arm64` |
| macOS (Intel)         | `santa-lang-blitzen-cli-{version}-macos-amd64` |
| macOS (Apple Silicon) | `santa-lang-blitzen-cli-{version}-macos-arm64` |

## Usage

```bash
# Run a solution
santa-cli solution.santa

# Run tests defined in a solution
santa-cli -t solution.santa

# Interactive REPL
santa-cli -r

# Or use make targets:
make run FILE=examples/aoc2022_day01.santa
make run-test FILE=examples/aoc2022_day01.santa
make repl
```

## Example

Here's a complete Advent of Code solution (2015 Day 1):

```santa
input: read("aoc://2015/1")

part_one: {
  input |> fold(0) |floor, direction| {
    if direction == "(" { floor + 1 } else { floor - 1 };
  }
}

part_two: {
  zip(1.., input) |> fold(0) |floor, [index, direction]| {
    let next_floor = if direction == "(" { floor + 1 } else { floor - 1 };
    if next_floor < 0 { break index } else { next_floor };
  }
}

test: {
  input: "()())"
  part_one: -1
  part_two: 5
}
```

Key language features shown:

- **`input:`** / **`part_one:`** / **`part_two:`** - AoC runner sections
- **`|>`** - Pipeline operator (thread value through functions)
- **`fold`** - Reduce with early exit support via `break`
- **`test:`** - Inline test cases with expected values

## Building

Requires Rust 1.91+ or use Docker:

```bash
# Build CLI (debug)
make build

# Build CLI (release)
make release

# Run tests
make test

# Run linting
make lint
```

## Development

Run `make help` to see all available targets:

```bash
make help          # Show all targets
make can-release   # Run all CI checks (lint + test)
make lint          # Run rustfmt check and clippy
make fmt           # Format code
make test          # Run all tests
make bench         # Run criterion benchmarks
make test-examples # Run example test suite
```

### Scripts

**Test Runner** - Run all example solutions in test mode:

```bash
make test-examples
# or directly:
./examples/run-tests.sh
```

Options: `-i, --include PATTERN`, `-e, --exclude PATTERN`, `-t, --timeout SECONDS`

**Benchmark Script** - Compare Blitzen VM performance against another santa-cli binary:

```bash
./examples/benchmark-2022.sh /path/to/baseline/santa-cli
```

Requires [hyperfine](https://github.com/sharkdp/hyperfine). Results saved to `benchmark-results/`.

**Criterion Benchmarks** - Run micro-benchmarks for VM components:

```bash
make bench
```

HTML reports are generated in `target/criterion/`.

## Project Structure

```
├── lang/                   # Core language library
│   └── src/
│       ├── lexer/          # Tokenization
│       ├── parser/         # AST construction
│       ├── vm/             # Compiler, bytecode, runtime
│       └── runner/         # AoC runner support
├── runtime/cli/            # Command-line interface
├── examples/               # AoC solutions (.santa files)
└── benchmarks/             # Criterion benchmarks
```

## Other Reindeer

The language has been implemented multiple times to explore different execution models and technologies.

| Codename | Type | Language |
|----------|------|----------|
| [Comet](https://github.com/eddmann/santa-lang-comet) | Tree-walking interpreter | Rust |
| [Blitzen](https://github.com/eddmann/santa-lang-blitzen) | Bytecode VM | Rust |
| [Dasher](https://github.com/eddmann/santa-lang-dasher) | LLVM native compiler | Rust |
| [Donner](https://github.com/eddmann/santa-lang-donner) | JVM bytecode compiler | Kotlin |
| [Vixen](https://github.com/eddmann/santa-lang-vixen) | Embedded bytecode VM | C |
| [Prancer](https://github.com/eddmann/santa-lang-prancer) | Tree-walking interpreter | TypeScript |

## License

MIT License - see [LICENSE](LICENSE) for details.

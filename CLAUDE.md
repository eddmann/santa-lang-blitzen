# santa-lang Blitzen Development Guidelines

Guidelines for Claude Code when developing the santa-lang's Blitzen VM.

## Development Approach

### Test-Driven Development (TDD)

- **Classic Testing School**: Write tests first, then implementation
- Red-Green-Refactor cycle for each feature
- Tests live in separate `tests.rs` files alongside the code they test
- Use `expect_test` for lexer and parser snapshot tests
- Unit tests for individual components, integration tests for full programs

### Code Style

- **Descriptive naming over comments**: Names should explain intent
- Idiomatic Rust 1.91.1
- All code must pass `cargo clippy` with no warnings
- Run `cargo fmt` before committing
- Prefer composition over inheritance
- Small, focused functions

### Dependencies

When adding new dependencies, always use the latest stable version from crates.io.

- **im-rs**: `git = "ssh://git@github.com/eddmann/im-rs.git"` (persistent collections)
- **ordered-float**: For decimal value handling
- **unicode-segmentation**: For grapheme cluster string indexing
- **regex**: For pattern matching functions
- **expect-test**: For snapshot testing (dev dependency)
- **criterion**: For benchmarks

## Project Structure

```
├── Cargo.toml                 # Workspace root
├── lang/
│   ├── Cargo.toml
│   └── src/
│       ├── lib.rs
│       ├── lexer/
│       │   ├── mod.rs
│       │   ├── token.rs
│       │   └── tests.rs
│       ├── parser/
│       │   ├── mod.rs
│       │   ├── ast.rs
│       │   └── tests.rs
│       ├── vm/
│       │   ├── mod.rs
│       │   ├── compiler.rs
│       │   ├── bytecode.rs
│       │   ├── value.rs
│       │   ├── runtime.rs
│       │   └── tests.rs
│       └── runner/
│           ├── mod.rs
│           └── tests.rs
├── runtime/
│   └── cli/
│       ├── Cargo.toml
│       └── src/
│           ├── main.rs
│           └── external.rs
└── benchmarks/
    ├── Cargo.toml
    └── benches/
        └── vm_benchmarks.rs
```

## Workflow

### Resuming Work

Before starting any work, always:

1. Run `git log --oneline -10` to see recent commits and current progress
2. Read PLAN.md to find the current phase and check release gate status
3. Look for `[x]` completed items vs `[ ]` pending items in release gates

### Starting a New Phase

1. Read the phase requirements in PLAN.md
2. Reference LANG.txt sections mentioned in the phase
3. Write failing tests first
4. Implement until tests pass
5. Run `cargo clippy` and fix warnings
6. Verify all release gate criteria are met

### Completing a Phase

1. Run `cargo test` and `cargo clippy` - all must pass
2. Mark release gate items as complete in PLAN.md: `[ ]` → `[x]`
3. Commit with conventional commit message
4. Move to next phase

### Testing Commands

```bash
cargo test                    # Run all tests
cargo test --lib             # Run library tests only
cargo test -p lang   # Run lang crate tests
cargo clippy                 # Check for lints
cargo bench                  # Run benchmarks
```

### Commit Guidelines

- Commit after each release gate is passed
- Use conventional commit messages
- Reference the phase number in commit message
- Examples:
  - `feat(lexer): implement token types [Phase 1]`
  - `feat(parser): add expression parsing [Phase 2]`
  - `feat(vm): implement core execution loop [Phase 7]`

## Source of Truth

**LANG.txt** is the authoritative specification. When in doubt:

1. Check LANG.txt for the correct behavior
2. Reference the specific section number
3. Write a test that validates the LANG.txt behavior
4. Implement to pass the test

## Key Implementation Notes

### Value Types (from LANG.txt)

- 10 types: Nil, Integer, Decimal, Boolean, String, List, Set, Dict, Function, LazySequence
- Use im-rs Vector/HashSet/HashMap for persistent collections
- Hashability rules per LANG.txt §3.11

### Operator Precedence (from LANG.txt §14.5)

Follow the exact precedence table - this is critical for correct parsing.

### Built-in Functions

58 total functions per Appendix B. Each must match the exact signature and behavior.

### Tail-Call Optimization

Only self-recursive calls in tail position. Transform to loop, not trampoline.

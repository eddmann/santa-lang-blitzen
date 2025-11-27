# Blitzen: Santa-Lang VM Implementation Plan

A stack-based bytecode VM implementation of santa-lang with 100% feature parity.

## Source of Truth

**LANG.txt** is the authoritative specification. All implementation decisions MUST conform to LANG.txt. Every phase includes:

- Specific LANG.txt section references
- Tests derived from LANG.txt examples
- Validation against LANG.txt-defined behaviors

## LANG.txt Coverage Checklist

| Section                  | Description                                 | Phase              |
| ------------------------ | ------------------------------------------- | ------------------ |
| §2 Lexical Structure     | Tokens, keywords, literals, comments        | Phase 1            |
| §3 Type System           | All 10 value types, hashability             | Phase 4            |
| §4 Operators             | Arithmetic, comparison, logical, precedence | Phases 2, 5, 7     |
| §5 Variables & Bindings  | let, mut, destructuring, shadowing          | Phases 3, 6        |
| §6 Expressions           | Literals, blocks, calls, infix, pipeline    | Phase 2            |
| §7 Control Flow          | if, if-let, match, return, break            | Phases 3, 6        |
| §8 Functions             | Lambdas, closures, partial, TCO, memoize    | Phases 2, 5, 8, 14 |
| §9 Pattern Matching      | All pattern types, guards                   | Phases 3, 15       |
| §10 Collections          | List, Set, Dict, Range, LazySeq             | Phases 4, 12       |
| §11 Built-in Functions   | All 58 functions                            | Phases 9-13        |
| §12 AOC Runner           | Sections, tests, script mode                | Phase 16           |
| §13 External Functions   | read, puts, env                             | Phase 18           |
| §14 Semantics            | Truthiness, precedence, scoping             | Phases 4, 7        |
| §15 Implementation Notes | Error handling, TCO requirements            | Phases 14, 17      |
| Appendix A               | Grammar (EBNF)                              | Phases 1-3         |
| Appendix B               | Built-in function reference                 | Phases 9-13        |
| Appendix C               | Operator precedence table                   | Phase 2            |
| Appendix D               | Example programs (integration tests)        | Phase 20           |

## Project Setup Files

Before Phase 1, create these project files:

1. **PLAN.md** - This plan document committed to project root
2. **CLAUDE.md** - Development guidelines for Claude Code
3. **continue.sh** - Script to resume development with Claude
4. **.gitignore** - Ignore santa-lang-rs reference implementation

---

## Project Structure

```
/
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

## Dependencies

```toml
# lang/Cargo.toml
[dependencies]
im = { git = "ssh://git@github.com/eddmann/im-rs.git" }
ordered-float = "4.2"
unicode-segmentation = "1.10"
regex = "1.10"

[dev-dependencies]
expect-test = "1.5"
```

---

## Phase 1: Lexer

**Goal**: Tokenize santa-lang source into a stream of tokens.

**LANG.txt Reference**: §2 Lexical Structure, Appendix A (Grammar - lexical rules)

### 1.1 Token Types

Define all token types per LANG.txt §2:

- Keywords: `let`, `mut`, `if`, `else`, `match`, `return`, `break`, `nil`, `true`, `false`
- Literals: Integer, Decimal, String (with escape sequences)
- Identifiers: `[a-zA-Z][a-zA-Z0-9_?]*`
- Operators: `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `&&`, `||`, `!`, `|>`, `>>`, `..`, `..=`
- Delimiters: `(`, `)`, `[`, `]`, `{`, `}`, `#{`, `,`, `:`, `;`, `|`, `` ` ``
- Comments: `// ...`
- Special: `_` (placeholder/wildcard)

### 1.2 Lexer Implementation

- UTF-8 source input
- Track line/column for error reporting
- Handle underscores in numeric literals (`1_000_000`)
- String escape sequences: `\n`, `\t`, `\"`, `\\`
- Produce `Vec<Token>` with spans

### 1.3 Tests (expect_test snapshots)

```rust
// lexer/tests.rs
#[test]
fn lex_integer_literals() { ... }
#[test]
fn lex_decimal_literals() { ... }
#[test]
fn lex_string_with_escapes() { ... }
#[test]
fn lex_operators() { ... }
#[test]
fn lex_keywords_vs_identifiers() { ... }
#[test]
fn lex_range_operators() { ... }
#[test]
fn lex_comments() { ... }
```

### Release Gate 1

- [x] All token types from LANG.txt Section 2 are lexed correctly
- [x] Error positions (line:column) are accurate
- [x] All expect_test snapshots pass
- [x] `cargo clippy` clean

---

## Phase 2: Parser - Expressions

**Goal**: Parse expressions into an AST using Pratt parsing.

**LANG.txt Reference**: §4 Operators, §6 Expressions, §8.1-8.7 Functions, §14.5 Operator Precedence, Appendix A & C

### 2.1 AST Node Types

```rust
pub enum Expr {
    // Literals
    Integer(i64),
    Decimal(f64),
    String(String),
    Boolean(bool),
    Nil,

    // Collections
    List(Vec<Expr>),
    Set(Vec<Expr>),
    Dict(Vec<(Expr, Expr)>),
    Range { start: Box<Expr>, end: Option<Box<Expr>>, inclusive: bool },

    // Identifiers & Placeholders
    Identifier(String),
    Placeholder,
    RestIdentifier(String),

    // Operations
    Prefix { op: PrefixOp, right: Box<Expr> },
    Infix { left: Box<Expr>, op: InfixOp, right: Box<Expr> },
    Index { collection: Box<Expr>, index: Box<Expr> },

    // Functions
    Function { params: Vec<Param>, body: Box<Expr> },
    Call { function: Box<Expr>, args: Vec<Expr> },
    InfixCall { function: String, left: Box<Expr>, right: Box<Expr> },

    // Control Flow
    If { condition: Box<Expr>, then_branch: Box<Expr>, else_branch: Option<Box<Expr>> },
    IfLet { pattern: Pattern, value: Box<Expr>, then_branch: Box<Expr>, else_branch: Option<Box<Expr>> },
    Match { subject: Box<Expr>, arms: Vec<MatchArm> },
    Block(Vec<Stmt>),

    // Spread
    Spread(Box<Expr>),
}
```

### 2.2 Operator Precedence (from LANG.txt 14.5)

1. `[]` - Index (highest)
2. `()` - Call
3. `!` `-` - Prefix
4. `*` `/` `%` `` ` `` - Multiplicative/Infix
5. `+` `-` - Additive
6. `>>` `|>` `..` `..=` - Composition/Pipeline/Range
7. `<` `<=` `>` `>=` - Comparison
8. `==` `!=` `=` - Equality/Assignment
9. `&&` `||` - Logical (lowest)

### 2.3 Tests

```rust
// parser/tests.rs
#[test]
fn parse_literals() { ... }
#[test]
fn parse_binary_operators() { ... }
#[test]
fn parse_operator_precedence() { ... }
#[test]
fn parse_function_expressions() { ... }
#[test]
fn parse_partial_application() { ... }
#[test]
fn parse_collections() { ... }
#[test]
fn parse_ranges() { ... }
#[test]
fn parse_index_expressions() { ... }
#[test]
fn parse_pipeline() { ... }
#[test]
fn parse_composition() { ... }
```

### Release Gate 2

- [x] All expression forms from LANG.txt parse correctly
- [x] Operator precedence matches specification exactly
- [x] Partial application (`_ + 1`) produces Function AST
- [x] All expect_test snapshots pass
- [x] `cargo clippy` clean

---

## Phase 3: Parser - Statements, Patterns & Sections

**Goal**: Complete the parser with statements, pattern matching, and AOC sections.

**LANG.txt Reference**: §5 Variables & Bindings, §7 Control Flow, §9 Pattern Matching, §12 AOC Runner, Appendix A

### 3.1 Statement Types

```rust
pub enum Stmt {
    Let { mutable: bool, pattern: Pattern, value: Expr },
    Return(Expr),
    Break(Expr),
    Expr(Expr),
}
```

### 3.2 Pattern Types

```rust
pub enum Pattern {
    Wildcard,                           // _
    Identifier(String),                 // x
    RestIdentifier(String),             // ..rest
    Literal(Literal),                   // 42, "hello", true
    List(Vec<Pattern>),                 // [a, b, ..rest]
    Range { start: i64, end: Option<i64>, inclusive: bool },
}
```

### 3.3 AOC Sections (Top-Level)

```rust
pub enum Section {
    Input(Expr),
    PartOne(Expr),
    PartTwo(Expr),
    Test { input: Expr, part_one: Option<Expr>, part_two: Option<Expr> },
}

pub struct Program {
    pub statements: Vec<Stmt>,
    pub sections: Vec<Section>,
}
```

### 3.4 Special Syntax Features

Per LANG.txt:

- **Dict shorthand** (§3.7): `#{name, age}` → `#{"name": name, "age": age}`
- **Trailing comma** (§10.2): `[1, 2, 3,]` is valid
- **Empty set vs empty block** (§3.6): Context-dependent `{}` disambiguation
- **Infix function calls** (§6.5): `` `includes?` `` backtick syntax

### 3.5 Tests

```rust
#[test]
fn parse_let_binding() { ... }
#[test]
fn parse_let_destructuring() { ... }
#[test]
fn parse_mutable_binding() { ... }
#[test]
fn parse_match_expression() { ... }
#[test]
fn parse_match_with_guards() { ... }
#[test]
fn parse_aoc_sections() { ... }
#[test]
fn parse_test_sections() { ... }
#[test]
fn parse_trailing_lambda() { ... }
```

### Release Gate 3

- [x] All statement forms parse correctly
- [x] Destructuring patterns work (including nested, rest)
- [x] Match expressions with guards parse correctly
- [x] AOC sections (`input:`, `part_one:`, `part_two:`, `test:`) parse
- [x] Trailing lambda syntax works
- [x] All expect_test snapshots pass
- [x] `cargo clippy` clean

---

## Phase 4: VM Foundation - Value System & Bytecode

**Goal**: Define the runtime value system and bytecode instruction set.

**LANG.txt Reference**: §3 Type System (all types), §3.11 Hashability, §14.1 Truthy/Falsy Values

### 4.1 Value Types (im-rs Persistent Collections)

```rust
use im::{Vector, HashSet, HashMap};

pub enum Value {
    Nil,
    Integer(i64),
    Decimal(OrderedFloat<f64>),
    Boolean(bool),
    String(Rc<String>),
    List(Vector<Value>),          // im::Vector - persistent vector
    Set(HashSet<Value>),          // im::HashSet - persistent set
    Dict(HashMap<Value, Value>),  // im::HashMap - persistent map
    Function(Rc<Function>),
    LazySequence(Rc<RefCell<LazySeq>>),
    Range { start: i64, end: Option<i64>, inclusive: bool },
}

// For Sets and Dict keys (LANG.txt 3.11 Hashability)
impl Hash for Value { ... }  // Nil, Integer, Decimal, Boolean, String, List (if elements hashable), Set
impl Eq for Value { ... }    // Dict, LazySequence, Function are NOT hashable
```

### 4.2 Collection Operations (defined here, executed in Phase 7)

| Operation | List           | Set        | Dict                     | String           |
| --------- | -------------- | ---------- | ------------------------ | ---------------- |
| `+`       | Concatenate    | Union      | Merge (right precedence) | Concatenate      |
| `-`       | N/A            | Difference | N/A                      | N/A              |
| `*`       | Repeat         | N/A        | N/A                      | Repeat           |
| `[]`      | Index (neg OK) | N/A        | Key lookup               | Index (grapheme) |
| `[a..b]`  | Slice          | N/A        | N/A                      | Slice            |

### 4.3 Bytecode Instructions

```rust
pub enum OpCode {
    // Stack manipulation
    Constant(u16),      // Push constant from pool
    Pop,                // Discard top
    Dup,                // Duplicate top

    // Variables
    GetLocal(u16),      // Load local variable
    SetLocal(u16),      // Store local variable
    GetGlobal(u16),     // Load global by name index
    SetGlobal(u16),     // Store global by name index

    // Arithmetic
    Add, Sub, Mul, Div, Mod, Neg,

    // Comparison
    Eq, Ne, Lt, Le, Gt, Ge,

    // Logical
    Not, And, Or,

    // Collections
    MakeList(u16),      // Create list from N stack values
    MakeSet(u16),       // Create set from N stack values
    MakeDict(u16),      // Create dict from N*2 stack values
    MakeRange,          // start, end -> Range
    Index,              // collection, index -> value

    // Functions
    MakeClosure(u16),   // Create closure from function index
    Call(u8),           // Call with N arguments
    Return,             // Return from function

    // Control flow
    Jump(i16),          // Unconditional jump
    JumpIfFalse(i16),   // Conditional jump
    JumpIfTrue(i16),    // Short-circuit jump

    // Built-ins
    CallBuiltin(u16, u8),  // builtin_id, arg_count

    // Special
    Break,              // Break from iteration
    Spread,             // Spread collection
    Pipeline,           // Pipeline application
}
```

### 4.4 Chunk Structure

```rust
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub lines: Vec<u32>,        // Line number for each instruction
    pub functions: Vec<Rc<CompiledFunction>>,
}

pub struct CompiledFunction {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: Option<String>,
    pub upvalues: Vec<UpvalueDesc>,
}
```

### 4.5 Tests

```rust
#[test]
fn value_equality() { ... }
#[test]
fn value_hashing() { ... }
#[test]
fn value_truthiness() { ... }
#[test]
fn collection_operations() { ... }  // List concat, Set union, Dict merge
#[test]
fn bytecode_encoding() { ... }
```

### Release Gate 4

- [x] All value types implemented with correct equality semantics
- [x] Hashable values work correctly in Sets and Dict keys
- [x] Truthiness matches LANG.txt Section 14.1
- [x] Bytecode instruction set is complete
- [x] All tests pass
- [x] `cargo clippy` clean

---

## Phase 5: Compiler - Expressions

**Goal**: Compile expression AST to bytecode.

**LANG.txt Reference**: §4 Operators, §4.7 Pipeline, §4.8 Composition, §8.4 Partial Application

### 5.1 Compiler Structure

```rust
pub struct Compiler {
    chunk: Chunk,
    locals: Vec<Local>,
    scope_depth: u32,
    enclosing: Option<Box<Compiler>>,
}

impl Compiler {
    pub fn compile(&mut self, expr: &Expr) -> Result<(), CompileError>;
}
```

### 5.2 Expression Compilation

- Literals -> `Constant` instructions
- Binary ops -> compile both sides, emit operator instruction
- Unary ops -> compile operand, emit operator instruction
- Function expressions -> create new Compiler scope, compile body
- Partial application -> detect placeholders, wrap in function
- Collections -> compile elements, emit `MakeList`/`MakeSet`/`MakeDict`
- Ranges -> compile bounds, emit `MakeRange`
- Index -> compile collection and index, emit `Index`
- Pipeline -> rewrite `a |> f(b)` to `f(b, a)`, compile as call
- Composition -> compile as closure creation

### 5.3 Tests

```rust
#[test]
fn compile_integer_literal() { ... }
#[test]
fn compile_binary_expression() { ... }
#[test]
fn compile_nested_expression() { ... }
#[test]
fn compile_function() { ... }
#[test]
fn compile_partial_application() { ... }
#[test]
fn compile_pipeline() { ... }
```

### Release Gate 5

- [x] All expression types compile to correct bytecode
- [x] Partial application generates correct closure
- [x] Pipeline operator compiles correctly
- [x] Constants are deduplicated in pool
- [x] All tests pass
- [x] `cargo clippy` clean

---

## Phase 6: Compiler - Statements & Control Flow

**Goal**: Complete compilation with statements and control flow.

**LANG.txt Reference**: §5 Variables & Bindings, §7 Control Flow, §14.6 Scoping Rules

### 6.1 Statement Compilation

- `let` bindings -> local variable slots, compile initializer
- `let mut` -> mutable local flag
- Destructuring -> pattern compilation with slot allocation
- `return` -> compile value, emit `Return`
- `break` -> compile value, emit `Break` (with jump fixup)

### 6.2 Control Flow Compilation

- `if/else` -> `JumpIfFalse`, compile branches, patch jumps
- `if let` -> compile value, pattern match, conditional jump
- `match` -> compile subject, generate pattern tests with jumps
- Blocks -> new scope, compile statements, pop scope

### 6.3 Scope Management

```rust
struct Local {
    name: String,
    depth: u32,
    mutable: bool,
    captured: bool,
}
```

### 6.4 Tests

```rust
#[test]
fn compile_let_binding() { ... }
#[test]
fn compile_destructuring() { ... }
#[test]
fn compile_if_expression() { ... }
#[test]
fn compile_match_expression() { ... }
#[test]
fn compile_block_scoping() { ... }
#[test]
fn compile_shadowing() { ... }
```

### Release Gate 6

- [ ] Variable scoping works correctly
- [ ] Shadowing behaves per LANG.txt
- [ ] Control flow generates correct jumps
- [ ] Match compilation handles all pattern types
- [ ] Guard clauses compile correctly
- [ ] All tests pass
- [ ] `cargo clippy` clean

---

## Phase 7: VM Runtime - Core Execution

**Goal**: Execute bytecode with a stack-based virtual machine.

**LANG.txt Reference**: §4.1 Type Coercion Rules, §4 All Operators, §14.1 Truthy/Falsy, §14.2 Expression Evaluation

### 7.1 VM Structure

```rust
pub struct VM {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    globals: HashMap<String, Value>,
    builtins: HashMap<String, BuiltinFn>,
}

struct CallFrame {
    closure: Rc<Closure>,
    ip: usize,
    stack_base: usize,
}
```

### 7.2 Execution Loop

```rust
impl VM {
    pub fn run(&mut self) -> Result<Value, RuntimeError> {
        loop {
            let instruction = self.read_byte();
            match instruction {
                OpCode::Constant(idx) => { ... }
                OpCode::Add => { ... }
                // ... all opcodes
            }
        }
    }
}
```

### 7.3 Type Coercion Rules

- Integer + Decimal: left operand determines result type
- String + String: concatenation
- List + List: concatenation
- Set + Set: union
- Dict + Dict: merge (right precedence)

### 7.4 Tests

```rust
#[test]
fn vm_arithmetic() { ... }
#[test]
fn vm_comparison() { ... }
#[test]
fn vm_string_operations() { ... }
#[test]
fn vm_collection_operations() { ... }
#[test]
fn vm_type_coercion() { ... }
#[test]
fn vm_function_call() { ... }
```

### Release Gate 7

- [ ] All arithmetic operations work correctly
- [ ] Type coercion matches LANG.txt Section 4.1
- [ ] Function calls execute correctly
- [ ] Local variables work in functions
- [ ] Basic expressions evaluate correctly
- [ ] All tests pass
- [ ] `cargo clippy` clean

---

## Phase 8: Closures & Upvalues

**Goal**: Implement closures with captured variables.

**LANG.txt Reference**: §8.3 Closures (including mutable capture example)

### 8.1 Upvalue System

```rust
pub struct Upvalue {
    location: UpvalueLocation,
}

enum UpvalueLocation {
    Stack(usize),      // Still on stack
    Heap(Rc<RefCell<Value>>),  // Closed over
}

pub struct Closure {
    function: Rc<CompiledFunction>,
    upvalues: Vec<Rc<RefCell<Upvalue>>>,
}
```

### 8.2 Closure Compilation

- Track captured variables during compilation
- Generate `UpvalueDesc` for each capture
- At runtime, create upvalues when making closure

### 8.3 Mutable Captures

- Mutable variables captured by closures
- Counter example from LANG.txt Section 8.3

### 8.4 Tests

```rust
#[test]
fn closure_capture_local() { ... }
#[test]
fn closure_capture_upvalue() { ... }
#[test]
fn closure_mutable_capture() { ... }
#[test]
fn nested_closures() { ... }
```

### Release Gate 8

- [ ] Simple closures capture variables correctly
- [ ] Nested closures work
- [ ] Mutable captures update correctly
- [ ] Upvalues close over when scope exits
- [ ] Counter example works correctly
- [ ] All tests pass
- [ ] `cargo clippy` clean

---

## Phase 9: Built-in Functions - Core

**Goal**: Implement core built-in functions.

**LANG.txt Reference**: §11.1 Type Conversion, §11.2 Collection Access, §11.3 Collection Modification, Appendix B

### 9.1 Type Conversion

- `int(value)` - parse to integer (round decimals half away from zero)
- `ints(string)` - extract all integers with regex
- `list(value)` - convert to list
- `set(value)` - convert to set
- `dict(value)` - convert to dictionary

### 9.2 Collection Access

- `get(index, collection)` - get element at index
- `size(collection)` - get size
- `first(collection)`, `second(collection)`, `rest(collection)`
- `keys(dict)`, `values(dict)`

### 9.3 Collection Modification

- `push(value, collection)` - add value
- `assoc(key, value, collection)` - associate key
- `update(key, fn, collection)` - update with function
- `update_d(key, default, fn, collection)` - update with default

### 9.4 Tests

```rust
#[test]
fn builtin_int_conversion() { ... }
#[test]
fn builtin_ints_extraction() { ... }
#[test]
fn builtin_collection_access() { ... }
#[test]
fn builtin_collection_modification() { ... }
```

### Release Gate 9

- [ ] All type conversion functions work per spec
- [ ] Collection access handles all types
- [ ] Collection modification is immutable (returns new)
- [ ] Edge cases return nil (not errors) where specified
- [ ] All tests pass
- [ ] `cargo clippy` clean

---

## Phase 10: Built-in Functions - Transformations

**Goal**: Implement higher-order transformation functions.

**LANG.txt Reference**: §11.4 Transformation, §11.5 Reduction, §11.6 Iteration, Appendix B

### 10.1 Transformation Functions

- `map(fn, collection)` - transform each element
- `filter(predicate, collection)` - keep matching
- `flat_map(fn, collection)` - map and flatten
- `filter_map(fn, collection)` - map and filter truthy
- `find_map(fn, collection)` - find first truthy mapped

### 10.2 Reduction Functions

- `reduce(fn, collection)` - reduce with first as initial
- `fold(initial, fn, collection)` - fold with initial
- `fold_s(initial, fn, collection)` - fold with state
- `scan(initial, fn, collection)` - intermediate results

### 10.3 Iteration

- `each(fn, collection)` - side effects, returns nil

### 10.4 Dict/Collection Callbacks

Handle callbacks that receive (value) or (value, key):

```rust
fn call_mapper(&mut self, mapper: &Value, value: Value, key: Option<Value>) -> Value;
```

### 10.5 Tests

```rust
#[test]
fn builtin_map() { ... }
#[test]
fn builtin_filter() { ... }
#[test]
fn builtin_reduce() { ... }
#[test]
fn builtin_fold() { ... }
#[test]
fn builtin_dict_callbacks() { ... }
```

### Release Gate 10

- [ ] All transformation functions work on all collection types
- [ ] Dict callbacks handle both (value) and (value, key) arities
- [ ] Reduce on empty collection throws RuntimeErr
- [ ] Fold on empty collection returns initial
- [ ] All tests pass
- [ ] `cargo clippy` clean

---

## Phase 11: Built-in Functions - Search & Aggregation

**Goal**: Complete search and aggregation functions.

**LANG.txt Reference**: §11.7 Search, §11.8 Aggregation, §11.9 Sequence Manipulation, §11.10 Set Operations, §11.11 Predicates, Appendix B

### 11.1 Search Functions

- `find(predicate, collection)` - find first match
- `count(predicate, collection)` - count matches

### 11.2 Aggregation Functions

- `sum(collection)` - sum all elements
- `max(..values)` - find maximum
- `min(..values)` - find minimum

### 11.3 Sequence Manipulation

- `skip(n, collection)` - skip n elements
- `take(n, collection)` - take n elements
- `sort(comparator, collection)` - sort by comparator
- `reverse(collection)` - reverse order
- `rotate(steps, collection)` - rotate list
- `chunk(size, collection)` - split into chunks

### 11.4 Set Operations

- `union(..collections)` - set union
- `intersection(..collections)` - set intersection

### 11.5 Predicates

- `includes?(collection, value)` - check presence
- `excludes?(collection, value)` - check absence
- `any?(predicate, collection)` - any match
- `all?(predicate, collection)` - all match

### 11.6 Tests

```rust
#[test]
fn builtin_find() { ... }
#[test]
fn builtin_aggregation() { ... }
#[test]
fn builtin_sequence_manipulation() { ... }
#[test]
fn builtin_set_operations() { ... }
#[test]
fn builtin_predicates() { ... }
```

### Release Gate 11

- [ ] All search functions work correctly
- [ ] max/min handle varargs and single collection
- [ ] sort accepts both boolean and integer comparators
- [ ] Set operations work on mixed collection types
- [ ] All tests pass
- [ ] `cargo clippy` clean

---

## Phase 12: Lazy Sequences

**Goal**: Implement infinite lazy sequences.

**LANG.txt Reference**: §3.8 Lazy Sequence, §10.5 Lazy Sequences, §11.12 Lazy Sequence Generation, §11.13 Range Generation, §14.3 Lazy Evaluation

### 12.1 LazySequence Type

```rust
pub enum LazySeq {
    Range { current: i64, end: Option<i64>, inclusive: bool },
    Repeat { value: Value },
    Cycle { source: im::Vector<Value>, index: usize },
    Iterate { generator: Rc<Closure>, current: Value },
    Map { source: Rc<RefCell<LazySeq>>, mapper: Rc<Closure> },
    Filter { source: Rc<RefCell<LazySeq>>, predicate: Rc<Closure> },
    Skip { source: Rc<RefCell<LazySeq>>, remaining: usize },
    Zip { sources: Vec<Rc<RefCell<LazySeq>>> },
    Combinations { source: Vec<Value>, size: usize, indices: Vec<usize> },
}
```

### 12.2 Lazy Sequence Generators

- `repeat(value)` - repeat indefinitely
- `cycle(collection)` - cycle through collection
- `iterate(fn, initial)` - apply fn repeatedly
- `combinations(size, collection)` - all combinations
- `range(from, to, step)` - custom step range
- `zip(..collections)` - zip multiple collections

### 12.3 Lazy Operations

- Ranges (`..`, `..=`, `..` unbounded) are lazy
- `map`, `filter` on lazy sequences return lazy sequences
- `take`, `find`, `reduce` consume lazy sequences

### 12.4 Break from Iteration

```rust
// Handle break in reduce/fold/each on infinite sequences
OpCode::Break => {
    // Return from iteration with value
}
```

### 12.5 Tests

```rust
#[test]
fn lazy_unbounded_range() { ... }
#[test]
fn lazy_iterate() { ... }
#[test]
fn lazy_repeat_cycle() { ... }
#[test]
fn lazy_map_filter_chain() { ... }
#[test]
fn lazy_break_from_reduce() { ... }
#[test]
fn lazy_combinations() { ... }
```

### Release Gate 12

- [ ] Unbounded ranges work with take/find
- [ ] iterate generates correct sequences
- [ ] Lazy map/filter compose correctly
- [ ] break works in reduce/fold/each on infinite sequences
- [ ] combinations generates correct subsets
- [ ] All tests pass
- [ ] `cargo clippy` clean

---

## Phase 13: Built-in Functions - Remaining

**Goal**: Complete all remaining built-in functions.

**LANG.txt Reference**: §3.3 String (grapheme indexing), §11.14 String Functions, §11.15 Math Functions, §4.5 Bitwise Operators, §11.16 Utility Functions, Appendix B

### 13.1 String Functions

- `lines(string)` - split on newlines
- `split(separator, string)` - split by separator
- `regex_match(pattern, string)` - capture groups
- `regex_match_all(pattern, string)` - all matches

### 13.2 Math Functions

- `abs(value)` - absolute value
- `signum(value)` - sign of number
- `vec_add(a, b)` - vector addition

### 13.3 Bitwise Functions

- `bit_and`, `bit_or`, `bit_xor`, `bit_not`
- `bit_shift_left`, `bit_shift_right`

### 13.4 Utility Functions

- `id(value)` - identity function
- `type(value)` - get type name
- `memoize(fn)` - create memoized function
- `or(a, b)`, `and(a, b)` - logical operators as functions
- `evaluate(source)` - eval string as code

### 13.5 Tests

```rust
#[test]
fn builtin_string_functions() { ... }
#[test]
fn builtin_regex() { ... }
#[test]
fn builtin_math() { ... }
#[test]
fn builtin_bitwise() { ... }
#[test]
fn builtin_memoize() { ... }
#[test]
fn builtin_evaluate() { ... }
```

### Release Gate 13

- [ ] String grapheme-cluster indexing works (LANG.txt 3.3)
- [ ] Regex functions use PCRE-compatible patterns
- [ ] memoize caches correctly
- [ ] evaluate runs sandboxed
- [ ] All tests pass
- [ ] `cargo clippy` clean

---

## Phase 14: Tail-Call Optimization

**Goal**: Implement TCO for self-recursive functions.

**LANG.txt Reference**: §8.9 Recursion and Tail-Call Optimization, §15.4 Performance Characteristics

### 14.1 TCO Detection

- Detect self-recursive calls in tail position
- Mark functions with `is_tail_recursive` flag
- Transform recursive call into loop

### 14.2 Implementation

```rust
// Instead of pushing new frame, reuse current frame
if self.is_tail_call() {
    // Reset locals, update arguments
    // Jump to function start
}
```

### 14.3 Tests

```rust
#[test]
fn tco_factorial() { ... }
#[test]
fn tco_deep_recursion() { ... }
#[test]
fn tco_not_tail_position() { ... }
```

### Release Gate 14

- [ ] Self-recursive tail calls don't grow stack
- [ ] Non-tail recursive calls work normally
- [ ] Can handle deep recursion (>10000 calls)
- [ ] All tests pass
- [ ] `cargo clippy` clean

---

## Phase 15: Pattern Matching Runtime

**Goal**: Complete pattern matching execution.

**LANG.txt Reference**: §9 Pattern Matching (all subsections), §5.4 Destructuring

### 15.1 Pattern Compilation

Generate bytecode for pattern matching:

- Literal patterns: equality check
- List patterns: length check, element checks
- Rest patterns: slice extraction
- Range patterns: bounds check
- Guard clauses: conditional after pattern match

### 15.2 Destructuring Execution

- List destructuring in let bindings
- Nested destructuring
- Mutable destructuring

### 15.3 Tests

```rust
#[test]
fn pattern_literal() { ... }
#[test]
fn pattern_list_basic() { ... }
#[test]
fn pattern_list_rest() { ... }
#[test]
fn pattern_nested() { ... }
#[test]
fn pattern_range() { ... }
#[test]
fn pattern_guard() { ... }
#[test]
fn destructuring_let() { ... }
```

### Release Gate 15

- [ ] All pattern types match correctly
- [ ] Rest patterns work in any position
- [ ] Nested patterns work
- [ ] Guards evaluate correctly
- [ ] Match returns nil if no arm matches
- [ ] All tests pass
- [ ] `cargo clippy` clean

---

## Phase 16: AOC Runner

**Goal**: Implement the AOC solution runner.

**LANG.txt Reference**: §12 AOC Runner (all subsections including §12.5 Script Mode)

### 16.1 Section Handling

```rust
pub struct AocRunner {
    input_section: Option<Value>,
    part_one: Option<CompiledFunction>,
    part_two: Option<CompiledFunction>,
    tests: Vec<TestCase>,
}
```

### 16.2 Execution Flow

1. Evaluate top-level statements
2. Evaluate `input:` section, bind to environment
3. Run `part_one:` and `part_two:` with timing
4. For tests: create fresh environment, run with test input

### 16.3 Error Handling

- Multiple `input:` sections → RuntimeErr
- Multiple `part_one:` or `part_two:` → RuntimeErr

### 16.4 Tests

```rust
#[test]
fn runner_simple_solution() { ... }
#[test]
fn runner_with_tests() { ... }
#[test]
fn runner_script_mode() { ... }
#[test]
fn runner_duplicate_section_error() { ... }
```

### Release Gate 16

- [ ] Solutions execute with input binding
- [ ] Tests run against expected values
- [ ] Timing information is collected
- [ ] Script mode (no sections) works
- [ ] Duplicate sections produce errors
- [ ] All tests pass
- [ ] `cargo clippy` clean

---

## Phase 17: Error Handling & Reporting

**Goal**: Comprehensive error handling with source locations.

**LANG.txt Reference**: §15.5 Error Handling (recoverable vs RuntimeErr, complete list of error conditions)

### 17.1 Error Types

```rust
pub enum SantaError {
    LexError { message: String, line: u32, column: u32 },
    ParseError { message: String, span: Span },
    CompileError { message: String, span: Span },
    RuntimeError { message: String, trace: Vec<StackFrame> },
}
```

### 17.2 Error Display

- Show source context (5 lines around error)
- Caret pointing to error position
- Stack trace for runtime errors
- Colored output (red errors, green locations)

### 17.3 Runtime Errors (from LANG.txt 15.5)

- Type mismatches
- Division by zero
- Invalid regex patterns
- Non-hashable values in sets/dict keys
- Wrong arity
- Shadowing built-ins
- return/break in invalid context

### 17.4 Tests

```rust
#[test]
fn error_source_location() { ... }
#[test]
fn error_stack_trace() { ... }
#[test]
fn error_type_mismatch() { ... }
#[test]
fn error_division_by_zero() { ... }
```

### Release Gate 17

- [ ] All error types have accurate source locations
- [ ] Stack traces show call chain
- [ ] Error messages are clear and helpful
- [ ] Colored output works in terminal
- [ ] All tests pass
- [ ] `cargo clippy` clean

---

## Phase 18: CLI Runtime

**Goal**: Build the command-line interface matching santa-lang-rs.

**LANG.txt Reference**: §13 External Functions (read, puts, env)

### 18.1 Commands

```
santa-cli <SCRIPT>        Run solution file
santa-cli -t <SCRIPT>     Run test suite
santa-cli -r              Start REPL
santa-cli -h              Show help
```

### 18.2 External Functions

- `puts(..values)` - print to stdout
- `read(path)` - read file/URL/aoc input
- `env()` - print REPL environment

### 18.3 AoC Input Fetching

- Parse `aoc://year/day` URLs
- Use `SANTA_CLI_SESSION_TOKEN` env var
- Cache inputs locally

### 18.4 REPL

- Interactive prompt with `>>`
- Line history (rustyline)
- Persistent environment across inputs
- Graceful error handling

### 18.5 Output Format

```
Part 1: <value> <duration>ms
Part 2: <value> <duration>ms
```

### 18.6 Exit Codes

- 0: Success
- 1: Argument error
- 2: Runtime error
- 3: Test failure

### 18.7 Tests

```rust
// Integration tests with assert_cmd
#[test]
fn cli_run_script() { ... }
#[test]
fn cli_run_tests() { ... }
#[test]
fn cli_repl() { ... }
#[test]
fn cli_error_output() { ... }
```

### Release Gate 18

- [ ] All CLI commands work
- [ ] Output format matches santa-lang-rs
- [ ] REPL works interactively
- [ ] AoC input fetching works
- [ ] Exit codes are correct
- [ ] All tests pass
- [ ] `cargo clippy` clean

---

## Phase 19: Benchmarks

**Goal**: Criterion benchmarks for performance validation.

**LANG.txt Reference**: §15.4 Performance Characteristics

### 19.1 Benchmark Categories

```rust
// benchmarks/benches/vm_benchmarks.rs
fn benchmark_lexer(c: &mut Criterion) { ... }
fn benchmark_parser(c: &mut Criterion) { ... }
fn benchmark_vm_arithmetic(c: &mut Criterion) { ... }
fn benchmark_vm_collections(c: &mut Criterion) { ... }
fn benchmark_vm_functions(c: &mut Criterion) { ... }
fn benchmark_aoc_solutions(c: &mut Criterion) { ... }
```

### 19.2 Comparison Benchmarks

- Compare against santa-lang-rs (tree-walking)
- Measure compilation time vs execution time
- Memory usage benchmarks

### 19.3 Setup

```toml
# benchmarks/Cargo.toml
[dependencies]
blitzen-lang = { path = "../lang" }
criterion = { version = "0.5", features = ["html_reports"] }

[[bench]]
name = "vm_benchmarks"
harness = false
```

### Release Gate 19

- [ ] Benchmarks cover all major components
- [ ] Performance meets or exceeds tree-walking interpreter
- [ ] Reports generated with `cargo bench`
- [ ] No performance regressions in CI

---

## Phase 20: Integration & Polish

**Goal**: Final integration, documentation, and polish.

**LANG.txt Reference**: Appendix D (Example Programs), Full specification validation

### 20.1 Integration Tests

Run all LANG.txt Appendix D examples as integration tests:

- Example 1: Fibonacci Sequence (memoize)
- Example 2: AOC 2022 Day 1 (full solution with tests)
- Example 3: Word Frequency Counter (dict operations)
- Example 4: Prime Numbers (lazy infinite sequences)
- Example 5: Recursive List Sum (pattern matching)

Additional validation:

- Run full AoC solutions from santa-lang-rs test suite
- Verify 100% feature parity with LANG.txt
- Cross-reference all built-in function behaviors against Appendix B

### 20.2 Documentation

- README.md with usage examples
- Inline documentation for public API
- Architecture overview

### 20.3 CI Setup

- GitHub Actions workflow
- Run tests on PR
- Run benchmarks on main
- Clippy and format checks

### 20.4 Final Verification Against LANG.txt

- [ ] All LANG.txt §2 lexical rules pass
- [ ] All LANG.txt §3 type behaviors verified
- [ ] All LANG.txt §4 operator behaviors verified
- [ ] All LANG.txt §11 built-in functions (58 total from Appendix B) pass
- [ ] All LANG.txt Appendix D examples execute correctly
- [ ] All santa-lang-rs test cases pass
- [ ] Performance meets targets
- [ ] Documentation is complete
- [ ] CI is green

### Release Gate 20 (Final)

- [ ] 100% LANG.txt compliance verified (all sections)
- [ ] All 58 built-in functions match Appendix B signatures
- [ ] All integration tests pass
- [ ] Benchmarks show expected performance
- [ ] Documentation complete
- [ ] CI pipeline configured
- [ ] Ready for production use

---

## Summary

| Phase | Component            | Key Deliverable             |
| ----- | -------------------- | --------------------------- |
| 1     | Lexer                | Token stream from source    |
| 2     | Parser Expressions   | Expression AST              |
| 3     | Parser Complete      | Full AST with sections      |
| 4     | VM Foundation        | Value system & bytecode     |
| 5     | Compiler Expressions | Expression bytecode         |
| 6     | Compiler Complete    | Full compilation            |
| 7     | VM Core              | Basic execution             |
| 8     | Closures             | Captured variables          |
| 9     | Builtins Core        | Type conversion & access    |
| 10    | Builtins Transform   | map/filter/reduce           |
| 11    | Builtins Search      | find/count/sort             |
| 12    | Lazy Sequences       | Infinite sequences          |
| 13    | Builtins Complete    | All remaining               |
| 14    | TCO                  | Tail-call optimization      |
| 15    | Pattern Matching     | Full pattern support        |
| 16    | AOC Runner           | Section execution           |
| 17    | Error Handling       | Rich error reporting        |
| 18    | CLI                  | Command-line interface      |
| 19    | Benchmarks           | Performance validation      |
| 20    | Integration          | Final polish & verification |

Each phase builds on previous phases. Release gates ensure quality before proceeding.

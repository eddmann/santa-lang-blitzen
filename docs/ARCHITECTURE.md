# Blitzen Architecture

Blitzen is a bytecode virtual machine implementation of [santa-lang](https://eddmann.com/santa-lang/), written in Rust. This document provides a comprehensive overview of the system architecture, covering the compilation pipeline, bytecode format, and runtime execution model.

## Table of Contents

- [Overview](#overview)
- [Compilation Pipeline](#compilation-pipeline)
- [FrostByte Bytecode Format](#frostbyte-bytecode-format)
- [Compiler Design](#compiler-design)
- [VM Architecture](#vm-architecture)
- [Value Representation](#value-representation)
- [Tail-Call Optimization](#tail-call-optimization)
- [Upvalue Management](#upvalue-management)
- [Builtin Fast-Path](#builtin-fast-path)
- [Project Structure](#project-structure)

## Overview

Blitzen follows a classic bytecode compilation approach:

```
Source Code → Lexer → Parser → Compiler → Blitzen VM
                                  ↓
                          FrostByte Bytecode
```

| Component      | Description                                              |
| -------------- | -------------------------------------------------------- |
| **Lexer**      | Tokenizes source into keywords, operators, and literals  |
| **Parser**     | Builds an Abstract Syntax Tree (AST) using Pratt parsing |
| **Compiler**   | Translates AST to FrostByte bytecode                     |
| **Blitzen VM** | Stack-based virtual machine that executes bytecode       |

## Compilation Pipeline

### Lexer (`lang/src/lexer/`)

The lexer transforms source text into a stream of tokens. It handles:

- **Keywords**: `let`, `mut`, `if`, `else`, `match`, `true`, `false`, `nil`, `break`, `return`
- **Operators**: Arithmetic (`+`, `-`, `*`, `/`, `%`), comparison (`<`, `<=`, `>`, `>=`, `==`, `!=`), logical (`&&`, `||`, `!`), pipeline (`|>`), composition (`>>`), range (`..`, `..=`)
- **Literals**: Integers, decimals, strings (with escape sequences), booleans
- **Comments**: Line comments starting with `//`

The lexer tracks source positions (line, column) for error reporting.

### Parser (`lang/src/parser/`)

The parser uses **Pratt parsing** (operator precedence parsing) to build an AST. Precedence levels from lowest to highest:

| Level | Operators         | Description           |
| ----- | ----------------- | --------------------- |
| 1     | `=`               | Assignment            |
| 2     | `\|\|`            | Logical OR            |
| 3     | `&&`              | Logical AND           |
| 4     | `==` `!=`         | Equality              |
| 5     | `<` `<=` `>` `>=` | Comparison            |
| 6     | `\|>` `>>` `..`   | Pipeline/Compose/Range|
| 7     | `+` `-`           | Sum                   |
| 8     | `*` `/` `%`       | Product               |
| 9     | `!` `-`           | Prefix (unary)        |
| 10    | `()`              | Call                  |
| 11    | `[]`              | Index                 |

The parser also handles:

- **Runner sections**: `input:`, `part_one:`, `part_two:`, `test:` with attributes like `@slow`
- **Pattern matching**: `match` expressions with guards and destructuring
- **Collection literals**: Lists `[]`, sets `{}`, dictionaries `#{}`

### Compiler (`lang/src/vm/compiler.rs`)

The compiler translates AST nodes into bytecode. Key responsibilities:

1. **Variable resolution**: Distinguishes locals, upvalues, and globals
2. **Placeholder conversion**: Transforms `_ + 1` into lambda with synthetic parameters
3. **Scope management**: Tracks lexical scopes for proper variable binding
4. **Tail-call detection**: Identifies tail positions for TCO
5. **Pattern compilation**: Generates bytecode for destructuring and matching

## FrostByte Bytecode Format

FrostByte is the bytecode instruction set for the Blitzen VM. Instructions are encoded as a single-byte opcode followed by variable-length operands.

### Instruction Categories

#### Stack Manipulation

| Opcode         | Operands | Description                            |
| -------------- | -------- | -------------------------------------- |
| `Constant`     | 1 byte   | Push constant from pool (index 0-255)  |
| `ConstantLong` | 2 bytes  | Push constant (index 0-65535)          |
| `Pop`          | -        | Discard top of stack                   |
| `PopN`         | 1 byte   | Pop N values, keeping top              |
| `Dup`          | -        | Duplicate top of stack                 |
| `Nil`          | -        | Push nil                               |
| `True`         | -        | Push true                              |
| `False`        | -        | Push false                             |

#### Variable Access

| Opcode         | Operands | Description                            |
| -------------- | -------- | -------------------------------------- |
| `GetLocal`     | 1 byte   | Load local variable by slot index      |
| `SetLocal`     | 1 byte   | Store local variable by slot index     |
| `GetGlobal`    | 1 byte   | Load global by name (constant index)   |
| `SetGlobal`    | 1 byte   | Store global by name (constant index)  |
| `GetUpvalue`   | 1 byte   | Load captured variable by upvalue index|
| `SetUpvalue`   | 1 byte   | Store captured variable                |
| `CloseUpvalue` | -        | Close upvalue at stack top             |

#### Arithmetic Operations

| Opcode | Operands | Description                                      |
| ------ | -------- | ------------------------------------------------ |
| `Add`  | -        | Addition: numbers, strings, lists, sets, dicts   |
| `Sub`  | -        | Subtraction: numbers, sets                       |
| `Mul`  | -        | Multiplication: numbers, string/list repetition  |
| `Div`  | -        | Division: integer or decimal                     |
| `Mod`  | -        | Modulo: floored (Python-style)                   |
| `Neg`  | -        | Unary negation                                   |

#### Comparison Operations

| Opcode | Operands | Description          |
| ------ | -------- | -------------------- |
| `Eq`   | -        | Equal                |
| `Ne`   | -        | Not equal            |
| `Lt`   | -        | Less than            |
| `Le`   | -        | Less than or equal   |
| `Gt`   | -        | Greater than         |
| `Ge`   | -        | Greater than or equal|

#### Logical Operations

| Opcode   | Operands | Description                        |
| -------- | -------- | ---------------------------------- |
| `Not`    | -        | Logical NOT (based on truthiness)  |
| `ToBool` | -        | Convert top of stack to boolean    |

#### Collection Operations

| Opcode       | Operands | Description                                    |
| ------------ | -------- | ---------------------------------------------- |
| `MakeList`   | 1 byte   | Create list from N stack values                |
| `MakeSet`    | 1 byte   | Create set from N stack values                 |
| `MakeDict`   | 1 byte   | Create dict from N key-value pairs             |
| `MakeRange`  | -        | Create range from start, end, inclusive flag   |
| `Index`      | -        | Index operation: collection[index]             |
| `Slice`      | -        | Slice operation: collection[start..end]        |
| `Size`       | -        | Get collection size                            |
| `RangeCheck` | 5 bytes  | Check if integer is in range (start, end, inclusive) |
| `Spread`     | -        | Mark value for spreading into collection/call  |

#### Function Operations

| Opcode        | Operands | Description                              |
| ------------- | -------- | ---------------------------------------- |
| `MakeClosure` | 1 byte   | Create closure from function index       |
| `Call`        | 1 byte   | Call function with N arguments           |
| `TailCall`    | 1 byte   | Tail call optimization (N arguments)     |
| `Return`      | -        | Return from function                     |

#### Control Flow

| Opcode           | Operands | Description                              |
| ---------------- | -------- | ---------------------------------------- |
| `Jump`           | 2 bytes  | Unconditional jump (signed offset)       |
| `JumpIfFalse`    | 2 bytes  | Jump if top is falsy (don't pop)         |
| `JumpIfTrue`     | 2 bytes  | Jump if top is truthy (don't pop)        |
| `PopJumpIfFalse` | 2 bytes  | Pop and jump if falsy (for `&&`)         |
| `PopJumpIfTrue`  | 2 bytes  | Pop and jump if truthy (for `\|\|`)      |

#### Builtins

| Opcode        | Operands           | Description                           |
| ------------- | ------------------ | ------------------------------------- |
| `CallBuiltin` | 2 bytes + 1 byte   | Call builtin by ID with N arguments   |

#### Special

| Opcode  | Operands | Description                              |
| ------- | -------- | ---------------------------------------- |
| `Break` | -        | Break from iteration with value          |

### Chunk Structure

A `Chunk` contains all the data for a compiled unit:

```rust
pub struct Chunk {
    pub code: Vec<u8>,                    // Raw bytecode
    pub constants: Vec<Value>,            // Constant pool
    pub lines: Vec<u32>,                  // Line numbers for debugging
    pub functions: Vec<Rc<CompiledFunction>>, // Nested functions
}
```

### CompiledFunction Structure

```rust
pub struct CompiledFunction {
    pub arity: u8,                    // Number of parameters
    pub is_variadic: bool,            // Accepts rest parameter?
    pub chunk: Chunk,                 // Function bytecode
    pub name: Option<String>,         // Function name (for debugging)
    pub upvalues: Vec<UpvalueDesc>,   // Upvalue descriptors
}

pub struct UpvalueDesc {
    pub index: u8,       // Index into enclosing scope
    pub is_local: bool,  // True if capturing from immediate parent's locals
}
```

## Compiler Design

### Placeholder Conversion

The compiler transforms placeholder syntax into lambda expressions:

```santa
// Source
_ + 1

// Compiled as equivalent to
|__arg0| __arg0 + 1
```

The compiler:

1. Counts placeholders in the expression
2. Creates a new function with arity equal to placeholder count
3. Generates synthetic parameter names (`__arg0`, `__arg1`, etc.)
4. Compiles the expression, substituting `GetLocal` for placeholders

### Scope and Variable Resolution

Variable resolution follows this order:

1. **Locals**: Variables in the current scope (resolved by slot index)
2. **Upvalues**: Captured variables from enclosing scopes
3. **Globals**: Top-level definitions and builtins

The compiler maintains a stack of `Local` structures:

```rust
struct Local {
    name: String,
    depth: u32,      // Scope depth
    mutable: bool,   // Declared with `mut`?
    captured: bool,  // Captured by closure?
}
```

### Forward References

The compiler supports mutual recursion through pre-declaration:

```santa
{
    let is_even = |n| if n == 0 { true } else { is_odd(n - 1) };
    let is_odd = |n| if n == 0 { false } else { is_even(n - 1) };
    is_even(10)
}
```

When a block has 2+ let bindings, the compiler pre-declares all unique names with `nil` values before compiling the initializers.

## VM Architecture

The Blitzen VM is a stack-based virtual machine with the following components:

### VM Structure

```rust
pub struct VM {
    stack: Vec<Value>,                              // Value stack
    frames: Vec<CallFrame>,                         // Call frame stack
    globals: StdHashMap<String, Value>,             // Global variables (std::collections)
    open_upvalues: Vec<Rc<RefCell<Upvalue>>>,       // Open upvalue tracking
    externals: StdHashMap<String, Rc<ExternalFn>>,  // External functions (std::collections)
}
```

Note: `globals` and `externals` use the standard library `HashMap` (`std::collections::HashMap`), not the persistent `im-rs` HashMap used for `Value::Dict`.

### CallFrame Structure

```rust
struct CallFrame {
    closure: Rc<Closure>,    // The closure being executed
    ip: usize,               // Instruction pointer
    stack_base: usize,       // Base of locals on stack
}
```

### Execution Loop

The main execution loop follows the classic fetch-decode-execute cycle:

```rust
fn execute_until(&mut self, return_depth: usize) -> Result<Value, RuntimeError> {
    loop {
        let instruction = self.read_byte();

        match OpCode::try_from(instruction) {
            Ok(OpCode::Constant) => {
                let idx = self.read_byte() as usize;
                let value = self.current_chunk().constants[idx].clone();
                self.push(value);
            }
            // ... handle other opcodes
            Ok(OpCode::Return) => {
                let frame = self.frames.pop().unwrap();
                self.close_upvalues(frame.stack_base);
                let result = self.pop();

                while self.stack.len() > frame.stack_base {
                    self.pop();
                }

                if self.frames.len() <= return_depth {
                    return Ok(result);
                }
                self.push(result);
            }
            // ...
        }
    }
}
```

### Operator Functions

Binary operators (`+`, `-`, `*`, `/`, etc.) are registered as global function values, enabling their use in higher-order functions:

```santa
reduce(+, [1, 2, 3, 4])  // 10
```

These are implemented as small closures that wrap the corresponding opcode.

## Value Representation

The `Value` enum represents all runtime values:

```rust
pub enum Value {
    Nil,
    Integer(i64),
    Decimal(OrderedFloat<f64>),
    Boolean(bool),
    String(Rc<String>),
    List(Vector<Value>),           // Persistent vector (im-rs)
    Set(HashSet<Value>),           // Persistent set (im-rs)
    Dict(HashMap<Value, Value>),   // Persistent map (im-rs)
    Function(Rc<Closure>),
    LazySequence(Rc<RefCell<LazySeq>>),
    Range { start: i64, end: Option<i64>, inclusive: bool },
    ExternalFunction(String),
    PartialApplication { closure: Rc<Closure>, args: Vec<Value> },
    MemoizedFunction(Rc<RefCell<MemoizedFn>>),
    SpreadMarker(Box<Value>),      // Internal: marks spread in collection/call
}
```

### Closure Structure

Closures combine a compiled function with captured upvalues:

```rust
pub struct Closure {
    pub function: Rc<CompiledFunction>,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
}
```

### Persistent Collections

Blitzen uses the `im-rs` crate for persistent (immutable) data structures:

- **`Vector<Value>`**: Persistent vector with O(log n) random access and O(1) amortized push
- **`HashSet<Value>`**: Persistent hash set with structural sharing
- **`HashMap<Value, Value>`**: Persistent hash map with structural sharing

These enable efficient "modifications" without copying the entire structure.

### Lazy Sequences

Lazy sequences enable infinite data structures and deferred computation:

```rust
pub enum LazySeq {
    Range { current: i64, end: Option<i64>, inclusive: bool, step: i64 },
    Repeat { value: Value },
    Cycle { source: Vector<Value>, index: usize },
    Iterate { generator: Value, current: Value },
    Map { source: Rc<RefCell<LazySeq>>, mapper: Value },
    Filter { source: Rc<RefCell<LazySeq>>, predicate: Value },
    // ... and more variants
}
```

## Tail-Call Optimization

Blitzen implements tail-call optimization (TCO) to prevent stack overflow in recursive functions.

### Compiler Side

The compiler tracks whether an expression is in "tail position":

1. Sets `in_tail_position = true` before compiling function body
2. Clears it for non-tail contexts (operands, condition expressions)
3. Preserves it through control flow (both branches of `if` in tail position)

When compiling a call in tail position, the compiler emits `TailCall` instead of `Call`:

```rust
let opcode = if in_tail { OpCode::TailCall } else { OpCode::Call };
self.emit_with_operand(opcode, args.len() as u8);
```

### VM Side

The VM handles `TailCall` specially for self-recursive calls:

```rust
Ok(OpCode::TailCall) => {
    let argc = self.read_byte() as usize;
    let callee = self.peek(argc).clone();

    if let Value::Function(ref new_closure) = callee {
        let current_closure = &self.current_frame().closure;

        // Check if same function (compare Rc pointers)
        if Rc::ptr_eq(&new_closure.function, &current_closure.function) {
            // Self-recursive tail call - reuse frame
            let stack_base = self.frames[frame_idx].stack_base;

            // Move arguments to replace old locals
            for i in 0..argc {
                self.stack[stack_base + i] = self.stack[func_pos + i].clone();
            }

            // Reset instruction pointer to beginning
            self.frames[frame_idx].ip = 0;
            continue; // Jump to start of function
        }
    }

    // Not self-recursion, do normal call
    self.call_value(argc)?;
}
```

Key points:

- **Self-recursive**: Same `Rc<CompiledFunction>` pointer comparison
- **Frame reuse**: Arguments moved to parameter slots, IP reset to 0
- **Non-recursive**: Falls back to normal call (creates new frame)

## Upvalue Management

Closures capture variables from enclosing scopes through upvalues.

### Compiler Side

The compiler builds an upvalue chain for nested functions:

```rust
fn resolve_upvalue(&mut self, name: &str) -> Option<u8> {
    let enclosing = self.enclosing.as_mut()?;

    // Check immediate parent's locals
    if let Some(local_idx) = enclosing.resolve_local(name) {
        enclosing.locals[local_idx].captured = true;
        return Some(self.add_upvalue(local_idx as u8, true));
    }

    // Check parent's upvalues (transitive capture)
    if let Some(upvalue_idx) = enclosing.resolve_upvalue(name) {
        return Some(self.add_upvalue(upvalue_idx, false));
    }

    None
}
```

Each `UpvalueDesc` indicates:

- `index`: Slot in parent's locals (if `is_local`) or parent's upvalues (if not)
- `is_local`: Whether capturing from immediate parent

### VM Side: Open vs Closed Upvalues

```rust
pub enum Upvalue {
    Open(usize),       // Variable still on stack (slot index)
    Closed(Value),     // Variable moved to heap
}
```

#### Capturing an Upvalue

```rust
fn capture_upvalue(&mut self, slot: usize) -> Rc<RefCell<Upvalue>> {
    // Reuse existing open upvalue for same slot
    for upvalue in &self.open_upvalues {
        if let Upvalue::Open(s) = &*upvalue.borrow() && *s == slot {
            return upvalue.clone();
        }
    }

    // Create new open upvalue
    let upvalue = Rc::new(RefCell::new(Upvalue::Open(slot)));
    self.open_upvalues.push(upvalue.clone());
    upvalue
}
```

#### Closing Upvalues

When a scope exits, open upvalues referencing local slots must be "closed":

```rust
fn close_upvalues(&mut self, from_slot: usize) {
    for upvalue in &self.open_upvalues {
        if let Upvalue::Open(slot) = &*upvalue.borrow() && *slot >= from_slot {
            // Move value from stack to heap
            let value = self.stack[*slot].clone();
            *upvalue.borrow_mut() = Upvalue::Closed(value);
        }
    }

    // Remove closed upvalues from tracking list
    self.open_upvalues.retain(|u| matches!(&*u.borrow(), Upvalue::Open(_)));
}
```

This happens on:

- `Return` (before popping frame)
- `CloseUpvalue` opcode (explicit)
- `PopN` (block scope exit)

## Builtin Fast-Path

Built-in functions bypass closure overhead using the `CallBuiltin` opcode.

### Compiler Optimization

When the compiler sees a call to a known builtin (not shadowed by local/upvalue/global):

```rust
if let Some(builtin_id) = BuiltinId::from_name(name)
    && self.resolve_local(name).is_none()
    && self.resolve_upvalue(name).is_none()
    && !self.global_names.contains(name)
{
    // Emit CallBuiltin instead of Call
    self.emit(OpCode::CallBuiltin);
    self.chunk().write_operand_u16(builtin_id as u16);
    self.chunk().write_operand(args.len() as u8);
}
```

### Builtin ID Registry

Builtins are identified by a 16-bit ID:

```rust
#[repr(u16)]
pub enum BuiltinId {
    // Type conversion
    Int = 0, Ints = 1, List = 2, Set = 3, Dict = 4,

    // Collection access
    Get = 10, Size = 11, First = 12, Second = 13, ...

    // Transformation
    Map = 30, Filter = 31, FlatMap = 32, ...

    // ... extensive builtin library
}
```

### VM Execution

```rust
Ok(OpCode::CallBuiltin) => {
    let builtin_id = self.read_u16();
    let argc = self.read_byte() as usize;

    // Collect arguments (with spread expansion)
    let args = self.collect_args(argc)?;

    // Execute builtin directly
    let id = BuiltinId::try_from(builtin_id)?;
    let result = if id.requires_callback() {
        self.call_callback_builtin(id, &args, line)?
    } else {
        call_builtin(id, &args, line)?
    };
    self.push(result);
}
```

Benefits:

- No closure allocation for builtin calls
- Direct dispatch via ID lookup
- Supports variadic builtins with proper arity checking

### Spread Expansion

Both `CallBuiltin` and function `Call` operations support spreading collections via the `SpreadMarker` wrapper. When arguments are evaluated, any `SpreadMarker` is expanded into individual elements before being passed to the function. This enables syntax like `func(...list)` to work correctly.

## Project Structure

```
santa-lang-blitzen/
├── lang/                       # Core language library
│   └── src/
│       ├── lib.rs              # Module exports
│       ├── error.rs            # Error types
│       ├── lexer/              # Tokenization
│       │   ├── mod.rs          # Lexer implementation
│       │   ├── token.rs        # Token and Span types
│       │   └── tests.rs        # Lexer tests
│       ├── parser/             # AST construction
│       │   ├── mod.rs          # Parser (Pratt parsing)
│       │   ├── ast.rs          # AST node definitions
│       │   └── tests.rs        # Parser tests
│       ├── vm/                 # Virtual machine
│       │   ├── mod.rs          # VM module exports
│       │   ├── bytecode.rs     # OpCode, Chunk, CompiledFunction
│       │   ├── compiler.rs     # AST to bytecode compiler
│       │   ├── runtime.rs      # VM execution engine
│       │   ├── value.rs        # Value types, Closure, Upvalue
│       │   ├── builtins.rs     # Built-in function implementations
│       │   └── tests.rs        # VM tests
│       └── runner/             # AoC runner support
│           ├── mod.rs          # Runner implementation
│           └── tests.rs        # Runner tests
├── runtime/cli/                # Command-line interface
│   ├── src/
│   │   ├── main.rs             # CLI entry point
│   │   ├── external.rs         # External functions (puts, read, env)
│   │   └── output.rs           # Output formatting
│   └── tests/
│       └── integration_tests.rs
├── benchmarks/                 # Performance benchmarks
│   └── benches/
│       └── vm_benchmarks.rs    # Criterion benchmarks
├── examples/                   # AoC solution examples
└── docs/                       # Documentation
    └── ARCHITECTURE.md         # This file
```

## See Also

- [santa-lang documentation](https://eddmann.com/santa-lang/)
- [Built-in functions reference](https://eddmann.com/santa-lang/builtins/)
- [AoC Runner documentation](https://eddmann.com/santa-lang/runner/)

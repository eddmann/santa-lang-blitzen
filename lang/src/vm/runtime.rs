// RuntimeError is 128+ bytes but boxing would add overhead on successful paths.
// Error paths are not performance-critical for an interpreter.
#![allow(clippy::result_large_err)]

use im_rc::{HashMap, HashSet, Vector};
use ordered_float::OrderedFloat;
use std::cell::RefCell;
use std::collections::HashMap as StdHashMap;
use std::rc::Rc;
use unicode_segmentation::UnicodeSegmentation;

use super::builtins::{BuiltinId, call_builtin};
use super::bytecode::{Chunk, CompiledFunction, OpCode};
use super::value::{Closure, LazySeq, Upvalue, Value};

/// A frame in the call stack for error reporting
#[derive(Debug, Clone)]
pub struct StackFrame {
    pub function_name: Option<String>,
    pub line: u32,
}

/// Runtime error with message and optional stack trace
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: String,
    pub line: u32,
    /// True if this is a break exception (for iteration control flow)
    pub is_break: bool,
    /// The value returned by break
    pub break_value: Option<Value>,
    /// Call stack trace for debugging (boxed to keep error size small)
    pub stack_trace: Box<Vec<StackFrame>>,
}

impl RuntimeError {
    pub fn new(message: impl Into<String>, line: u32) -> Self {
        Self {
            message: message.into(),
            line,
            is_break: false,
            break_value: None,
            stack_trace: Box::new(Vec::new()),
        }
    }

    /// Create a break exception with a value
    pub fn break_with(value: Value, line: u32) -> Self {
        Self {
            message: "break".to_string(),
            line,
            is_break: true,
            break_value: Some(value),
            stack_trace: Box::new(Vec::new()),
        }
    }

    /// Add a stack frame to the trace
    pub fn add_frame(&mut self, function_name: Option<String>, line: u32) {
        self.stack_trace.push(StackFrame {
            function_name,
            line,
        });
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Runtime error at line {}: {}", self.line, self.message)
    }
}

impl std::error::Error for RuntimeError {}

/// Call frame for function execution
#[derive(Debug)]
struct CallFrame {
    /// The closure being executed
    closure: Rc<Closure>,
    /// Instruction pointer within the closure's chunk
    ip: usize,
    /// Base of the stack for this frame's locals
    stack_base: usize,
}

impl CallFrame {
    fn new(closure: Rc<Closure>, stack_base: usize) -> Self {
        Self {
            closure,
            ip: 0,
            stack_base,
        }
    }

    fn chunk(&self) -> &Chunk {
        &self.closure.function.chunk
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.chunk().code[self.ip];
        self.ip += 1;
        byte
    }

    fn read_u16(&mut self) -> u16 {
        let hi = self.chunk().code[self.ip] as u16;
        let lo = self.chunk().code[self.ip + 1] as u16;
        self.ip += 2;
        (hi << 8) | lo
    }

    fn current_line(&self) -> u32 {
        self.chunk().get_line(self.ip.saturating_sub(1))
    }
}

/// External function type - closures that can access the VM
pub type ExternalFn = Box<dyn Fn(&[Value], &VM) -> Result<Value, RuntimeError>>;

/// The santa-lang virtual machine
pub struct VM {
    /// Value stack
    stack: Vec<Value>,
    /// Call frames
    frames: Vec<CallFrame>,
    /// Global variables
    globals: StdHashMap<String, Value>,
    /// Open upvalues (keyed by stack slot)
    open_upvalues: Vec<Rc<RefCell<Upvalue>>>,
    /// External functions (e.g., puts, read, env)
    externals: StdHashMap<String, Rc<ExternalFn>>,
}

impl VM {
    /// Create a new VM
    pub fn new() -> Self {
        let mut vm = Self {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
            globals: StdHashMap::new(),
            open_upvalues: Vec::new(),
            externals: StdHashMap::new(),
        };
        vm.register_operator_functions();
        vm
    }

    /// Register an external function (e.g., puts, read, env)
    /// External functions can access the VM state (for env() to read globals)
    pub fn register_external<F>(&mut self, name: &str, func: F)
    where
        F: Fn(&[Value], &VM) -> Result<Value, RuntimeError> + 'static,
    {
        self.externals
            .insert(name.to_string(), Rc::new(Box::new(func)));
    }

    /// Get a reference to the globals map (for external functions like env())
    pub fn globals(&self) -> &StdHashMap<String, Value> {
        &self.globals
    }

    /// Register operator functions as globals
    /// Per LANG.txt and Phase 15 requirements, operators can be used as function values
    /// Example: reduce(+, [1, 2, 3]) uses + as a 2-arity function
    fn register_operator_functions(&mut self) {
        use super::bytecode::{Chunk, OpCode};
        use super::value::Closure;

        // Helper to create a binary operator function
        let create_binop = |op: OpCode, name: &str| -> Value {
            let mut chunk = Chunk::new();
            // GetLocal 0 (first parameter)
            chunk.write(OpCode::GetLocal, 0);
            chunk.write_operand(0);
            // GetLocal 1 (second parameter)
            chunk.write(OpCode::GetLocal, 0);
            chunk.write_operand(1);
            // Perform the operation
            chunk.write(op, 0);
            // Return the result
            chunk.write(OpCode::Return, 0);

            let function = Rc::new(CompiledFunction {
                arity: 2,
                is_variadic: false,
                chunk,
                name: Some(name.to_string()),
                upvalues: Vec::new(),
            });

            Value::Function(Rc::new(Closure {
                function,
                upvalues: Vec::new(),
            }))
        };

        // Register all binary operators as global function values
        // Arithmetic operators
        self.globals
            .insert("+".to_string(), create_binop(OpCode::Add, "+"));
        self.globals
            .insert("-".to_string(), create_binop(OpCode::Sub, "-"));
        self.globals
            .insert("*".to_string(), create_binop(OpCode::Mul, "*"));
        self.globals
            .insert("/".to_string(), create_binop(OpCode::Div, "/"));
        self.globals
            .insert("%".to_string(), create_binop(OpCode::Mod, "%"));

        // Comparison operators
        self.globals
            .insert("<".to_string(), create_binop(OpCode::Lt, "<"));
        self.globals
            .insert(">".to_string(), create_binop(OpCode::Gt, ">"));
        self.globals
            .insert("<=".to_string(), create_binop(OpCode::Le, "<="));
        self.globals
            .insert(">=".to_string(), create_binop(OpCode::Ge, ">="));
        self.globals
            .insert("==".to_string(), create_binop(OpCode::Eq, "=="));
        self.globals
            .insert("!=".to_string(), create_binop(OpCode::Ne, "!="));
    }

    /// Run a compiled function
    pub fn run(&mut self, function: Rc<CompiledFunction>) -> Result<Value, RuntimeError> {
        // Clear stack from any previous run to ensure clean state
        // (globals are preserved, but locals should start fresh)
        self.stack.clear();

        // Create main closure (no upvalues)
        let closure = Rc::new(Closure {
            function,
            upvalues: Vec::new(),
        });

        // Push initial frame
        self.frames.push(CallFrame::new(closure, 0));

        // Run execution loop
        self.execute_until(0)
    }

    /// Main execution loop
    /// Runs until frames.len() drops to return_depth
    fn execute_until(&mut self, return_depth: usize) -> Result<Value, RuntimeError> {
        loop {
            let instruction = self.read_byte();

            match OpCode::try_from(instruction) {
                Ok(OpCode::Constant) => {
                    let idx = self.read_byte() as usize;
                    let value = self.current_chunk().constants[idx].clone();
                    self.push(value);
                }
                Ok(OpCode::ConstantLong) => {
                    let idx = self.read_u16() as usize;
                    let value = self.current_chunk().constants[idx].clone();
                    self.push(value);
                }

                Ok(OpCode::Nil) => self.push(Value::Nil),
                Ok(OpCode::True) => self.push(Value::Boolean(true)),
                Ok(OpCode::False) => self.push(Value::Boolean(false)),

                Ok(OpCode::Pop) => {
                    self.pop();
                }
                Ok(OpCode::PopN) => {
                    let n = self.read_byte() as usize;
                    // Keep the top value, pop n values below it
                    let top = self.pop();
                    // Close any upvalues that reference the slots we're about to pop
                    let from_slot = self.stack.len() - n;
                    self.close_upvalues(from_slot);
                    for _ in 0..n {
                        self.pop();
                    }
                    self.push(top);
                }
                Ok(OpCode::Dup) => {
                    let value = self.peek(0).clone();
                    self.push(value);
                }

                // Variables
                Ok(OpCode::GetLocal) => {
                    let slot = self.read_byte() as usize;
                    let base = self.current_frame().stack_base;
                    let value = self.stack[base + slot].clone();
                    self.push(value);
                }
                Ok(OpCode::SetLocal) => {
                    let slot = self.read_byte() as usize;
                    let base = self.current_frame().stack_base;
                    let value = self.peek(0).clone();
                    self.stack[base + slot] = value;
                }
                Ok(OpCode::GetGlobal) => {
                    let idx = self.read_byte() as usize;
                    let name = self.get_constant_string(idx)?;
                    let value = if let Some(v) = self.globals.get(&name) {
                        v.clone()
                    } else if self.externals.contains_key(&name) {
                        // External function - create a placeholder value
                        Value::ExternalFunction(name.clone())
                    } else {
                        return Err(self.error(format!("Undefined variable '{}'", name)));
                    };
                    self.push(value);
                }
                Ok(OpCode::SetGlobal) => {
                    let idx = self.read_byte() as usize;
                    let name = self.get_constant_string(idx)?;
                    let value = self.peek(0).clone();
                    self.globals.insert(name, value);
                }
                Ok(OpCode::GetUpvalue) => {
                    let idx = self.read_byte() as usize;
                    let value = {
                        let upvalue = &self.current_frame().closure.upvalues[idx];
                        match &*upvalue.borrow() {
                            Upvalue::Open(slot) => self.stack[*slot].clone(),
                            Upvalue::Closed(value) => value.clone(),
                        }
                    };
                    self.push(value);
                }
                Ok(OpCode::SetUpvalue) => {
                    let idx = self.read_byte() as usize;
                    let value = self.peek(0).clone();
                    let upvalue = self.current_frame().closure.upvalues[idx].clone();
                    match &mut *upvalue.borrow_mut() {
                        Upvalue::Open(slot) => self.stack[*slot] = value,
                        Upvalue::Closed(v) => *v = value,
                    }
                }
                Ok(OpCode::CloseUpvalue) => {
                    let slot = self.stack.len() - 1;
                    self.close_upvalues(slot);
                    self.pop();
                }

                // Arithmetic
                Ok(OpCode::Add) => self.binary_add()?,
                Ok(OpCode::Sub) => self.binary_sub()?,
                Ok(OpCode::Mul) => self.binary_mul()?,
                Ok(OpCode::Div) => self.binary_div()?,
                Ok(OpCode::Mod) => self.binary_mod()?,
                Ok(OpCode::Neg) => self.unary_neg()?,

                // Comparison
                Ok(OpCode::Eq) => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Boolean(a == b));
                }
                Ok(OpCode::Ne) => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Boolean(a != b));
                }
                Ok(OpCode::Lt) => self.binary_lt()?,
                Ok(OpCode::Le) => self.binary_le()?,
                Ok(OpCode::Gt) => self.binary_gt()?,
                Ok(OpCode::Ge) => self.binary_ge()?,

                // Logical
                Ok(OpCode::Not) => {
                    let value = self.pop();
                    self.push(Value::Boolean(!value.is_truthy()));
                }

                // Collections
                Ok(OpCode::MakeList) => {
                    let count = self.read_byte() as usize;
                    let mut elements = Vector::new();
                    for i in 0..count {
                        let value = self.peek(count - 1 - i).clone();
                        match value {
                            Value::SpreadMarker(inner) => {
                                // Expand spread value into list
                                match *inner {
                                    Value::List(list) => {
                                        for item in list.iter() {
                                            elements.push_back(item.clone());
                                        }
                                    }
                                    Value::String(s) => {
                                        use unicode_segmentation::UnicodeSegmentation;
                                        for g in s.graphemes(true) {
                                            elements
                                                .push_back(Value::String(Rc::new(g.to_string())));
                                        }
                                    }
                                    Value::Set(set) => {
                                        for item in set.iter() {
                                            elements.push_back(item.clone());
                                        }
                                    }
                                    Value::Range {
                                        start,
                                        end,
                                        inclusive,
                                    } => {
                                        if let Some(end_val) = end {
                                            let actual_end =
                                                if inclusive { end_val + 1 } else { end_val };
                                            for n in start..actual_end {
                                                elements.push_back(Value::Integer(n));
                                            }
                                        } else {
                                            return Err(self
                                                .error("Cannot spread unbounded range into list"));
                                        }
                                    }
                                    other => {
                                        return Err(self.error(format!(
                                            "Cannot spread {} into list",
                                            other.type_name()
                                        )));
                                    }
                                }
                            }
                            _ => elements.push_back(value),
                        }
                    }
                    for _ in 0..count {
                        self.pop();
                    }
                    self.push(Value::List(elements));
                }
                Ok(OpCode::MakeSet) => {
                    let count = self.read_byte() as usize;
                    let mut elements = HashSet::new();
                    for i in 0..count {
                        let value = self.peek(count - 1 - i).clone();
                        match value {
                            Value::SpreadMarker(inner) => {
                                // Expand spread value into set
                                match *inner {
                                    Value::List(list) => {
                                        for item in list.iter() {
                                            if !item.is_hashable() {
                                                return Err(self.error(format!(
                                                    "Cannot use {} as set element (not hashable)",
                                                    item.type_name()
                                                )));
                                            }
                                            elements.insert(item.clone());
                                        }
                                    }
                                    Value::Set(set) => {
                                        for item in set.iter() {
                                            elements.insert(item.clone());
                                        }
                                    }
                                    Value::String(s) => {
                                        use unicode_segmentation::UnicodeSegmentation;
                                        for g in s.graphemes(true) {
                                            elements.insert(Value::String(Rc::new(g.to_string())));
                                        }
                                    }
                                    Value::Range {
                                        start,
                                        end,
                                        inclusive,
                                    } => {
                                        if let Some(end_val) = end {
                                            let actual_end =
                                                if inclusive { end_val + 1 } else { end_val };
                                            for n in start..actual_end {
                                                elements.insert(Value::Integer(n));
                                            }
                                        } else {
                                            return Err(self
                                                .error("Cannot spread unbounded range into set"));
                                        }
                                    }
                                    other => {
                                        return Err(self.error(format!(
                                            "Cannot spread {} into set",
                                            other.type_name()
                                        )));
                                    }
                                }
                            }
                            _ => {
                                if !value.is_hashable() {
                                    return Err(self.error(format!(
                                        "Cannot use {} as set element (not hashable)",
                                        value.type_name()
                                    )));
                                }
                                elements.insert(value);
                            }
                        }
                    }
                    for _ in 0..count {
                        self.pop();
                    }
                    self.push(Value::Set(elements));
                }
                Ok(OpCode::MakeDict) => {
                    let count = self.read_byte() as usize;
                    let mut entries = HashMap::new();
                    // Stack has: key1, val1, key2, val2, ... (from bottom to top)
                    // peek(0) is topmost = last val, peek(1) = last key, etc.
                    for i in 0..count {
                        // For entry i (0-indexed from first pushed):
                        // key is at peek((count - 1 - i) * 2 + 1)
                        // val is at peek((count - 1 - i) * 2)
                        let val = self.peek((count - 1 - i) * 2).clone();
                        let key = self.peek((count - 1 - i) * 2 + 1).clone();
                        if !key.is_hashable() {
                            return Err(self.error(format!(
                                "Cannot use {} as dictionary key (not hashable)",
                                key.type_name()
                            )));
                        }
                        entries.insert(key, val);
                    }
                    for _ in 0..count * 2 {
                        self.pop();
                    }
                    self.push(Value::Dict(entries));
                }
                Ok(OpCode::MakeRange) => {
                    let inclusive = self.pop();
                    let end = self.pop();
                    let start = self.pop();

                    let start = match start {
                        Value::Integer(n) => n,
                        _ => return Err(self.error("Range start must be an integer")),
                    };
                    let end = match end {
                        Value::Integer(n) => Some(n),
                        Value::Nil => None,
                        _ => return Err(self.error("Range end must be an integer or nil")),
                    };
                    let inclusive = match inclusive {
                        Value::Boolean(b) => b,
                        _ => return Err(self.error("Range inclusive flag must be a boolean")),
                    };

                    self.push(Value::Range {
                        start,
                        end,
                        inclusive,
                    });
                }
                Ok(OpCode::Index) => self.index_op()?,
                Ok(OpCode::Slice) => self.slice_op()?,
                Ok(OpCode::Size) => {
                    let value = self.pop();
                    let size = match &value {
                        Value::List(v) => v.len() as i64,
                        Value::String(s) => {
                            use unicode_segmentation::UnicodeSegmentation;
                            s.graphemes(true).count() as i64
                        }
                        Value::Set(s) => s.len() as i64,
                        Value::Dict(d) => d.len() as i64,
                        _ => {
                            return Err(
                                self.error(format!("Cannot get size of {}", value.type_name()))
                            );
                        }
                    };
                    self.push(Value::Integer(size));
                }
                Ok(OpCode::RangeCheck) => {
                    let value = self.pop();
                    let start = self.read_u16() as i16 as i64;
                    let end = self.read_u16() as i16 as i64;
                    let inclusive = self.read_byte() != 0;

                    let in_range = match value {
                        Value::Integer(n) => {
                            if inclusive {
                                n >= start && n <= end
                            } else {
                                n >= start && n < end
                            }
                        }
                        _ => false,
                    };
                    self.push(Value::Boolean(in_range));
                }

                // Functions
                Ok(OpCode::MakeClosure) => {
                    let fn_idx = self.read_byte() as usize;
                    let function = self.current_chunk().functions[fn_idx].clone();

                    // Capture upvalues
                    let mut upvalues = Vec::new();
                    for upvalue_desc in &function.upvalues {
                        if upvalue_desc.is_local {
                            // Capture from current frame's locals
                            let slot =
                                self.current_frame().stack_base + upvalue_desc.index as usize;
                            upvalues.push(self.capture_upvalue(slot));
                        } else {
                            // Capture from enclosing closure's upvalues
                            let upvalue = self.current_frame().closure.upvalues
                                [upvalue_desc.index as usize]
                                .clone();
                            upvalues.push(upvalue);
                        }
                    }

                    let closure = Rc::new(Closure { function, upvalues });
                    self.push(Value::Function(closure));
                }
                Ok(OpCode::Call) => {
                    let argc = self.read_byte() as usize;
                    // Expand any spread markers in arguments
                    let actual_argc = self.expand_spread_in_args(argc)?;
                    self.call_value(actual_argc)?;
                }
                Ok(OpCode::TailCall) => {
                    let argc = self.read_byte() as usize;
                    // Expand any spread markers in arguments
                    let actual_argc = self.expand_spread_in_args(argc)?;
                    let callee = self.peek(actual_argc).clone();

                    // Check if this is self-recursion (same closure)
                    if let Value::Function(ref new_closure) = callee {
                        let current_closure = &self.current_frame().closure;

                        // Check if same function (compare Rc pointers)
                        if Rc::ptr_eq(&new_closure.function, &current_closure.function) {
                            // Self-recursive tail call - reuse frame
                            let frame_idx = self.frames.len() - 1;
                            let stack_base = self.frames[frame_idx].stack_base;

                            // Pop the function value
                            let func_pos = self.stack.len() - actual_argc - 1;
                            self.stack.remove(func_pos);

                            // Move arguments to replace old locals
                            for i in 0..actual_argc {
                                self.stack[stack_base + i] = self.stack[func_pos + i].clone();
                            }

                            // Truncate stack to remove extra args
                            self.stack.truncate(stack_base + actual_argc);

                            // Reset instruction pointer to beginning of function
                            self.frames[frame_idx].ip = 0;

                            continue; // Skip incrementing IP, jump to start
                        }
                    }

                    // Not self-recursion, do normal call
                    self.call_value(actual_argc)?;
                }
                Ok(OpCode::Return) => {
                    // Close any open upvalues BEFORE popping the return value
                    // This ensures captured locals are properly preserved
                    let frame = self.frames.pop().unwrap();
                    self.close_upvalues(frame.stack_base);

                    // Now pop the return value (which is on top of stack)
                    let result = self.pop();

                    // Pop remaining locals from the stack
                    while self.stack.len() > frame.stack_base {
                        self.pop();
                    }

                    if self.frames.len() <= return_depth {
                        // Return to caller (either top-level or nested execution)
                        return Ok(result);
                    }

                    self.push(result);
                }

                // Control flow
                Ok(OpCode::Jump) => {
                    let offset = self.read_u16() as usize;
                    self.current_frame_mut().ip += offset;
                }
                Ok(OpCode::JumpIfFalse) => {
                    let offset = self.read_u16() as usize;
                    if !self.peek(0).is_truthy() {
                        self.current_frame_mut().ip += offset;
                    }
                }
                Ok(OpCode::JumpIfTrue) => {
                    let offset = self.read_u16() as usize;
                    if self.peek(0).is_truthy() {
                        self.current_frame_mut().ip += offset;
                    }
                }
                Ok(OpCode::PopJumpIfFalse) => {
                    // Used for && (AND) short-circuit:
                    // - If value is falsy: push it back (as result), jump over second operand
                    // - If value is truthy: discard it, evaluate second operand (becomes result)
                    let offset = self.read_u16() as usize;
                    let value = self.pop();
                    if !value.is_truthy() {
                        self.push(value); // Keep falsy value as result
                        self.current_frame_mut().ip += offset;
                    }
                    // If truthy, value is discarded and we continue to evaluate next operand
                }
                Ok(OpCode::PopJumpIfTrue) => {
                    // Used for || (OR) short-circuit:
                    // - If value is truthy: push it back (as result), jump over second operand
                    // - If value is falsy: discard it, evaluate second operand (becomes result)
                    let offset = self.read_u16() as usize;
                    let value = self.pop();
                    if value.is_truthy() {
                        self.push(value); // Keep truthy value as result
                        self.current_frame_mut().ip += offset;
                    }
                    // If falsy, value is discarded and we continue to evaluate next operand
                }

                // Built-ins
                Ok(OpCode::CallBuiltin) => {
                    let builtin_id = self.read_u16();
                    let argc = self.read_byte() as usize;

                    // Collect arguments from stack, expanding any spread markers
                    let mut args = Vec::with_capacity(argc);
                    for i in 0..argc {
                        let arg = self.peek(argc - 1 - i).clone();
                        match arg {
                            Value::SpreadMarker(inner) => {
                                // Expand the spread
                                match *inner {
                                    Value::List(list) => {
                                        for item in list.iter() {
                                            args.push(item.clone());
                                        }
                                    }
                                    Value::String(s) => {
                                        for g in s.graphemes(true) {
                                            args.push(Value::String(Rc::new(g.to_string())));
                                        }
                                    }
                                    Value::Set(set) => {
                                        for item in set.iter() {
                                            args.push(item.clone());
                                        }
                                    }
                                    Value::Range {
                                        start,
                                        end,
                                        inclusive,
                                    } => {
                                        if let Some(end_val) = end {
                                            let actual_end =
                                                if inclusive { end_val + 1 } else { end_val };
                                            for n in start..actual_end {
                                                args.push(Value::Integer(n));
                                            }
                                        } else {
                                            return Err(self.error(
                                                "Cannot spread unbounded range into builtin call",
                                            ));
                                        }
                                    }
                                    other => {
                                        return Err(self.error(format!(
                                            "Cannot spread {} into builtin call",
                                            other.type_name()
                                        )));
                                    }
                                }
                            }
                            _ => args.push(arg),
                        }
                    }

                    // Pop arguments from stack
                    for _ in 0..argc {
                        self.pop();
                    }

                    // Execute built-in
                    let line = self.current_frame().current_line();
                    let id = BuiltinId::try_from(builtin_id)
                        .map_err(|_| self.error(format!("Unknown builtin id: {}", builtin_id)))?;

                    // Check if this builtin requires callback support
                    let result = if id.requires_callback() {
                        self.call_callback_builtin(id, &args, line)?
                    } else {
                        call_builtin(id, &args, line)?
                    };
                    self.push(result);
                }

                // Special
                Ok(OpCode::Break) => {
                    // Break pops a value from the stack and propagates it up
                    let value = self.pop();
                    let line = self.current_frame().current_line();
                    return Err(RuntimeError::break_with(value, line));
                }
                Ok(OpCode::Spread) => {
                    // Wrap the value in SpreadMarker for later expansion in MakeList/MakeSet/Call
                    let value = self.pop();
                    self.push(Value::SpreadMarker(Box::new(value)));
                }

                Err(byte) => {
                    return Err(self.error(format!("Unknown opcode: {}", byte)));
                }
            }
        }
    }

    // Helper methods

    fn read_byte(&mut self) -> u8 {
        self.current_frame_mut().read_byte()
    }

    fn read_u16(&mut self) -> u16 {
        self.current_frame_mut().read_u16()
    }

    fn current_frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    fn current_chunk(&self) -> &Chunk {
        self.current_frame().chunk()
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Stack underflow")
    }

    fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - distance]
    }

    fn error(&self, message: impl Into<String>) -> RuntimeError {
        RuntimeError::new(message, self.current_frame().current_line())
    }

    fn get_constant_string(&self, idx: usize) -> Result<String, RuntimeError> {
        match &self.current_chunk().constants[idx] {
            Value::String(s) => Ok((**s).clone()),
            _ => Err(self.error("Expected string constant")),
        }
    }

    /// Expand any SpreadMarker values in call arguments
    /// Returns the new argument count after expansion
    fn expand_spread_in_args(&mut self, argc: usize) -> Result<usize, RuntimeError> {
        // Check if any args are spread markers
        let mut has_spread = false;
        for i in 0..argc {
            if matches!(self.peek(i), Value::SpreadMarker(_)) {
                has_spread = true;
                break;
            }
        }

        if !has_spread {
            return Ok(argc);
        }

        // Collect and expand arguments
        // Stack: [callee, arg0, arg1, ..., arg_n-1] where arg_n-1 is on top
        let callee_pos = self.stack.len() - argc - 1;
        let mut expanded_args = Vec::new();

        for i in 0..argc {
            let arg = self.peek(argc - 1 - i).clone();
            match arg {
                Value::SpreadMarker(inner) => {
                    // Expand the inner value
                    match *inner {
                        Value::List(list) => {
                            for item in list.iter() {
                                expanded_args.push(item.clone());
                            }
                        }
                        Value::String(s) => {
                            for g in s.graphemes(true) {
                                expanded_args.push(Value::String(Rc::new(g.to_string())));
                            }
                        }
                        Value::Set(set) => {
                            for item in set.iter() {
                                expanded_args.push(item.clone());
                            }
                        }
                        Value::Range {
                            start,
                            end,
                            inclusive,
                        } => {
                            if let Some(end_val) = end {
                                let actual_end = if inclusive { end_val + 1 } else { end_val };
                                for n in start..actual_end {
                                    expanded_args.push(Value::Integer(n));
                                }
                            } else {
                                return Err(
                                    self.error("Cannot spread unbounded range into function call")
                                );
                            }
                        }
                        other => {
                            return Err(self.error(format!(
                                "Cannot spread {} into function call",
                                other.type_name()
                            )));
                        }
                    }
                }
                _ => expanded_args.push(arg),
            }
        }

        // Remove old arguments from stack (keep callee)
        self.stack.truncate(callee_pos + 1);

        // Push expanded arguments
        let new_argc = expanded_args.len();
        for arg in expanded_args {
            self.push(arg);
        }

        Ok(new_argc)
    }

    // Arithmetic operations

    fn binary_add(&mut self) -> Result<(), RuntimeError> {
        let b = self.pop();
        let a = self.pop();

        let result = match (&a, &b) {
            // Numeric addition - left operand determines result type (LANG.txt §4.1)
            (Value::Integer(x), Value::Integer(y)) => Value::Integer(x + y),
            (Value::Integer(x), Value::Decimal(y)) => {
                // Compute in f64, then convert to i64
                Value::Integer((*x as f64 + y.0) as i64)
            }
            (Value::Decimal(x), Value::Integer(y)) => Value::Decimal(OrderedFloat(x.0 + *y as f64)),
            (Value::Decimal(x), Value::Decimal(y)) => Value::Decimal(OrderedFloat(x.0 + y.0)),

            // String concatenation - only String + any type coerces to string
            // Note: Integer + String is NOT supported (matches Rust/TypeScript implementations)
            (Value::String(x), Value::String(y)) => Value::String(Rc::new(format!("{}{}", x, y))),
            (Value::String(x), y) => Value::String(Rc::new(format!("{}{}", x, y))),

            // List concatenation
            (Value::List(x), Value::List(y)) => {
                let mut result = x.clone();
                for elem in y.iter() {
                    result.push_back(elem.clone());
                }
                Value::List(result)
            }

            // Set union
            (Value::Set(x), Value::Set(y)) => Value::Set(x.clone().union(y.clone())),

            // Set + List = add list elements to set
            (Value::Set(x), Value::List(y)) => {
                let mut result = x.clone();
                for elem in y.iter() {
                    if !elem.is_hashable() {
                        return Err(self.error(format!(
                            "Cannot add {} to set (not hashable)",
                            elem.type_name()
                        )));
                    }
                    result.insert(elem.clone());
                }
                Value::Set(result)
            }

            // Set + LazySequence = materialize lazy seq and add elements to set
            (Value::Set(x), Value::LazySequence(seq)) => {
                let mut result = x.clone();
                let mut seq_clone = seq.borrow().clone();
                while let Some(elem) = self.lazy_seq_next_with_callback(&mut seq_clone)? {
                    if !elem.is_hashable() {
                        return Err(self.error(format!(
                            "Cannot add {} to set (not hashable)",
                            elem.type_name()
                        )));
                    }
                    result.insert(elem);
                }
                Value::Set(result)
            }

            // Set + hashable scalar = add element to set
            (Value::Set(x), y) if y.is_hashable() => {
                let mut result = x.clone();
                result.insert(y.clone());
                Value::Set(result)
            }

            // List + Set = append set elements to list
            (Value::List(x), Value::Set(y)) => {
                let mut result = x.clone();
                for elem in y.iter() {
                    result.push_back(elem.clone());
                }
                Value::List(result)
            }

            // List + LazySequence = materialize lazy seq and append
            (Value::List(x), Value::LazySequence(seq)) => {
                let mut result = x.clone();
                let mut seq_clone = seq.borrow().clone();
                while let Some(elem) = self.lazy_seq_next_with_callback(&mut seq_clone)? {
                    result.push_back(elem);
                }
                Value::List(result)
            }

            // Dictionary merge (right precedence - values from y override x)
            (Value::Dict(x), Value::Dict(y)) => {
                // union takes values from self over other, so we use y.union(x)
                // and then the y values will be kept, achieving right precedence
                Value::Dict(y.clone().union(x.clone()))
            }

            _ => {
                return Err(self.error(format!(
                    "Cannot add {} and {}",
                    a.type_name(),
                    b.type_name()
                )));
            }
        };

        self.push(result);
        Ok(())
    }

    fn binary_sub(&mut self) -> Result<(), RuntimeError> {
        let b = self.pop();
        let a = self.pop();

        let result = match (&a, &b) {
            // Numeric subtraction - left operand determines result type
            (Value::Integer(x), Value::Integer(y)) => Value::Integer(x - y),
            (Value::Integer(x), Value::Decimal(y)) => {
                // Compute in f64, then convert to i64
                Value::Integer((*x as f64 - y.0) as i64)
            }
            (Value::Decimal(x), Value::Integer(y)) => Value::Decimal(OrderedFloat(x.0 - *y as f64)),
            (Value::Decimal(x), Value::Decimal(y)) => Value::Decimal(OrderedFloat(x.0 - y.0)),

            // Set difference
            (Value::Set(x), Value::Set(y)) => Value::Set(x.clone().difference(y.clone())),

            // Set - List = remove list elements from set
            (Value::Set(x), Value::List(y)) => {
                let mut result = x.clone();
                for elem in y.iter() {
                    result.remove(elem);
                }
                Value::Set(result)
            }

            _ => {
                return Err(self.error(format!(
                    "Cannot subtract {} from {}",
                    b.type_name(),
                    a.type_name()
                )));
            }
        };

        self.push(result);
        Ok(())
    }

    fn binary_mul(&mut self) -> Result<(), RuntimeError> {
        let b = self.pop();
        let a = self.pop();

        let result = match (&a, &b) {
            // Numeric multiplication - left operand determines result type (LANG.txt §4.1)
            (Value::Integer(x), Value::Integer(y)) => Value::Integer(x * y),
            (Value::Integer(x), Value::Decimal(y)) => {
                // Compute in f64, then convert to i64
                Value::Integer((*x as f64 * y.0) as i64)
            }
            (Value::Decimal(x), Value::Integer(y)) => Value::Decimal(OrderedFloat(x.0 * *y as f64)),
            (Value::Decimal(x), Value::Decimal(y)) => Value::Decimal(OrderedFloat(x.0 * y.0)),

            // String repetition
            (Value::String(s), Value::Integer(n)) => {
                if *n < 0 {
                    return Err(self.error("Cannot repeat string negative times"));
                }
                Value::String(Rc::new(s.repeat(*n as usize)))
            }

            // List repetition
            (Value::List(v), Value::Integer(n)) => {
                if *n < 0 {
                    return Err(self.error("Cannot repeat list negative times"));
                }
                let mut result = Vector::new();
                for _ in 0..*n {
                    for elem in v.iter() {
                        result.push_back(elem.clone());
                    }
                }
                Value::List(result)
            }

            _ => {
                return Err(self.error(format!(
                    "Cannot multiply {} by {}",
                    a.type_name(),
                    b.type_name()
                )));
            }
        };

        self.push(result);
        Ok(())
    }

    fn binary_div(&mut self) -> Result<(), RuntimeError> {
        let b = self.pop();
        let a = self.pop();

        let result = match (&a, &b) {
            // Python-style floored division (floors toward negative infinity)
            // http://python-history.blogspot.com/2010/08/why-pythons-integer-division-floors.html
            (Value::Integer(x), Value::Integer(y)) => {
                if *y == 0 {
                    return Err(self.error("Division by zero"));
                }
                let d = x / y;
                let r = x % y;
                let result = if (r != 0) && ((r < 0) != (*y < 0)) {
                    d - 1
                } else {
                    d
                };
                Value::Integer(result)
            }
            // Mixed and decimal division
            (Value::Integer(x), Value::Decimal(y)) => {
                if y.0 == 0.0 {
                    return Err(self.error("Division by zero"));
                }
                Value::Integer((*x as f64 / y.0).floor() as i64)
            }
            (Value::Decimal(x), Value::Integer(y)) => {
                if *y == 0 {
                    return Err(self.error("Division by zero"));
                }
                Value::Decimal(OrderedFloat(x.0 / *y as f64))
            }
            (Value::Decimal(x), Value::Decimal(y)) => {
                if y.0 == 0.0 {
                    return Err(self.error("Division by zero"));
                }
                Value::Decimal(OrderedFloat(x.0 / y.0))
            }

            _ => {
                return Err(self.error(format!(
                    "Cannot divide {} by {}",
                    a.type_name(),
                    b.type_name()
                )));
            }
        };

        self.push(result);
        Ok(())
    }

    fn binary_mod(&mut self) -> Result<(), RuntimeError> {
        let b = self.pop();
        let a = self.pop();

        let result = match (&a, &b) {
            // Floored modulo (Python-style) - result has same sign as divisor
            (Value::Integer(x), Value::Integer(y)) => {
                if *y == 0 {
                    return Err(self.error("Modulo by zero"));
                }
                // Python-style floored modulo
                // Result has the same sign as divisor (y)
                let rem = x % y;
                let result = if rem != 0 && (rem < 0) != (*y < 0) {
                    rem + y
                } else {
                    rem
                };
                Value::Integer(result)
            }
            (Value::Decimal(x), Value::Decimal(y)) => {
                if y.0 == 0.0 {
                    return Err(self.error("Modulo by zero"));
                }
                // Python-style floored modulo for floats
                let rem = x.0 % y.0;
                let result = if rem != 0.0 && (rem < 0.0) != (y.0 < 0.0) {
                    rem + y.0
                } else {
                    rem
                };
                Value::Decimal(OrderedFloat(result))
            }

            _ => {
                return Err(self.error(format!(
                    "Cannot compute modulo of {} and {}",
                    a.type_name(),
                    b.type_name()
                )));
            }
        };

        self.push(result);
        Ok(())
    }

    fn unary_neg(&mut self) -> Result<(), RuntimeError> {
        let value = self.pop();

        let result = match value {
            Value::Integer(n) => Value::Integer(-n),
            Value::Decimal(n) => Value::Decimal(OrderedFloat(-n.0)),
            _ => return Err(self.error(format!("Cannot negate {}", value.type_name()))),
        };

        self.push(result);
        Ok(())
    }

    fn binary_lt(&mut self) -> Result<(), RuntimeError> {
        let b = self.pop();
        let a = self.pop();

        let result = match (&a, &b) {
            (Value::Integer(x), Value::Integer(y)) => *x < *y,
            (Value::Integer(x), Value::Decimal(y)) => (*x as f64) < y.0,
            (Value::Decimal(x), Value::Integer(y)) => x.0 < (*y as f64),
            (Value::Decimal(x), Value::Decimal(y)) => x.0 < y.0,
            (Value::String(x), Value::String(y)) => x.as_str() < y.as_str(),
            _ => {
                return Err(self.error(format!(
                    "Cannot compare {} < {}",
                    a.type_name(),
                    b.type_name()
                )));
            }
        };

        self.push(Value::Boolean(result));
        Ok(())
    }

    fn binary_le(&mut self) -> Result<(), RuntimeError> {
        let b = self.pop();
        let a = self.pop();

        let result = match (&a, &b) {
            (Value::Integer(x), Value::Integer(y)) => *x <= *y,
            (Value::Integer(x), Value::Decimal(y)) => (*x as f64) <= y.0,
            (Value::Decimal(x), Value::Integer(y)) => x.0 <= (*y as f64),
            (Value::Decimal(x), Value::Decimal(y)) => x.0 <= y.0,
            (Value::String(x), Value::String(y)) => x.as_str() <= y.as_str(),
            _ => {
                return Err(self.error(format!(
                    "Cannot compare {} <= {}",
                    a.type_name(),
                    b.type_name()
                )));
            }
        };

        self.push(Value::Boolean(result));
        Ok(())
    }

    fn binary_gt(&mut self) -> Result<(), RuntimeError> {
        let b = self.pop();
        let a = self.pop();

        let result = match (&a, &b) {
            (Value::Integer(x), Value::Integer(y)) => *x > *y,
            (Value::Integer(x), Value::Decimal(y)) => (*x as f64) > y.0,
            (Value::Decimal(x), Value::Integer(y)) => x.0 > (*y as f64),
            (Value::Decimal(x), Value::Decimal(y)) => x.0 > y.0,
            (Value::String(x), Value::String(y)) => x.as_str() > y.as_str(),
            _ => {
                return Err(self.error(format!(
                    "Cannot compare {} > {}",
                    a.type_name(),
                    b.type_name()
                )));
            }
        };

        self.push(Value::Boolean(result));
        Ok(())
    }

    fn binary_ge(&mut self) -> Result<(), RuntimeError> {
        let b = self.pop();
        let a = self.pop();

        let result = match (&a, &b) {
            (Value::Integer(x), Value::Integer(y)) => *x >= *y,
            (Value::Integer(x), Value::Decimal(y)) => (*x as f64) >= y.0,
            (Value::Decimal(x), Value::Integer(y)) => x.0 >= (*y as f64),
            (Value::Decimal(x), Value::Decimal(y)) => x.0 >= y.0,
            (Value::String(x), Value::String(y)) => x.as_str() >= y.as_str(),
            _ => {
                return Err(self.error(format!(
                    "Cannot compare {} >= {}",
                    a.type_name(),
                    b.type_name()
                )));
            }
        };

        self.push(Value::Boolean(result));
        Ok(())
    }

    fn index_op(&mut self) -> Result<(), RuntimeError> {
        let index = self.pop();
        let collection = self.pop();

        let result = match (&collection, &index) {
            // List indexing
            (Value::List(list), Value::Integer(idx)) => {
                let len = list.len() as i64;
                let actual_idx = if *idx < 0 { len + idx } else { *idx };
                if actual_idx < 0 || actual_idx >= len {
                    Value::Nil
                } else {
                    list[actual_idx as usize].clone()
                }
            }

            // String indexing (grapheme clusters)
            (Value::String(s), Value::Integer(idx)) => {
                use unicode_segmentation::UnicodeSegmentation;
                let graphemes: Vec<&str> = s.graphemes(true).collect();
                let len = graphemes.len() as i64;
                let actual_idx = if *idx < 0 { len + idx } else { *idx };
                if actual_idx < 0 || actual_idx >= len {
                    Value::Nil
                } else {
                    Value::String(Rc::new(graphemes[actual_idx as usize].to_string()))
                }
            }

            // List indexing with Range
            (
                Value::List(list),
                Value::Range {
                    start,
                    end,
                    inclusive,
                },
            ) => {
                let len = list.len() as i64;
                let actual_start = if *start < 0 {
                    (len + start).max(0)
                } else {
                    (*start).min(len)
                } as usize;

                let actual_end = match end {
                    None => list.len(),
                    Some(e) => {
                        let idx = if *e < 0 {
                            (len + e).max(0)
                        } else {
                            (*e).min(len)
                        };
                        if *inclusive {
                            (idx + 1).min(len) as usize
                        } else {
                            idx as usize
                        }
                    }
                };

                if actual_start >= actual_end {
                    Value::List(Vector::new())
                } else {
                    Value::List(list.clone().slice(actual_start..actual_end))
                }
            }

            // String indexing with Range
            (
                Value::String(s),
                Value::Range {
                    start,
                    end,
                    inclusive,
                },
            ) => {
                use unicode_segmentation::UnicodeSegmentation;
                let graphemes: Vec<&str> = s.graphemes(true).collect();
                let len = graphemes.len() as i64;
                let actual_start = if *start < 0 {
                    (len + start).max(0)
                } else {
                    (*start).min(len)
                } as usize;

                let actual_end = match end {
                    None => graphemes.len(),
                    Some(e) => {
                        let idx = if *e < 0 {
                            (len + e).max(0)
                        } else {
                            (*e).min(len)
                        };
                        if *inclusive {
                            (idx + 1).min(len) as usize
                        } else {
                            idx as usize
                        }
                    }
                };

                if actual_start >= actual_end {
                    Value::String(Rc::new(String::new()))
                } else {
                    Value::String(Rc::new(graphemes[actual_start..actual_end].join("")))
                }
            }

            // Dictionary lookup
            (Value::Dict(dict), key) => dict.get(key).cloned().unwrap_or(Value::Nil),

            // Range indexing with Integer
            // Note: Negative indices are not supported for ranges (return nil)
            (
                Value::Range {
                    start,
                    end,
                    inclusive,
                },
                Value::Integer(idx),
            ) => {
                // Negative indices not supported for ranges
                if *idx < 0 {
                    Value::Nil
                } else {
                    let actual_idx = start + idx;

                    // Check bounds
                    let in_bounds = match end {
                        Some(e) => {
                            if *inclusive {
                                actual_idx <= *e
                            } else {
                                actual_idx < *e
                            }
                        }
                        None => true, // Unbounded: always in bounds for positive index
                    };

                    if in_bounds {
                        Value::Integer(actual_idx)
                    } else {
                        Value::Nil
                    }
                }
            }

            _ => {
                return Err(self.error(format!(
                    "Cannot index {} with {}",
                    collection.type_name(),
                    index.type_name()
                )));
            }
        };

        self.push(result);
        Ok(())
    }

    fn slice_op(&mut self) -> Result<(), RuntimeError> {
        let end = self.pop();
        let start = self.pop();
        let collection = self.pop();

        let start_idx = match &start {
            Value::Integer(n) => *n,
            _ => return Err(self.error("Slice start must be an integer")),
        };

        let result = match &collection {
            Value::List(list) => {
                let len = list.len() as i64;
                let actual_start = if start_idx < 0 {
                    (len + start_idx).max(0)
                } else {
                    start_idx.min(len)
                } as usize;

                let actual_end = match &end {
                    Value::Nil => list.len(),
                    Value::Integer(n) => {
                        let idx = if *n < 0 {
                            (len + n).max(0)
                        } else {
                            (*n).min(len)
                        };
                        idx as usize
                    }
                    _ => return Err(self.error("Slice end must be an integer or nil")),
                };

                if actual_start >= actual_end {
                    Value::List(Vector::new())
                } else {
                    Value::List(list.clone().slice(actual_start..actual_end))
                }
            }

            Value::String(s) => {
                use unicode_segmentation::UnicodeSegmentation;
                let graphemes: Vec<&str> = s.graphemes(true).collect();
                let len = graphemes.len() as i64;
                let actual_start = if start_idx < 0 {
                    (len + start_idx).max(0)
                } else {
                    start_idx.min(len)
                } as usize;

                let actual_end = match &end {
                    Value::Nil => graphemes.len(),
                    Value::Integer(n) => {
                        let idx = if *n < 0 {
                            (len + n).max(0)
                        } else {
                            (*n).min(len)
                        };
                        idx as usize
                    }
                    _ => return Err(self.error("Slice end must be an integer or nil")),
                };

                if actual_start >= actual_end {
                    Value::String(Rc::new(String::new()))
                } else {
                    Value::String(Rc::new(graphemes[actual_start..actual_end].join("")))
                }
            }

            _ => return Err(self.error(format!("Cannot slice {}", collection.type_name()))),
        };

        self.push(result);
        Ok(())
    }

    // Function calling

    fn call_value(&mut self, argc: usize) -> Result<(), RuntimeError> {
        let callee = self.peek(argc).clone();

        match callee {
            Value::Function(closure) => {
                self.call_closure(closure, argc)?;
            }
            Value::PartialApplication {
                closure,
                args: partial_args,
            } => {
                // Calling a partial application: combine partial args with new args
                self.call_partial_application(closure, partial_args, argc)?;
            }
            Value::ExternalFunction(name) => {
                // Collect arguments from stack
                let mut args = Vec::with_capacity(argc);
                for i in 0..argc {
                    args.push(self.peek(argc - 1 - i).clone());
                }

                // Call external function
                let external =
                    self.externals.get(&name).cloned().ok_or_else(|| {
                        self.error(format!("External function '{}' not found", name))
                    })?;

                let result = external(&args, self)?;

                // Pop arguments and function from stack
                for _ in 0..=argc {
                    self.pop();
                }

                // Push result
                self.push(result);
            }
            Value::MemoizedFunction(memoized_fn) => {
                // Collect arguments from stack
                let mut args: Vector<Value> = Vector::new();
                for i in 0..argc {
                    args.push_back(self.peek(argc - 1 - i).clone());
                }

                // Check cache first
                {
                    let cache = &memoized_fn.borrow().cache;
                    if let Some(cached_result) = cache.get(&args) {
                        // Pop arguments and function from stack
                        for _ in 0..=argc {
                            self.pop();
                        }
                        self.push(cached_result.clone());
                        return Ok(());
                    }
                }

                // Not in cache - call the underlying function
                let closure = memoized_fn.borrow().closure.clone();
                let args_vec: Vec<Value> = args.iter().cloned().collect();
                let result = self.call_closure_sync(&closure, args_vec)?;

                // Store in cache
                memoized_fn.borrow_mut().cache.insert(args, result.clone());

                // Pop arguments and function from stack
                for _ in 0..=argc {
                    self.pop();
                }

                // Push result
                self.push(result);
            }
            _ => {
                return Err(self.error(format!("Cannot call {}", callee.type_name())));
            }
        }

        Ok(())
    }

    fn call_closure(&mut self, closure: Rc<Closure>, argc: usize) -> Result<(), RuntimeError> {
        let arity = closure.function.arity as usize;
        let is_variadic = closure.function.is_variadic;

        if argc < arity {
            // Auto-currying: create a partial application
            let mut partial_args = Vec::with_capacity(argc);
            for i in 0..argc {
                partial_args.push(self.peek(argc - 1 - i).clone());
            }

            // Pop arguments and function from stack
            for _ in 0..=argc {
                self.pop();
            }

            // Push the partial application
            self.push(Value::PartialApplication {
                closure,
                args: partial_args,
            });
            return Ok(());
        }

        if argc > arity && !is_variadic {
            return Err(self.error(format!("Expected {} arguments but got {}", arity, argc)));
        }

        if self.frames.len() >= 256 {
            return Err(self.error("Stack overflow"));
        }

        // For variadic functions, collect extra args into a list
        if is_variadic && argc > arity {
            // Collect extra args (they're on top of stack)
            let extra_count = argc - arity;
            let mut rest_args: Vector<Value> = Vector::new();
            for i in 0..extra_count {
                // Pop in reverse order so the list is in argument order
                rest_args.push_back(self.peek(extra_count - 1 - i).clone());
            }
            // Pop the extra args
            for _ in 0..extra_count {
                self.pop();
            }
            // Push the rest list as the last argument
            self.push(Value::List(rest_args));
        } else if is_variadic {
            // Variadic but no extra args - push empty list for rest param
            self.push(Value::List(Vector::new()));
        }

        // Stack base is where the function value was (we'll pop it)
        // For variadic, stack now has: func, regular_args..., rest_list
        let final_argc = if is_variadic { arity + 1 } else { argc };
        let stack_base = self.stack.len() - final_argc - 1;

        // Remove the function from stack, keep args
        self.stack.remove(stack_base);

        self.frames
            .push(CallFrame::new(closure, self.stack.len() - final_argc));
        Ok(())
    }

    fn call_partial_application(
        &mut self,
        closure: Rc<Closure>,
        partial_args: Vec<Value>,
        new_argc: usize,
    ) -> Result<(), RuntimeError> {
        let arity = closure.function.arity as usize;
        let is_variadic = closure.function.is_variadic;
        let total_args = partial_args.len() + new_argc;

        if total_args < arity {
            // Still need more args - create new partial application
            let mut all_args = partial_args;
            for i in 0..new_argc {
                all_args.push(self.peek(new_argc - 1 - i).clone());
            }

            // Pop new arguments and callee from stack
            for _ in 0..=new_argc {
                self.pop();
            }

            self.push(Value::PartialApplication {
                closure,
                args: all_args,
            });
            return Ok(());
        }

        if total_args > arity && !is_variadic {
            return Err(self.error(format!(
                "Expected {} arguments but got {}",
                arity, total_args
            )));
        }

        if self.frames.len() >= 256 {
            return Err(self.error("Stack overflow"));
        }

        // Remove the partial application from stack
        let stack_base = self.stack.len() - new_argc - 1;
        self.stack.remove(stack_base);

        // Push partial args first (they come before the new args)
        // We need to insert them at the right position
        let insert_pos = self.stack.len() - new_argc;
        for (i, arg) in partial_args.into_iter().enumerate() {
            self.stack.insert(insert_pos + i, arg);
        }

        // For variadic functions, collect extra args into a list
        let final_arity = if is_variadic {
            let extra_count = total_args - arity;
            if extra_count > 0 {
                // Collect extra args from top of stack
                let mut rest_args: Vector<Value> = Vector::new();
                for i in 0..extra_count {
                    rest_args.push_back(self.peek(extra_count - 1 - i).clone());
                }
                // Pop the extra args
                for _ in 0..extra_count {
                    self.pop();
                }
                // Push the rest list
                self.push(Value::List(rest_args));
            } else {
                // No extra args - push empty list for rest param
                self.push(Value::List(Vector::new()));
            }
            arity + 1 // Regular args + rest list
        } else {
            arity
        };

        self.frames
            .push(CallFrame::new(closure, self.stack.len() - final_arity));
        Ok(())
    }

    // Upvalue management

    fn capture_upvalue(&mut self, slot: usize) -> Rc<RefCell<Upvalue>> {
        // Check if we already have an open upvalue for this slot
        for upvalue in &self.open_upvalues {
            if let Upvalue::Open(s) = &*upvalue.borrow()
                && *s == slot
            {
                return upvalue.clone();
            }
        }

        // Create new upvalue
        let upvalue = Rc::new(RefCell::new(Upvalue::Open(slot)));
        self.open_upvalues.push(upvalue.clone());
        upvalue
    }

    fn close_upvalues(&mut self, from_slot: usize) {
        let mut to_close = Vec::new();

        for (i, upvalue) in self.open_upvalues.iter().enumerate() {
            if let Upvalue::Open(slot) = &*upvalue.borrow()
                && *slot >= from_slot
            {
                to_close.push((i, *slot));
            }
        }

        // Close them (transfer value from stack to heap)
        // Process in reverse order so indices remain valid
        for (i, slot) in to_close.iter().rev() {
            // Only close if the slot is still valid on the stack
            if *slot < self.stack.len() {
                let value = self.stack[*slot].clone();
                *self.open_upvalues[*i].borrow_mut() = Upvalue::Closed(value);
            }
        }

        // Remove closed upvalues from the list
        self.open_upvalues
            .retain(|u| matches!(&*u.borrow(), Upvalue::Open(_)));
    }

    /// Define a global variable (for external use)
    pub fn define_global(&mut self, name: impl Into<String>, value: Value) {
        self.globals.insert(name.into(), value);
    }

    // =========================================================================
    // Callback-based Built-in Functions
    // =========================================================================
    //
    // These builtins live in runtime.rs (not builtins.rs) because they need to:
    // 1. Call closures during execution (e.g., map's mapper function)
    // 2. Iterate LazySequence variants that contain closures (Map, Filter, Iterate)
    //
    // Pure builtins in builtins.rs operate only on data without invoking closures.
    // Callback builtins here require VM context via `&mut self` to execute closures.
    //
    // Examples:
    // - map/filter/reduce: invoke user-provided functions on each element
    // - get/first on LazySequence: may need to evaluate Map/Filter closures
    // - take on LazySequence: iterates sequences that may contain closures

    /// Execute a callback-based builtin function
    fn call_callback_builtin(
        &mut self,
        id: BuiltinId,
        args: &[Value],
        line: u32,
    ) -> Result<Value, RuntimeError> {
        match id {
            BuiltinId::Map => self.builtin_map(args, line),
            BuiltinId::Filter => self.builtin_filter(args, line),
            BuiltinId::FlatMap => self.builtin_flat_map(args, line),
            BuiltinId::FilterMap => self.builtin_filter_map(args, line),
            BuiltinId::FindMap => self.builtin_find_map(args, line),
            BuiltinId::Reduce => self.builtin_reduce(args, line),
            BuiltinId::Fold => self.builtin_fold(args, line),
            BuiltinId::FoldS => self.builtin_fold_s(args, line),
            BuiltinId::Scan => self.builtin_scan(args, line),
            BuiltinId::Each => self.builtin_each(args, line),
            BuiltinId::Update => self.builtin_update(args, line),
            BuiltinId::UpdateD => self.builtin_update_d(args, line),
            BuiltinId::Find => self.builtin_find(args, line),
            BuiltinId::Count => self.builtin_count(args, line),
            BuiltinId::Sort => self.builtin_sort(args, line),
            BuiltinId::Any => self.builtin_any(args, line),
            BuiltinId::All => self.builtin_all(args, line),
            BuiltinId::Iterate => self.builtin_iterate(args, line),
            BuiltinId::Take => self.builtin_take(args, line),
            BuiltinId::Second => self.builtin_second(args, line),
            BuiltinId::Rest => self.builtin_rest(args, line),
            BuiltinId::Get => self.builtin_get(args, line),
            BuiltinId::First => self.builtin_first(args, line),
            BuiltinId::List => self.builtin_list_callback(args, line),
            BuiltinId::Set => self.builtin_set_callback(args, line),
            BuiltinId::Memoize => self.builtin_memoize(args, line),
            BuiltinId::Sum => self.builtin_sum(args, line),
            BuiltinId::Size => self.builtin_size(args, line),
            _ => Err(RuntimeError::new(
                format!("{} is not a callback builtin", id.name()),
                line,
            )),
        }
    }

    /// Call a closure with arguments and return the result
    fn call_closure_sync(
        &mut self,
        closure: &Rc<Closure>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let arity = closure.function.arity as usize;

        // Handle arity mismatch by padding with nil or truncating
        let actual_args: Vec<Value> = if args.len() < arity {
            let mut padded = args;
            while padded.len() < arity {
                padded.push(Value::Nil);
            }
            padded
        } else if args.len() > arity {
            args.into_iter().take(arity).collect()
        } else {
            args
        };

        // Push arguments onto stack
        for arg in &actual_args {
            self.push(arg.clone());
        }

        // Create new frame
        if self.frames.len() >= 256 {
            return Err(self.error("Stack overflow"));
        }

        // Remember current depth to return here after closure completes
        let return_depth = self.frames.len();

        let stack_base = self.stack.len() - arity;
        self.frames
            .push(CallFrame::new(closure.clone(), stack_base));

        // Execute until we return to current depth
        match self.execute_until(return_depth) {
            Ok(result) => Ok(result),
            Err(e) => {
                // On error (including break), clean up the frame we pushed
                // The frame might already be popped if Return was executed
                if self.frames.len() > return_depth {
                    let frame = self.frames.pop().unwrap();
                    self.close_upvalues(frame.stack_base);
                    // Pop locals from the stack
                    while self.stack.len() > frame.stack_base {
                        self.pop();
                    }
                }
                Err(e)
            }
        }
    }

    /// Get the arity of a closure
    fn closure_arity(&self, closure: &Rc<Closure>) -> usize {
        closure.function.arity as usize
    }

    /// Get the effective arity of a callable (function, partial application, or memoized function)
    fn callable_arity(&self, callable: &Value) -> usize {
        match callable {
            Value::Function(c) => c.function.arity as usize,
            Value::PartialApplication { closure, args } => {
                (closure.function.arity as usize).saturating_sub(args.len())
            }
            Value::MemoizedFunction(m) => m.borrow().closure.function.arity as usize,
            _ => 0,
        }
    }

    /// Call any callable value (Function, PartialApplication, or MemoizedFunction) with arguments
    fn call_callable_sync(
        &mut self,
        callable: &Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match callable {
            Value::Function(closure) => self.call_closure_sync(closure, args),
            Value::PartialApplication {
                closure,
                args: partial_args,
            } => {
                // Combine partial args with new args
                let mut all_args = partial_args.clone();
                all_args.extend(args);
                self.call_closure_sync(closure, all_args)
            }
            Value::MemoizedFunction(memoized_fn) => {
                // Convert args to Vector for cache key
                let args_key: im_rc::Vector<Value> = args.iter().cloned().collect();

                // Check cache first
                {
                    let cache = &memoized_fn.borrow().cache;
                    if let Some(cached_result) = cache.get(&args_key) {
                        return Ok(cached_result.clone());
                    }
                }

                // Not in cache - call the underlying function
                let closure = memoized_fn.borrow().closure.clone();
                let result = self.call_closure_sync(&closure, args)?;

                // Store in cache
                memoized_fn
                    .borrow_mut()
                    .cache
                    .insert(args_key, result.clone());

                Ok(result)
            }
            _ => Err(self.error(format!("Cannot call {}", callable.type_name()))),
        }
    }

    /// Get the next element from a lazy sequence, handling callbacks
    fn lazy_seq_next_with_callback(
        &mut self,
        seq: &mut LazySeq,
    ) -> Result<Option<Value>, RuntimeError> {
        match seq {
            LazySeq::Range {
                current,
                end,
                inclusive,
                step,
            } => match end {
                Some(e) => {
                    // Use stored step for direction
                    let actual_end = if *step > 0 {
                        if *inclusive { *e } else { *e - 1 }
                    } else if *inclusive {
                        *e
                    } else {
                        *e + 1
                    };

                    if *step > 0 {
                        // Ascending
                        if *current > actual_end {
                            return Ok(None);
                        }
                    } else {
                        // Descending
                        if *current < actual_end {
                            return Ok(None);
                        }
                    }

                    let value = Value::Integer(*current);
                    *current += *step;
                    Ok(Some(value))
                }
                None => {
                    // Unbounded (use stored step)
                    let value = Value::Integer(*current);
                    *current += *step;
                    Ok(Some(value))
                }
            },
            LazySeq::RangeStep { current, end, step } => {
                // range(from, to, step) is inclusive of 'to'
                if *step > 0 && *current > *end {
                    return Ok(None);
                }
                if *step < 0 && *current < *end {
                    return Ok(None);
                }
                let value = Value::Integer(*current);
                *current += *step;
                Ok(Some(value))
            }
            LazySeq::Repeat { value } => Ok(Some(value.clone())),
            LazySeq::Cycle { source, index } => {
                if source.is_empty() {
                    return Ok(None);
                }
                let value = source[*index % source.len()].clone();
                *index += 1;
                Ok(Some(value))
            }
            LazySeq::Iterate { generator, current } => {
                let result = current.clone();
                let next = self.call_callable_sync(generator, vec![current.clone()])?;
                *current = next;
                Ok(Some(result))
            }
            LazySeq::Combinations {
                source,
                size,
                indices,
            } => {
                if indices.is_empty() || source.is_empty() || *size == 0 {
                    return Ok(None);
                }

                if indices[0] > source.len() - *size {
                    return Ok(None);
                }

                let combination: Vector<Value> =
                    indices.iter().map(|&i| source[i].clone()).collect();

                // Advance indices
                let n = source.len();
                let mut i = *size;
                while i > 0 {
                    i -= 1;
                    if indices[i] < n - *size + i {
                        indices[i] += 1;
                        for j in (i + 1)..*size {
                            indices[j] = indices[j - 1] + 1;
                        }
                        break;
                    }
                    if i == 0 {
                        indices[0] = n;
                    }
                }

                Ok(Some(Value::List(combination)))
            }
            LazySeq::Map { source, mapper } => {
                match self.lazy_seq_next_with_callback(&mut source.borrow_mut())? {
                    Some(value) => {
                        let mapped = self.call_closure_sync(mapper, vec![value])?;
                        Ok(Some(mapped))
                    }
                    None => Ok(None),
                }
            }
            LazySeq::Filter { source, predicate } => {
                loop {
                    match self.lazy_seq_next_with_callback(&mut source.borrow_mut())? {
                        Some(value) => {
                            // Swallow errors in filter predicates, treating them as "false"
                            // This matches reference behavior where filter callbacks that error
                            // are treated as if the element doesn't pass the filter
                            let result =
                                match self.call_closure_sync(predicate, vec![value.clone()]) {
                                    Ok(v) => v,
                                    Err(_) => continue,
                                };
                            if result.is_truthy() {
                                return Ok(Some(value));
                            }
                        }
                        None => return Ok(None),
                    }
                }
            }
            LazySeq::FilterMap { source, mapper } => {
                loop {
                    match self.lazy_seq_next_with_callback(&mut source.borrow_mut())? {
                        Some(value) => {
                            let mapped = self.call_closure_sync(mapper, vec![value])?;
                            if mapped.is_truthy() {
                                return Ok(Some(mapped));
                            }
                            // Not truthy, continue to next element
                        }
                        None => return Ok(None),
                    }
                }
            }
            LazySeq::Skip { source, remaining } => {
                while *remaining > 0 {
                    match self.lazy_seq_next_with_callback(&mut source.borrow_mut())? {
                        Some(_) => *remaining -= 1,
                        None => return Ok(None),
                    }
                }
                self.lazy_seq_next_with_callback(&mut source.borrow_mut())
            }
            LazySeq::Zip { sources } => {
                let mut tuple = Vector::new();
                for source in sources {
                    match self.lazy_seq_next_with_callback(&mut source.borrow_mut())? {
                        Some(value) => tuple.push_back(value),
                        None => return Ok(None),
                    }
                }
                Ok(Some(Value::List(tuple)))
            }
            LazySeq::Empty => Ok(None),
        }
    }

    // =========================================================================
    // Transformation Functions (§11.4)
    // =========================================================================

    /// map(mapper, collection) → Collection
    fn builtin_map(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let mapper = &args[0];
        let collection = &args[1];

        // Validate mapper is callable
        if !matches!(
            mapper,
            Value::Function(_) | Value::PartialApplication { .. }
        ) {
            return Err(RuntimeError::new(
                format!(
                    "map expects Function as first argument, got {}",
                    mapper.type_name()
                ),
                line,
            ));
        }

        let arity = self.callable_arity(mapper);

        // Clone mapper for use in closures
        let mapper = mapper.clone();

        match collection {
            Value::List(list) => {
                let mut result = Vector::new();
                for (idx, elem) in list.iter().enumerate() {
                    let call_args = if arity >= 2 {
                        vec![elem.clone(), Value::Integer(idx as i64)]
                    } else {
                        vec![elem.clone()]
                    };
                    let mapped = self.call_callable_sync(&mapper, call_args)?;
                    result.push_back(mapped);
                }
                Ok(Value::List(result))
            }
            Value::Set(set) => {
                let mut result = HashSet::new();
                for elem in set.iter() {
                    let mapped = self.call_callable_sync(&mapper, vec![elem.clone()])?;
                    if !mapped.is_hashable() {
                        return Err(RuntimeError::new(
                            format!("Cannot add {} to set (not hashable)", mapped.type_name()),
                            line,
                        ));
                    }
                    result.insert(mapped);
                }
                Ok(Value::Set(result))
            }
            Value::Dict(dict) => {
                let mut result = HashMap::new();
                for (key, value) in dict.iter() {
                    let call_args = if arity >= 2 {
                        vec![value.clone(), key.clone()]
                    } else {
                        vec![value.clone()]
                    };
                    let mapped = self.call_callable_sync(&mapper, call_args)?;
                    result.insert(key.clone(), mapped);
                }
                Ok(Value::Dict(result))
            }
            Value::String(s) => {
                // String maps to List
                let mut result = Vector::new();
                for (idx, grapheme) in s.graphemes(true).enumerate() {
                    let elem = Value::String(Rc::new(grapheme.to_string()));
                    let call_args = if arity >= 2 {
                        vec![elem, Value::Integer(idx as i64)]
                    } else {
                        vec![elem]
                    };
                    let mapped = self.call_callable_sync(&mapper, call_args)?;
                    result.push_back(mapped);
                }
                Ok(Value::List(result))
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => {
                // For ranges with partial application, eagerly evaluate
                if let Value::PartialApplication { closure, args } = &mapper {
                    let mut result = Vector::new();
                    match end {
                        Some(e) => {
                            let actual_end = if *inclusive { *e } else { e - 1 };
                            let mut all_args = args.clone();
                            if start <= &actual_end {
                                for i in *start..=actual_end {
                                    all_args.push(Value::Integer(i));
                                    let mapped =
                                        self.call_closure_sync(closure, all_args.clone())?;
                                    result.push_back(mapped);
                                    all_args.pop();
                                }
                            }
                        }
                        None => {
                            return Err(RuntimeError::new(
                                "Cannot lazily map over unbounded range with partial application"
                                    .to_string(),
                                line,
                            ));
                        }
                    }
                    return Ok(Value::List(result));
                }

                // Regular function - can use lazy evaluation
                let closure = match &mapper {
                    Value::Function(c) => c.clone(),
                    _ => unreachable!(),
                };

                match end {
                    Some(e) => {
                        // Bounded range - eagerly evaluate and return List
                        let mut result = Vector::new();
                        let actual_end = if *inclusive { *e } else { e - 1 };
                        if start <= &actual_end {
                            for i in *start..=actual_end {
                                let mapped =
                                    self.call_closure_sync(&closure, vec![Value::Integer(i)])?;
                                result.push_back(mapped);
                            }
                        } else {
                            // Descending range
                            let mut i = *start;
                            while i >= actual_end {
                                let mapped =
                                    self.call_closure_sync(&closure, vec![Value::Integer(i)])?;
                                result.push_back(mapped);
                                i -= 1;
                            }
                        }
                        Ok(Value::List(result))
                    }
                    None => {
                        // Unbounded range - return LazySequence (always ascending)
                        Ok(Value::LazySequence(Rc::new(RefCell::new(LazySeq::Map {
                            source: Rc::new(RefCell::new(LazySeq::Range {
                                current: *start,
                                end: None,
                                inclusive: *inclusive,
                                step: 1,
                            })),
                            mapper: closure,
                        }))))
                    }
                }
            }
            Value::LazySequence(lazy_seq) => {
                // For lazy sequences with partial application, error for now
                if let Value::PartialApplication { .. } = &mapper {
                    return Err(RuntimeError::new(
                        "Cannot lazily map over lazy sequence with partial application".to_string(),
                        line,
                    ));
                }
                let closure = match &mapper {
                    Value::Function(c) => c.clone(),
                    _ => unreachable!(),
                };
                // Wrap in LazySeq::Map for lazy composition
                Ok(Value::LazySequence(Rc::new(RefCell::new(LazySeq::Map {
                    source: lazy_seq.clone(),
                    mapper: closure,
                }))))
            }
            _ => Err(RuntimeError::new(
                format!("map does not support {}", collection.type_name()),
                line,
            )),
        }
    }

    /// filter(predicate, collection) → Collection
    fn builtin_filter(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let predicate = &args[0];
        let collection = &args[1];

        // Validate predicate is callable
        if !matches!(
            predicate,
            Value::Function(_) | Value::PartialApplication { .. }
        ) {
            return Err(RuntimeError::new(
                format!(
                    "filter expects Function as first argument, got {}",
                    predicate.type_name()
                ),
                line,
            ));
        }

        let arity = self.callable_arity(predicate);
        let predicate = predicate.clone();

        match collection {
            Value::List(list) => {
                let mut result = Vector::new();
                for (idx, elem) in list.iter().enumerate() {
                    let call_args = if arity >= 2 {
                        vec![elem.clone(), Value::Integer(idx as i64)]
                    } else {
                        vec![elem.clone()]
                    };
                    // Swallow errors in filter predicates, treating them as "false"
                    let keep = match self.call_callable_sync(&predicate, call_args) {
                        Ok(v) => v,
                        Err(_) => continue,
                    };
                    if keep.is_truthy() {
                        result.push_back(elem.clone());
                    }
                }
                Ok(Value::List(result))
            }
            Value::Set(set) => {
                let mut result = HashSet::new();
                for elem in set.iter() {
                    // Swallow errors in filter predicates, treating them as "false"
                    let keep = match self.call_callable_sync(&predicate, vec![elem.clone()]) {
                        Ok(v) => v,
                        Err(_) => continue,
                    };
                    if keep.is_truthy() {
                        result.insert(elem.clone());
                    }
                }
                Ok(Value::Set(result))
            }
            Value::Dict(dict) => {
                let mut result = HashMap::new();
                for (key, value) in dict.iter() {
                    let call_args = if arity >= 2 {
                        vec![value.clone(), key.clone()]
                    } else {
                        vec![value.clone()]
                    };
                    // Swallow errors in filter predicates, treating them as "false"
                    let keep = match self.call_callable_sync(&predicate, call_args) {
                        Ok(v) => v,
                        Err(_) => continue,
                    };
                    if keep.is_truthy() {
                        result.insert(key.clone(), value.clone());
                    }
                }
                Ok(Value::Dict(result))
            }
            Value::String(s) => {
                // String filters to List
                let mut result = Vector::new();
                for (idx, grapheme) in s.graphemes(true).enumerate() {
                    let elem = Value::String(Rc::new(grapheme.to_string()));
                    let call_args = if arity >= 2 {
                        vec![elem.clone(), Value::Integer(idx as i64)]
                    } else {
                        vec![elem.clone()]
                    };
                    // Swallow errors in filter predicates, treating them as "false"
                    let keep = match self.call_callable_sync(&predicate, call_args) {
                        Ok(v) => v,
                        Err(_) => continue,
                    };
                    if keep.is_truthy() {
                        result.push_back(elem);
                    }
                }
                Ok(Value::List(result))
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => {
                // For lazy sequences, need a raw closure
                let closure = match &predicate {
                    Value::Function(c) => c.clone(),
                    Value::PartialApplication { .. } => {
                        // For ranges with partial application, eagerly evaluate
                        let mut result = Vector::new();
                        match end {
                            Some(e) => {
                                let actual_end = if *inclusive { *e } else { e - 1 };
                                if start <= &actual_end {
                                    for i in *start..=actual_end {
                                        // Swallow errors in filter predicates, treating them as "false"
                                        let keep = match self
                                            .call_callable_sync(&predicate, vec![Value::Integer(i)])
                                        {
                                            Ok(v) => v,
                                            Err(_) => continue,
                                        };
                                        if keep.is_truthy() {
                                            result.push_back(Value::Integer(i));
                                        }
                                    }
                                }
                            }
                            None => {
                                return Err(RuntimeError::new(
                                    "Cannot lazily filter unbounded range with partial application"
                                        .to_string(),
                                    line,
                                ));
                            }
                        }
                        return Ok(Value::List(result));
                    }
                    _ => unreachable!(),
                };

                // Range filters to LazySequence
                let step = match end {
                    Some(e) if *e < *start => -1,
                    _ => 1,
                };
                Ok(Value::LazySequence(Rc::new(RefCell::new(
                    LazySeq::Filter {
                        source: Rc::new(RefCell::new(LazySeq::Range {
                            current: *start,
                            end: *end,
                            inclusive: *inclusive,
                            step,
                        })),
                        predicate: closure,
                    },
                ))))
            }
            Value::LazySequence(lazy_seq) => {
                let closure = match &predicate {
                    Value::Function(c) => c.clone(),
                    Value::PartialApplication { .. } => {
                        return Err(RuntimeError::new(
                            "Cannot lazily filter lazy sequence with partial application"
                                .to_string(),
                            line,
                        ));
                    }
                    _ => unreachable!(),
                };
                // Wrap in LazySeq::Filter for lazy composition
                Ok(Value::LazySequence(Rc::new(RefCell::new(
                    LazySeq::Filter {
                        source: lazy_seq.clone(),
                        predicate: closure,
                    },
                ))))
            }
            _ => Err(RuntimeError::new(
                format!("filter does not support {}", collection.type_name()),
                line,
            )),
        }
    }

    /// flat_map(mapper, collection) → List
    /// For nested lists, maps into inner elements then flattens
    fn builtin_flat_map(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let mapper = &args[0];
        let collection = &args[1];

        let closure = match mapper {
            Value::Function(c) => c.clone(),
            _ => {
                return Err(RuntimeError::new(
                    format!(
                        "flat_map expects Function as first argument, got {}",
                        mapper.type_name()
                    ),
                    line,
                ));
            }
        };

        let arity = self.closure_arity(&closure);
        let mut result = Vector::new();

        // Helper to flatten a mapped result into the result vector
        let flatten_mapped = |result: &mut Vector<Value>,
                              mapped: Value,
                              vm: &mut Self|
         -> Result<(), RuntimeError> {
            match mapped {
                Value::List(inner) => {
                    for item in inner.iter() {
                        result.push_back(item.clone());
                    }
                }
                Value::LazySequence(seq) => {
                    // Materialize lazy sequence and flatten
                    let mut seq_clone = seq.borrow().clone();
                    while let Some(item) = vm.lazy_seq_next_with_callback(&mut seq_clone)? {
                        result.push_back(item);
                    }
                }
                _ => result.push_back(mapped),
            }
            Ok(())
        };

        match collection {
            Value::List(list) => {
                for (idx, elem) in list.iter().enumerate() {
                    // Apply mapper to each element, then flatten if result is a list or lazy seq
                    let call_args = if arity >= 2 {
                        vec![elem.clone(), Value::Integer(idx as i64)]
                    } else {
                        vec![elem.clone()]
                    };
                    let mapped = self.call_closure_sync(&closure, call_args)?;
                    flatten_mapped(&mut result, mapped, self)?;
                }
            }
            Value::LazySequence(seq) => {
                let mut seq_clone = seq.borrow().clone();
                while let Some(elem) = self.lazy_seq_next_with_callback(&mut seq_clone)? {
                    let mapped = self.call_closure_sync(&closure, vec![elem])?;
                    flatten_mapped(&mut result, mapped, self)?;
                }
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => match end {
                Some(e) => {
                    let actual_end = if *inclusive { *e } else { e - 1 };
                    let range_iter: Box<dyn Iterator<Item = i64>> = if start <= &actual_end {
                        Box::new(*start..=actual_end)
                    } else {
                        Box::new((actual_end..=*start).rev())
                    };
                    for i in range_iter {
                        let mapped = self.call_closure_sync(&closure, vec![Value::Integer(i)])?;
                        flatten_mapped(&mut result, mapped, self)?;
                    }
                }
                None => {
                    return Err(RuntimeError::new(
                        "flat_map on unbounded range not supported",
                        line,
                    ));
                }
            },
            _ => {
                return Err(RuntimeError::new(
                    format!("flat_map does not support {}", collection.type_name()),
                    line,
                ));
            }
        }

        Ok(Value::List(result))
    }

    /// filter_map(mapper, collection) → Collection
    fn builtin_filter_map(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let mapper = &args[0];
        let collection = &args[1];

        let closure = match mapper {
            Value::Function(c) => c.clone(),
            _ => {
                return Err(RuntimeError::new(
                    format!(
                        "filter_map expects Function as first argument, got {}",
                        mapper.type_name()
                    ),
                    line,
                ));
            }
        };

        let arity = self.closure_arity(&closure);

        match collection {
            Value::List(list) => {
                let mut result = Vector::new();
                for (idx, elem) in list.iter().enumerate() {
                    let call_args = if arity >= 2 {
                        vec![elem.clone(), Value::Integer(idx as i64)]
                    } else {
                        vec![elem.clone()]
                    };
                    let mapped = self.call_closure_sync(&closure, call_args)?;
                    if mapped.is_truthy() {
                        result.push_back(mapped);
                    }
                }
                Ok(Value::List(result))
            }
            Value::Set(set) => {
                let mut result = HashSet::new();
                for elem in set.iter() {
                    let mapped = self.call_closure_sync(&closure, vec![elem.clone()])?;
                    if mapped.is_truthy() {
                        if !mapped.is_hashable() {
                            return Err(RuntimeError::new(
                                format!("Cannot add {} to set (not hashable)", mapped.type_name()),
                                line,
                            ));
                        }
                        result.insert(mapped);
                    }
                }
                Ok(Value::Set(result))
            }
            Value::Dict(dict) => {
                let mut result = HashMap::new();
                for (key, value) in dict.iter() {
                    let call_args = if arity >= 2 {
                        vec![value.clone(), key.clone()]
                    } else {
                        vec![value.clone()]
                    };
                    let mapped = self.call_closure_sync(&closure, call_args)?;
                    if mapped.is_truthy() {
                        result.insert(key.clone(), mapped);
                    }
                }
                Ok(Value::Dict(result))
            }
            Value::String(s) => {
                let mut result = Vector::new();
                for (idx, grapheme) in s.graphemes(true).enumerate() {
                    let elem = Value::String(Rc::new(grapheme.to_string()));
                    let call_args = if arity >= 2 {
                        vec![elem, Value::Integer(idx as i64)]
                    } else {
                        vec![elem]
                    };
                    let mapped = self.call_closure_sync(&closure, call_args)?;
                    if mapped.is_truthy() {
                        result.push_back(mapped);
                    }
                }
                Ok(Value::List(result))
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => {
                let mut result = Vector::new();
                match end {
                    Some(e) => {
                        let actual_end = if *inclusive { *e } else { e - 1 };
                        let range_iter: Box<dyn Iterator<Item = i64>> = if start <= &actual_end {
                            Box::new(*start..=actual_end)
                        } else {
                            Box::new((actual_end..=*start).rev())
                        };
                        for i in range_iter {
                            let mapped =
                                self.call_closure_sync(&closure, vec![Value::Integer(i)])?;
                            if mapped.is_truthy() {
                                result.push_back(mapped);
                            }
                        }
                    }
                    None => {
                        return Err(RuntimeError::new(
                            "filter_map on unbounded range not supported",
                            line,
                        ));
                    }
                }
                Ok(Value::List(result))
            }
            Value::LazySequence(seq) => {
                // Return a lazy FilterMap sequence instead of eagerly consuming
                let lazy_filter_map = LazySeq::FilterMap {
                    source: seq.clone(),
                    mapper: closure,
                };
                Ok(Value::LazySequence(Rc::new(RefCell::new(lazy_filter_map))))
            }
            _ => Err(RuntimeError::new(
                format!("filter_map does not support {}", collection.type_name()),
                line,
            )),
        }
    }

    /// find_map(mapper, collection) → Value | Nil
    fn builtin_find_map(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let mapper = &args[0];
        let collection = &args[1];

        let closure = match mapper {
            Value::Function(c) => c.clone(),
            _ => {
                return Err(RuntimeError::new(
                    format!(
                        "find_map expects Function as first argument, got {}",
                        mapper.type_name()
                    ),
                    line,
                ));
            }
        };

        let arity = self.closure_arity(&closure);

        match collection {
            Value::List(list) => {
                for (idx, elem) in list.iter().enumerate() {
                    let call_args = if arity >= 2 {
                        vec![elem.clone(), Value::Integer(idx as i64)]
                    } else {
                        vec![elem.clone()]
                    };
                    let mapped = self.call_closure_sync(&closure, call_args)?;
                    if mapped.is_truthy() {
                        return Ok(mapped);
                    }
                }
                Ok(Value::Nil)
            }
            Value::Set(set) => {
                for elem in set.iter() {
                    let mapped = self.call_closure_sync(&closure, vec![elem.clone()])?;
                    if mapped.is_truthy() {
                        return Ok(mapped);
                    }
                }
                Ok(Value::Nil)
            }
            Value::Dict(dict) => {
                for (key, value) in dict.iter() {
                    let call_args = if arity >= 2 {
                        vec![value.clone(), key.clone()]
                    } else {
                        vec![value.clone()]
                    };
                    let mapped = self.call_closure_sync(&closure, call_args)?;
                    if mapped.is_truthy() {
                        return Ok(mapped);
                    }
                }
                Ok(Value::Nil)
            }
            Value::String(s) => {
                for (idx, grapheme) in s.graphemes(true).enumerate() {
                    let elem = Value::String(Rc::new(grapheme.to_string()));
                    let call_args = if arity >= 2 {
                        vec![elem, Value::Integer(idx as i64)]
                    } else {
                        vec![elem]
                    };
                    let mapped = self.call_closure_sync(&closure, call_args)?;
                    if mapped.is_truthy() {
                        return Ok(mapped);
                    }
                }
                Ok(Value::Nil)
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => {
                match end {
                    Some(e) => {
                        let actual_end = if *inclusive { *e } else { e - 1 };
                        let range_iter: Box<dyn Iterator<Item = i64>> = if start <= &actual_end {
                            Box::new(*start..=actual_end)
                        } else {
                            Box::new((actual_end..=*start).rev())
                        };
                        for i in range_iter {
                            let mapped =
                                self.call_closure_sync(&closure, vec![Value::Integer(i)])?;
                            if mapped.is_truthy() {
                                return Ok(mapped);
                            }
                        }
                        Ok(Value::Nil)
                    }
                    None => {
                        // Unbounded range - keep searching until found
                        let mut i = *start;
                        loop {
                            let mapped =
                                self.call_closure_sync(&closure, vec![Value::Integer(i)])?;
                            if mapped.is_truthy() {
                                return Ok(mapped);
                            }
                            i += 1;
                        }
                    }
                }
            }
            Value::LazySequence(seq) => {
                let mut seq_clone = seq.borrow().clone();
                loop {
                    match self.lazy_seq_next_with_callback(&mut seq_clone)? {
                        Some(elem) => {
                            let mapped = self.call_closure_sync(&closure, vec![elem])?;
                            if mapped.is_truthy() {
                                return Ok(mapped);
                            }
                        }
                        None => return Ok(Value::Nil),
                    }
                }
            }
            _ => Err(RuntimeError::new(
                format!("find_map does not support {}", collection.type_name()),
                line,
            )),
        }
    }

    // =========================================================================
    // Reduction Functions (§11.5)
    // =========================================================================

    /// reduce(reducer, collection) → Value
    fn builtin_reduce(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let reducer = &args[0];
        let collection = &args[1];

        let closure = match reducer {
            Value::Function(c) => c.clone(),
            _ => {
                return Err(RuntimeError::new(
                    format!(
                        "reduce expects Function as first argument, got {}",
                        reducer.type_name()
                    ),
                    line,
                ));
            }
        };

        let arity = self.closure_arity(&closure);

        match collection {
            Value::List(list) => {
                if list.is_empty() {
                    return Err(RuntimeError::new("reduce on empty List", line));
                }
                let mut acc = list[0].clone();
                for (idx, elem) in list.iter().skip(1).enumerate() {
                    let call_args = if arity >= 3 {
                        vec![acc, elem.clone(), Value::Integer((idx + 1) as i64)]
                    } else {
                        vec![acc, elem.clone()]
                    };
                    acc = self.call_closure_sync(&closure, call_args)?;
                }
                Ok(acc)
            }
            Value::Set(set) => {
                if set.is_empty() {
                    return Err(RuntimeError::new("reduce on empty Set", line));
                }
                let mut iter = set.iter();
                let mut acc = iter.next().unwrap().clone();
                for elem in iter {
                    acc = self.call_closure_sync(&closure, vec![acc, elem.clone()])?;
                }
                Ok(acc)
            }
            Value::Dict(dict) => {
                if dict.is_empty() {
                    return Err(RuntimeError::new("reduce on empty Dictionary", line));
                }
                let mut iter = dict.iter();
                let (_, first_value) = iter.next().unwrap();
                let mut acc = first_value.clone();
                for (key, value) in iter {
                    let call_args = if arity >= 3 {
                        vec![acc, value.clone(), key.clone()]
                    } else {
                        vec![acc, value.clone()]
                    };
                    acc = self.call_closure_sync(&closure, call_args)?;
                }
                Ok(acc)
            }
            Value::String(s) => {
                let graphemes: Vec<&str> = s.graphemes(true).collect();
                if graphemes.is_empty() {
                    return Err(RuntimeError::new("reduce on empty String", line));
                }
                let mut acc = Value::String(Rc::new(graphemes[0].to_string()));
                for grapheme in graphemes.iter().skip(1) {
                    let elem = Value::String(Rc::new(grapheme.to_string()));
                    acc = self.call_closure_sync(&closure, vec![acc, elem])?;
                }
                Ok(acc)
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => match end {
                Some(e) => {
                    let actual_end = if *inclusive { *e } else { e - 1 };
                    if start > &actual_end {
                        return Err(RuntimeError::new("reduce on empty Range", line));
                    }
                    let mut acc = Value::Integer(*start);
                    for i in (start + 1)..=actual_end {
                        match self.call_closure_sync(&closure, vec![acc.clone(), Value::Integer(i)])
                        {
                            Ok(v) => acc = v,
                            Err(e) if e.is_break => {
                                return Ok(e.break_value.unwrap_or(Value::Nil));
                            }
                            Err(e) => return Err(e),
                        }
                    }
                    Ok(acc)
                }
                None => {
                    // Unbounded range - must use break to terminate
                    let mut acc = Value::Integer(*start);
                    let mut i = *start + 1;
                    loop {
                        match self.call_closure_sync(&closure, vec![acc.clone(), Value::Integer(i)])
                        {
                            Ok(v) => acc = v,
                            Err(e) if e.is_break => {
                                return Ok(e.break_value.unwrap_or(Value::Nil));
                            }
                            Err(e) => return Err(e),
                        }
                        i += 1;
                    }
                }
            },
            Value::LazySequence(seq) => {
                // Get first element as initial accumulator
                let mut seq_clone = seq.borrow().clone();
                let first = self.lazy_seq_next_with_callback(&mut seq_clone)?;
                let mut acc = match first {
                    Some(v) => v,
                    None => return Err(RuntimeError::new("reduce on empty LazySequence", line)),
                };

                loop {
                    let next = self.lazy_seq_next_with_callback(&mut seq_clone)?;
                    match next {
                        Some(elem) => {
                            match self.call_closure_sync(&closure, vec![acc.clone(), elem]) {
                                Ok(v) => acc = v,
                                Err(e) if e.is_break => {
                                    return Ok(e.break_value.unwrap_or(Value::Nil));
                                }
                                Err(e) => return Err(e),
                            }
                        }
                        None => break,
                    }
                }
                Ok(acc)
            }
            _ => Err(RuntimeError::new(
                format!("reduce does not support {}", collection.type_name()),
                line,
            )),
        }
    }

    /// fold(initial, folder, collection) → Value
    fn builtin_fold(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let initial = &args[0];
        let folder = &args[1];
        let collection = &args[2];

        // Validate folder is callable
        if !matches!(
            folder,
            Value::Function(_) | Value::PartialApplication { .. }
        ) {
            return Err(RuntimeError::new(
                format!(
                    "fold expects Function as second argument, got {}",
                    folder.type_name()
                ),
                line,
            ));
        }

        let arity = self.callable_arity(folder);
        let folder = folder.clone();
        let mut acc = initial.clone();

        match collection {
            Value::List(list) => {
                for (idx, elem) in list.iter().enumerate() {
                    let call_args = if arity >= 3 {
                        vec![acc.clone(), elem.clone(), Value::Integer(idx as i64)]
                    } else {
                        vec![acc.clone(), elem.clone()]
                    };
                    match self.call_callable_sync(&folder, call_args) {
                        Ok(v) => acc = v,
                        Err(e) if e.is_break => {
                            return Ok(e.break_value.unwrap_or(Value::Nil));
                        }
                        Err(e) => return Err(e),
                    }
                }
                Ok(acc)
            }
            Value::Set(set) => {
                for elem in set.iter() {
                    match self.call_callable_sync(&folder, vec![acc.clone(), elem.clone()]) {
                        Ok(v) => acc = v,
                        Err(e) if e.is_break => {
                            return Ok(e.break_value.unwrap_or(Value::Nil));
                        }
                        Err(e) => return Err(e),
                    }
                }
                Ok(acc)
            }
            Value::Dict(dict) => {
                for (key, value) in dict.iter() {
                    let call_args = if arity >= 3 {
                        vec![acc.clone(), value.clone(), key.clone()]
                    } else {
                        vec![acc.clone(), value.clone()]
                    };
                    match self.call_callable_sync(&folder, call_args) {
                        Ok(v) => acc = v,
                        Err(e) if e.is_break => {
                            return Ok(e.break_value.unwrap_or(Value::Nil));
                        }
                        Err(e) => return Err(e),
                    }
                }
                Ok(acc)
            }
            Value::String(s) => {
                for (idx, grapheme) in s.graphemes(true).enumerate() {
                    let elem = Value::String(Rc::new(grapheme.to_string()));
                    let call_args = if arity >= 3 {
                        vec![acc.clone(), elem, Value::Integer(idx as i64)]
                    } else {
                        vec![acc.clone(), elem]
                    };
                    match self.call_callable_sync(&folder, call_args) {
                        Ok(v) => acc = v,
                        Err(e) if e.is_break => {
                            return Ok(e.break_value.unwrap_or(Value::Nil));
                        }
                        Err(e) => return Err(e),
                    }
                }
                Ok(acc)
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => {
                match end {
                    Some(e) => {
                        let actual_end = if *inclusive { *e } else { e - 1 };
                        if start <= &actual_end {
                            for i in *start..=actual_end {
                                match self.call_callable_sync(
                                    &folder,
                                    vec![acc.clone(), Value::Integer(i)],
                                ) {
                                    Ok(v) => acc = v,
                                    Err(e) if e.is_break => {
                                        return Ok(e.break_value.unwrap_or(Value::Nil));
                                    }
                                    Err(e) => return Err(e),
                                }
                            }
                        } else {
                            // Descending range
                            let mut i = *start;
                            while i >= actual_end {
                                match self.call_callable_sync(
                                    &folder,
                                    vec![acc.clone(), Value::Integer(i)],
                                ) {
                                    Ok(v) => acc = v,
                                    Err(e) if e.is_break => {
                                        return Ok(e.break_value.unwrap_or(Value::Nil));
                                    }
                                    Err(e) => return Err(e),
                                }
                                i -= 1;
                            }
                        }
                        Ok(acc)
                    }
                    None => {
                        // Unbounded range - must use break to terminate
                        let mut i = *start;
                        loop {
                            match self
                                .call_callable_sync(&folder, vec![acc.clone(), Value::Integer(i)])
                            {
                                Ok(v) => acc = v,
                                Err(e) if e.is_break => {
                                    return Ok(e.break_value.unwrap_or(Value::Nil));
                                }
                                Err(e) => return Err(e),
                            }
                            i += 1;
                        }
                    }
                }
            }
            Value::LazySequence(seq) => {
                let mut seq_clone = seq.borrow().clone();
                while let Some(elem) = self.lazy_seq_next_with_callback(&mut seq_clone)? {
                    match self.call_callable_sync(&folder, vec![acc.clone(), elem]) {
                        Ok(v) => acc = v,
                        Err(e) if e.is_break => {
                            return Ok(e.break_value.unwrap_or(Value::Nil));
                        }
                        Err(e) => return Err(e),
                    }
                }
                Ok(acc)
            }
            _ => Err(RuntimeError::new(
                format!("fold does not support {}", collection.type_name()),
                line,
            )),
        }
    }

    /// fold_s(initial, folder, collection) → Value
    /// Fold with state - accumulator is a list where first element is result
    fn builtin_fold_s(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let initial = &args[0];
        let folder = &args[1];
        let collection = &args[2];

        let closure = match folder {
            Value::Function(c) => c.clone(),
            _ => {
                return Err(RuntimeError::new(
                    format!(
                        "fold_s expects Function as second argument, got {}",
                        folder.type_name()
                    ),
                    line,
                ));
            }
        };

        let mut acc = initial.clone();

        match collection {
            Value::List(list) => {
                for elem in list.iter() {
                    acc = self.call_closure_sync(&closure, vec![acc, elem.clone()])?;
                }
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => match end {
                Some(e) => {
                    let actual_end = if *inclusive { *e } else { e - 1 };
                    if start <= &actual_end {
                        for i in *start..=actual_end {
                            acc = self.call_closure_sync(&closure, vec![acc, Value::Integer(i)])?;
                        }
                    } else {
                        // Reverse range (start > end)
                        for i in (actual_end..=*start).rev() {
                            acc = self.call_closure_sync(&closure, vec![acc, Value::Integer(i)])?;
                        }
                    }
                }
                None => {
                    return Err(RuntimeError::new(
                        "fold_s on unbounded range requires break",
                        line,
                    ));
                }
            },
            Value::LazySequence(seq) => {
                let mut seq_clone = seq.borrow().clone();
                while let Some(elem) = self.lazy_seq_next_with_callback(&mut seq_clone)? {
                    acc = self.call_closure_sync(&closure, vec![acc, elem])?;
                }
            }
            _ => {
                return Err(RuntimeError::new(
                    format!("fold_s does not support {}", collection.type_name()),
                    line,
                ));
            }
        }

        // Return first element of accumulator list
        match acc {
            Value::List(list) => Ok(list.front().cloned().unwrap_or(Value::Nil)),
            _ => Ok(acc),
        }
    }

    /// scan(initial, folder, collection) → List
    fn builtin_scan(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let initial = &args[0];
        let folder = &args[1];
        let collection = &args[2];

        let closure = match folder {
            Value::Function(c) => c.clone(),
            _ => {
                return Err(RuntimeError::new(
                    format!(
                        "scan expects Function as second argument, got {}",
                        folder.type_name()
                    ),
                    line,
                ));
            }
        };

        let arity = self.closure_arity(&closure);
        let mut acc = initial.clone();
        let mut results = Vector::new();
        results.push_back(initial.clone()); // scan includes initial value as first element

        match collection {
            Value::List(list) => {
                for (idx, elem) in list.iter().enumerate() {
                    let call_args = if arity >= 3 {
                        vec![acc, elem.clone(), Value::Integer(idx as i64)]
                    } else {
                        vec![acc, elem.clone()]
                    };
                    acc = self.call_closure_sync(&closure, call_args)?;
                    results.push_back(acc.clone());
                }
            }
            Value::Set(set) => {
                for elem in set.iter() {
                    acc = self.call_closure_sync(&closure, vec![acc, elem.clone()])?;
                    results.push_back(acc.clone());
                }
            }
            Value::Dict(dict) => {
                for (key, value) in dict.iter() {
                    let call_args = if arity >= 3 {
                        vec![acc, value.clone(), key.clone()]
                    } else {
                        vec![acc, value.clone()]
                    };
                    acc = self.call_closure_sync(&closure, call_args)?;
                    results.push_back(acc.clone());
                }
            }
            Value::String(s) => {
                for (idx, grapheme) in s.graphemes(true).enumerate() {
                    let elem = Value::String(Rc::new(grapheme.to_string()));
                    let call_args = if arity >= 3 {
                        vec![acc, elem, Value::Integer(idx as i64)]
                    } else {
                        vec![acc, elem]
                    };
                    acc = self.call_closure_sync(&closure, call_args)?;
                    results.push_back(acc.clone());
                }
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => match end {
                Some(e) => {
                    let actual_end = if *inclusive { *e } else { e - 1 };
                    if start <= &actual_end {
                        for i in *start..=actual_end {
                            acc = self.call_closure_sync(&closure, vec![acc, Value::Integer(i)])?;
                            results.push_back(acc.clone());
                        }
                    }
                }
                None => {
                    return Err(RuntimeError::new(
                        "scan on unbounded range not supported",
                        line,
                    ));
                }
            },
            _ => {
                return Err(RuntimeError::new(
                    format!("scan does not support {}", collection.type_name()),
                    line,
                ));
            }
        }

        Ok(Value::List(results))
    }

    // =========================================================================
    // Iteration Functions (§11.6)
    // =========================================================================

    /// each(side_effect, collection) → Nil
    fn builtin_each(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let side_effect = &args[0];
        let collection = &args[1];

        let closure = match side_effect {
            Value::Function(c) => c.clone(),
            _ => {
                return Err(RuntimeError::new(
                    format!(
                        "each expects Function as first argument, got {}",
                        side_effect.type_name()
                    ),
                    line,
                ));
            }
        };

        let arity = self.closure_arity(&closure);

        match collection {
            Value::List(list) => {
                for (idx, elem) in list.iter().enumerate() {
                    let call_args = if arity >= 2 {
                        vec![elem.clone(), Value::Integer(idx as i64)]
                    } else {
                        vec![elem.clone()]
                    };
                    self.call_closure_sync(&closure, call_args)?;
                }
            }
            Value::Set(set) => {
                for elem in set.iter() {
                    self.call_closure_sync(&closure, vec![elem.clone()])?;
                }
            }
            Value::Dict(dict) => {
                for (key, value) in dict.iter() {
                    let call_args = if arity >= 2 {
                        vec![value.clone(), key.clone()]
                    } else {
                        vec![value.clone()]
                    };
                    self.call_closure_sync(&closure, call_args)?;
                }
            }
            Value::String(s) => {
                for (idx, grapheme) in s.graphemes(true).enumerate() {
                    let elem = Value::String(Rc::new(grapheme.to_string()));
                    let call_args = if arity >= 2 {
                        vec![elem, Value::Integer(idx as i64)]
                    } else {
                        vec![elem]
                    };
                    self.call_closure_sync(&closure, call_args)?;
                }
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => {
                match end {
                    Some(e) => {
                        let actual_end = if *inclusive { *e } else { e - 1 };
                        if start <= &actual_end {
                            for i in *start..=actual_end {
                                match self.call_closure_sync(&closure, vec![Value::Integer(i)]) {
                                    Ok(_) => {}
                                    Err(e) if e.is_break => return Ok(Value::Nil),
                                    Err(e) => return Err(e),
                                }
                            }
                        } else {
                            // Descending range
                            let mut i = *start;
                            while i >= actual_end {
                                match self.call_closure_sync(&closure, vec![Value::Integer(i)]) {
                                    Ok(_) => {}
                                    Err(e) if e.is_break => return Ok(Value::Nil),
                                    Err(e) => return Err(e),
                                }
                                i -= 1;
                            }
                        }
                    }
                    None => {
                        // Unbounded range - must use break to terminate
                        let mut i = *start;
                        loop {
                            match self.call_closure_sync(&closure, vec![Value::Integer(i)]) {
                                Ok(_) => {}
                                Err(e) if e.is_break => return Ok(Value::Nil),
                                Err(e) => return Err(e),
                            }
                            i += 1;
                        }
                    }
                }
            }
            Value::LazySequence(seq) => {
                let mut seq_clone = seq.borrow().clone();
                while let Some(elem) = self.lazy_seq_next_with_callback(&mut seq_clone)? {
                    match self.call_closure_sync(&closure, vec![elem]) {
                        Ok(_) => {}
                        Err(e) if e.is_break => return Ok(Value::Nil),
                        Err(e) => return Err(e),
                    }
                }
            }
            _ => {
                return Err(RuntimeError::new(
                    format!("each does not support {}", collection.type_name()),
                    line,
                ));
            }
        }

        Ok(Value::Nil)
    }

    // =========================================================================
    // Update Functions (§11.3) - requires callbacks
    // =========================================================================

    /// update(key, fn, collection) → Collection
    fn builtin_update(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let key = &args[0];
        let updater = &args[1];
        let collection = &args[2];

        let closure = match updater {
            Value::Function(c) => c.clone(),
            _ => {
                return Err(RuntimeError::new(
                    format!(
                        "update expects Function as second argument, got {}",
                        updater.type_name()
                    ),
                    line,
                ));
            }
        };

        match collection {
            Value::List(list) => match key {
                Value::Integer(idx) => {
                    let len = list.len();
                    let actual_idx = if *idx < 0 {
                        (len as i64 + idx) as usize
                    } else {
                        *idx as usize
                    };
                    if actual_idx >= len {
                        return Ok(collection.clone());
                    }
                    let current = list[actual_idx].clone();
                    let new_value = self.call_closure_sync(&closure, vec![current])?;
                    let mut result = list.clone();
                    result.set(actual_idx, new_value);
                    Ok(Value::List(result))
                }
                _ => Err(RuntimeError::new(
                    format!("List index must be Integer, got {}", key.type_name()),
                    line,
                )),
            },
            Value::Dict(dict) => match dict.get(key) {
                Some(current) => {
                    let new_value = self.call_closure_sync(&closure, vec![current.clone()])?;
                    let mut result = dict.clone();
                    result.insert(key.clone(), new_value);
                    Ok(Value::Dict(result))
                }
                None => Ok(collection.clone()),
            },
            _ => Err(RuntimeError::new(
                format!(
                    "update expects List or Dictionary, got {}",
                    collection.type_name()
                ),
                line,
            )),
        }
    }

    /// update_d(key, default, fn, collection) → Collection
    fn builtin_update_d(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let key = &args[0];
        let default = &args[1];
        let updater = &args[2];
        let collection = &args[3];

        let closure = match updater {
            Value::Function(c) => c.clone(),
            _ => {
                return Err(RuntimeError::new(
                    format!(
                        "update_d expects Function as third argument, got {}",
                        updater.type_name()
                    ),
                    line,
                ));
            }
        };

        match collection {
            Value::Dict(dict) => {
                let current = dict.get(key).cloned().unwrap_or_else(|| default.clone());
                let new_value = self.call_closure_sync(&closure, vec![current])?;
                let mut result = dict.clone();
                result.insert(key.clone(), new_value);
                Ok(Value::Dict(result))
            }
            _ => Err(RuntimeError::new(
                format!(
                    "update_d expects Dictionary, got {}",
                    collection.type_name()
                ),
                line,
            )),
        }
    }

    // =========================================================================
    // Search Functions (§11.7)
    // =========================================================================

    /// find(predicate, collection) → Value | Nil
    fn builtin_find(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let predicate = &args[0];
        let collection = &args[1];

        let closure = match predicate {
            Value::Function(c) => c.clone(),
            _ => {
                return Err(RuntimeError::new(
                    format!(
                        "find expects Function as first argument, got {}",
                        predicate.type_name()
                    ),
                    line,
                ));
            }
        };

        let arity = self.closure_arity(&closure);

        match collection {
            Value::List(list) => {
                for (idx, elem) in list.iter().enumerate() {
                    let call_args = if arity >= 2 {
                        vec![elem.clone(), Value::Integer(idx as i64)]
                    } else {
                        vec![elem.clone()]
                    };
                    let result = self.call_closure_sync(&closure, call_args)?;
                    if result.is_truthy() {
                        return Ok(elem.clone());
                    }
                }
                Ok(Value::Nil)
            }
            Value::Set(set) => {
                for elem in set.iter() {
                    let result = self.call_closure_sync(&closure, vec![elem.clone()])?;
                    if result.is_truthy() {
                        return Ok(elem.clone());
                    }
                }
                Ok(Value::Nil)
            }
            Value::Dict(dict) => {
                for (key, value) in dict.iter() {
                    let call_args = if arity >= 2 {
                        vec![value.clone(), key.clone()]
                    } else {
                        vec![value.clone()]
                    };
                    let result = self.call_closure_sync(&closure, call_args)?;
                    if result.is_truthy() {
                        return Ok(value.clone());
                    }
                }
                Ok(Value::Nil)
            }
            Value::String(s) => {
                use unicode_segmentation::UnicodeSegmentation;
                for (idx, grapheme) in s.graphemes(true).enumerate() {
                    let elem = Value::String(std::rc::Rc::new(grapheme.to_string()));
                    let call_args = if arity >= 2 {
                        vec![elem.clone(), Value::Integer(idx as i64)]
                    } else {
                        vec![elem.clone()]
                    };
                    let result = self.call_closure_sync(&closure, call_args)?;
                    if result.is_truthy() {
                        return Ok(elem);
                    }
                }
                Ok(Value::Nil)
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => match end {
                Some(e) => {
                    let actual_end = if *inclusive { *e } else { e - 1 };
                    if start <= &actual_end {
                        for i in *start..=actual_end {
                            let elem = Value::Integer(i);
                            let result = self.call_closure_sync(&closure, vec![elem.clone()])?;
                            if result.is_truthy() {
                                return Ok(elem);
                            }
                        }
                    } else {
                        let mut i = *start;
                        while i >= actual_end {
                            let elem = Value::Integer(i);
                            let result = self.call_closure_sync(&closure, vec![elem.clone()])?;
                            if result.is_truthy() {
                                return Ok(elem);
                            }
                            i -= 1;
                        }
                    }
                    Ok(Value::Nil)
                }
                None => {
                    // Allow find on unbounded range - will find first match
                    let mut i = *start;
                    loop {
                        let elem = Value::Integer(i);
                        let result = self.call_closure_sync(&closure, vec![elem.clone()])?;
                        if result.is_truthy() {
                            return Ok(elem);
                        }
                        i += 1;
                    }
                }
            },
            Value::LazySequence(seq) => {
                let mut seq_clone = seq.borrow().clone();
                loop {
                    match self.lazy_seq_next_with_callback(&mut seq_clone)? {
                        Some(elem) => {
                            let result = self.call_closure_sync(&closure, vec![elem.clone()])?;
                            if result.is_truthy() {
                                return Ok(elem);
                            }
                        }
                        None => return Ok(Value::Nil),
                    }
                }
            }
            _ => Err(RuntimeError::new(
                format!("find does not support {}", collection.type_name()),
                line,
            )),
        }
    }

    /// count(predicate, collection) → Integer
    fn builtin_count(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let predicate = &args[0];
        let collection = &args[1];

        let closure = match predicate {
            Value::Function(c) => c.clone(),
            _ => {
                return Err(RuntimeError::new(
                    format!(
                        "count expects Function as first argument, got {}",
                        predicate.type_name()
                    ),
                    line,
                ));
            }
        };

        let arity = self.closure_arity(&closure);
        let mut count = 0i64;

        match collection {
            Value::List(list) => {
                for (idx, elem) in list.iter().enumerate() {
                    let call_args = if arity >= 2 {
                        vec![elem.clone(), Value::Integer(idx as i64)]
                    } else {
                        vec![elem.clone()]
                    };
                    let result = self.call_closure_sync(&closure, call_args)?;
                    if result.is_truthy() {
                        count += 1;
                    }
                }
            }
            Value::Set(set) => {
                for elem in set.iter() {
                    let result = self.call_closure_sync(&closure, vec![elem.clone()])?;
                    if result.is_truthy() {
                        count += 1;
                    }
                }
            }
            Value::Dict(dict) => {
                for (key, value) in dict.iter() {
                    let call_args = if arity >= 2 {
                        vec![value.clone(), key.clone()]
                    } else {
                        vec![value.clone()]
                    };
                    let result = self.call_closure_sync(&closure, call_args)?;
                    if result.is_truthy() {
                        count += 1;
                    }
                }
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => match end {
                Some(e) => {
                    let actual_end = if *inclusive { *e } else { e - 1 };
                    if start <= &actual_end {
                        for i in *start..=actual_end {
                            let result =
                                self.call_closure_sync(&closure, vec![Value::Integer(i)])?;
                            if result.is_truthy() {
                                count += 1;
                            }
                        }
                    } else {
                        let mut i = *start;
                        while i >= actual_end {
                            let result =
                                self.call_closure_sync(&closure, vec![Value::Integer(i)])?;
                            if result.is_truthy() {
                                count += 1;
                            }
                            i -= 1;
                        }
                    }
                }
                None => {
                    return Err(RuntimeError::new(
                        "count on unbounded range may not terminate",
                        line,
                    ));
                }
            },
            Value::String(s) => {
                for (idx, grapheme) in s.graphemes(true).enumerate() {
                    let elem = Value::String(Rc::new(grapheme.to_string()));
                    let call_args = if arity >= 2 {
                        vec![elem, Value::Integer(idx as i64)]
                    } else {
                        vec![elem]
                    };
                    let result = self.call_closure_sync(&closure, call_args)?;
                    if result.is_truthy() {
                        count += 1;
                    }
                }
            }
            _ => {
                return Err(RuntimeError::new(
                    format!("count does not support {}", collection.type_name()),
                    line,
                ));
            }
        }

        Ok(Value::Integer(count))
    }

    /// sort(comparator, collection) → List
    fn builtin_sort(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let comparator = &args[0];
        let collection = &args[1];

        // Validate comparator is callable
        if !matches!(
            comparator,
            Value::Function(_) | Value::PartialApplication { .. }
        ) {
            return Err(RuntimeError::new(
                format!(
                    "sort expects Function as first argument, got {}",
                    comparator.type_name()
                ),
                line,
            ));
        }
        let comparator = comparator.clone();

        // Collect elements to sort
        let mut elements: Vec<Value> = match collection {
            Value::List(list) => list.iter().cloned().collect(),
            Value::Set(set) => set.iter().cloned().collect(),
            Value::Range {
                start,
                end,
                inclusive,
            } => match end {
                Some(e) => {
                    let actual_end = if *inclusive { *e } else { e - 1 };
                    if start <= &actual_end {
                        (*start..=actual_end).map(Value::Integer).collect()
                    } else {
                        (actual_end..=*start).rev().map(Value::Integer).collect()
                    }
                }
                None => return Err(RuntimeError::new("Cannot sort unbounded range", line)),
            },
            _ => {
                return Err(RuntimeError::new(
                    format!("sort does not support {}", collection.type_name()),
                    line,
                ));
            }
        };

        // Simple insertion sort using comparator
        // (Could use merge sort for better performance)
        for i in 1..elements.len() {
            let key = elements[i].clone();
            let mut j = i;
            while j > 0 {
                let cmp_result = self
                    .call_callable_sync(&comparator, vec![elements[j - 1].clone(), key.clone()])?;
                let should_swap = match cmp_result {
                    Value::Integer(n) => n > 0,
                    Value::Boolean(b) => b, // Treat true as "should swap"
                    _ => false,
                };
                if !should_swap {
                    break;
                }
                elements[j] = elements[j - 1].clone();
                j -= 1;
            }
            elements[j] = key;
        }

        Ok(Value::List(elements.into_iter().collect()))
    }

    // =========================================================================
    // Predicates (§11.11)
    // =========================================================================

    /// any?(predicate, collection) → Boolean
    fn builtin_any(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let predicate = &args[0];
        let collection = &args[1];

        let closure = match predicate {
            Value::Function(c) => c.clone(),
            _ => {
                return Err(RuntimeError::new(
                    format!(
                        "any? expects Function as first argument, got {}",
                        predicate.type_name()
                    ),
                    line,
                ));
            }
        };

        let arity = self.closure_arity(&closure);

        match collection {
            Value::List(list) => {
                for (idx, elem) in list.iter().enumerate() {
                    let call_args = if arity >= 2 {
                        vec![elem.clone(), Value::Integer(idx as i64)]
                    } else {
                        vec![elem.clone()]
                    };
                    let result = self.call_closure_sync(&closure, call_args)?;
                    if result.is_truthy() {
                        return Ok(Value::Boolean(true));
                    }
                }
                Ok(Value::Boolean(false))
            }
            Value::Set(set) => {
                for elem in set.iter() {
                    let result = self.call_closure_sync(&closure, vec![elem.clone()])?;
                    if result.is_truthy() {
                        return Ok(Value::Boolean(true));
                    }
                }
                Ok(Value::Boolean(false))
            }
            Value::Dict(dict) => {
                for (key, value) in dict.iter() {
                    let call_args = if arity >= 2 {
                        vec![value.clone(), key.clone()]
                    } else {
                        vec![value.clone()]
                    };
                    let result = self.call_closure_sync(&closure, call_args)?;
                    if result.is_truthy() {
                        return Ok(Value::Boolean(true));
                    }
                }
                Ok(Value::Boolean(false))
            }
            Value::String(s) => {
                use unicode_segmentation::UnicodeSegmentation;
                for (idx, grapheme) in s.graphemes(true).enumerate() {
                    let elem = Value::String(std::rc::Rc::new(grapheme.to_string()));
                    let call_args = if arity >= 2 {
                        vec![elem, Value::Integer(idx as i64)]
                    } else {
                        vec![elem]
                    };
                    let result = self.call_closure_sync(&closure, call_args)?;
                    if result.is_truthy() {
                        return Ok(Value::Boolean(true));
                    }
                }
                Ok(Value::Boolean(false))
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => match end {
                Some(e) => {
                    let actual_end = if *inclusive { *e } else { e - 1 };
                    if start <= &actual_end {
                        for i in *start..=actual_end {
                            let result =
                                self.call_closure_sync(&closure, vec![Value::Integer(i)])?;
                            if result.is_truthy() {
                                return Ok(Value::Boolean(true));
                            }
                        }
                    } else {
                        let mut i = *start;
                        while i >= actual_end {
                            let result =
                                self.call_closure_sync(&closure, vec![Value::Integer(i)])?;
                            if result.is_truthy() {
                                return Ok(Value::Boolean(true));
                            }
                            i -= 1;
                        }
                    }
                    Ok(Value::Boolean(false))
                }
                None => Err(RuntimeError::new(
                    "any? on unbounded range may not terminate",
                    line,
                )),
            },
            Value::LazySequence(seq) => {
                let mut seq_clone = seq.borrow().clone();
                loop {
                    match self.lazy_seq_next_with_callback(&mut seq_clone)? {
                        Some(elem) => {
                            let result = self.call_closure_sync(&closure, vec![elem])?;
                            if result.is_truthy() {
                                return Ok(Value::Boolean(true));
                            }
                        }
                        None => return Ok(Value::Boolean(false)),
                    }
                }
            }
            _ => Err(RuntimeError::new(
                format!("any? does not support {}", collection.type_name()),
                line,
            )),
        }
    }

    /// all?(predicate, collection) → Boolean
    fn builtin_all(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let predicate = &args[0];
        let collection = &args[1];

        let closure = match predicate {
            Value::Function(c) => c.clone(),
            _ => {
                return Err(RuntimeError::new(
                    format!(
                        "all? expects Function as first argument, got {}",
                        predicate.type_name()
                    ),
                    line,
                ));
            }
        };

        let arity = self.closure_arity(&closure);

        match collection {
            Value::List(list) => {
                for (idx, elem) in list.iter().enumerate() {
                    let call_args = if arity >= 2 {
                        vec![elem.clone(), Value::Integer(idx as i64)]
                    } else {
                        vec![elem.clone()]
                    };
                    let result = self.call_closure_sync(&closure, call_args)?;
                    if !result.is_truthy() {
                        return Ok(Value::Boolean(false));
                    }
                }
                Ok(Value::Boolean(true))
            }
            Value::Set(set) => {
                for elem in set.iter() {
                    let result = self.call_closure_sync(&closure, vec![elem.clone()])?;
                    if !result.is_truthy() {
                        return Ok(Value::Boolean(false));
                    }
                }
                Ok(Value::Boolean(true))
            }
            Value::Dict(dict) => {
                for (key, value) in dict.iter() {
                    let call_args = if arity >= 2 {
                        vec![value.clone(), key.clone()]
                    } else {
                        vec![value.clone()]
                    };
                    let result = self.call_closure_sync(&closure, call_args)?;
                    if !result.is_truthy() {
                        return Ok(Value::Boolean(false));
                    }
                }
                Ok(Value::Boolean(true))
            }
            Value::String(s) => {
                use unicode_segmentation::UnicodeSegmentation;
                for (idx, grapheme) in s.graphemes(true).enumerate() {
                    let elem = Value::String(std::rc::Rc::new(grapheme.to_string()));
                    let call_args = if arity >= 2 {
                        vec![elem, Value::Integer(idx as i64)]
                    } else {
                        vec![elem]
                    };
                    let result = self.call_closure_sync(&closure, call_args)?;
                    if !result.is_truthy() {
                        return Ok(Value::Boolean(false));
                    }
                }
                Ok(Value::Boolean(true))
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => match end {
                Some(e) => {
                    let actual_end = if *inclusive { *e } else { e - 1 };
                    if start <= &actual_end {
                        for i in *start..=actual_end {
                            let result =
                                self.call_closure_sync(&closure, vec![Value::Integer(i)])?;
                            if !result.is_truthy() {
                                return Ok(Value::Boolean(false));
                            }
                        }
                    } else {
                        let mut i = *start;
                        while i >= actual_end {
                            let result =
                                self.call_closure_sync(&closure, vec![Value::Integer(i)])?;
                            if !result.is_truthy() {
                                return Ok(Value::Boolean(false));
                            }
                            i -= 1;
                        }
                    }
                    Ok(Value::Boolean(true))
                }
                None => Err(RuntimeError::new(
                    "all? on unbounded range may not terminate",
                    line,
                )),
            },
            Value::LazySequence(seq) => {
                let mut seq_clone = seq.borrow().clone();
                let mut idx = 0;
                while let Some(elem) = self.lazy_seq_next_with_callback(&mut seq_clone)? {
                    let call_args = if arity >= 2 {
                        vec![elem, Value::Integer(idx)]
                    } else {
                        vec![elem]
                    };
                    let result = self.call_closure_sync(&closure, call_args)?;
                    if !result.is_truthy() {
                        return Ok(Value::Boolean(false));
                    }
                    idx += 1;
                }
                Ok(Value::Boolean(true))
            }
            _ => Err(RuntimeError::new(
                format!("all? does not support {}", collection.type_name()),
                line,
            )),
        }
    }

    // =========================================================================
    // Lazy Sequence Generation (§11.12)
    // =========================================================================

    /// iterate(generator, initial) → LazySequence
    /// Generate a lazy sequence by repeatedly applying generator to previous result.
    fn builtin_iterate(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let generator = &args[0];
        let initial = &args[1];

        // Validate generator is callable
        if !matches!(
            generator,
            Value::Function(_) | Value::PartialApplication { .. } | Value::MemoizedFunction(_)
        ) {
            return Err(RuntimeError::new(
                format!(
                    "iterate expects Function as first argument, got {}",
                    generator.type_name()
                ),
                line,
            ));
        }

        Ok(Value::LazySequence(Rc::new(RefCell::new(
            LazySeq::Iterate {
                generator: generator.clone(),
                current: initial.clone(),
            },
        ))))
    }

    // =========================================================================
    // Collection Operations (§11.9)
    // =========================================================================

    /// take(n, collection) → List
    /// Take n elements from collection, with callback support for lazy sequences.
    fn builtin_take(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let total = &args[0];
        let collection = &args[1];

        let n = match total {
            Value::Integer(n) => *n as usize,
            _ => {
                return Err(RuntimeError::new(
                    format!("take expects Integer, got {}", total.type_name()),
                    line,
                ));
            }
        };

        match collection {
            Value::List(list) => {
                let count = n.min(list.len());
                Ok(Value::List(list.clone().slice(0..count)))
            }
            Value::Set(set) => Ok(Value::List(set.iter().take(n).cloned().collect())),
            Value::Range {
                start,
                end,
                inclusive,
            } => {
                let mut result = Vector::new();
                let step = match end {
                    Some(e) if e < start => -1,
                    _ => 1,
                };

                let actual_end = end.map(|e| if *inclusive { e } else { e - step });

                for i in 0..n {
                    let val = start + (i as i64) * step;
                    if let Some(e) = actual_end
                        && ((step > 0 && val > e) || (step < 0 && val < e))
                    {
                        break;
                    }
                    result.push_back(Value::Integer(val));
                }
                Ok(Value::List(result))
            }
            Value::LazySequence(seq) => {
                let mut result = Vector::new();
                let mut seq_clone = seq.borrow().clone();

                for _ in 0..n {
                    match self.lazy_seq_next_with_callback(&mut seq_clone)? {
                        Some(value) => result.push_back(value),
                        None => break,
                    }
                }

                Ok(Value::List(result))
            }
            _ => Err(RuntimeError::new(
                format!("take does not support {}", collection.type_name()),
                line,
            )),
        }
    }

    /// get(index, collection) → Value | Nil
    /// Get element at index. Returns nil if not found.
    fn builtin_get(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let index = &args[0];
        let collection = &args[1];

        match collection {
            Value::List(list) => match index {
                Value::Integer(idx) => {
                    let len = list.len() as i64;
                    let actual_idx = if *idx < 0 { len + idx } else { *idx };
                    if actual_idx < 0 || actual_idx >= len {
                        Ok(Value::Nil)
                    } else {
                        Ok(list[actual_idx as usize].clone())
                    }
                }
                _ => Err(RuntimeError::new(
                    format!("List index must be Integer, got {}", index.type_name()),
                    line,
                )),
            },
            Value::Set(set) => {
                if set.contains(index) {
                    Ok(index.clone())
                } else {
                    Ok(Value::Nil)
                }
            }
            Value::Dict(dict) => Ok(dict.get(index).cloned().unwrap_or(Value::Nil)),
            Value::String(s) => match index {
                Value::Integer(idx) => {
                    let graphemes: Vec<&str> = s.graphemes(true).collect();
                    let len = graphemes.len() as i64;
                    let actual_idx = if *idx < 0 { len + idx } else { *idx };
                    if actual_idx < 0 || actual_idx >= len {
                        Ok(Value::Nil)
                    } else {
                        Ok(Value::String(Rc::new(
                            graphemes[actual_idx as usize].to_string(),
                        )))
                    }
                }
                _ => Err(RuntimeError::new(
                    format!("String index must be Integer, got {}", index.type_name()),
                    line,
                )),
            },
            Value::Range {
                start,
                end,
                inclusive,
            } => match index {
                Value::Integer(idx) => {
                    if *idx < 0 {
                        match end {
                            Some(e) => {
                                let len = if *inclusive {
                                    (e - start).abs() + 1
                                } else {
                                    (e - start).abs()
                                };
                                let actual_idx = len + idx;
                                if actual_idx < 0 {
                                    Ok(Value::Nil)
                                } else {
                                    let step = if start <= e { 1 } else { -1 };
                                    Ok(Value::Integer(start + actual_idx * step))
                                }
                            }
                            None => Err(RuntimeError::new(
                                "Cannot use negative index on unbounded range",
                                line,
                            )),
                        }
                    } else {
                        if let Some(e) = end {
                            let len = if *inclusive {
                                (e - start).abs() + 1
                            } else {
                                (e - start).abs()
                            };
                            if *idx >= len {
                                return Ok(Value::Nil);
                            }
                        }
                        let step = match end {
                            Some(e) if e < start => -1,
                            _ => 1,
                        };
                        Ok(Value::Integer(start + idx * step))
                    }
                }
                _ => Err(RuntimeError::new(
                    format!("Range index must be Integer, got {}", index.type_name()),
                    line,
                )),
            },
            Value::LazySequence(seq) => match index {
                Value::Integer(idx) => {
                    if *idx < 0 {
                        return Err(RuntimeError::new(
                            "Cannot use negative index on lazy sequence",
                            line,
                        ));
                    }
                    // Consume idx elements to get to the requested index
                    let mut seq_clone = seq.borrow().clone();
                    for _ in 0..*idx {
                        if self.lazy_seq_next_with_callback(&mut seq_clone)?.is_none() {
                            return Ok(Value::Nil);
                        }
                    }
                    // Return the element at idx
                    match self.lazy_seq_next_with_callback(&mut seq_clone)? {
                        Some(value) => Ok(value),
                        None => Ok(Value::Nil),
                    }
                }
                _ => Err(RuntimeError::new(
                    format!(
                        "LazySequence index must be Integer, got {}",
                        index.type_name()
                    ),
                    line,
                )),
            },
            _ => Err(RuntimeError::new(
                format!("get not supported for {}", collection.type_name()),
                line,
            )),
        }
    }

    /// first(collection) → Value | Nil
    /// Get first element. Returns nil if collection is empty.
    fn builtin_first(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let collection = &args[0];

        match collection {
            Value::List(v) => Ok(v.front().cloned().unwrap_or(Value::Nil)),
            Value::Set(s) => Ok(s.iter().next().cloned().unwrap_or(Value::Nil)),
            Value::String(s) => Ok(s
                .graphemes(true)
                .next()
                .map(|g| Value::String(Rc::new(g.to_string())))
                .unwrap_or(Value::Nil)),
            Value::Range { start, .. } => Ok(Value::Integer(*start)),
            Value::LazySequence(seq) => {
                // Use callback-based iteration to support all LazySeq variants
                let mut seq_clone = seq.borrow().clone();
                match self.lazy_seq_next_with_callback(&mut seq_clone)? {
                    Some(value) => Ok(value),
                    None => Ok(Value::Nil),
                }
            }
            _ => Err(RuntimeError::new(
                format!("first not supported for {}", collection.type_name()),
                line,
            )),
        }
    }

    /// second(collection) → Value | Nil
    /// Get second element. Returns nil if collection has fewer than 2 elements.
    fn builtin_second(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let collection = &args[0];

        match collection {
            Value::List(v) => Ok(v.get(1).cloned().unwrap_or(Value::Nil)),
            Value::Set(s) => Ok(s.iter().nth(1).cloned().unwrap_or(Value::Nil)),
            Value::String(s) => Ok(s
                .graphemes(true)
                .nth(1)
                .map(|g| Value::String(Rc::new(g.to_string())))
                .unwrap_or(Value::Nil)),
            Value::Range {
                start,
                end,
                inclusive,
            } => {
                let step = match end {
                    Some(e) if *e < *start => -1,
                    _ => 1,
                };
                let second = start + step;
                if let Some(e) = end {
                    let in_range = if *inclusive {
                        (step > 0 && second <= *e) || (step < 0 && second >= *e)
                    } else {
                        (step > 0 && second < *e) || (step < 0 && second > *e)
                    };
                    if !in_range {
                        return Ok(Value::Nil);
                    }
                }
                Ok(Value::Integer(second))
            }
            Value::LazySequence(seq) => {
                // Consume two elements, return the second
                let mut seq_clone = seq.borrow().clone();
                // Skip first element
                self.lazy_seq_next_with_callback(&mut seq_clone)?;
                // Return second element (or nil if not available)
                match self.lazy_seq_next_with_callback(&mut seq_clone)? {
                    Some(value) => Ok(value),
                    None => Ok(Value::Nil),
                }
            }
            _ => Err(RuntimeError::new(
                format!("second not supported for {}", collection.type_name()),
                line,
            )),
        }
    }

    /// rest(collection) → Collection
    /// Get all but first element. Returns empty collection if input has ≤1 element.
    fn builtin_rest(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let collection = &args[0];

        match collection {
            Value::List(v) => {
                if v.is_empty() {
                    Ok(Value::List(Vector::new()))
                } else {
                    Ok(Value::List(v.clone().slice(1..)))
                }
            }
            Value::Set(s) => {
                let mut iter = s.iter();
                iter.next(); // Skip first
                Ok(Value::Set(iter.cloned().collect()))
            }
            Value::String(s) => {
                let mut graphemes = s.graphemes(true);
                graphemes.next(); // Skip first
                Ok(Value::String(Rc::new(graphemes.collect())))
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => {
                let step = match end {
                    Some(e) if *e < *start => -1,
                    _ => 1,
                };
                let new_start = start + step;

                if let Some(e) = end {
                    let still_valid = if *inclusive {
                        (step > 0 && new_start <= *e) || (step < 0 && new_start >= *e)
                    } else {
                        (step > 0 && new_start < *e) || (step < 0 && new_start > *e)
                    };
                    if !still_valid {
                        return Ok(Value::List(Vector::new()));
                    }
                }

                match end {
                    Some(e) => Ok(Value::Range {
                        start: new_start,
                        end: Some(*e),
                        inclusive: *inclusive,
                    }),
                    None => Ok(Value::Range {
                        start: new_start,
                        end: None,
                        inclusive: *inclusive,
                    }),
                }
            }
            Value::LazySequence(seq) => {
                // Optimize rest for specific LazySeq types to avoid O(n) Skip accumulation
                let seq_borrowed = seq.borrow();
                let new_seq = match &*seq_borrowed {
                    // Cycle: directly advance the index (O(1) instead of O(n) via Skip)
                    LazySeq::Cycle { source, index } => LazySeq::Cycle {
                        source: source.clone(),
                        index: index + 1,
                    },
                    // Range: advance the current position
                    LazySeq::Range {
                        current,
                        end,
                        inclusive,
                        step,
                    } => LazySeq::Range {
                        current: current + step,
                        end: *end,
                        inclusive: *inclusive,
                        step: *step,
                    },
                    // RangeStep: advance by step
                    LazySeq::RangeStep { current, end, step } => LazySeq::RangeStep {
                        current: current + step,
                        end: *end,
                        step: *step,
                    },
                    // Flatten Skip chains to avoid deep recursion
                    LazySeq::Skip {
                        source: inner_source,
                        remaining: inner_remaining,
                    } => LazySeq::Skip {
                        source: inner_source.clone(),
                        remaining: inner_remaining + 1,
                    },
                    // Default: wrap in Skip(1)
                    _ => LazySeq::Skip {
                        source: seq.clone(),
                        remaining: 1,
                    },
                };
                drop(seq_borrowed);
                Ok(Value::LazySequence(Rc::new(RefCell::new(new_seq))))
            }
            _ => Err(RuntimeError::new(
                format!("rest not supported for {}", collection.type_name()),
                line,
            )),
        }
    }

    /// list(value) → List
    /// Convert value to a list, with callback support for LazySequence.
    fn builtin_list_callback(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let value = &args[0];

        match value {
            // List (identity)
            Value::List(_) => Ok(value.clone()),

            // Set - convert to list
            Value::Set(s) => Ok(Value::List(s.iter().cloned().collect())),

            // Dictionary - returns list of [key, value] tuples
            Value::Dict(d) => {
                let tuples: Vector<Value> = d
                    .iter()
                    .map(|(k, v)| {
                        let mut tuple = Vector::new();
                        tuple.push_back(k.clone());
                        tuple.push_back(v.clone());
                        Value::List(tuple)
                    })
                    .collect();
                Ok(Value::List(tuples))
            }

            // String - each grapheme cluster
            Value::String(s) => {
                use unicode_segmentation::UnicodeSegmentation;
                let chars: Vector<Value> = s
                    .graphemes(true)
                    .map(|g| Value::String(Rc::new(g.to_string())))
                    .collect();
                Ok(Value::List(chars))
            }

            // Range - materialize to list
            Value::Range {
                start,
                end,
                inclusive,
            } => match end {
                Some(e) => {
                    let mut result = Vector::new();
                    if start <= e {
                        let actual_end = if *inclusive { *e } else { e - 1 };
                        for i in *start..=actual_end {
                            result.push_back(Value::Integer(i));
                        }
                    } else {
                        let actual_end = if *inclusive { *e } else { e + 1 };
                        let mut i = *start;
                        while i >= actual_end {
                            result.push_back(Value::Integer(i));
                            i -= 1;
                        }
                    }
                    Ok(Value::List(result))
                }
                None => Err(RuntimeError::new(
                    "Cannot convert unbounded range to list",
                    line,
                )),
            },

            // LazySequence - materialize
            Value::LazySequence(seq) => {
                let mut result = Vector::new();
                let mut seq_clone = seq.borrow().clone();
                while let Some(elem) = self.lazy_seq_next_with_callback(&mut seq_clone)? {
                    result.push_back(elem);
                }
                Ok(Value::List(result))
            }

            _ => Err(RuntimeError::new(
                format!("Cannot convert {} to list", value.type_name()),
                line,
            )),
        }
    }

    /// memoize(function) → MemoizedFunction
    /// Wrap a function with memoization cache. Per LANG.txt §11.16
    fn builtin_memoize(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        use super::value::MemoizedFn;

        let function = &args[0];

        match function {
            Value::Function(closure) => {
                let memoized = MemoizedFn {
                    closure: closure.clone(),
                    cache: im_rc::HashMap::new(),
                };
                Ok(Value::MemoizedFunction(Rc::new(RefCell::new(memoized))))
            }
            _ => Err(RuntimeError::new(
                format!("memoize expects Function, got {}", function.type_name()),
                line,
            )),
        }
    }

    /// set(value) → Set
    /// Convert value to a set, with callback support for LazySequence.
    fn builtin_set_callback(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        use im_rc::HashSet;

        let value = &args[0];

        match value {
            // Set (identity)
            Value::Set(_) => Ok(value.clone()),

            // List - convert to set
            Value::List(v) => {
                let mut result = HashSet::new();
                for elem in v.iter() {
                    if !elem.is_hashable() {
                        return Err(RuntimeError::new(
                            format!("Cannot add {} to set (not hashable)", elem.type_name()),
                            line,
                        ));
                    }
                    result.insert(elem.clone());
                }
                Ok(Value::Set(result))
            }

            // String - each grapheme cluster
            Value::String(s) => {
                use unicode_segmentation::UnicodeSegmentation;
                let chars: HashSet<Value> = s
                    .graphemes(true)
                    .map(|g| Value::String(Rc::new(g.to_string())))
                    .collect();
                Ok(Value::Set(chars))
            }

            // Range - materialize to set
            Value::Range {
                start,
                end,
                inclusive,
            } => match end {
                Some(e) => {
                    let mut result = HashSet::new();
                    if start <= e {
                        let actual_end = if *inclusive { *e } else { e - 1 };
                        for i in *start..=actual_end {
                            result.insert(Value::Integer(i));
                        }
                    } else {
                        let actual_end = if *inclusive { *e } else { e + 1 };
                        let mut i = *start;
                        while i >= actual_end {
                            result.insert(Value::Integer(i));
                            i -= 1;
                        }
                    }
                    Ok(Value::Set(result))
                }
                None => Err(RuntimeError::new(
                    "Cannot convert unbounded range to set",
                    line,
                )),
            },

            // LazySequence - materialize
            Value::LazySequence(seq) => {
                let mut result = HashSet::new();
                let mut seq_clone = seq.borrow().clone();
                while let Some(elem) = self.lazy_seq_next_with_callback(&mut seq_clone)? {
                    if !elem.is_hashable() {
                        return Err(RuntimeError::new(
                            format!("Cannot add {} to set (not hashable)", elem.type_name()),
                            line,
                        ));
                    }
                    result.insert(elem);
                }
                Ok(Value::Set(result))
            }

            _ => Err(RuntimeError::new(
                format!("Cannot convert {} to set", value.type_name()),
                line,
            )),
        }
    }

    /// sum(collection) → Integer | Decimal
    /// Sum all numeric elements in a collection.
    fn builtin_sum(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let collection = &args[0];

        // Helper to sum an iterator of values
        fn sum_values<'a>(
            iter: impl Iterator<Item = &'a Value>,
            line: u32,
        ) -> Result<Value, RuntimeError> {
            let mut has_decimal = false;
            let mut int_sum: i64 = 0;
            let mut decimal_sum: f64 = 0.0;

            for elem in iter {
                match elem {
                    Value::Integer(n) => {
                        if has_decimal {
                            decimal_sum += *n as f64;
                        } else {
                            int_sum += n;
                        }
                    }
                    Value::Decimal(d) => {
                        if !has_decimal {
                            has_decimal = true;
                            decimal_sum = int_sum as f64;
                        }
                        decimal_sum += d.0;
                    }
                    _ => {
                        return Err(RuntimeError::new(
                            format!("sum: non-numeric element {}", elem.type_name()),
                            line,
                        ));
                    }
                }
            }

            if has_decimal {
                Ok(Value::Decimal(OrderedFloat(decimal_sum)))
            } else {
                Ok(Value::Integer(int_sum))
            }
        }

        match collection {
            Value::List(list) => sum_values(list.iter(), line),
            Value::Set(set) => sum_values(set.iter(), line),
            Value::Dict(dict) => sum_values(dict.values(), line),
            Value::Range {
                start,
                end,
                inclusive,
            } => match end {
                Some(e) => {
                    let actual_end = if *inclusive { *e } else { e - 1 };
                    if *start > actual_end {
                        return Ok(Value::Integer(0));
                    }
                    // Sum of arithmetic sequence: n * (first + last) / 2
                    let n = actual_end - start + 1;
                    let sum = n * (start + actual_end) / 2;
                    Ok(Value::Integer(sum))
                }
                None => Err(RuntimeError::new("Cannot sum unbounded range", line)),
            },
            Value::LazySequence(seq) => {
                let mut has_decimal = false;
                let mut int_sum: i64 = 0;
                let mut decimal_sum: f64 = 0.0;

                let mut seq_clone = seq.borrow().clone();
                while let Some(elem) = self.lazy_seq_next_with_callback(&mut seq_clone)? {
                    match elem {
                        Value::Integer(n) => {
                            if has_decimal {
                                decimal_sum += n as f64;
                            } else {
                                int_sum += n;
                            }
                        }
                        Value::Decimal(d) => {
                            if !has_decimal {
                                has_decimal = true;
                                decimal_sum = int_sum as f64;
                            }
                            decimal_sum += d.0;
                        }
                        _ => {
                            return Err(RuntimeError::new(
                                format!("sum: non-numeric element {}", elem.type_name()),
                                line,
                            ));
                        }
                    }
                }

                if has_decimal {
                    Ok(Value::Decimal(OrderedFloat(decimal_sum)))
                } else {
                    Ok(Value::Integer(int_sum))
                }
            }
            _ => Err(RuntimeError::new(
                format!("sum does not support {}", collection.type_name()),
                line,
            )),
        }
    }

    /// size(collection) → Integer
    /// Get the size/length of a collection.
    fn builtin_size(&mut self, args: &[Value], line: u32) -> Result<Value, RuntimeError> {
        let collection = &args[0];

        let size = match collection {
            Value::List(v) => v.len() as i64,
            Value::Set(s) => s.len() as i64,
            Value::Dict(d) => d.len() as i64,
            Value::String(s) => s.graphemes(true).count() as i64,
            Value::Range {
                start,
                end,
                inclusive,
            } => match end {
                Some(e) => {
                    let diff = (e - start).abs();
                    if *inclusive { diff + 1 } else { diff }
                }
                None => {
                    return Err(RuntimeError::new(
                        "Cannot get size of unbounded range",
                        line,
                    ));
                }
            },
            Value::LazySequence(seq) => {
                // Materialize and count the lazy sequence
                let mut count: i64 = 0;
                let mut seq_clone = seq.borrow().clone();
                while self.lazy_seq_next_with_callback(&mut seq_clone)?.is_some() {
                    count += 1;
                }
                count
            }
            _ => {
                return Err(RuntimeError::new(
                    format!("size not supported for {}", collection.type_name()),
                    line,
                ));
            }
        };
        Ok(Value::Integer(size))
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

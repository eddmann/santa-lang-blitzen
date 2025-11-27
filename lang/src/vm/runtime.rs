use im_rc::{HashMap, HashSet, Vector};
use ordered_float::OrderedFloat;
use std::cell::RefCell;
use std::collections::HashMap as StdHashMap;
use std::rc::Rc;

use super::bytecode::{Chunk, CompiledFunction, OpCode};
use super::value::{Closure, Upvalue, Value};

/// Runtime error with message and optional stack trace
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: String,
    pub line: u32,
}

impl RuntimeError {
    pub fn new(message: impl Into<String>, line: u32) -> Self {
        Self {
            message: message.into(),
            line,
        }
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
}

impl VM {
    /// Create a new VM
    pub fn new() -> Self {
        Self {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
            globals: StdHashMap::new(),
            open_upvalues: Vec::new(),
        }
    }

    /// Run a compiled function
    pub fn run(&mut self, function: Rc<CompiledFunction>) -> Result<Value, RuntimeError> {
        // Create main closure (no upvalues)
        let closure = Rc::new(Closure {
            function,
            upvalues: Vec::new(),
        });

        // Push initial frame
        self.frames.push(CallFrame::new(closure, 0));

        // Run execution loop
        self.execute()
    }

    /// Main execution loop
    fn execute(&mut self) -> Result<Value, RuntimeError> {
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
                    let value = self
                        .globals
                        .get(&name)
                        .cloned()
                        .ok_or_else(|| self.error(format!("Undefined variable '{}'", name)))?;
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
                        elements.push_back(self.peek(count - 1 - i).clone());
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
                        if !value.is_hashable() {
                            return Err(self.error(format!(
                                "Cannot use {} as set element (not hashable)",
                                value.type_name()
                            )));
                        }
                        elements.insert(value);
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
                            )
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
                            let upvalue =
                                self.current_frame().closure.upvalues[upvalue_desc.index as usize]
                                    .clone();
                            upvalues.push(upvalue);
                        }
                    }

                    let closure = Rc::new(Closure { function, upvalues });
                    self.push(Value::Function(closure));
                }
                Ok(OpCode::Call) => {
                    let argc = self.read_byte() as usize;
                    self.call_value(argc)?;
                }
                Ok(OpCode::TailCall) => {
                    // TODO: Implement tail call optimization in Phase 14
                    let argc = self.read_byte() as usize;
                    self.call_value(argc)?;
                }
                Ok(OpCode::Return) => {
                    let result = self.pop();

                    // Close any open upvalues in this frame
                    let frame = self.frames.pop().unwrap();
                    self.close_upvalues(frame.stack_base);

                    // Pop locals from the stack
                    while self.stack.len() > frame.stack_base {
                        self.pop();
                    }

                    if self.frames.is_empty() {
                        // Top-level return
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

                // Built-ins (stub for now)
                Ok(OpCode::CallBuiltin) => {
                    let _builtin_id = self.read_u16();
                    let _argc = self.read_byte();
                    // TODO: Implement built-in functions in Phase 9
                    return Err(self.error("Built-in functions not yet implemented"));
                }

                // Special
                Ok(OpCode::Break) => {
                    // TODO: Implement break handling in Phase 12
                    return Err(self.error("Break not yet implemented"));
                }
                Ok(OpCode::Spread) => {
                    // TODO: Implement spread in Phase 12
                    return Err(self.error("Spread not yet implemented"));
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
            (Value::Decimal(x), Value::Integer(y)) => {
                Value::Decimal(OrderedFloat(x.0 + *y as f64))
            }
            (Value::Decimal(x), Value::Decimal(y)) => Value::Decimal(OrderedFloat(x.0 + y.0)),

            // String concatenation
            (Value::String(x), Value::String(y)) => {
                Value::String(Rc::new(format!("{}{}", x, y)))
            }

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
                )))
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
            (Value::Decimal(x), Value::Integer(y)) => {
                Value::Decimal(OrderedFloat(x.0 - *y as f64))
            }
            (Value::Decimal(x), Value::Decimal(y)) => Value::Decimal(OrderedFloat(x.0 - y.0)),

            // Set difference
            (Value::Set(x), Value::Set(y)) => Value::Set(x.clone().difference(y.clone())),

            _ => {
                return Err(self.error(format!(
                    "Cannot subtract {} from {}",
                    b.type_name(),
                    a.type_name()
                )))
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
            (Value::Decimal(x), Value::Integer(y)) => {
                Value::Decimal(OrderedFloat(x.0 * *y as f64))
            }
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
                )))
            }
        };

        self.push(result);
        Ok(())
    }

    fn binary_div(&mut self) -> Result<(), RuntimeError> {
        let b = self.pop();
        let a = self.pop();

        let result = match (&a, &b) {
            // Integer division (truncates toward zero)
            (Value::Integer(x), Value::Integer(y)) => {
                if *y == 0 {
                    return Err(self.error("Division by zero"));
                }
                // Truncate toward zero, not floor
                Value::Integer(x / y)
            }
            // Mixed and decimal division
            (Value::Integer(x), Value::Decimal(y)) => {
                if y.0 == 0.0 {
                    return Err(self.error("Division by zero"));
                }
                Value::Integer((*x as f64 / y.0) as i64)
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
                )))
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
                )))
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
                )))
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
                )))
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
                )))
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
                )))
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

            // Dictionary lookup
            (Value::Dict(dict), key) => dict.get(key).cloned().unwrap_or(Value::Nil),

            _ => {
                return Err(self.error(format!(
                    "Cannot index {} with {}",
                    collection.type_name(),
                    index.type_name()
                )))
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
                        let idx = if *n < 0 { (len + n).max(0) } else { (*n).min(len) };
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
                        let idx = if *n < 0 { (len + n).max(0) } else { (*n).min(len) };
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
            _ => {
                return Err(self.error(format!("Cannot call {}", callee.type_name())));
            }
        }

        Ok(())
    }

    fn call_closure(&mut self, closure: Rc<Closure>, argc: usize) -> Result<(), RuntimeError> {
        let arity = closure.function.arity as usize;

        if argc != arity {
            return Err(self.error(format!(
                "Expected {} arguments but got {}",
                arity, argc
            )));
        }

        if self.frames.len() >= 64 {
            return Err(self.error("Stack overflow"));
        }

        // Stack base is where the function value was (we'll pop it)
        let stack_base = self.stack.len() - argc - 1;

        // Remove the function from stack, keep args
        self.stack.remove(stack_base);

        self.frames
            .push(CallFrame::new(closure, self.stack.len() - argc));
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
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

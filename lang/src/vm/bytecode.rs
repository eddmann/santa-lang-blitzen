use super::value::Value;
use std::rc::Rc;

/// Bytecode instruction set for the santa-lang VM
/// Based on PLAN.md §4.3 Bytecode Instructions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum OpCode {
    // Stack manipulation
    /// Push constant from pool (1 byte operand: constant index)
    Constant = 0,
    /// Push constant from pool with wide index (2 byte operand)
    ConstantLong = 1,
    /// Discard top of stack
    Pop = 2,
    /// Duplicate top of stack
    Dup = 3,

    // Variables
    /// Load local variable (1 byte operand: slot index)
    GetLocal = 10,
    /// Store local variable (1 byte operand: slot index)
    SetLocal = 11,
    /// Load global by name index (1 byte operand)
    GetGlobal = 12,
    /// Store global by name index (1 byte operand)
    SetGlobal = 13,
    /// Get upvalue (1 byte operand: upvalue index)
    GetUpvalue = 14,
    /// Set upvalue (1 byte operand: upvalue index)
    SetUpvalue = 15,
    /// Close upvalue at stack top
    CloseUpvalue = 16,

    // Arithmetic (§4.1)
    /// Addition: numbers, strings, lists, sets, dicts
    Add = 20,
    /// Subtraction: numbers, sets
    Sub = 21,
    /// Multiplication: numbers, string/list repetition
    Mul = 22,
    /// Division: integer or decimal
    Div = 23,
    /// Modulo: floored (Python-style)
    Mod = 24,
    /// Unary negation
    Neg = 25,

    // Comparison (§4.2)
    /// Equal
    Eq = 30,
    /// Not equal
    Ne = 31,
    /// Less than
    Lt = 32,
    /// Less than or equal
    Le = 33,
    /// Greater than
    Gt = 34,
    /// Greater than or equal
    Ge = 35,

    // Logical (§4.4)
    /// Logical NOT (based on truthiness)
    Not = 40,

    // Collections (§4.6, §10)
    /// Create list from N stack values (1 byte operand: count)
    MakeList = 50,
    /// Create set from N stack values (1 byte operand: count)
    MakeSet = 51,
    /// Create dict from N*2 stack values (1 byte operand: pair count)
    MakeDict = 52,
    /// Create range: start, end (or nil), inclusive flag -> Range
    MakeRange = 53,
    /// Index operation: collection, index -> value
    Index = 54,
    /// Slice operation: collection, start, end -> value
    Slice = 55,

    // Functions (§8)
    /// Create closure from function index (1 byte operand)
    MakeClosure = 60,
    /// Call with N arguments (1 byte operand: arg count)
    Call = 61,
    /// Tail call optimization (1 byte operand: arg count)
    TailCall = 62,
    /// Return from function
    Return = 63,

    // Control flow (§7)
    /// Unconditional jump (2 byte signed operand: offset)
    Jump = 70,
    /// Jump if top of stack is falsy (2 byte operand)
    JumpIfFalse = 71,
    /// Jump if top of stack is truthy (2 byte operand) - short circuit
    JumpIfTrue = 72,
    /// Pop and jump if falsy (for && short-circuit)
    PopJumpIfFalse = 73,
    /// Pop and jump if truthy (for || short-circuit)
    PopJumpIfTrue = 74,

    // Built-ins (§11)
    /// Call built-in function (2 byte operand: builtin_id, 1 byte: arg_count)
    CallBuiltin = 80,

    // Special
    /// Break from iteration (for reduce/fold/each on infinite sequences)
    Break = 90,
    /// Spread collection onto stack
    Spread = 91,
    /// Nil literal
    Nil = 92,
    /// True literal
    True = 93,
    /// False literal
    False = 94,
}

impl TryFrom<u8> for OpCode {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(OpCode::Constant),
            1 => Ok(OpCode::ConstantLong),
            2 => Ok(OpCode::Pop),
            3 => Ok(OpCode::Dup),
            10 => Ok(OpCode::GetLocal),
            11 => Ok(OpCode::SetLocal),
            12 => Ok(OpCode::GetGlobal),
            13 => Ok(OpCode::SetGlobal),
            14 => Ok(OpCode::GetUpvalue),
            15 => Ok(OpCode::SetUpvalue),
            16 => Ok(OpCode::CloseUpvalue),
            20 => Ok(OpCode::Add),
            21 => Ok(OpCode::Sub),
            22 => Ok(OpCode::Mul),
            23 => Ok(OpCode::Div),
            24 => Ok(OpCode::Mod),
            25 => Ok(OpCode::Neg),
            30 => Ok(OpCode::Eq),
            31 => Ok(OpCode::Ne),
            32 => Ok(OpCode::Lt),
            33 => Ok(OpCode::Le),
            34 => Ok(OpCode::Gt),
            35 => Ok(OpCode::Ge),
            40 => Ok(OpCode::Not),
            50 => Ok(OpCode::MakeList),
            51 => Ok(OpCode::MakeSet),
            52 => Ok(OpCode::MakeDict),
            53 => Ok(OpCode::MakeRange),
            54 => Ok(OpCode::Index),
            55 => Ok(OpCode::Slice),
            60 => Ok(OpCode::MakeClosure),
            61 => Ok(OpCode::Call),
            62 => Ok(OpCode::TailCall),
            63 => Ok(OpCode::Return),
            70 => Ok(OpCode::Jump),
            71 => Ok(OpCode::JumpIfFalse),
            72 => Ok(OpCode::JumpIfTrue),
            73 => Ok(OpCode::PopJumpIfFalse),
            74 => Ok(OpCode::PopJumpIfTrue),
            80 => Ok(OpCode::CallBuiltin),
            90 => Ok(OpCode::Break),
            91 => Ok(OpCode::Spread),
            92 => Ok(OpCode::Nil),
            93 => Ok(OpCode::True),
            94 => Ok(OpCode::False),
            _ => Err(value),
        }
    }
}

/// Bytecode chunk containing compiled code
#[derive(Debug, Clone)]
pub struct Chunk {
    /// Raw bytecode
    pub code: Vec<u8>,
    /// Constant pool
    pub constants: Vec<Value>,
    /// Line number for each byte (run-length encoded would be better for production)
    pub lines: Vec<u32>,
    /// Compiled functions referenced by this chunk
    pub functions: Vec<Rc<CompiledFunction>>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
            functions: Vec::new(),
        }
    }

    /// Write an opcode to the chunk
    pub fn write(&mut self, op: OpCode, line: u32) {
        self.code.push(op as u8);
        self.lines.push(line);
    }

    /// Write a raw byte operand
    pub fn write_operand(&mut self, byte: u8) {
        // Use the same line as the previous instruction
        let line = self.lines.last().copied().unwrap_or(0);
        self.code.push(byte);
        self.lines.push(line);
    }

    /// Write a 16-bit operand (big-endian)
    pub fn write_operand_u16(&mut self, value: u16) {
        self.write_operand((value >> 8) as u8);
        self.write_operand((value & 0xFF) as u8);
    }

    /// Add a constant to the pool and return its index
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    /// Get the line number for a bytecode offset
    pub fn get_line(&self, offset: usize) -> u32 {
        self.lines.get(offset).copied().unwrap_or(0)
    }

    /// Get the current code length (useful for jump patching)
    pub fn len(&self) -> usize {
        self.code.len()
    }

    /// Check if chunk is empty
    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }

    /// Patch a jump instruction's offset
    pub fn patch_jump(&mut self, offset: usize) {
        // Calculate the jump distance from the instruction after the jump operand
        let jump = self.code.len() - offset - 2;
        if jump > u16::MAX as usize {
            panic!("Jump offset too large");
        }
        self.code[offset] = (jump >> 8) as u8;
        self.code[offset + 1] = (jump & 0xFF) as u8;
    }

    /// Disassemble a single instruction at the given offset
    /// Returns a string representation and the next offset
    pub fn disassemble_instruction(&self, offset: usize) -> String {
        let instruction = self.code[offset];
        let line = self.get_line(offset);

        let op = OpCode::try_from(instruction);
        match op {
            Ok(OpCode::Constant) => {
                let idx = self.code[offset + 1] as usize;
                let value = &self.constants[idx];
                format!("{offset:04} [{line:4}] Constant {idx} ({value})")
            }
            Ok(OpCode::ConstantLong) => {
                let idx =
                    ((self.code[offset + 1] as usize) << 8) | (self.code[offset + 2] as usize);
                let value = &self.constants[idx];
                format!("{offset:04} [{line:4}] ConstantLong {idx} ({value})")
            }
            Ok(OpCode::GetLocal) | Ok(OpCode::SetLocal) => {
                let slot = self.code[offset + 1];
                format!("{offset:04} [{line:4}] {:?} {slot}", op.unwrap())
            }
            Ok(OpCode::GetGlobal) | Ok(OpCode::SetGlobal) => {
                let idx = self.code[offset + 1] as usize;
                let name = &self.constants[idx];
                format!("{offset:04} [{line:4}] {:?} {idx} ({name})", op.unwrap())
            }
            Ok(OpCode::GetUpvalue) | Ok(OpCode::SetUpvalue) => {
                let idx = self.code[offset + 1];
                format!("{offset:04} [{line:4}] {:?} {idx}", op.unwrap())
            }
            Ok(OpCode::MakeList) | Ok(OpCode::MakeSet) | Ok(OpCode::MakeDict) => {
                let count = self.code[offset + 1];
                format!("{offset:04} [{line:4}] {:?} {count}", op.unwrap())
            }
            Ok(OpCode::MakeClosure) => {
                let idx = self.code[offset + 1];
                format!("{offset:04} [{line:4}] MakeClosure {idx}")
            }
            Ok(OpCode::Call) | Ok(OpCode::TailCall) => {
                let argc = self.code[offset + 1];
                format!("{offset:04} [{line:4}] {:?} {argc}", op.unwrap())
            }
            Ok(OpCode::Jump)
            | Ok(OpCode::JumpIfFalse)
            | Ok(OpCode::JumpIfTrue)
            | Ok(OpCode::PopJumpIfFalse)
            | Ok(OpCode::PopJumpIfTrue) => {
                let jump =
                    ((self.code[offset + 1] as u16) << 8) | (self.code[offset + 2] as u16);
                let target = offset + 3 + jump as usize;
                format!("{offset:04} [{line:4}] {:?} -> {target}", op.unwrap())
            }
            Ok(OpCode::CallBuiltin) => {
                let builtin_id =
                    ((self.code[offset + 1] as u16) << 8) | (self.code[offset + 2] as u16);
                let argc = self.code[offset + 3];
                format!("{offset:04} [{line:4}] CallBuiltin {builtin_id} ({argc} args)")
            }
            Ok(op) => format!("{offset:04} [{line:4}] {op:?}"),
            Err(byte) => format!("{offset:04} [{line:4}] Unknown({byte})"),
        }
    }

    /// Disassemble the entire chunk
    pub fn disassemble(&self, name: &str) -> String {
        let mut output = format!("== {name} ==\n");
        let mut offset = 0;

        while offset < self.code.len() {
            output.push_str(&self.disassemble_instruction(offset));
            output.push('\n');
            offset += self.instruction_size(offset);
        }

        output
    }

    /// Get the size of an instruction at the given offset
    fn instruction_size(&self, offset: usize) -> usize {
        let instruction = self.code[offset];
        match OpCode::try_from(instruction) {
            Ok(OpCode::Constant) => 2,
            Ok(OpCode::ConstantLong) => 3,
            Ok(OpCode::GetLocal)
            | Ok(OpCode::SetLocal)
            | Ok(OpCode::GetGlobal)
            | Ok(OpCode::SetGlobal)
            | Ok(OpCode::GetUpvalue)
            | Ok(OpCode::SetUpvalue) => 2,
            Ok(OpCode::MakeList)
            | Ok(OpCode::MakeSet)
            | Ok(OpCode::MakeDict)
            | Ok(OpCode::MakeClosure)
            | Ok(OpCode::Call)
            | Ok(OpCode::TailCall) => 2,
            Ok(OpCode::Jump)
            | Ok(OpCode::JumpIfFalse)
            | Ok(OpCode::JumpIfTrue)
            | Ok(OpCode::PopJumpIfFalse)
            | Ok(OpCode::PopJumpIfTrue) => 3,
            Ok(OpCode::CallBuiltin) => 4,
            _ => 1,
        }
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}

/// Compiled function representation
#[derive(Debug)]
pub struct CompiledFunction {
    /// Number of parameters
    pub arity: u8,
    /// The bytecode for this function
    pub chunk: Chunk,
    /// Function name (for debugging)
    pub name: Option<String>,
    /// Upvalue descriptors
    pub upvalues: Vec<UpvalueDesc>,
}

/// Describes how to capture an upvalue
#[derive(Debug, Clone)]
pub struct UpvalueDesc {
    /// Index into enclosing function's locals (if is_local) or upvalues (if !is_local)
    pub index: u8,
    /// True if capturing from immediately enclosing scope's locals
    pub is_local: bool,
}

impl CompiledFunction {
    pub fn new(arity: u8, name: Option<String>) -> Self {
        Self {
            arity,
            chunk: Chunk::new(),
            name,
            upvalues: Vec::new(),
        }
    }
}

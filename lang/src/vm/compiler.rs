use std::rc::Rc;

use crate::lexer::Span;
use crate::parser::ast::{
    Expr, InfixOp, LiteralPattern, MatchArm, Param, ParamKind, Pattern, PrefixOp, SpannedExpr,
    SpannedStmt, Stmt,
};

use super::builtins::BuiltinId;
use super::bytecode::{Chunk, CompiledFunction, OpCode};
use super::value::Value;

/// Compile error with source location
#[derive(Debug, Clone)]
pub struct CompileError {
    pub message: String,
    pub span: Span,
}

impl CompileError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Compile error at {}:{}: {}",
            self.span.line, self.span.column, self.message
        )
    }
}

impl std::error::Error for CompileError {}

/// Local variable in scope
#[derive(Debug, Clone)]
struct Local {
    name: String,
    depth: u32,
    mutable: bool,
    captured: bool,
}

/// Compiler state for expression compilation
pub struct Compiler {
    /// Current function being compiled
    function: CompiledFunction,
    /// Local variables in current scope
    locals: Vec<Local>,
    /// Current scope depth
    scope_depth: u32,
    /// Enclosing compiler (for nested functions)
    enclosing: Option<Box<Compiler>>,
    /// Current line for error reporting
    current_line: u32,
    /// Whether we're currently in tail position
    in_tail_position: bool,
    /// Whether current block has pre-declared locals (for forward references)
    did_predeclare_block: bool,
    /// Names of globals declared in this compilation unit (allows shadowing builtins)
    global_names: std::collections::HashSet<String>,
}

impl Compiler {
    /// Create a new top-level compiler
    pub fn new() -> Self {
        Self {
            function: CompiledFunction::new(0, None),
            locals: Vec::new(),
            scope_depth: 0,
            enclosing: None,
            current_line: 1,
            in_tail_position: false,
            did_predeclare_block: false,
            global_names: std::collections::HashSet::new(),
        }
    }

    /// Create a compiler for a nested function
    fn new_function(name: Option<String>, arity: u8, enclosing: Compiler) -> Self {
        Self::new_function_with_variadic(name, arity, false, enclosing)
    }

    fn new_function_with_variadic(name: Option<String>, arity: u8, is_variadic: bool, enclosing: Compiler) -> Self {
        let current_line = enclosing.current_line;
        // Inherit global_names from enclosing - nested functions should see same globals
        let global_names = enclosing.global_names.clone();
        let function = if is_variadic {
            CompiledFunction::new_variadic(arity, name)
        } else {
            CompiledFunction::new(arity, name)
        };
        Self {
            function,
            locals: Vec::new(),
            scope_depth: 0,
            enclosing: Some(Box::new(enclosing)),
            current_line,
            in_tail_position: false,
            did_predeclare_block: false,
            global_names,
        }
    }

    /// Compile a single expression and return the compiled function
    pub fn compile_expression(expr: &SpannedExpr) -> Result<CompiledFunction, CompileError> {
        let mut compiler = Compiler::new();
        compiler.expression(expr)?;
        compiler.emit(OpCode::Return);
        Ok(compiler.function)
    }

    /// Compile a single expression with known global names (for sections that reference
    /// globals defined in top-level statements)
    pub fn compile_expression_with_globals(
        expr: &SpannedExpr,
        global_names: &std::collections::HashSet<String>,
    ) -> Result<CompiledFunction, CompileError> {
        let mut compiler = Compiler::new();
        compiler.global_names = global_names.clone();
        compiler.expression(expr)?;
        compiler.emit(OpCode::Return);
        Ok(compiler.function)
    }

    /// Compile a block of statements
    /// The last statement's value is returned, or Nil if the last statement doesn't produce a value
    pub fn compile_statements(stmts: &[SpannedStmt]) -> Result<CompiledFunction, CompileError> {
        let mut compiler = Compiler::new();

        if stmts.is_empty() {
            // Empty program returns nil
            compiler.emit_constant(Value::Nil)?;
            compiler.emit(OpCode::Return);
            return Ok(compiler.function);
        }

        // Compile all statements
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;

            if is_last {
                // For the last statement, we want to leave its value on the stack
                match &stmt.node {
                    Stmt::Expr(expr) => {
                        // Expression statement - compile and leave value on stack
                        compiler.expression(expr)?;
                    }
                    _ => {
                        // Other statements (Let, Return, Break) - compile normally
                        compiler.statement(stmt)?;
                        // If it's not an expression, push nil as the return value
                        if !matches!(&stmt.node, Stmt::Expr(_)) {
                            compiler.emit_constant(Value::Nil)?;
                        }
                    }
                }
            } else {
                // Non-last statements - compile and pop their values
                compiler.statement(stmt)?;
                // Pop the value to avoid stack buildup. Both expression statements
                // and let statements (at global scope) leave values on the stack
                // that we don't need.
                if matches!(&stmt.node, Stmt::Expr(_) | Stmt::Let { .. }) {
                    compiler.emit(OpCode::Pop);
                }
            }
        }

        compiler.emit(OpCode::Return);
        Ok(compiler.function)
    }

    /// Compile statements and return both the function and the global names discovered.
    /// This is used by the runner to pass global names to subsequent expression compilations.
    pub fn compile_statements_with_globals(
        stmts: &[SpannedStmt],
    ) -> Result<(CompiledFunction, std::collections::HashSet<String>), CompileError> {
        let mut compiler = Compiler::new();

        if stmts.is_empty() {
            // Empty program returns nil
            compiler.emit_constant(Value::Nil)?;
            compiler.emit(OpCode::Return);
            return Ok((compiler.function, compiler.global_names));
        }

        // Compile all statements
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;

            if is_last {
                // For the last statement, we want to leave its value on the stack
                match &stmt.node {
                    Stmt::Expr(expr) => {
                        // Expression statement - compile and leave value on stack
                        compiler.expression(expr)?;
                    }
                    _ => {
                        // Other statements (Let, Return, Break) - compile normally
                        compiler.statement(stmt)?;
                        // If it's not an expression, push nil as the return value
                        if !matches!(&stmt.node, Stmt::Expr(_)) {
                            compiler.emit_constant(Value::Nil)?;
                        }
                    }
                }
            } else {
                // Non-last statements - compile and pop their values
                compiler.statement(stmt)?;
                // Pop the value to avoid stack buildup. Both expression statements
                // and let statements (at global scope) leave values on the stack
                // that we don't need.
                if matches!(&stmt.node, Stmt::Expr(_) | Stmt::Let { .. }) {
                    compiler.emit(OpCode::Pop);
                }
            }
        }

        compiler.emit(OpCode::Return);
        Ok((compiler.function, compiler.global_names))
    }

    /// Get the current chunk
    fn chunk(&mut self) -> &mut Chunk {
        &mut self.function.chunk
    }

    /// Emit an opcode
    fn emit(&mut self, op: OpCode) {
        let line = self.current_line;
        self.chunk().write(op, line);
    }

    /// Emit an opcode with a single byte operand
    fn emit_with_operand(&mut self, op: OpCode, operand: u8) {
        self.emit(op);
        self.chunk().write_operand(operand);
    }

    /// Emit a constant instruction
    fn emit_constant(&mut self, value: Value) -> Result<(), CompileError> {
        let idx = self.chunk().add_constant(value);
        if idx <= 255 {
            self.emit_with_operand(OpCode::Constant, idx as u8);
        } else if idx <= 65535 {
            self.emit(OpCode::ConstantLong);
            self.chunk().write_operand_u16(idx as u16);
        } else {
            return Err(CompileError::new(
                "Too many constants in one chunk",
                Span {
                    start: 0,
                    end: 0,
                    line: self.current_line,
                    column: 1,
                },
            ));
        }
        Ok(())
    }

    /// Emit a jump instruction and return the offset for patching
    fn emit_jump(&mut self, op: OpCode) -> usize {
        self.emit(op);
        // Write placeholder for jump offset
        self.chunk().write_operand(0xFF);
        self.chunk().write_operand(0xFF);
        self.chunk().len() - 2
    }

    /// Patch a jump instruction
    fn patch_jump(&mut self, offset: usize) {
        self.chunk().patch_jump(offset);
    }

    /// Patch a jump instruction to a specific target
    fn patch_jump_to(&mut self, offset: usize, target: usize) {
        self.chunk().patch_jump_to(offset, target);
    }

    /// Compile an expression
    fn expression(&mut self, expr: &SpannedExpr) -> Result<(), CompileError> {
        self.current_line = expr.span.line;

        match &expr.node {
            // Literals
            Expr::Integer(n) => self.emit_constant(Value::Integer(*n))?,
            Expr::Decimal(n) => {
                self.emit_constant(Value::Decimal(ordered_float::OrderedFloat(*n)))?
            }
            Expr::String(s) => self.emit_constant(Value::String(Rc::new(s.clone())))?,
            Expr::Boolean(true) => self.emit(OpCode::True),
            Expr::Boolean(false) => self.emit(OpCode::False),
            Expr::Nil => self.emit(OpCode::Nil),

            // Prefix operations
            Expr::Prefix { op, right } => {
                self.expression(right)?;
                match op {
                    PrefixOp::Neg => self.emit(OpCode::Neg),
                    PrefixOp::Not => self.emit(OpCode::Not),
                }
            }

            // Infix operations
            Expr::Infix { left, op, right } => {
                self.compile_infix(left, *op, right, expr.span)?;
            }

            // Collections
            Expr::List(elements) => {
                // Only make the list a partial application if any element IS directly
                // a placeholder or a simple placeholder expression (e.g., `_ + 1`).
                // Don't trigger partial app for deeply nested placeholders like `f(g(_, x))`.
                let has_direct_placeholder = elements.iter().any(Self::is_simple_placeholder_expr);
                if has_direct_placeholder {
                    self.compile_partial_application(expr)?;
                } else {
                    // List elements are never in tail position - they need to be collected
                    // to build the list, so any function calls inside must use regular Call
                    // (not TailCall) even if the list itself is in tail position.
                    let saved_tail = self.in_tail_position;
                    self.in_tail_position = false;
                    for elem in elements {
                        self.expression(elem)?;
                    }
                    self.in_tail_position = saved_tail;
                    if elements.len() > 255 {
                        return Err(CompileError::new(
                            "List literal too large (max 255 elements)",
                            expr.span,
                        ));
                    }
                    self.emit_with_operand(OpCode::MakeList, elements.len() as u8);
                }
            }

            Expr::Set(elements) => {
                // Set elements are never in tail position
                let saved_tail = self.in_tail_position;
                self.in_tail_position = false;
                for elem in elements {
                    self.expression(elem)?;
                }
                self.in_tail_position = saved_tail;
                if elements.len() > 255 {
                    return Err(CompileError::new(
                        "Set literal too large (max 255 elements)",
                        expr.span,
                    ));
                }
                self.emit_with_operand(OpCode::MakeSet, elements.len() as u8);
            }

            Expr::Dict(entries) => {
                // Dict keys and values are never in tail position
                let saved_tail = self.in_tail_position;
                self.in_tail_position = false;
                for (key, value) in entries {
                    self.expression(key)?;
                    self.expression(value)?;
                }
                self.in_tail_position = saved_tail;
                if entries.len() > 255 {
                    return Err(CompileError::new(
                        "Dict literal too large (max 255 entries)",
                        expr.span,
                    ));
                }
                self.emit_with_operand(OpCode::MakeDict, entries.len() as u8);
            }

            // Range expressions
            Expr::Range {
                start,
                end,
                inclusive,
            } => {
                self.expression(start)?;
                if let Some(end_expr) = end {
                    self.expression(end_expr)?;
                } else {
                    self.emit(OpCode::Nil);
                }
                // Push inclusive flag as boolean
                if *inclusive {
                    self.emit(OpCode::True);
                } else {
                    self.emit(OpCode::False);
                }
                self.emit(OpCode::MakeRange);
            }

            // Identifier lookup
            Expr::Identifier(name) => {
                self.compile_identifier(name, expr.span)?;
            }

            // Placeholder in expression context => partial application
            Expr::Placeholder => {
                // If we reach here directly, it's an error - placeholder should be
                // handled by containing expression
                return Err(CompileError::new(
                    "Placeholder '_' can only be used in expressions for partial application",
                    expr.span,
                ));
            }

            // Index operation
            Expr::Index { collection, index } => {
                self.expression(collection)?;
                self.expression(index)?;
                self.emit(OpCode::Index);
            }

            // Function definition
            Expr::Function { params, body } => {
                self.compile_function(params, body, expr.span)?;
            }

            // Function call
            Expr::Call { function, args } => {
                self.compile_call(function, args, expr.span)?;
            }

            // Infix function call (using backticks)
            Expr::InfixCall {
                function,
                left,
                right,
            } => {
                // Check for placeholders - if any, generate partial application
                if Self::contains_placeholder(left) || Self::contains_placeholder(right) {
                    self.compile_partial_infix_call(function, left, right, expr.span)?;
                } else {
                    // Check if this is a builtin - if so, emit CallBuiltin directly
                    // (avoids the wrapper closure for better performance and variadic support)
                    // Only use builtin if no local, upvalue, or global shadows it
                    if let Some(builtin_id) = BuiltinId::from_name(function)
                        && self.resolve_local(function).is_none()
                        && self.resolve_upvalue(function).is_none()
                        && !self.global_names.contains(function)
                    {
                        // Check arity for the builtin
                        let (min_arity, _max_arity) = builtin_id.arity();
                        if min_arity > 2 {
                            return Err(CompileError::new(
                                format!(
                                    "Builtin '{}' requires at least {} arguments",
                                    function, min_arity
                                ),
                                expr.span,
                            ));
                        }
                        // Compile arguments and call builtin directly
                        self.expression(left)?;
                        self.expression(right)?;
                        self.emit(OpCode::CallBuiltin);
                        self.chunk().write_operand_u16(builtin_id as u16);
                        self.chunk().write_operand(2);
                    } else {
                        // Not a builtin - compile as: function(left, right)
                        self.compile_identifier(function, expr.span)?;
                        self.expression(left)?;
                        self.expression(right)?;
                        self.emit_with_operand(OpCode::Call, 2);
                    }
                }
            }

            // Control flow - if expression
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // Save and clear tail position for condition
                let in_tail = self.in_tail_position;
                self.in_tail_position = false;

                self.expression(condition)?;
                let then_jump = self.emit_jump(OpCode::JumpIfFalse);
                self.emit(OpCode::Pop); // Pop condition if truthy

                // Restore tail position for then branch
                self.in_tail_position = in_tail;
                self.expression(then_branch)?;

                if let Some(else_expr) = else_branch {
                    let else_jump = self.emit_jump(OpCode::Jump);
                    self.patch_jump(then_jump);
                    self.emit(OpCode::Pop); // Pop condition if falsy

                    // Restore tail position for else branch
                    self.in_tail_position = in_tail;
                    self.expression(else_expr)?;
                    self.patch_jump(else_jump);
                } else {
                    let else_jump = self.emit_jump(OpCode::Jump);
                    self.patch_jump(then_jump);
                    self.emit(OpCode::Pop); // Pop condition
                    self.emit(OpCode::Nil); // if without else returns nil
                    self.patch_jump(else_jump);
                }

                // Clear tail position after if
                self.in_tail_position = false;
            }

            // Block expression
            Expr::Block(stmts) => {
                self.compile_block(stmts, expr.span)?;
            }

            // Assignment
            Expr::Assignment { name, value } => {
                self.expression(value)?;
                self.compile_assignment(name, expr.span)?;
            }

            // Spread
            Expr::Spread(inner) => {
                self.expression(inner)?;
                self.emit(OpCode::Spread);
            }

            // Match expression
            Expr::Match { subject, arms } => {
                self.compile_match(subject, arms, expr.span)?;
            }

            // If-let expression
            Expr::IfLet {
                pattern,
                value,
                then_branch,
                else_branch,
            } => {
                self.compile_if_let(pattern, value, then_branch, else_branch, expr.span)?;
            }
        }

        Ok(())
    }

    /// Compile an infix operation
    fn compile_infix(
        &mut self,
        left: &SpannedExpr,
        op: InfixOp,
        right: &SpannedExpr,
        span: Span,
    ) -> Result<(), CompileError> {
        // For Pipeline and Compose: only make them partial applications if the
        // left operand is directly a placeholder (like `_ |> f`). Don't trigger
        // for nested placeholders like `x |> update_d(..., _ + 1)`.
        // For other operators: check for any placeholders in operands.
        match op {
            InfixOp::Pipeline => {
                if Self::is_simple_placeholder_expr(left) {
                    return self.compile_partial_infix(left, op, right, span);
                }
                return self.compile_pipeline(left, right, span);
            }
            InfixOp::Compose => {
                // Compose with placeholder left side: `_ >> f` means `|x| f(x)`
                if Self::is_simple_placeholder_expr(left) {
                    return self.compile_partial_infix(left, op, right, span);
                }
                return self.compile_composition(left, right, span);
            }
            _ => {}
        }

        // Check for partial application (placeholder in operands)
        // For && and ||, don't convert to partial application - placeholders in operands
        // create their own closures, and short-circuit evaluation should still work
        if Self::contains_placeholder(left) || Self::contains_placeholder(right) {
            if op != InfixOp::And && op != InfixOp::Or {
                return self.compile_partial_infix(left, op, right, span);
            }
        }

        // Special handling for short-circuit operators
        match op {
            InfixOp::And => {
                // Operands are NOT in tail position
                let saved_tail = self.in_tail_position;
                self.in_tail_position = false;
                self.expression(left)?;
                let jump = self.emit_jump(OpCode::PopJumpIfFalse);
                self.expression(right)?;
                self.patch_jump(jump);
                self.in_tail_position = saved_tail;
                return Ok(());
            }
            InfixOp::Or => {
                // Operands are NOT in tail position
                let saved_tail = self.in_tail_position;
                self.in_tail_position = false;
                self.expression(left)?;
                let jump = self.emit_jump(OpCode::PopJumpIfTrue);
                self.expression(right)?;
                self.patch_jump(jump);
                self.in_tail_position = saved_tail;
                return Ok(());
            }
            InfixOp::Pipeline | InfixOp::Compose => {
                unreachable!("Handled above")
            }
            _ => {}
        }

        // Regular infix operations
        // Operands are NOT in tail position (there's an operation after)
        let saved_tail = self.in_tail_position;
        self.in_tail_position = false;
        self.expression(left)?;
        self.expression(right)?;
        self.in_tail_position = saved_tail;

        match op {
            InfixOp::Add => self.emit(OpCode::Add),
            InfixOp::Sub => self.emit(OpCode::Sub),
            InfixOp::Mul => self.emit(OpCode::Mul),
            InfixOp::Div => self.emit(OpCode::Div),
            InfixOp::Mod => self.emit(OpCode::Mod),
            InfixOp::Eq => self.emit(OpCode::Eq),
            InfixOp::Ne => self.emit(OpCode::Ne),
            InfixOp::Lt => self.emit(OpCode::Lt),
            InfixOp::Le => self.emit(OpCode::Le),
            InfixOp::Gt => self.emit(OpCode::Gt),
            InfixOp::Ge => self.emit(OpCode::Ge),
            InfixOp::And | InfixOp::Or | InfixOp::Pipeline | InfixOp::Compose => {
                unreachable!("Handled above")
            }
        }

        Ok(())
    }

    /// Compile a pipeline expression: value |> func or value |> func(args)
    fn compile_pipeline(
        &mut self,
        left: &SpannedExpr,
        right: &SpannedExpr,
        span: Span,
    ) -> Result<(), CompileError> {
        // Pipeline: left |> right
        // If right is a call with placeholder: func(_, args...) => func(left, args...)
        // If right is a call: func(args...) => func(args..., left)
        // If right is an identifier: func => func(left)

        match &right.node {
            Expr::Call { function, args } => {
                // Check if any arg is directly a placeholder - if so, substitute left
                let has_placeholder_arg = args.iter().any(|a| matches!(a.node, Expr::Placeholder));

                // Check if function is a builtin call
                // Only use builtin if no local, upvalue, or global shadows it
                if let Expr::Identifier(name) = &function.node
                    && let Some(builtin_id) = BuiltinId::from_name(name)
                    && self.resolve_local(name).is_none()
                    && self.resolve_upvalue(name).is_none()
                    && !self.global_names.contains(name)
                {
                    if has_placeholder_arg {
                        // Replace placeholder with left: builtin(args with _ replaced by left)
                        for arg in args {
                            if matches!(arg.node, Expr::Placeholder) {
                                self.expression(left)?;
                            } else {
                                self.expression(arg)?;
                            }
                        }
                        if args.len() > 255 {
                            return Err(CompileError::new("Too many arguments", span));
                        }
                        self.emit(OpCode::CallBuiltin);
                        self.chunk().write_operand_u16(builtin_id as u16);
                        self.chunk().write_operand(args.len() as u8);
                    } else {
                        // No placeholder: append left at end: builtin(args..., left)
                        for arg in args {
                            self.expression(arg)?;
                        }
                        self.expression(left)?;
                        if args.len() + 1 > 255 {
                            return Err(CompileError::new("Too many arguments", span));
                        }
                        self.emit(OpCode::CallBuiltin);
                        self.chunk().write_operand_u16(builtin_id as u16);
                        self.chunk().write_operand((args.len() + 1) as u8);
                    }
                } else if has_placeholder_arg {
                    // Non-builtin with placeholder: func(args with _ replaced by left)
                    self.expression(function)?;
                    for arg in args {
                        if matches!(arg.node, Expr::Placeholder) {
                            self.expression(left)?;
                        } else {
                            self.expression(arg)?;
                        }
                    }
                    if args.len() > 255 {
                        return Err(CompileError::new("Too many arguments", span));
                    }
                    self.emit_with_operand(OpCode::Call, args.len() as u8);
                } else {
                    // For non-builtin calls: a |> f(b) means (f(b))(a)
                    // First compile the call f(b) to get its result
                    self.expression(right)?;
                    // Then call that result with left as argument
                    self.expression(left)?;
                    self.emit_with_operand(OpCode::Call, 1);
                }
            }
            Expr::Identifier(name) => {
                // Check if it's a builtin
                // Only use builtin if no local, upvalue, or global shadows it
                if let Some(builtin_id) = BuiltinId::from_name(name)
                    && self.resolve_local(name).is_none()
                    && self.resolve_upvalue(name).is_none()
                    && !self.global_names.contains(name)
                {
                    // Compile: builtin(left)
                    self.expression(left)?;
                    self.emit(OpCode::CallBuiltin);
                    self.chunk().write_operand_u16(builtin_id as u16);
                    self.chunk().write_operand(1);
                } else {
                    // Compile: func(left)
                    self.expression(right)?;
                    self.expression(left)?;
                    self.emit_with_operand(OpCode::Call, 1);
                }
            }
            Expr::Function { .. } => {
                // Compile: func(left)
                self.expression(right)?;
                self.expression(left)?;
                self.emit_with_operand(OpCode::Call, 1);
            }
            _ => {
                // For other expressions (e.g., partial functions), just call
                self.expression(right)?;
                self.expression(left)?;
                self.emit_with_operand(OpCode::Call, 1);
            }
        }

        Ok(())
    }

    /// Compile a pipeline expression with placeholder support
    /// Used inside partial applications where _ needs to be substituted
    fn compile_pipeline_with_placeholders(
        &mut self,
        left: &SpannedExpr,
        right: &SpannedExpr,
        span: Span,
        placeholder_idx: &mut u8,
    ) -> Result<(), CompileError> {
        // Similar to compile_pipeline but uses compile_with_placeholders for operands
        match &right.node {
            Expr::Call { function, args } => {
                // Check if function is a builtin call
                if let Expr::Identifier(name) = &function.node
                    && let Some(builtin_id) = BuiltinId::from_name(name)
                    && self.resolve_local(name).is_none()
                    && self.resolve_upvalue(name).is_none()
                {
                    // Compile: builtin(args..., left)
                    for arg in args {
                        self.compile_with_placeholders(arg, placeholder_idx)?;
                    }
                    self.compile_with_placeholders(left, placeholder_idx)?;
                    if args.len() + 1 > 255 {
                        return Err(CompileError::new("Too many arguments", span));
                    }
                    self.emit(OpCode::CallBuiltin);
                    self.chunk().write_operand_u16(builtin_id as u16);
                    self.chunk().write_operand((args.len() + 1) as u8);
                } else {
                    // Compile: func(args..., left)
                    self.compile_with_placeholders(function, placeholder_idx)?;
                    for arg in args {
                        self.compile_with_placeholders(arg, placeholder_idx)?;
                    }
                    self.compile_with_placeholders(left, placeholder_idx)?;
                    if args.len() + 1 > 255 {
                        return Err(CompileError::new("Too many arguments", span));
                    }
                    self.emit_with_operand(OpCode::Call, (args.len() + 1) as u8);
                }
            }
            Expr::Identifier(name) => {
                // Check if it's a builtin
                if let Some(builtin_id) = BuiltinId::from_name(name)
                    && self.resolve_local(name).is_none()
                    && self.resolve_upvalue(name).is_none()
                {
                    // Compile: builtin(left)
                    self.compile_with_placeholders(left, placeholder_idx)?;
                    self.emit(OpCode::CallBuiltin);
                    self.chunk().write_operand_u16(builtin_id as u16);
                    self.chunk().write_operand(1);
                } else {
                    // Compile: func(left)
                    self.compile_with_placeholders(right, placeholder_idx)?;
                    self.compile_with_placeholders(left, placeholder_idx)?;
                    self.emit_with_operand(OpCode::Call, 1);
                }
            }
            Expr::Function { .. } => {
                // Compile: func(left)
                self.compile_with_placeholders(right, placeholder_idx)?;
                self.compile_with_placeholders(left, placeholder_idx)?;
                self.emit_with_operand(OpCode::Call, 1);
            }
            _ => {
                // For other expressions, just call
                self.compile_with_placeholders(right, placeholder_idx)?;
                self.compile_with_placeholders(left, placeholder_idx)?;
                self.emit_with_operand(OpCode::Call, 1);
            }
        }

        Ok(())
    }

    /// Compile composition expression with placeholder support
    /// `(_ + 1) >> (_ * 2)` creates |x| (x + 1) * 2
    /// The left side's result flows into the right side's placeholder
    fn compile_compose_with_placeholders(
        &mut self,
        left: &SpannedExpr,
        right: &SpannedExpr,
        span: Span,
        placeholder_idx: &mut u8,
    ) -> Result<(), CompileError> {
        // For composition with placeholders:
        // 1. Compile left side (its placeholders use the input arg)
        // 2. Store intermediate result
        // 3. Compile right side using the intermediate result for its placeholders

        // Compile left operand with current placeholder index
        self.compile_with_placeholders(left, placeholder_idx)?;

        // Now the result of left is on the stack
        // We need to store it in a local and use it for right side's placeholders

        // Add a local slot for intermediate result
        let result_slot = self.locals.len() as u8;
        self.locals.push(Local {
            name: String::from("__compose_result"),
            depth: self.scope_depth,
            mutable: false,
            captured: false,
        });

        // Right side: replace its placeholders with the intermediate result
        // We use a special compilation that references the result slot instead of arg slot
        self.compile_expr_with_slot_substitution(right, result_slot, span)?;

        // Pop the intermediate local
        self.locals.pop();

        Ok(())
    }

    /// Compile an expression substituting placeholders with a specific local slot
    fn compile_expr_with_slot_substitution(
        &mut self,
        expr: &SpannedExpr,
        slot: u8,
        span: Span,
    ) -> Result<(), CompileError> {
        match &expr.node {
            Expr::Placeholder => {
                self.emit_with_operand(OpCode::GetLocal, slot);
            }
            Expr::Infix { left, op, right } => {
                match op {
                    InfixOp::And => {
                        self.compile_expr_with_slot_substitution(left, slot, span)?;
                        let jump = self.emit_jump(OpCode::PopJumpIfFalse);
                        self.compile_expr_with_slot_substitution(right, slot, span)?;
                        self.patch_jump(jump);
                    }
                    InfixOp::Or => {
                        self.compile_expr_with_slot_substitution(left, slot, span)?;
                        let jump = self.emit_jump(OpCode::PopJumpIfTrue);
                        self.compile_expr_with_slot_substitution(right, slot, span)?;
                        self.patch_jump(jump);
                    }
                    _ => {
                        self.compile_expr_with_slot_substitution(left, slot, span)?;
                        self.compile_expr_with_slot_substitution(right, slot, span)?;
                        match op {
                            InfixOp::Add => self.emit(OpCode::Add),
                            InfixOp::Sub => self.emit(OpCode::Sub),
                            InfixOp::Mul => self.emit(OpCode::Mul),
                            InfixOp::Div => self.emit(OpCode::Div),
                            InfixOp::Mod => self.emit(OpCode::Mod),
                            InfixOp::Eq => self.emit(OpCode::Eq),
                            InfixOp::Ne => self.emit(OpCode::Ne),
                            InfixOp::Lt => self.emit(OpCode::Lt),
                            InfixOp::Le => self.emit(OpCode::Le),
                            InfixOp::Gt => self.emit(OpCode::Gt),
                            InfixOp::Ge => self.emit(OpCode::Ge),
                            InfixOp::And | InfixOp::Or => unreachable!("Handled above"),
                            InfixOp::Pipeline | InfixOp::Compose => {
                                return Err(CompileError::new(
                                    "Nested pipeline/compose in composition not supported",
                                    span,
                                ));
                            }
                        }
                    }
                }
            }
            Expr::Prefix { op, right } => {
                self.compile_expr_with_slot_substitution(right, slot, span)?;
                match op {
                    PrefixOp::Neg => self.emit(OpCode::Neg),
                    PrefixOp::Not => self.emit(OpCode::Not),
                }
            }
            // For non-placeholder expressions, just compile normally
            _ => {
                self.expression(expr)?;
            }
        }
        Ok(())
    }

    /// Compile function composition: f >> g creates |x| g(f(x))
    /// Special case: f >> g(a, b) creates |x| g(a, b, f(x))
    fn compile_composition(
        &mut self,
        left: &SpannedExpr,
        right: &SpannedExpr,
        span: Span,
    ) -> Result<(), CompileError> {
        // Function composition creates a new function that applies f then g
        // We compile this as a closure that captures both functions

        // Store enclosing compiler
        let enclosing = std::mem::take(self);
        *self = Compiler::new_function(None, 1, enclosing);

        // Add parameter slot for x
        self.locals.push(Local {
            name: String::new(), // Anonymous parameter
            depth: 0,
            mutable: false,
            captured: false,
        });

        // Check if right is a Call expression
        // If so: f >> g(a, b) creates |x| g(a, b, f(x))
        // Otherwise: f >> g creates |x| g(f(x))
        match &right.node {
            Expr::Call { function, args } => {
                // Check if the right function is a builtin
                let is_right_builtin = if let Expr::Identifier(name) = &function.node {
                    BuiltinId::from_name(name).is_some()
                        && self.resolve_local(name).is_none()
                        && self.resolve_upvalue(name).is_none()
                } else {
                    false
                };

                // Check if any args are DIRECT placeholders (not nested inside expressions)
                // In composition context: f >> g(_, b) means put left(x) in place of _
                // But f >> g(_ + 1) means g creates a partial, and we call that with left(x)
                let has_direct_placeholders =
                    args.iter().any(|arg| matches!(arg.node, Expr::Placeholder));

                if has_direct_placeholders {
                    // f >> g(_, b) creates |x| g(f(x), b)
                    // First compile left(x) and store in a temporary
                    match &left.node {
                        Expr::Call { function: left_fn, args: left_args } => {
                            // Check if left function is a builtin
                            let is_left_builtin = if let Expr::Identifier(name) = &left_fn.node {
                                BuiltinId::from_name(name).is_some()
                                    && self.resolve_local(name).is_none()
                                    && self.resolve_upvalue(name).is_none()
                            } else {
                                false
                            };

                            if !is_left_builtin {
                                self.expression(left_fn)?;
                            }

                            for arg in left_args {
                                self.expression(arg)?;
                            }

                            self.emit_with_operand(OpCode::GetLocal, 0);

                            let left_total_args = left_args.len() + 1;
                            if is_left_builtin {
                                if let Expr::Identifier(name) = &left_fn.node {
                                    let builtin_id = BuiltinId::from_name(name).unwrap();
                                    self.emit(OpCode::CallBuiltin);
                                    self.chunk().write_operand_u16(builtin_id as u16);
                                    self.chunk().write_operand(left_total_args as u8);
                                }
                            } else {
                                self.emit_with_operand(OpCode::Call, left_total_args as u8);
                            }
                        }
                        _ => {
                            // left is f, compile as f(x)
                            let is_left_builtin = if let Expr::Identifier(name) = &left.node {
                                BuiltinId::from_name(name).is_some()
                                    && self.resolve_local(name).is_none()
                                    && self.resolve_upvalue(name).is_none()
                            } else {
                                false
                            };

                            if !is_left_builtin {
                                self.emit_upvalue_load_or_expression(left, span)?;
                            }

                            self.emit_with_operand(OpCode::GetLocal, 0);

                            if is_left_builtin {
                                if let Expr::Identifier(name) = &left.node {
                                    let builtin_id = BuiltinId::from_name(name).unwrap();
                                    self.emit(OpCode::CallBuiltin);
                                    self.chunk().write_operand_u16(builtin_id as u16);
                                    self.chunk().write_operand(1);
                                }
                            } else {
                                self.emit_with_operand(OpCode::Call, 1);
                            }
                        }
                    }

                    // Now left(x) result is on stack. Store it in local slot 1.
                    self.locals.push(Local {
                        name: String::from("__compose_result"),
                        depth: 0,
                        mutable: false,
                        captured: false,
                    });

                    // Compile function g for non-builtins
                    if !is_right_builtin {
                        self.expression(function)?;
                    }

                    // Compile args, replacing placeholders with the result
                    for arg in args {
                        if matches!(arg.node, Expr::Placeholder) {
                            self.emit_with_operand(OpCode::GetLocal, 1); // the stored result
                        } else {
                            self.expression(arg)?;
                        }
                    }

                    // Call g with all args
                    if is_right_builtin {
                        if let Expr::Identifier(name) = &function.node {
                            let builtin_id = BuiltinId::from_name(name).unwrap();
                            self.emit(OpCode::CallBuiltin);
                            self.chunk().write_operand_u16(builtin_id as u16);
                            self.chunk().write_operand(args.len() as u8);
                        }
                    } else {
                        self.emit_with_operand(OpCode::Call, args.len() as u8);
                    }

                    self.emit(OpCode::Return);

                    // Restore enclosing compiler
                    let compiled_fn = std::mem::replace(&mut self.function, CompiledFunction::new(0, None));
                    let enclosing = self.enclosing.take().expect("should have enclosing");
                    *self = *enclosing;

                    let fn_idx = self.chunk().functions.len();
                    self.chunk().functions.push(Rc::new(compiled_fn));
                    self.emit_with_operand(OpCode::MakeClosure, fn_idx as u8);

                    return Ok(());
                }

                // Compile: g(a, b, left(x))
                if !is_right_builtin {
                    // Step 1: Compile the function g (for non-builtins)
                    self.expression(function)?;
                }

                // Step 2: Compile all the existing arguments a, b, ...
                for arg in args {
                    self.expression(arg)?;
                }

                // Step 3: Compile left(x) as the last argument
                // If left is also a Call, append x to its args: f(a) becomes f(a, x)
                // Otherwise: f becomes f(x)
                match &left.node {
                    Expr::Call { function: left_fn, args: left_args } => {
                        // Check if left function is a builtin
                        let is_left_builtin = if let Expr::Identifier(name) = &left_fn.node {
                            BuiltinId::from_name(name).is_some()
                                && self.resolve_local(name).is_none()
                                && self.resolve_upvalue(name).is_none()
                        } else {
                            false
                        };

                        if !is_left_builtin {
                            // Compile function for non-builtins
                            self.expression(left_fn)?;
                        }

                        // Compile args
                        for arg in left_args {
                            self.expression(arg)?;
                        }

                        // Add x as last argument
                        self.emit_with_operand(OpCode::GetLocal, 0);

                        let left_total_args = left_args.len() + 1;
                        if left_total_args > 255 {
                            return Err(CompileError::new("Too many arguments in composition", span));
                        }

                        // Emit call (builtin or regular)
                        if is_left_builtin {
                            if let Expr::Identifier(name) = &left_fn.node {
                                let builtin_id = BuiltinId::from_name(name).unwrap();
                                self.emit(OpCode::CallBuiltin);
                                self.chunk().write_operand_u16(builtin_id as u16);
                                self.chunk().write_operand(left_total_args as u8);
                            }
                        } else {
                            self.emit_with_operand(OpCode::Call, left_total_args as u8);
                        }
                    }
                    _ => {
                        // left is f, so compile as f(x)
                        // Check if left is a builtin identifier
                        let is_left_builtin = if let Expr::Identifier(name) = &left.node {
                            BuiltinId::from_name(name).is_some()
                                && self.resolve_local(name).is_none()
                                && self.resolve_upvalue(name).is_none()
                        } else {
                            false
                        };

                        if !is_left_builtin {
                            self.emit_upvalue_load_or_expression(left, span)?;
                        }

                        self.emit_with_operand(OpCode::GetLocal, 0);

                        if is_left_builtin {
                            if let Expr::Identifier(name) = &left.node {
                                let builtin_id = BuiltinId::from_name(name).unwrap();
                                self.emit(OpCode::CallBuiltin);
                                self.chunk().write_operand_u16(builtin_id as u16);
                                self.chunk().write_operand(1);
                            }
                        } else {
                            self.emit_with_operand(OpCode::Call, 1);
                        }
                    }
                }

                // Step 4: Call g with all arguments (original args + left(x))
                let total_args = args.len() + 1;
                if total_args > 255 {
                    return Err(CompileError::new("Too many arguments in composition", span));
                }

                if is_right_builtin {
                    if let Expr::Identifier(name) = &function.node {
                        let builtin_id = BuiltinId::from_name(name).unwrap();
                        self.emit(OpCode::CallBuiltin);
                        self.chunk().write_operand_u16(builtin_id as u16);
                        self.chunk().write_operand(total_args as u8);
                    }
                } else {
                    self.emit_with_operand(OpCode::Call, total_args as u8);
                }
            }
            _ => {
                // Normal composition: |x| right(left(x))
                // Check if right is a builtin identifier
                let is_right_builtin = if let Expr::Identifier(name) = &right.node {
                    BuiltinId::from_name(name).is_some()
                        && self.resolve_local(name).is_none()
                        && self.resolve_upvalue(name).is_none()
                } else {
                    false
                };

                if !is_right_builtin {
                    // Compile right function reference for non-builtins
                    self.emit_upvalue_load_or_expression(right, span)?;
                }

                // Compile: left(arg)
                // If left is a Call, append x to its args
                match &left.node {
                    Expr::Call { function: left_fn, args: left_args } => {
                        // Check if left function is a builtin
                        let is_left_builtin = if let Expr::Identifier(name) = &left_fn.node {
                            BuiltinId::from_name(name).is_some()
                                && self.resolve_local(name).is_none()
                                && self.resolve_upvalue(name).is_none()
                        } else {
                            false
                        };

                        if !is_left_builtin {
                            self.expression(left_fn)?;
                        }

                        for arg in left_args {
                            self.expression(arg)?;
                        }

                        self.emit_with_operand(OpCode::GetLocal, 0);

                        let left_total_args = left_args.len() + 1;
                        if left_total_args > 255 {
                            return Err(CompileError::new("Too many arguments in composition", span));
                        }

                        if is_left_builtin {
                            if let Expr::Identifier(name) = &left_fn.node {
                                let builtin_id = BuiltinId::from_name(name).unwrap();
                                self.emit(OpCode::CallBuiltin);
                                self.chunk().write_operand_u16(builtin_id as u16);
                                self.chunk().write_operand(left_total_args as u8);
                            }
                        } else {
                            self.emit_with_operand(OpCode::Call, left_total_args as u8);
                        }
                    }
                    _ => {
                        // Check if left is a builtin identifier
                        let is_left_builtin = if let Expr::Identifier(name) = &left.node {
                            BuiltinId::from_name(name).is_some()
                                && self.resolve_local(name).is_none()
                                && self.resolve_upvalue(name).is_none()
                        } else {
                            false
                        };

                        if !is_left_builtin {
                            self.emit_upvalue_load_or_expression(left, span)?;
                        }

                        self.emit_with_operand(OpCode::GetLocal, 0);

                        if is_left_builtin {
                            if let Expr::Identifier(name) = &left.node {
                                let builtin_id = BuiltinId::from_name(name).unwrap();
                                self.emit(OpCode::CallBuiltin);
                                self.chunk().write_operand_u16(builtin_id as u16);
                                self.chunk().write_operand(1);
                            }
                        } else {
                            self.emit_with_operand(OpCode::Call, 1);
                        }
                    }
                }

                // Call right(left_result)
                if is_right_builtin {
                    if let Expr::Identifier(name) = &right.node {
                        let builtin_id = BuiltinId::from_name(name).unwrap();
                        self.emit(OpCode::CallBuiltin);
                        self.chunk().write_operand_u16(builtin_id as u16);
                        self.chunk().write_operand(1);
                    }
                } else {
                    self.emit_with_operand(OpCode::Call, 1);
                }
            }
        }

        self.emit(OpCode::Return);

        // Get the compiled function and restore enclosing
        let compiled_fn = std::mem::replace(&mut self.function, CompiledFunction::new(0, None));
        let enclosing = self.enclosing.take().expect("should have enclosing");
        *self = *enclosing;

        // Add the function to constants and emit MakeClosure
        let fn_idx = self.chunk().functions.len();
        self.chunk().functions.push(Rc::new(compiled_fn));
        self.emit_with_operand(OpCode::MakeClosure, fn_idx as u8);

        // Emit upvalue descriptions (captured variables)
        // For now, this is a simplified implementation
        // TODO: Proper upvalue handling in Phase 8

        Ok(())
    }

    /// Helper for composition - load expression or as upvalue
    fn emit_upvalue_load_or_expression(
        &mut self,
        expr: &SpannedExpr,
        _span: Span,
    ) -> Result<(), CompileError> {
        // For composition, we need to evaluate both operands when the composition
        // is created, then capture them. For now, inline the expressions.
        self.expression(expr)
    }

    /// Check if an expression contains a placeholder at the immediate level
    /// Does NOT recurse into function bodies, since placeholders in nested lambdas
    /// are scoped to that lambda, not the outer expression
    fn contains_placeholder(expr: &SpannedExpr) -> bool {
        match &expr.node {
            Expr::Placeholder => true,
            Expr::Prefix { right, .. } => Self::contains_placeholder(right),
            Expr::Infix { left, right, .. } => {
                Self::contains_placeholder(left) || Self::contains_placeholder(right)
            }
            Expr::InfixCall { left, right, .. } => {
                Self::contains_placeholder(left) || Self::contains_placeholder(right)
            }
            Expr::List(elems) => elems.iter().any(Self::contains_placeholder),
            Expr::Call { function, args } => {
                // Only check if function itself is a placeholder, or if args are DIRECTLY placeholders.
                // Don't recurse into args - placeholders inside arg expressions create partials
                // for those args only, which are consumed by the call.
                Self::contains_placeholder(function)
                    || args
                        .iter()
                        .any(|arg| matches!(arg.node, Expr::Placeholder))
            }
            Expr::Index { collection, index } => {
                Self::contains_placeholder(collection) || Self::contains_placeholder(index)
            }
            // Do NOT recurse into function bodies - placeholders there are scoped to that function
            Expr::Function { .. } => false,
            _ => false,
        }
    }

    /// Check if a list of expressions contains a placeholder
    fn contains_placeholder_expr_list(exprs: &[SpannedExpr]) -> bool {
        exprs.iter().any(Self::contains_placeholder)
    }

    /// Check if an expression is a "simple" placeholder expression.
    /// Returns true for:
    /// - Direct placeholder: `_`
    /// - Prefix with placeholder: `-_`, `!_`
    /// - Infix with placeholder(s) at top level: `_ + 1`, `2 * _`
    /// - Index with placeholder: `_[0]`, `list[_]`
    /// Returns false for:
    /// - Calls containing placeholders: `f(_)`, `g(a, _ + 1)` - these are handled by compile_call
    /// - Pipelines with placeholders: `_ |> f` - handled by compile_infix
    fn is_simple_placeholder_expr(expr: &SpannedExpr) -> bool {
        match &expr.node {
            Expr::Placeholder => true,
            Expr::Prefix { right, .. } => Self::is_simple_placeholder_expr(right),
            Expr::Infix { left, right, op } => {
                // For infix ops, check if either side is a simple placeholder expr
                // but NOT for pipeline/compose which have special handling
                !matches!(op, InfixOp::Pipeline | InfixOp::Compose)
                    && (Self::is_simple_placeholder_expr(left)
                        || Self::is_simple_placeholder_expr(right))
            }
            Expr::Index { collection, index } => {
                Self::is_simple_placeholder_expr(collection)
                    || Self::is_simple_placeholder_expr(index)
            }
            // Calls are NOT simple placeholder expressions - they have their own handling
            Expr::Call { .. } => false,
            _ => false,
        }
    }

    /// Count the number of placeholders in an expression at the immediate level
    /// Does NOT recurse into function bodies
    fn count_placeholders(expr: &SpannedExpr) -> usize {
        match &expr.node {
            Expr::Placeholder => 1,
            Expr::Prefix { right, .. } => Self::count_placeholders(right),
            Expr::Infix { left, right, .. } => {
                Self::count_placeholders(left) + Self::count_placeholders(right)
            }
            Expr::InfixCall { left, right, .. } => {
                Self::count_placeholders(left) + Self::count_placeholders(right)
            }
            Expr::List(elems) => elems.iter().map(Self::count_placeholders).sum(),
            Expr::Call { function, args } => {
                Self::count_placeholders(function)
                    + args.iter().map(Self::count_placeholders).sum::<usize>()
            }
            Expr::Index { collection, index } => {
                Self::count_placeholders(collection) + Self::count_placeholders(index)
            }
            // Do NOT recurse into function bodies
            Expr::Function { .. } => 0,
            _ => 0,
        }
    }

    /// Check if an expression is directly a placeholder (not nested inside another expr)
    fn is_direct_placeholder(expr: &SpannedExpr) -> bool {
        matches!(&expr.node, Expr::Placeholder)
    }

    /// Compile partial application from an expression with placeholders
    fn compile_partial_application(&mut self, expr: &SpannedExpr) -> Result<(), CompileError> {
        let placeholder_count = Self::count_placeholders(expr);

        // Create a new function with arity = placeholder_count
        let enclosing = std::mem::take(self);
        *self = Compiler::new_function(None, placeholder_count as u8, enclosing);

        // Add locals for each placeholder parameter
        for i in 0..placeholder_count {
            self.locals.push(Local {
                name: format!("__arg{}", i),
                depth: 0,
                mutable: false,
                captured: false,
            });
        }

        // Compile the expression, replacing placeholders with local lookups
        let mut placeholder_idx = 0;
        self.compile_with_placeholders(expr, &mut placeholder_idx)?;

        self.emit(OpCode::Return);

        // Get the compiled function and restore enclosing
        let compiled_fn = std::mem::replace(&mut self.function, CompiledFunction::new(0, None));
        let enclosing = self.enclosing.take().expect("should have enclosing");
        *self = *enclosing;

        // Add the function to constants and emit MakeClosure
        let fn_idx = self.chunk().functions.len();
        self.chunk().functions.push(Rc::new(compiled_fn));
        self.emit_with_operand(OpCode::MakeClosure, fn_idx as u8);

        Ok(())
    }

    /// Compile partial application for infix expressions
    fn compile_partial_infix(
        &mut self,
        left: &SpannedExpr,
        op: InfixOp,
        right: &SpannedExpr,
        span: Span,
    ) -> Result<(), CompileError> {
        let left_has_placeholder = Self::contains_placeholder(left);
        let right_has_placeholder = Self::contains_placeholder(right);
        // For Compose, both sides share the same input - only 1 argument needed
        let placeholder_count = if op == InfixOp::Compose {
            if left_has_placeholder || right_has_placeholder { 1 } else { 0 }
        } else {
            (if left_has_placeholder { 1 } else { 0 })
                + (if right_has_placeholder { 1 } else { 0 })
        };

        // Create a new function
        let enclosing = std::mem::take(self);
        *self = Compiler::new_function(None, placeholder_count as u8, enclosing);

        // Add locals for parameters
        for i in 0..placeholder_count {
            self.locals.push(Local {
                name: format!("__arg{}", i),
                depth: 0,
                mutable: false,
                captured: false,
            });
        }

        let mut arg_idx = 0;

        // Helper to compile an operand with potential placeholder substitution
        let compile_operand =
            |compiler: &mut Compiler, operand: &SpannedExpr, arg_idx: &mut u8| -> Result<(), CompileError> {
                if Self::contains_placeholder(operand) {
                    compiler.emit_with_operand(OpCode::GetLocal, *arg_idx);
                    *arg_idx += 1;
                } else {
                    compiler.expression(operand)?;
                }
                Ok(())
            };

        // Handle short-circuit operators specially - they need interleaved compilation with jumps
        match op {
            InfixOp::And => {
                compile_operand(self, left, &mut arg_idx)?;
                let jump = self.emit_jump(OpCode::PopJumpIfFalse);
                compile_operand(self, right, &mut arg_idx)?;
                self.patch_jump(jump);
            }
            InfixOp::Or => {
                compile_operand(self, left, &mut arg_idx)?;
                let jump = self.emit_jump(OpCode::PopJumpIfTrue);
                compile_operand(self, right, &mut arg_idx)?;
                self.patch_jump(jump);
            }
            InfixOp::Pipeline => {
                // For pipeline with placeholders, we compile it using the placeholder version
                self.compile_pipeline_with_placeholders(left, right, span, &mut arg_idx)?;
            }
            InfixOp::Compose => {
                // For compose with placeholders: (_ + 1) >> (_ * 2) creates |x| (x + 1) * 2
                self.compile_compose_with_placeholders(left, right, span, &mut arg_idx)?;
            }
            _ => {
                // Non-short-circuit operators: compile both sides then emit operation
                compile_operand(self, left, &mut arg_idx)?;
                compile_operand(self, right, &mut arg_idx)?;

                match op {
                    InfixOp::Add => self.emit(OpCode::Add),
                    InfixOp::Sub => self.emit(OpCode::Sub),
                    InfixOp::Mul => self.emit(OpCode::Mul),
                    InfixOp::Div => self.emit(OpCode::Div),
                    InfixOp::Mod => self.emit(OpCode::Mod),
                    InfixOp::Eq => self.emit(OpCode::Eq),
                    InfixOp::Ne => self.emit(OpCode::Ne),
                    InfixOp::Lt => self.emit(OpCode::Lt),
                    InfixOp::Le => self.emit(OpCode::Le),
                    InfixOp::Gt => self.emit(OpCode::Gt),
                    InfixOp::Ge => self.emit(OpCode::Ge),
                    InfixOp::And | InfixOp::Or | InfixOp::Pipeline | InfixOp::Compose => {
                        unreachable!()
                    }
                }
            }
        }

        self.emit(OpCode::Return);

        // Get the compiled function and restore enclosing
        let compiled_fn = std::mem::replace(&mut self.function, CompiledFunction::new(0, None));
        let enclosing = self.enclosing.take().expect("should have enclosing");
        *self = *enclosing;

        // Add the function to constants and emit MakeClosure
        let fn_idx = self.chunk().functions.len();
        self.chunk().functions.push(Rc::new(compiled_fn));
        self.emit_with_operand(OpCode::MakeClosure, fn_idx as u8);

        Ok(())
    }

    /// Compile expression with placeholders replaced by local lookups
    fn compile_with_placeholders(
        &mut self,
        expr: &SpannedExpr,
        placeholder_idx: &mut u8,
    ) -> Result<(), CompileError> {
        self.current_line = expr.span.line;

        match &expr.node {
            Expr::Placeholder => {
                self.emit_with_operand(OpCode::GetLocal, *placeholder_idx);
                *placeholder_idx += 1;
            }
            Expr::Integer(n) => self.emit_constant(Value::Integer(*n))?,
            Expr::Decimal(n) => {
                self.emit_constant(Value::Decimal(ordered_float::OrderedFloat(*n)))?
            }
            Expr::String(s) => self.emit_constant(Value::String(Rc::new(s.clone())))?,
            Expr::Boolean(true) => self.emit(OpCode::True),
            Expr::Boolean(false) => self.emit(OpCode::False),
            Expr::Nil => self.emit(OpCode::Nil),
            Expr::Prefix { op, right } => {
                self.compile_with_placeholders(right, placeholder_idx)?;
                match op {
                    PrefixOp::Neg => self.emit(OpCode::Neg),
                    PrefixOp::Not => self.emit(OpCode::Not),
                }
            }
            Expr::Infix { left, op, right } => {
                // Handle short-circuit operators specially
                match op {
                    InfixOp::And => {
                        self.compile_with_placeholders(left, placeholder_idx)?;
                        let jump = self.emit_jump(OpCode::PopJumpIfFalse);
                        self.compile_with_placeholders(right, placeholder_idx)?;
                        self.patch_jump(jump);
                    }
                    InfixOp::Or => {
                        self.compile_with_placeholders(left, placeholder_idx)?;
                        let jump = self.emit_jump(OpCode::PopJumpIfTrue);
                        self.compile_with_placeholders(right, placeholder_idx)?;
                        self.patch_jump(jump);
                    }
                    InfixOp::Pipeline => {
                        // Compile pipeline with placeholders supported
                        self.compile_pipeline_with_placeholders(left, right, expr.span, placeholder_idx)?;
                    }
                    InfixOp::Compose => {
                        // Compose in partial application is complex - for now, just compile
                        // the left and right with placeholder support and use regular compose logic
                        // This may need more work for complex cases
                        return Err(CompileError::new(
                            "Compose (>>) not allowed in partial application",
                            expr.span,
                        ));
                    }
                    _ => {
                        // Non-short-circuit operators
                        self.compile_with_placeholders(left, placeholder_idx)?;
                        self.compile_with_placeholders(right, placeholder_idx)?;
                        match op {
                            InfixOp::Add => self.emit(OpCode::Add),
                            InfixOp::Sub => self.emit(OpCode::Sub),
                            InfixOp::Mul => self.emit(OpCode::Mul),
                            InfixOp::Div => self.emit(OpCode::Div),
                            InfixOp::Mod => self.emit(OpCode::Mod),
                            InfixOp::Eq => self.emit(OpCode::Eq),
                            InfixOp::Ne => self.emit(OpCode::Ne),
                            InfixOp::Lt => self.emit(OpCode::Lt),
                            InfixOp::Le => self.emit(OpCode::Le),
                            InfixOp::Gt => self.emit(OpCode::Gt),
                            InfixOp::Ge => self.emit(OpCode::Ge),
                            InfixOp::And | InfixOp::Or | InfixOp::Pipeline | InfixOp::Compose => {
                                unreachable!()
                            }
                        }
                    }
                }
            }
            Expr::Identifier(name) => {
                self.compile_identifier(name, expr.span)?;
            }
            Expr::Call { function, args } => {
                // Handle function calls with placeholders in arguments
                // Check if this is a builtin call
                if let Expr::Identifier(name) = &function.node
                    && let Some(builtin_id) = BuiltinId::from_name(name)
                    && self.resolve_local(name).is_none()
                    && self.resolve_upvalue(name).is_none()
                {
                    // Compile arguments with placeholder substitution
                    for arg in args {
                        self.compile_with_placeholders(arg, placeholder_idx)?;
                    }
                    // Emit CallBuiltin instruction
                    self.emit(OpCode::CallBuiltin);
                    self.chunk().write_operand_u16(builtin_id as u16);
                    self.chunk().write_operand(args.len() as u8);
                } else {
                    // Regular function call
                    self.compile_with_placeholders(function, placeholder_idx)?;
                    for arg in args {
                        self.compile_with_placeholders(arg, placeholder_idx)?;
                    }
                    self.emit_with_operand(OpCode::Call, args.len() as u8);
                }
            }
            Expr::Index { collection, index } => {
                // Handle index operations with placeholders
                self.compile_with_placeholders(collection, placeholder_idx)?;
                self.compile_with_placeholders(index, placeholder_idx)?;
                self.emit(OpCode::Index);
            }
            Expr::List(elements) => {
                // Handle list literals with placeholders
                for elem in elements {
                    self.compile_with_placeholders(elem, placeholder_idx)?;
                }
                if elements.len() > 255 {
                    return Err(CompileError::new(
                        "List literal too large (max 255 elements)",
                        expr.span,
                    ));
                }
                self.emit_with_operand(OpCode::MakeList, elements.len() as u8);
            }
            Expr::Set(elements) => {
                // Handle set literals with placeholders
                for elem in elements {
                    self.compile_with_placeholders(elem, placeholder_idx)?;
                }
                if elements.len() > 255 {
                    return Err(CompileError::new(
                        "Set literal too large (max 255 elements)",
                        expr.span,
                    ));
                }
                self.emit_with_operand(OpCode::MakeSet, elements.len() as u8);
            }
            Expr::Dict(entries) => {
                // Handle dict literals with placeholders
                for (key, value) in entries {
                    self.compile_with_placeholders(key, placeholder_idx)?;
                    self.compile_with_placeholders(value, placeholder_idx)?;
                }
                if entries.len() > 255 {
                    return Err(CompileError::new(
                        "Dict literal too large (max 255 entries)",
                        expr.span,
                    ));
                }
                self.emit_with_operand(OpCode::MakeDict, entries.len() as u8);
            }
            Expr::Range { start, end, inclusive } => {
                // Handle range literals with placeholders
                self.compile_with_placeholders(start, placeholder_idx)?;
                if let Some(end_expr) = end {
                    self.compile_with_placeholders(end_expr, placeholder_idx)?;
                } else {
                    self.emit(OpCode::Nil);
                }
                // Push inclusive flag as boolean
                if *inclusive {
                    self.emit(OpCode::True);
                } else {
                    self.emit(OpCode::False);
                }
                self.emit(OpCode::MakeRange);
            }
            Expr::InfixCall {
                function,
                left,
                right,
            } => {
                // Handle infix calls (backtick syntax) with placeholders
                self.compile_identifier(function, expr.span)?;
                self.compile_with_placeholders(left, placeholder_idx)?;
                self.compile_with_placeholders(right, placeholder_idx)?;
                self.emit_with_operand(OpCode::Call, 2);
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // Handle if expressions with placeholders
                self.compile_with_placeholders(condition, placeholder_idx)?;
                let then_jump = self.emit_jump(OpCode::JumpIfFalse);
                self.emit(OpCode::Pop); // Pop condition if truthy
                self.compile_with_placeholders(then_branch, placeholder_idx)?;

                if let Some(else_expr) = else_branch {
                    let else_jump = self.emit_jump(OpCode::Jump);
                    self.patch_jump(then_jump);
                    self.emit(OpCode::Pop); // Pop condition if falsy
                    self.compile_with_placeholders(else_expr, placeholder_idx)?;
                    self.patch_jump(else_jump);
                } else {
                    let else_jump = self.emit_jump(OpCode::Jump);
                    self.patch_jump(then_jump);
                    self.emit(OpCode::Pop); // Pop condition
                    self.emit(OpCode::Nil); // No else => nil
                    self.patch_jump(else_jump);
                }
            }
            Expr::Block(stmts) => {
                // Handle block expressions - compile each statement
                // For simplicity, we treat blocks as a sequence of expressions
                // where only the last one's value is kept
                for (i, stmt) in stmts.iter().enumerate() {
                    self.compile_stmt_in_partial_context(stmt, placeholder_idx)?;
                    // Pop intermediate results (not the last one)
                    if i < stmts.len() - 1 {
                        self.emit(OpCode::Pop);
                    }
                }
            }
            Expr::Function { params, body } => {
                // Lambdas inside partial applications - compile normally
                // They capture from the enclosing scope, not the partial application params
                self.compile_function(params, body, expr.span)?;
            }
            Expr::Spread(inner) => {
                // Handle spread expressions with placeholders
                self.compile_with_placeholders(inner, placeholder_idx)?;
                self.emit(OpCode::Spread);
            }
            _ => {
                return Err(CompileError::new(
                    "Expression type not supported in partial application",
                    expr.span,
                ));
            }
        }
        Ok(())
    }

    /// Helper to compile a statement in partial application context
    fn compile_stmt_in_partial_context(
        &mut self,
        stmt: &SpannedStmt,
        placeholder_idx: &mut u8,
    ) -> Result<(), CompileError> {
        match &stmt.node {
            Stmt::Expr(expr) => {
                self.compile_with_placeholders(expr, placeholder_idx)?;
            }
            Stmt::Let { pattern, value, mutable } => {
                // Compile the value expression
                self.compile_with_placeholders(value, placeholder_idx)?;
                // Handle simple identifier patterns for now
                if let Pattern::Identifier(name) = pattern {
                    self.locals.push(Local {
                        name: name.clone(),
                        depth: self.scope_depth,
                        mutable: *mutable,
                        captured: false,
                    });
                } else {
                    return Err(CompileError::new(
                        "Only simple let bindings supported in partial application blocks",
                        stmt.span,
                    ));
                }
            }
            Stmt::Return(expr) => {
                self.compile_with_placeholders(expr, placeholder_idx)?;
                self.emit(OpCode::Return);
            }
            Stmt::Break(_) => {
                return Err(CompileError::new(
                    "Break not supported in partial application blocks",
                    stmt.span,
                ));
            }
        }
        Ok(())
    }

    /// Compile a function expression
    fn compile_function(
        &mut self,
        params: &[Param],
        body: &SpannedExpr,
        span: Span,
    ) -> Result<(), CompileError> {
        if params.len() > 255 {
            return Err(CompileError::new("Too many parameters (max 255)", span));
        }

        // Handle rest parameter - count regular params and detect variadic
        let mut regular_param_count = 0;
        let mut is_variadic = false;
        for param in params {
            match &param.name {
                ParamKind::Rest(_) => {
                    is_variadic = true;
                    break;
                }
                _ => regular_param_count += 1,
            }
        }

        // Create new compiler for function
        let enclosing = std::mem::take(self);
        *self = Compiler::new_function_with_variadic(None, regular_param_count as u8, is_variadic, enclosing);

        // Add parameter locals and compile destructuring if needed
        // Track pattern params and their local slots for deferred destructuring
        let mut pattern_params: Vec<(&Pattern, u8, Span)> = Vec::new();

        for (param_idx, param) in params.iter().enumerate() {
            // Record the local slot for this parameter (before adding)
            let local_slot = self.locals.len() as u8;

            match &param.name {
                ParamKind::Identifier(name) => {
                    self.locals.push(Local {
                        name: name.clone(),
                        depth: 0,
                        mutable: false,
                        captured: false,
                    });
                }
                ParamKind::Placeholder => {
                    self.locals.push(Local {
                        name: String::from("_"),
                        depth: 0,
                        mutable: false,
                        captured: false,
                    });
                }
                ParamKind::Rest(name) => {
                    self.locals.push(Local {
                        name: name.clone(),
                        depth: 0,
                        mutable: false,
                        captured: false,
                    });
                }
                ParamKind::Pattern(pattern) => {
                    // Pattern parameter: add a placeholder local at the param slot
                    self.locals.push(Local {
                        name: format!("__pattern_param_{}", param_idx),
                        depth: 0,
                        mutable: false,
                        captured: false,
                    });
                    // Defer destructuring until all params are added
                    pattern_params.push((pattern, local_slot, param.span));
                }
            }
        }

        // Now compile pattern destructuring with correct local slots
        for (pattern, local_slot, span) in pattern_params {
            self.compile_param_pattern_destructuring(pattern, local_slot, span)?;
        }

        // Compile body in tail position
        self.in_tail_position = true;
        self.expression(body)?;
        self.emit(OpCode::Return);

        // Get the compiled function and restore enclosing
        let compiled_fn = std::mem::replace(&mut self.function, CompiledFunction::new(0, None));
        let enclosing = self.enclosing.take().expect("should have enclosing");
        *self = *enclosing;

        // Add function to chunk
        let fn_idx = self.chunk().functions.len();
        if fn_idx > 255 {
            return Err(CompileError::new("Too many functions in one chunk", span));
        }
        self.chunk().functions.push(Rc::new(compiled_fn));
        self.emit_with_operand(OpCode::MakeClosure, fn_idx as u8);

        Ok(())
    }

    /// Compile a function call
    fn compile_call(
        &mut self,
        function: &SpannedExpr,
        args: &[SpannedExpr],
        span: Span,
    ) -> Result<(), CompileError> {
        if args.len() > 255 {
            return Err(CompileError::new("Too many arguments (max 255)", span));
        }

        // Check for direct placeholders in arguments - if any, generate partial application
        // Note: we only create a partial call if an argument IS a placeholder directly,
        // not if it contains one. Expressions like `_ + 1` are compiled as lambdas.
        if args.iter().any(Self::is_direct_placeholder) {
            return self.compile_partial_call(function, args, span);
        }

        // Check if this is a built-in function call
        // Only use builtin if no local, upvalue, or global shadows it
        if let Expr::Identifier(name) = &function.node
            && let Some(builtin_id) = BuiltinId::from_name(name)
            && self.resolve_local(name).is_none()
            && self.resolve_upvalue(name).is_none()
            && !self.global_names.contains(name)
        {
            let (min_arity, _max_arity) = builtin_id.arity();

            // Check if this is a partial call (fewer args than required)
            // If so, generate auto-currying
            if (args.len() as u8) < min_arity {
                return self.compile_builtin_curry(builtin_id, args, span);
            }

            // Compile arguments
            for arg in args {
                self.expression(arg)?;
            }

            // Emit CallBuiltin instruction
            self.emit(OpCode::CallBuiltin);
            self.chunk().write_operand_u16(builtin_id as u16);
            self.chunk().write_operand(args.len() as u8);
            return Ok(());
        }

        // Regular function call
        // Save tail position flag and clear it for arguments
        let in_tail = self.in_tail_position;
        self.in_tail_position = false;

        // Compile function expression
        self.expression(function)?;

        // Compile arguments
        for arg in args {
            self.expression(arg)?;
        }

        // Emit TailCall if in tail position, otherwise Call
        let opcode = if in_tail { OpCode::TailCall } else { OpCode::Call };
        self.emit_with_operand(opcode, args.len() as u8);
        Ok(())
    }

    /// Compile a partial function call (call with placeholder arguments)
    fn compile_partial_call(
        &mut self,
        function: &SpannedExpr,
        args: &[SpannedExpr],
        span: Span,
    ) -> Result<(), CompileError> {
        // Count total placeholders in all arguments
        let placeholder_count: usize = args.iter().map(Self::count_placeholders).sum();

        // Create a new function with arity = placeholder_count
        let enclosing = std::mem::take(self);
        *self = Compiler::new_function(None, placeholder_count as u8, enclosing);

        // Add locals for each placeholder parameter
        for i in 0..placeholder_count {
            self.locals.push(Local {
                name: format!("__arg{}", i),
                depth: 0,
                mutable: false,
                captured: false,
            });
        }

        let mut placeholder_idx = 0u8;

        // Check if this is a builtin call
        // Only use builtin if no local, upvalue, or global shadows it
        if let Expr::Identifier(name) = &function.node
            && let Some(builtin_id) = BuiltinId::from_name(name)
            && self.resolve_local(name).is_none()
            && self.resolve_upvalue(name).is_none()
            && !self.global_names.contains(name)
        {
            // Compile arguments with placeholder substitution
            for arg in args {
                self.compile_with_placeholders(arg, &mut placeholder_idx)?;
            }
            // Emit CallBuiltin instruction
            self.emit(OpCode::CallBuiltin);
            self.chunk().write_operand_u16(builtin_id as u16);
            self.chunk().write_operand(args.len() as u8);
        } else {
            // Regular function call
            self.compile_with_placeholders(function, &mut placeholder_idx)?;
            for arg in args {
                self.compile_with_placeholders(arg, &mut placeholder_idx)?;
            }
            self.emit_with_operand(OpCode::Call, args.len() as u8);
        }

        self.emit(OpCode::Return);

        // Get the compiled function and restore enclosing
        let compiled_fn = std::mem::replace(&mut self.function, CompiledFunction::new(0, None));
        let enclosing = self.enclosing.take().expect("should have enclosing");
        *self = *enclosing;

        // Add the function to constants and emit MakeClosure
        let fn_idx = self.chunk().functions.len();
        self.chunk().functions.push(Rc::new(compiled_fn));
        self.emit_with_operand(OpCode::MakeClosure, fn_idx as u8);

        // Check if we need to error on unsupported span usage
        let _ = span;

        Ok(())
    }

    /// Compile a partial infix call (backtick call with placeholder arguments)
    fn compile_partial_infix_call(
        &mut self,
        function: &str,
        left: &SpannedExpr,
        right: &SpannedExpr,
        span: Span,
    ) -> Result<(), CompileError> {
        // Count total placeholders
        let placeholder_count = Self::count_placeholders(left) + Self::count_placeholders(right);

        // Create a new function with arity = placeholder_count
        let enclosing = std::mem::take(self);
        *self = Compiler::new_function(None, placeholder_count as u8, enclosing);

        // Add locals for each placeholder parameter
        for i in 0..placeholder_count {
            self.locals.push(Local {
                name: format!("__arg{}", i),
                depth: 0,
                mutable: false,
                captured: false,
            });
        }

        let mut placeholder_idx = 0u8;

        // Compile as: function(left, right)
        self.compile_identifier(function, span)?;
        self.compile_with_placeholders(left, &mut placeholder_idx)?;
        self.compile_with_placeholders(right, &mut placeholder_idx)?;
        self.emit_with_operand(OpCode::Call, 2);

        self.emit(OpCode::Return);

        // Get the compiled function and restore enclosing
        let compiled_fn = std::mem::replace(&mut self.function, CompiledFunction::new(0, None));
        let enclosing = self.enclosing.take().expect("should have enclosing");
        *self = *enclosing;

        // Add the function to constants and emit MakeClosure
        let fn_idx = self.chunk().functions.len();
        self.chunk().functions.push(Rc::new(compiled_fn));
        self.emit_with_operand(OpCode::MakeClosure, fn_idx as u8);

        Ok(())
    }

    /// Compile identifier lookup
    fn compile_identifier(&mut self, name: &str, span: Span) -> Result<(), CompileError> {
        // First check locals
        if let Some(idx) = self.resolve_local(name) {
            self.emit_with_operand(OpCode::GetLocal, idx as u8);
            return Ok(());
        }

        // Then check upvalues
        if let Some(idx) = self.resolve_upvalue(name) {
            self.emit_with_operand(OpCode::GetUpvalue, idx);
            return Ok(());
        }

        // Check if this is a builtin function being used as a value
        // In this case, create a wrapper closure that calls the builtin
        // BUT only if we don't know of a global with this name (from this compilation unit)
        if !self.global_names.contains(name) {
            if let Some(builtin_id) = BuiltinId::from_name(name) {
                return self.compile_builtin_as_value(builtin_id, span);
            }
        }

        // Emit global lookup - the runtime will check globals first, allowing
        // user-defined globals to shadow builtins
        let name_idx = self
            .chunk()
            .add_constant(Value::String(Rc::new(name.to_string())));
        if name_idx > 255 {
            return Err(CompileError::new("Too many global names", span));
        }
        self.emit_with_operand(OpCode::GetGlobal, name_idx as u8);
        Ok(())
    }

    /// Compile a builtin function reference as a first-class value
    /// Creates a wrapper closure that calls the builtin
    fn compile_builtin_as_value(
        &mut self,
        builtin_id: BuiltinId,
        _span: Span,
    ) -> Result<(), CompileError> {
        let (min_arity, max_arity) = builtin_id.arity();

        // For variadic builtins, use min_arity as the wrapper's arity
        // For fixed-arity builtins, use the exact arity
        let arity = if min_arity == max_arity {
            min_arity
        } else {
            // For variadic functions, we need to handle this differently
            // For now, use minimum arity
            min_arity
        };

        // Create a wrapper function
        let enclosing = std::mem::take(self);
        *self = Compiler::new_function(None, arity, enclosing);

        // Add parameter locals
        for i in 0..arity {
            self.locals.push(Local {
                name: format!("__builtin_arg{}", i),
                depth: 0,
                mutable: false,
                captured: false,
            });
        }

        // Load all parameters and call the builtin
        for i in 0..arity {
            self.emit_with_operand(OpCode::GetLocal, i);
        }

        self.emit(OpCode::CallBuiltin);
        self.chunk().write_operand_u16(builtin_id as u16);
        self.chunk().write_operand(arity);

        self.emit(OpCode::Return);

        // Get the compiled function and restore enclosing
        let compiled_fn = std::mem::replace(&mut self.function, CompiledFunction::new(0, None));
        let enclosing = self.enclosing.take().expect("should have enclosing");
        *self = *enclosing;

        // Add the function to constants and emit MakeClosure
        let fn_idx = self.chunk().functions.len();
        self.chunk().functions.push(Rc::new(compiled_fn));
        self.emit_with_operand(OpCode::MakeClosure, fn_idx as u8);

        Ok(())
    }

    /// Compile auto-currying for builtin function called with fewer args than required
    /// Creates a closure that evaluates the given args and waits for the remaining ones
    ///
    /// For split(","):
    ///   - Generates: |__arg0| split(",", __arg0)
    ///   - The "," is a constant, re-evaluated each call (could optimize with upvalues later)
    fn compile_builtin_curry(
        &mut self,
        builtin_id: BuiltinId,
        given_args: &[SpannedExpr],
        _span: Span,
    ) -> Result<(), CompileError> {
        let (min_arity, _max_arity) = builtin_id.arity();
        let remaining_arity = min_arity - given_args.len() as u8;

        // Create a wrapper function that takes the remaining arguments
        let enclosing = std::mem::take(self);
        *self = Compiler::new_function(None, remaining_arity, enclosing);

        // Add local slots for the new parameters
        for i in 0..remaining_arity {
            self.locals.push(Local {
                name: format!("__curry_arg{}", i),
                depth: 0,
                mutable: false,
                captured: false,
            });
        }

        // Re-compile the given arguments (constants will be re-evaluated each call)
        // This is simpler than upvalue capture and works for most cases
        for arg in given_args {
            self.expression(arg)?;
        }

        // Load the new parameters
        for i in 0..remaining_arity {
            self.emit_with_operand(OpCode::GetLocal, i);
        }

        // Call the builtin with all args
        self.emit(OpCode::CallBuiltin);
        self.chunk().write_operand_u16(builtin_id as u16);
        self.chunk().write_operand(min_arity);

        self.emit(OpCode::Return);

        // Get the compiled function and restore enclosing
        let compiled_fn = std::mem::replace(&mut self.function, CompiledFunction::new(0, None));
        let enclosing = self.enclosing.take().expect("should have enclosing");
        *self = *enclosing;

        // Add the function to constants and emit MakeClosure
        let fn_idx = self.chunk().functions.len();
        self.chunk().functions.push(Rc::new(compiled_fn));
        self.emit_with_operand(OpCode::MakeClosure, fn_idx as u8);

        Ok(())
    }

    /// Compile assignment
    fn compile_assignment(&mut self, name: &str, span: Span) -> Result<(), CompileError> {
        // Set* opcodes use peek (not pop), so the value stays on the stack
        // after assignment - no need for Dup

        // Check for local
        if let Some(idx) = self.resolve_local(name) {
            let local = &self.locals[idx];
            if !local.mutable {
                return Err(CompileError::new(
                    format!("Cannot assign to immutable variable '{}'", name),
                    span,
                ));
            }
            self.emit_with_operand(OpCode::SetLocal, idx as u8);
            return Ok(());
        }

        // Check for upvalue
        if let Some(idx) = self.resolve_upvalue(name) {
            self.emit_with_operand(OpCode::SetUpvalue, idx);
            return Ok(());
        }

        // Global assignment
        let name_idx = self
            .chunk()
            .add_constant(Value::String(Rc::new(name.to_string())));
        if name_idx > 255 {
            return Err(CompileError::new("Too many global names", span));
        }
        self.emit_with_operand(OpCode::SetGlobal, name_idx as u8);
        Ok(())
    }

    /// Resolve local variable by name
    fn resolve_local(&self, name: &str) -> Option<usize> {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                return Some(i);
            }
        }
        None
    }

    /// Resolve upvalue (captured variable from enclosing scope)
    fn resolve_upvalue(&mut self, name: &str) -> Option<u8> {
        // No enclosing compiler means we're at top level - no upvalues possible
        let enclosing = self.enclosing.as_mut()?;

        // First, check if the variable is a local in the immediately enclosing scope
        if let Some(local_idx) = enclosing.resolve_local(name) {
            // Mark the local as captured
            enclosing.locals[local_idx].captured = true;
            return Some(self.add_upvalue(local_idx as u8, true));
        }

        // Otherwise, check if it's an upvalue in the enclosing scope (transitive capture)
        if let Some(upvalue_idx) = enclosing.resolve_upvalue(name) {
            return Some(self.add_upvalue(upvalue_idx, false));
        }

        None
    }

    /// Add an upvalue to the current function's upvalue list
    fn add_upvalue(&mut self, index: u8, is_local: bool) -> u8 {
        // Check if we already have this upvalue
        for (i, upvalue) in self.function.upvalues.iter().enumerate() {
            if upvalue.index == index && upvalue.is_local == is_local {
                return i as u8;
            }
        }

        // Add new upvalue
        let upvalue_count = self.function.upvalues.len();
        if upvalue_count >= 256 {
            panic!("Too many upvalues in function");
        }

        self.function
            .upvalues
            .push(super::bytecode::UpvalueDesc { index, is_local });

        upvalue_count as u8
    }

    /// Compile a block of statements
    fn compile_block(&mut self, stmts: &[SpannedStmt], _span: Span) -> Result<(), CompileError> {
        // Save tail position
        let in_tail = self.in_tail_position;

        self.begin_scope();

        // Collect unique let binding names for forward reference support
        // Don't pre-declare shadowed variables (same name appears multiple times)
        let mut seen_names = std::collections::HashSet::new();
        let mut duplicate_names = std::collections::HashSet::new();

        for stmt in stmts.iter() {
            if let Stmt::Let {
                pattern: Pattern::Identifier(name),
                ..
            } = &stmt.node
                && !seen_names.insert(name.clone()) {
                    // Name already seen - it's a shadow, not a forward reference
                    duplicate_names.insert(name.clone());
                }
        }

        // Pre-declare unique let bindings with nil to support forward references (mutual recursion)
        // Only pre-declare names that appear once (shadows don't need pre-declaration)
        let did_predeclare = seen_names.len() >= 2;
        if did_predeclare {
            for stmt in stmts.iter() {
                if let Stmt::Let {
                    mutable,
                    pattern: Pattern::Identifier(name),
                    ..
                } = &stmt.node
                {
                    // Only pre-declare if not a duplicate (not shadowing)
                    if !duplicate_names.contains(name) {
                        // Emit nil placeholder
                        self.emit(OpCode::Nil);
                        // Add local (this reserves the slot)
                        self.add_local(name.clone(), *mutable);
                    }
                }
            }
        }

        // Save pre-declaration state for compile_let to use
        let saved_predeclare_state = std::mem::replace(&mut self.did_predeclare_block, did_predeclare);

        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;

            // Only the last statement is in tail position
            self.in_tail_position = is_last && in_tail;

            self.statement(stmt)?;

            // Pop non-final expression statements
            if !is_last && matches!(&stmt.node, Stmt::Expr(_)) {
                self.emit(OpCode::Pop);
            }
        }

        // If block is empty, push nil
        if stmts.is_empty() {
            self.emit(OpCode::Nil);
        }

        // The last statement's value stays on stack
        self.end_scope();

        // Restore pre-declaration state
        self.did_predeclare_block = saved_predeclare_state;

        // Clear tail position after block
        self.in_tail_position = false;
        Ok(())
    }

    /// Compile a statement
    fn statement(&mut self, stmt: &SpannedStmt) -> Result<(), CompileError> {
        self.current_line = stmt.span.line;

        match &stmt.node {
            Stmt::Expr(expr) => {
                self.expression(expr)?;
            }
            Stmt::Let {
                mutable,
                pattern,
                value,
            } => {
                self.compile_let(*mutable, pattern, value, stmt.span)?;
            }
            Stmt::Return(expr) => {
                self.expression(expr)?;
                self.emit(OpCode::Return);
            }
            Stmt::Break(expr) => {
                self.expression(expr)?;
                self.emit(OpCode::Break);
            }
        }
        Ok(())
    }

    /// Compile a let binding
    fn compile_let(
        &mut self,
        mutable: bool,
        pattern: &Pattern,
        value: &SpannedExpr,
        span: Span,
    ) -> Result<(), CompileError> {
        // At global scope (depth 0), use globals instead of locals
        let is_global_scope = self.scope_depth == 0;

        // For simple identifier patterns, check if already declared (forward reference support)
        if let Pattern::Identifier(name) = pattern {
            // Only check for pre-declaration if this block actually did pre-declare
            let is_predeclared = if self.did_predeclare_block && !is_global_scope {
                // Check if this local was already pre-declared in the CURRENT scope
                self.locals.iter().rev().find(|l| l.name == *name)
                    .map(|l| l.depth == self.scope_depth)
                    .unwrap_or(false)
            } else {
                false
            };

            if is_predeclared {
                // Already pre-declared in current scope - compile value and update the local
                // The slot already exists on the stack (with nil), so we just need to update it
                let idx = self.resolve_local(name).unwrap();
                self.expression(value)?;
                self.emit_with_operand(OpCode::SetLocal, idx as u8);
                // SetLocal copies value to slot but leaves it on stack - pop the extra copy
                // The value is now in its pre-declared slot, no need to leave it on stack again
                self.emit(OpCode::Pop);
            } else if is_global_scope {
                // Global scope - compile value then emit SetGlobal
                // Register the global name so it can shadow builtins
                self.global_names.insert(name.clone());
                self.expression(value)?;
                let name_idx = self.chunk().add_constant(Value::String(
                    std::rc::Rc::new(name.clone())
                ));
                if name_idx > 255 {
                    return Err(CompileError::new("Too many global names", span));
                }
                self.emit_with_operand(OpCode::SetGlobal, name_idx as u8);
                // Note: SetGlobal only peeks, doesn't pop, so value stays on stack
            } else {
                // Local scope - add local first (for self-recursion)
                self.add_local(name.clone(), mutable);
                // Compile the value expression (can now reference the name)
                self.expression(value)?;
            }
        } else {
            // For other patterns, compile value first, then bind
            self.expression(value)?;
            self.compile_pattern_binding(pattern, mutable, span)?;
        }

        Ok(())
    }

    /// Compile pattern binding for let statements
    fn compile_pattern_binding(
        &mut self,
        pattern: &Pattern,
        mutable: bool,
        span: Span,
    ) -> Result<(), CompileError> {
        let is_global_scope = self.scope_depth == 0;

        match pattern {
            Pattern::Identifier(name) => {
                if is_global_scope {
                    // Global binding - register and emit SetGlobal (value is on stack)
                    self.global_names.insert(name.clone());
                    let name_idx = self.chunk().add_constant(Value::String(
                        std::rc::Rc::new(name.clone())
                    ));
                    if name_idx > 255 {
                        return Err(CompileError::new("Too many global names", span));
                    }
                    self.emit_with_operand(OpCode::SetGlobal, name_idx as u8);
                    // Note: SetGlobal only peeks, doesn't pop, so value stays on stack
                } else {
                    // Local binding - add local (value is on stack)
                    self.add_local(name.clone(), mutable);
                }
            }
            Pattern::Wildcard => {
                // Discard the value
                self.emit(OpCode::Pop);
            }
            Pattern::List(patterns) => {
                self.compile_list_destructuring(patterns, mutable, span)?;
            }
            Pattern::RestIdentifier(_) => {
                // This shouldn't occur at top level - rest should be inside a list
                return Err(CompileError::new(
                    "Rest pattern can only appear inside list patterns",
                    span,
                ));
            }
            Pattern::Literal(_) | Pattern::Range { .. } => {
                // Literals in let patterns don't make sense - they're for matching
                return Err(CompileError::new(
                    "Literal patterns are not allowed in let bindings",
                    span,
                ));
            }
        }
        Ok(())
    }

    /// Compile list destructuring pattern
    fn compile_list_destructuring(
        &mut self,
        patterns: &[Pattern],
        mutable: bool,
        span: Span,
    ) -> Result<(), CompileError> {
        let is_global_scope = self.scope_depth == 0;

        // Value to destructure is on stack.
        // We make it a temporary anonymous local so we can access it via GetLocal
        // for each element extraction.
        self.add_local(String::new(), false);
        let list_slot = (self.locals.len() - 1) as u8;

        // Find the rest pattern position if any
        let rest_pos = patterns
            .iter()
            .position(|p| matches!(p, Pattern::RestIdentifier(_)));

        // For each pattern element, get the list, extract element, and bind
        for (i, pattern) in patterns.iter().enumerate() {
            match pattern {
                Pattern::Identifier(name) => {
                    // Get the list from its local slot
                    self.emit_with_operand(OpCode::GetLocal, list_slot);
                    let idx = if let Some(rp) = rest_pos {
                        if i < rp {
                            i as i64
                        } else {
                            // After rest: index from end
                            -((patterns.len() - i) as i64)
                        }
                    } else {
                        i as i64
                    };
                    self.emit_constant(Value::Integer(idx))?;
                    self.emit(OpCode::Index);

                    if is_global_scope {
                        // Global binding - register so it's found before builtins
                        self.global_names.insert(name.clone());
                        let name_idx = self.chunk().add_constant(Value::String(
                            std::rc::Rc::new(name.clone())
                        ));
                        if name_idx > 255 {
                            return Err(CompileError::new("Too many global names", span));
                        }
                        self.emit_with_operand(OpCode::SetGlobal, name_idx as u8);
                        // SetGlobal only peeks, doesn't pop. Pop the value since we don't
                        // need it on the stack during destructuring.
                        self.emit(OpCode::Pop);
                    } else {
                        // Local binding
                        self.add_local(name.clone(), mutable);
                    }
                }
                Pattern::Wildcard => {
                    // Skip this element - don't bind it
                    // We need to pop the value that was extracted for this position
                    // Actually, wildcards don't extract a value - we just skip them
                }
                Pattern::RestIdentifier(name) => {
                    // Get a slice of the middle elements
                    self.emit_with_operand(OpCode::GetLocal, list_slot);
                    // Start index
                    self.emit_constant(Value::Integer(i as i64))?;
                    // End index (nil for "to end" or negative for "from end")
                    let end_count = patterns.len() - i - 1;
                    if end_count == 0 {
                        self.emit(OpCode::Nil);
                    } else {
                        self.emit_constant(Value::Integer(-(end_count as i64)))?;
                    }
                    self.emit(OpCode::Slice);

                    if is_global_scope {
                        // Global binding - register so it's found before builtins
                        self.global_names.insert(name.clone());
                        let name_idx = self.chunk().add_constant(Value::String(
                            std::rc::Rc::new(name.clone())
                        ));
                        if name_idx > 255 {
                            return Err(CompileError::new("Too many global names", span));
                        }
                        self.emit_with_operand(OpCode::SetGlobal, name_idx as u8);
                        // SetGlobal only peeks, doesn't pop. Pop the value since we don't
                        // need it on the stack during destructuring.
                        self.emit(OpCode::Pop);
                    } else {
                        // Local binding
                        self.add_local(name.clone(), mutable);
                    }
                }
                Pattern::List(inner) => {
                    // Nested destructuring: get element, then recursively destructure
                    self.emit_with_operand(OpCode::GetLocal, list_slot);
                    self.emit_constant(Value::Integer(i as i64))?;
                    self.emit(OpCode::Index);
                    self.compile_list_destructuring(inner, mutable, span)?;
                    // At global scope, pop the inner list's anonymous local after recursion
                    // so the stack returns to just having our outer list.
                    if is_global_scope {
                        self.emit(OpCode::Pop);
                    }
                }
                Pattern::Literal(_) | Pattern::Range { .. } => {
                    return Err(CompileError::new(
                        "Literal patterns are not allowed in let destructuring",
                        span,
                    ));
                }
            }
        }

        // The anonymous list local will be popped by end_scope along with other locals
        Ok(())
    }

    /// Compile parameter pattern destructuring
    /// For function parameters like |[a, b]| or |[[x1, y1], [x2, y2]]|, destructure into locals
    fn compile_param_pattern_destructuring(
        &mut self,
        pattern: &Pattern,
        param_idx: u8,
        span: Span,
    ) -> Result<(), CompileError> {
        match pattern {
            Pattern::List(patterns) => {
                // Recursively destructure the list pattern
                self.compile_nested_pattern_destructuring(patterns, param_idx, &[], span)
            }
            _ => Err(CompileError::new(
                "Only list patterns supported in function parameters",
                span,
            )),
        }
    }

    /// Recursively compile nested pattern destructuring
    /// index_path is the sequence of indices to access the current nested element
    fn compile_nested_pattern_destructuring(
        &mut self,
        patterns: &[Pattern],
        param_idx: u8,
        index_path: &[usize],
        span: Span,
    ) -> Result<(), CompileError> {
        for (i, elem_pattern) in patterns.iter().enumerate() {
            // Build the full index path for this element
            let mut current_path = index_path.to_vec();
            current_path.push(i);

            match elem_pattern {
                Pattern::Identifier(name) => {
                    // Emit code to access the nested element
                    self.emit_nested_access(param_idx, &current_path)?;
                    // Add as local (value is on stack)
                    self.add_local(name.clone(), false);
                }
                Pattern::Wildcard => {
                    // Don't need to extract or bind wildcards
                }
                Pattern::RestIdentifier(name) => {
                    // Get the rest of the list starting at position i
                    // First access up to the parent
                    if index_path.is_empty() {
                        self.emit_with_operand(OpCode::GetLocal, param_idx);
                    } else {
                        self.emit_nested_access(param_idx, index_path)?;
                    }
                    self.emit_constant(Value::Integer(i as i64))?;
                    let end_count = patterns.len() - i - 1;
                    if end_count == 0 {
                        self.emit(OpCode::Nil);
                    } else {
                        self.emit_constant(Value::Integer(-(end_count as i64)))?;
                    }
                    self.emit(OpCode::Slice);
                    self.add_local(name.clone(), false);
                }
                Pattern::List(nested_patterns) => {
                    // Recursively handle nested list patterns
                    self.compile_nested_pattern_destructuring(
                        nested_patterns,
                        param_idx,
                        &current_path,
                        span,
                    )?;
                }
                _ => {
                    return Err(CompileError::new(
                        format!("Pattern type {:?} not supported in function parameters", elem_pattern),
                        span,
                    ));
                }
            }
        }
        Ok(())
    }

    /// Emit code to access a nested element via a sequence of indices
    fn emit_nested_access(&mut self, param_idx: u8, index_path: &[usize]) -> Result<(), CompileError> {
        // Get the parameter
        self.emit_with_operand(OpCode::GetLocal, param_idx);
        // Apply each index in the path
        for &idx in index_path {
            self.emit_constant(Value::Integer(idx as i64))?;
            self.emit(OpCode::Index);
        }
        Ok(())
    }

    /// Compile a match expression
    fn compile_match(
        &mut self,
        subject: &SpannedExpr,
        arms: &[MatchArm],
        _span: Span,
    ) -> Result<(), CompileError> {
        // Save and clear tail position for subject
        let in_tail = self.in_tail_position;
        self.in_tail_position = false;

        // Compile the subject - it stays on stack for pattern matching
        self.expression(subject)?;

        // Track jump targets for after each arm
        let mut end_jumps = Vec::new();

        for (arm_idx, arm) in arms.iter().enumerate() {
            let is_last = arm_idx == arms.len() - 1;

            // Compile pattern test - this may bind locals
            let locals_before = self.locals.len();
            let next_arm_jump = self.compile_pattern_test(&arm.pattern, arm.span)?;

            // Compile guard if present (not in tail position)
            let guard_jump = if let Some(guard) = &arm.guard {
                self.in_tail_position = false;
                self.expression(guard)?;
                Some(self.emit_jump(OpCode::JumpIfFalse))
            } else {
                None
            };

            // Check if any locals were bound by the pattern
            let locals_bound = self.locals.len() - locals_before;

            // Pop the subject only if no locals were bound
            // (If locals were bound, the subject IS those locals on stack)
            if locals_bound == 0 {
                self.emit(OpCode::Pop);
            }

            // Compile arm body (restore tail position)
            self.in_tail_position = in_tail;
            self.expression(&arm.body)?;

            // Clean up any locals bound by the pattern
            // PopN will close any upvalues referencing these slots before popping
            if locals_bound > 0 {
                self.emit_with_operand(OpCode::PopN, locals_bound as u8);
                // Remove from compiler's local list
                for _ in 0..locals_bound {
                    self.locals.pop();
                }
            }

            // Check if we need to emit failure handling code for this arm
            let has_failure_code = guard_jump.is_some() || next_arm_jump.is_some();

            // Jump to end - needed if not last OR if last arm has failure handling code
            if !is_last || has_failure_code {
                end_jumps.push(self.emit_jump(OpCode::Jump));
            }

            // Patch guard jump to next arm if guard failed
            let guard_skip_jump = if let Some(gj) = guard_jump {
                self.patch_jump(gj);
                // Pop the guard result (boolean) in the failure path
                self.emit(OpCode::Pop);
                // Pop any locals bound by the pattern EXCEPT the first one (the subject).
                // The subject was already on the stack before pattern matching and needs to
                // remain for the next arm to try matching.
                // NOTE: We use individual Pop instructions instead of PopN because PopN
                // preserves the top value (designed for returning results from blocks),
                // but here we just want to discard the bound locals.
                for _ in 1..locals_bound {
                    self.emit(OpCode::Pop);
                }
                // If there's also pattern failure code, we need to jump past it
                if next_arm_jump.is_some() {
                    Some(self.emit_jump(OpCode::Jump))
                } else {
                    None
                }
            } else {
                None
            };

            // Patch pattern match failure jump
            if let Some(pj) = next_arm_jump {
                self.patch_jump(pj);
                // Pop the pattern test result (boolean) in the failure path
                self.emit(OpCode::Pop);
            }

            // Patch the guard skip jump to here (after pattern failure handling)
            if let Some(gsj) = guard_skip_jump {
                self.patch_jump(gsj);
            }

            // Ensure locals are restored to state before this arm for next arm iteration.
            // The success path already cleans up via PopN + locals.pop(), but the failure
            // path may have added temporary locals (e.g., $match_subject in nested list
            // patterns) that need to be removed from the compiler's tracking.
            while self.locals.len() > locals_before {
                self.locals.pop();
            }

            // If this is the last arm and pattern/guard might fail, we need a fallback value
            if is_last && has_failure_code {
                // Pop the subject that's still on stack
                self.emit(OpCode::Pop);
                // Return nil as the match result when no pattern matches
                self.emit(OpCode::Nil);
            }
        }

        // Patch all end jumps to here
        for jump in end_jumps {
            self.patch_jump(jump);
        }

        // Clear tail position after match
        self.in_tail_position = false;

        Ok(())
    }

    /// Compile pattern test - returns jump offset for failure case
    fn compile_pattern_test(
        &mut self,
        pattern: &Pattern,
        span: Span,
    ) -> Result<Option<usize>, CompileError> {
        match pattern {
            Pattern::Wildcard => {
                // Always matches, no test needed
                Ok(None)
            }
            Pattern::Identifier(name) => {
                // Matches if value is not nil
                // Dup subject, check if not nil
                self.emit(OpCode::Dup);
                self.emit(OpCode::Nil);
                self.emit(OpCode::Ne);
                let fail_jump = self.emit_jump(OpCode::JumpIfFalse);
                // Pop the boolean result in success path
                self.emit(OpCode::Pop);
                // Bind the value as local
                self.add_local(name.clone(), false);
                Ok(Some(fail_jump))
            }
            Pattern::Literal(lit) => {
                // Dup subject, compare with literal
                self.emit(OpCode::Dup);
                match lit {
                    LiteralPattern::Integer(n) => self.emit_constant(Value::Integer(*n))?,
                    LiteralPattern::Decimal(n) => {
                        self.emit_constant(Value::Decimal(ordered_float::OrderedFloat(*n)))?
                    }
                    LiteralPattern::String(s) => {
                        self.emit_constant(Value::String(Rc::new(s.clone())))?
                    }
                    LiteralPattern::Boolean(b) => {
                        if *b {
                            self.emit(OpCode::True)
                        } else {
                            self.emit(OpCode::False)
                        }
                    }
                    LiteralPattern::Nil => self.emit(OpCode::Nil),
                }
                self.emit(OpCode::Eq);
                let fail_jump = self.emit_jump(OpCode::JumpIfFalse);
                // Pop the boolean result in the success path
                self.emit(OpCode::Pop);
                Ok(Some(fail_jump))
            }
            Pattern::Range {
                start,
                end,
                inclusive,
            } => {
                // Dup subject, emit RangeCheck instruction
                self.emit(OpCode::Dup);
                self.emit(OpCode::RangeCheck);
                // Write start (2 bytes)
                self.chunk().write_operand((*start >> 8) as u8);
                self.chunk().write_operand((*start & 0xFF) as u8);
                // Write end (2 bytes)
                let end_val = end.unwrap_or(i16::MAX as i64) as i16;
                self.chunk().write_operand((end_val >> 8) as u8);
                self.chunk().write_operand((end_val & 0xFF) as u8);
                // Write inclusive flag
                self.chunk().write_operand(if *inclusive { 1 } else { 0 });
                let fail_jump = self.emit_jump(OpCode::JumpIfFalse);
                // Pop the boolean result in the success path
                self.emit(OpCode::Pop);
                Ok(Some(fail_jump))
            }
            Pattern::List(patterns) => self.compile_list_pattern_test(patterns, span),
            Pattern::RestIdentifier(_) => Err(CompileError::new(
                "Rest pattern can only appear inside list patterns",
                span,
            )),
        }
    }

    /// Compile list pattern test
    fn compile_list_pattern_test(
        &mut self,
        patterns: &[Pattern],
        span: Span,
    ) -> Result<Option<usize>, CompileError> {
        // The subject (list) is on top of stack. We need to register it as a temporary
        // local so we can access it by index while binding other elements.
        let subject_local_idx = self.locals.len() as u8;
        self.add_local("__match_subject".to_string(), false);

        // Collect jumps that need their boolean popped in failure path
        // Each entry is (jump_offset, bound_values_at_failure) so we know how many
        // identifier values to pop when cleaning up
        let mut conditional_fail_jumps: Vec<(usize, usize)> = Vec::new();
        // Collect jumps that have already cleaned up (just need to jump to common point)
        let mut unconditional_fail_jumps: Vec<usize> = Vec::new();
        // Track how many values have been bound (identifiers/rest patterns)
        let mut bound_values: usize = 0;

        // Check size first (unless there's a rest pattern)
        let has_rest = patterns
            .iter()
            .any(|p| matches!(p, Pattern::RestIdentifier(_)));
        let required_len = if has_rest {
            patterns.len() - 1
        } else {
            patterns.len()
        };

        // Get subject and check size
        self.emit_with_operand(OpCode::GetLocal, subject_local_idx);
        self.emit(OpCode::Size);
        self.emit_constant(Value::Integer(required_len as i64))?;
        if has_rest {
            self.emit(OpCode::Ge);
        } else {
            self.emit(OpCode::Eq);
        }
        conditional_fail_jumps.push((self.emit_jump(OpCode::JumpIfFalse), bound_values));
        // Pop the boolean result (JumpIfFalse does NOT pop, so we need to pop in the success path)
        self.emit(OpCode::Pop);

        // Extract and bind each element
        let rest_pos = patterns
            .iter()
            .position(|p| matches!(p, Pattern::RestIdentifier(_)));

        for (i, pattern) in patterns.iter().enumerate() {
            match pattern {
                Pattern::Identifier(name) => {
                    self.emit_with_operand(OpCode::GetLocal, subject_local_idx);
                    let idx = self.calc_pattern_index(i, rest_pos, patterns.len());
                    self.emit_constant(Value::Integer(idx))?;
                    self.emit(OpCode::Index);
                    self.add_local(name.clone(), false);
                    bound_values += 1;
                }
                Pattern::Wildcard => {
                    // Skip - don't need to extract
                }
                Pattern::RestIdentifier(name) => {
                    self.emit_with_operand(OpCode::GetLocal, subject_local_idx);
                    self.emit_constant(Value::Integer(i as i64))?;
                    let end_count = patterns.len() - i - 1;
                    if end_count == 0 {
                        self.emit(OpCode::Nil);
                    } else {
                        self.emit_constant(Value::Integer(-(end_count as i64)))?;
                    }
                    self.emit(OpCode::Slice);
                    self.add_local(name.clone(), false);
                    bound_values += 1;
                }
                Pattern::Literal(lit) => {
                    // Extract and compare
                    self.emit_with_operand(OpCode::GetLocal, subject_local_idx);
                    let idx = self.calc_pattern_index(i, rest_pos, patterns.len());
                    self.emit_constant(Value::Integer(idx))?;
                    self.emit(OpCode::Index);
                    match lit {
                        LiteralPattern::Integer(n) => self.emit_constant(Value::Integer(*n))?,
                        LiteralPattern::Decimal(n) => {
                            self.emit_constant(Value::Decimal(ordered_float::OrderedFloat(*n)))?
                        }
                        LiteralPattern::String(s) => {
                            self.emit_constant(Value::String(Rc::new(s.clone())))?
                        }
                        LiteralPattern::Boolean(b) => {
                            if *b {
                                self.emit(OpCode::True)
                            } else {
                                self.emit(OpCode::False)
                            }
                        }
                        LiteralPattern::Nil => self.emit(OpCode::Nil),
                    }
                    self.emit(OpCode::Eq);
                    // If not equal, fail - collect jump with bound count for cleanup
                    conditional_fail_jumps
                        .push((self.emit_jump(OpCode::JumpIfFalse), bound_values));
                    self.emit(OpCode::Pop);
                    // Continue processing remaining patterns (don't return early!)
                }
                Pattern::List(inner) => {
                    self.emit_with_operand(OpCode::GetLocal, subject_local_idx);
                    let idx = self.calc_pattern_index(i, rest_pos, patterns.len());
                    self.emit_constant(Value::Integer(idx))?;
                    self.emit(OpCode::Index);
                    if let Some(inner_fail_jump) = self.compile_list_pattern_test(inner, span)? {
                        // The inner pattern failed. We need to clean up both the failure
                        // boolean AND the element we pushed via Index before jumping to
                        // our failure handler.
                        // Jump past cleanup code if success
                        let success_jump = self.emit_jump(OpCode::Jump);
                        // Patch inner failure to here
                        self.patch_jump(inner_fail_jump);
                        // Pop the failure boolean from inner pattern check
                        self.emit(OpCode::Pop);
                        // Pop the element we pushed for this nested pattern
                        self.emit(OpCode::Pop);
                        // Now jump to our failure point (already cleaned up)
                        unconditional_fail_jumps.push(self.emit_jump(OpCode::Jump));
                        // Patch success to continue
                        self.patch_jump(success_jump);
                    }
                }
                Pattern::Range {
                    start,
                    end,
                    inclusive,
                } => {
                    // Extract element and do range check
                    self.emit_with_operand(OpCode::GetLocal, subject_local_idx);
                    let idx = self.calc_pattern_index(i, rest_pos, patterns.len());
                    self.emit_constant(Value::Integer(idx))?;
                    self.emit(OpCode::Index);
                    // Emit RangeCheck instruction
                    self.emit(OpCode::RangeCheck);
                    // Write start (2 bytes)
                    self.chunk().write_operand((*start >> 8) as u8);
                    self.chunk().write_operand((*start & 0xFF) as u8);
                    // Write end (2 bytes)
                    let end_val = end.unwrap_or(i16::MAX as i64) as i16;
                    self.chunk().write_operand((end_val >> 8) as u8);
                    self.chunk().write_operand((end_val & 0xFF) as u8);
                    // Write inclusive flag
                    self.chunk().write_operand(if *inclusive { 1 } else { 0 });
                    // If not in range, fail - collect jump with bound count for cleanup
                    conditional_fail_jumps
                        .push((self.emit_jump(OpCode::JumpIfFalse), bound_values));
                    self.emit(OpCode::Pop);
                }
            }
        }

        // Handle failure jumps. There are two types:
        // - conditional_fail_jumps: JumpIfFalse from size/literal/range checks, need to pop
        //   boolean AND any bound values
        // - unconditional_fail_jumps: Jump from nested pattern cleanup, already cleaned up
        let total_fails = conditional_fail_jumps.len() + unconditional_fail_jumps.len();

        if total_fails == 0 {
            Ok(None)
        } else if total_fails == 1
            && unconditional_fail_jumps.is_empty()
            && conditional_fail_jumps[0].1 == 0
        {
            // Single conditional failure with no bound values - let caller handle the pop
            Ok(Some(conditional_fail_jumps[0].0))
        } else {
            // Multiple failures or failures with bound values - need proper cleanup
            // Jump past failure handling code
            let success_jump = self.emit_jump(OpCode::Jump);

            // Emit cleanup code for each conditional failure point
            // Each needs to: pop boolean, pop bound values, jump to common exit
            let mut cleanup_exits: Vec<usize> = Vec::new();
            for (jump, values_bound) in conditional_fail_jumps {
                self.patch_jump(jump);
                // Pop the boolean from the comparison
                self.emit(OpCode::Pop);
                // Pop any bound values (identifiers/rest patterns)
                for _ in 0..values_bound {
                    self.emit(OpCode::Pop);
                }
                cleanup_exits.push(self.emit_jump(OpCode::Jump));
            }

            // Handle unconditional failures: they already cleaned up, just need to get here
            for jump in &unconditional_fail_jumps {
                self.patch_jump(*jump);
            }

            // Patch all cleanup exits to here
            for exit in cleanup_exits {
                self.patch_jump(exit);
            }

            // Common failure exit: push a dummy boolean for the caller to pop
            self.emit(OpCode::False);
            let final_fail_jump = self.emit_jump(OpCode::JumpIfFalse);

            // Patch success jump to continue
            self.patch_jump(success_jump);

            Ok(Some(final_fail_jump))
        }
    }

    /// Calculate index for pattern element
    fn calc_pattern_index(&self, i: usize, rest_pos: Option<usize>, total: usize) -> i64 {
        if let Some(rp) = rest_pos {
            if i < rp {
                i as i64
            } else {
                // After rest: index from end
                -((total - i) as i64)
            }
        } else {
            i as i64
        }
    }

    /// Compile if-let expression
    fn compile_if_let(
        &mut self,
        pattern: &Pattern,
        value: &SpannedExpr,
        then_branch: &SpannedExpr,
        else_branch: &Option<Box<SpannedExpr>>,
        span: Span,
    ) -> Result<(), CompileError> {
        // Compile the value expression
        self.expression(value)?;

        // Compile pattern test
        let locals_before = self.locals.len();
        let fail_jump = self.compile_pattern_test(pattern, span)?;

        // Pop subject value if pattern matched (only if not bound by pattern)
        // For Identifier and List patterns, the subject becomes a local and will
        // be cleaned up with the other pattern locals via PopN
        let subject_is_bound = matches!(pattern, Pattern::Identifier(_) | Pattern::List(_));
        if !subject_is_bound {
            self.emit(OpCode::Pop);
        }

        // Compile then branch
        self.expression(then_branch)?;

        // Clean up locals from pattern
        let locals_bound = self.locals.len() - locals_before;
        if locals_bound > 0 {
            self.emit_with_operand(OpCode::PopN, locals_bound as u8);
            for _ in 0..locals_bound {
                self.locals.pop();
            }
        }

        // Jump over else
        let end_jump = self.emit_jump(OpCode::Jump);

        // Patch fail jump
        if let Some(fj) = fail_jump {
            self.patch_jump(fj);
            // Patterns with fail_jump have [subject, bool] on stack from the comparison
            // We need to pop both: first the boolean result, then the subject
            self.emit(OpCode::Pop); // Pop the boolean
        }

        // Pop subject value since pattern failed
        self.emit(OpCode::Pop);

        // Compile else branch
        if let Some(else_expr) = else_branch {
            self.expression(else_expr)?;
        } else {
            self.emit(OpCode::Nil);
        }

        // Patch end jump
        self.patch_jump(end_jump);

        Ok(())
    }

    /// Add a local variable
    fn add_local(&mut self, name: String, mutable: bool) {
        self.locals.push(Local {
            name,
            depth: self.scope_depth,
            mutable,
            captured: false,
        });
    }

    /// Begin a new scope
    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    /// End current scope and pop locals
    fn end_scope(&mut self) {
        // Track how many locals to pop from the compiler's locals list
        // The stack cleanup happens at function return (for captured locals)
        // or we rely on the block returning a value and proper scope management
        while !self.locals.is_empty()
            && self.locals.last().map(|l| l.depth).unwrap_or(0) > self.scope_depth - 1
        {
            let _local = self.locals.pop().unwrap();
            // Captured locals will be closed at function return via close_upvalues
            // Non-captured locals stay on stack until block returns its value
            // (the block return value sits on top, locals below it)
        }
        self.scope_depth -= 1;
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

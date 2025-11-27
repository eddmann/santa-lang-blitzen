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
        }
    }

    /// Create a compiler for a nested function
    fn new_function(name: Option<String>, arity: u8, enclosing: Compiler) -> Self {
        let current_line = enclosing.current_line;
        Self {
            function: CompiledFunction::new(arity, name),
            locals: Vec::new(),
            scope_depth: 0,
            enclosing: Some(Box::new(enclosing)),
            current_line,
        }
    }

    /// Compile a single expression and return the compiled function
    pub fn compile_expression(expr: &SpannedExpr) -> Result<CompiledFunction, CompileError> {
        let mut compiler = Compiler::new();
        compiler.expression(expr)?;
        compiler.emit(OpCode::Return);
        Ok(compiler.function)
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
                // Check if any element contains a placeholder (partial application)
                if Self::contains_placeholder_expr_list(elements) {
                    self.compile_partial_application(expr)?;
                } else {
                    for elem in elements {
                        self.expression(elem)?;
                    }
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
                for elem in elements {
                    self.expression(elem)?;
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
                for (key, value) in entries {
                    self.expression(key)?;
                    self.expression(value)?;
                }
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
                // Compile as: function(left, right)
                self.compile_identifier(function, expr.span)?;
                self.expression(left)?;
                self.expression(right)?;
                self.emit_with_operand(OpCode::Call, 2);
            }

            // Control flow - if expression
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.expression(condition)?;
                let then_jump = self.emit_jump(OpCode::JumpIfFalse);
                self.emit(OpCode::Pop); // Pop condition if truthy
                self.expression(then_branch)?;

                if let Some(else_expr) = else_branch {
                    let else_jump = self.emit_jump(OpCode::Jump);
                    self.patch_jump(then_jump);
                    self.emit(OpCode::Pop); // Pop condition if falsy
                    self.expression(else_expr)?;
                    self.patch_jump(else_jump);
                } else {
                    let else_jump = self.emit_jump(OpCode::Jump);
                    self.patch_jump(then_jump);
                    self.emit(OpCode::Pop); // Pop condition
                    self.emit(OpCode::Nil); // if without else returns nil
                    self.patch_jump(else_jump);
                }
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
        // Pipeline and Compose have their own handling and may contain
        // placeholders on the right side (partial application as argument)
        match op {
            InfixOp::Pipeline => {
                return self.compile_pipeline(left, right, span);
            }
            InfixOp::Compose => {
                return self.compile_composition(left, right, span);
            }
            _ => {}
        }

        // Check for partial application (placeholder in operands)
        if Self::contains_placeholder(left) || Self::contains_placeholder(right) {
            return self.compile_partial_infix(left, op, right, span);
        }

        // Special handling for short-circuit operators
        match op {
            InfixOp::And => {
                self.expression(left)?;
                let jump = self.emit_jump(OpCode::PopJumpIfFalse);
                self.expression(right)?;
                self.patch_jump(jump);
                return Ok(());
            }
            InfixOp::Or => {
                self.expression(left)?;
                let jump = self.emit_jump(OpCode::PopJumpIfTrue);
                self.expression(right)?;
                self.patch_jump(jump);
                return Ok(());
            }
            InfixOp::Pipeline | InfixOp::Compose => {
                unreachable!("Handled above")
            }
            _ => {}
        }

        // Regular infix operations
        self.expression(left)?;
        self.expression(right)?;

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
        // If right is a call: func(args...) => func(args..., left)
        // If right is an identifier: func => func(left)

        match &right.node {
            Expr::Call { function, args } => {
                // Compile: func(args..., left)
                self.expression(function)?;
                for arg in args {
                    self.expression(arg)?;
                }
                self.expression(left)?;
                if args.len() + 1 > 255 {
                    return Err(CompileError::new("Too many arguments", span));
                }
                self.emit_with_operand(OpCode::Call, (args.len() + 1) as u8);
            }
            Expr::Identifier(_) | Expr::Function { .. } => {
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

    /// Compile function composition: f >> g creates |x| g(f(x))
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

        // Compile: g(f(x))
        // First compile right (g) - will be upvalue
        // We need to capture left and right as upvalues

        // For now, implement simple composition by generating nested calls
        // This requires that left and right are already functions

        // Emit: right(left(local_0))
        // Step 1: Get left function
        self.emit_upvalue_load_or_expression(left, span)?;
        // Step 2: Get local_0 (the argument)
        self.emit_with_operand(OpCode::GetLocal, 0);
        // Step 3: Call left(local_0)
        self.emit_with_operand(OpCode::Call, 1);
        // Step 4: Get right function - result of left() is now on stack
        // We need right(result), so get right first
        // Actually we need to reorder: get right, get result, call
        // Let's restructure:

        // Reset and do it properly:
        self.function.chunk = Chunk::new();
        self.locals.clear();
        self.locals.push(Local {
            name: String::new(),
            depth: 0,
            mutable: false,
            captured: false,
        });

        // left_result = left(x)
        self.emit_upvalue_load_or_expression(left, span)?;
        self.emit_with_operand(OpCode::GetLocal, 0);
        self.emit_with_operand(OpCode::Call, 1);

        // result = right(left_result)
        self.emit_upvalue_load_or_expression(right, span)?;
        // Swap: we have [left_result, right] but need [right, left_result]
        // For now, let's use a different approach: compile right first
        // Actually, the stack after the first call has left_result on top
        // We need to call right with that result

        // Reset again - cleaner approach:
        self.function.chunk = Chunk::new();
        self.locals.clear();
        self.locals.push(Local {
            name: String::new(),
            depth: 0,
            mutable: false,
            captured: false,
        });

        // Compile right function reference (will be called last)
        self.emit_upvalue_load_or_expression(right, span)?;

        // Compile: left(arg)
        self.emit_upvalue_load_or_expression(left, span)?;
        self.emit_with_operand(OpCode::GetLocal, 0);
        self.emit_with_operand(OpCode::Call, 1);

        // Now stack has: [right_fn, left_result]
        // Call right_fn(left_result)
        self.emit_with_operand(OpCode::Call, 1);

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

    /// Check if an expression contains a placeholder
    fn contains_placeholder(expr: &SpannedExpr) -> bool {
        match &expr.node {
            Expr::Placeholder => true,
            Expr::Prefix { right, .. } => Self::contains_placeholder(right),
            Expr::Infix { left, right, .. } => {
                Self::contains_placeholder(left) || Self::contains_placeholder(right)
            }
            Expr::List(elems) => elems.iter().any(Self::contains_placeholder),
            Expr::Call { function, args } => {
                Self::contains_placeholder(function) || args.iter().any(Self::contains_placeholder)
            }
            _ => false,
        }
    }

    /// Check if a list of expressions contains a placeholder
    fn contains_placeholder_expr_list(exprs: &[SpannedExpr]) -> bool {
        exprs.iter().any(Self::contains_placeholder)
    }

    /// Count the number of placeholders in an expression
    fn count_placeholders(expr: &SpannedExpr) -> usize {
        match &expr.node {
            Expr::Placeholder => 1,
            Expr::Prefix { right, .. } => Self::count_placeholders(right),
            Expr::Infix { left, right, .. } => {
                Self::count_placeholders(left) + Self::count_placeholders(right)
            }
            Expr::List(elems) => elems.iter().map(Self::count_placeholders).sum(),
            Expr::Call { function, args } => {
                Self::count_placeholders(function)
                    + args.iter().map(Self::count_placeholders).sum::<usize>()
            }
            _ => 0,
        }
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
        let placeholder_count = (if left_has_placeholder { 1 } else { 0 })
            + (if right_has_placeholder { 1 } else { 0 });

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

        // Compile left side
        if Self::contains_placeholder(left) {
            self.emit_with_operand(OpCode::GetLocal, arg_idx);
            arg_idx += 1;
        } else {
            self.expression(left)?;
        }

        // Compile right side
        if Self::contains_placeholder(right) {
            self.emit_with_operand(OpCode::GetLocal, arg_idx);
        } else {
            self.expression(right)?;
        }

        // Emit the operation
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
            InfixOp::And | InfixOp::Or => {
                return Err(CompileError::new(
                    "Partial application not supported for && and ||",
                    span,
                ));
            }
            InfixOp::Pipeline | InfixOp::Compose => {
                return Err(CompileError::new(
                    "Partial application not supported for |> and >>",
                    span,
                ));
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
                        return Err(CompileError::new(
                            "Operator not allowed in partial application",
                            expr.span,
                        ));
                    }
                }
            }
            Expr::Identifier(name) => {
                self.compile_identifier(name, expr.span)?;
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

        // Handle rest parameter - count regular params
        let mut regular_param_count = 0;
        for param in params {
            match &param.name {
                ParamKind::Rest(_) => {
                    // TODO: Handle rest parameter in runtime (Phase 6+)
                    break;
                }
                _ => regular_param_count += 1,
            }
        }

        // Create new compiler for function
        let enclosing = std::mem::take(self);
        *self = Compiler::new_function(None, regular_param_count as u8, enclosing);

        // Add parameter locals
        for param in params {
            let name = match &param.name {
                ParamKind::Identifier(name) => name.clone(),
                ParamKind::Placeholder => String::from("_"),
                ParamKind::Rest(name) => name.clone(),
            };
            self.locals.push(Local {
                name,
                depth: 0,
                mutable: false,
                captured: false,
            });
        }

        // Compile body
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

        // Check if this is a built-in function call
        if let Expr::Identifier(name) = &function.node
            && let Some(builtin_id) = BuiltinId::from_name(name)
            && self.resolve_local(name).is_none()
            && self.resolve_upvalue(name).is_none()
        {
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
        // Compile function expression
        self.expression(function)?;

        // Compile arguments
        for arg in args {
            self.expression(arg)?;
        }

        self.emit_with_operand(OpCode::Call, args.len() as u8);
        Ok(())
    }

    /// Compile identifier lookup
    fn compile_identifier(&mut self, name: &str, span: Span) -> Result<(), CompileError> {
        // First check locals
        if let Some(idx) = self.resolve_local(name) {
            self.emit_with_operand(OpCode::GetLocal, idx as u8);
            return Ok(());
        }

        // Then check upvalues (will be implemented in Phase 8)
        if let Some(idx) = self.resolve_upvalue(name) {
            self.emit_with_operand(OpCode::GetUpvalue, idx);
            return Ok(());
        }

        // Finally, emit global lookup
        let name_idx = self
            .chunk()
            .add_constant(Value::String(Rc::new(name.to_string())));
        if name_idx > 255 {
            return Err(CompileError::new("Too many global names", span));
        }
        self.emit_with_operand(OpCode::GetGlobal, name_idx as u8);
        Ok(())
    }

    /// Compile assignment
    fn compile_assignment(&mut self, name: &str, span: Span) -> Result<(), CompileError> {
        // Emit dup so assignment returns the value
        self.emit(OpCode::Dup);

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

        self.function.upvalues.push(super::bytecode::UpvalueDesc {
            index,
            is_local,
        });

        upvalue_count as u8
    }

    /// Compile a block of statements
    fn compile_block(&mut self, stmts: &[SpannedStmt], _span: Span) -> Result<(), CompileError> {
        self.begin_scope();

        for (i, stmt) in stmts.iter().enumerate() {
            self.statement(stmt)?;

            // Pop non-final expression statements
            if i < stmts.len() - 1 && matches!(&stmt.node, Stmt::Expr(_)) {
                self.emit(OpCode::Pop);
            }
        }

        // If block is empty, push nil
        if stmts.is_empty() {
            self.emit(OpCode::Nil);
        }

        // The last statement's value stays on stack
        self.end_scope();
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
        // Compile the value expression
        self.expression(value)?;

        // Handle pattern binding
        self.compile_pattern_binding(pattern, mutable, span)?;

        Ok(())
    }

    /// Compile pattern binding for let statements
    fn compile_pattern_binding(
        &mut self,
        pattern: &Pattern,
        mutable: bool,
        span: Span,
    ) -> Result<(), CompileError> {
        match pattern {
            Pattern::Identifier(name) => {
                // Simple binding - add local (value is on stack)
                self.add_local(name.clone(), mutable);
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
                    self.add_local(name.clone(), mutable);
                }
                Pattern::Wildcard => {
                    // Skip this element - don't bind it
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
                    self.add_local(name.clone(), mutable);
                }
                Pattern::List(inner) => {
                    // Nested destructuring: get element, then recursively destructure
                    self.emit_with_operand(OpCode::GetLocal, list_slot);
                    self.emit_constant(Value::Integer(i as i64))?;
                    self.emit(OpCode::Index);
                    self.compile_list_destructuring(inner, mutable, span)?;
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

    /// Compile a match expression
    fn compile_match(
        &mut self,
        subject: &SpannedExpr,
        arms: &[MatchArm],
        _span: Span,
    ) -> Result<(), CompileError> {
        // Compile the subject - it stays on stack for pattern matching
        self.expression(subject)?;

        // Track jump targets for after each arm
        let mut end_jumps = Vec::new();

        for (arm_idx, arm) in arms.iter().enumerate() {
            let is_last = arm_idx == arms.len() - 1;

            // Compile pattern test - this may bind locals
            let locals_before = self.locals.len();
            let next_arm_jump = self.compile_pattern_test(&arm.pattern, arm.span)?;

            // Compile guard if present
            let guard_jump = if let Some(guard) = &arm.guard {
                self.expression(guard)?;
                Some(self.emit_jump(OpCode::JumpIfFalse))
            } else {
                None
            };

            // Pop the subject (pattern matched, we're committed to this arm)
            self.emit(OpCode::Pop);

            // Compile arm body
            self.expression(&arm.body)?;

            // Clean up any locals bound by the pattern
            let locals_bound = self.locals.len() - locals_before;
            if locals_bound > 0 {
                self.emit_with_operand(OpCode::PopN, locals_bound as u8);
                // Remove from compiler's local list
                for _ in 0..locals_bound {
                    self.locals.pop();
                }
            }

            // Jump to end
            if !is_last {
                end_jumps.push(self.emit_jump(OpCode::Jump));
            }

            // Patch guard jump to next arm if guard failed
            if let Some(gj) = guard_jump {
                self.patch_jump(gj);
            }

            // Patch pattern match failure jump
            if let Some(pj) = next_arm_jump {
                self.patch_jump(pj);
            }
        }

        // Patch all end jumps to here
        for jump in end_jumps {
            self.patch_jump(jump);
        }

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
                // Always matches, bind the value
                // Value is on stack (via Dup from match), we keep it as local
                self.add_local(name.clone(), false);
                Ok(None)
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
                Ok(Some(self.emit_jump(OpCode::JumpIfFalse)))
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
                Ok(Some(self.emit_jump(OpCode::JumpIfFalse)))
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
        // Check size first (unless there's a rest pattern)
        let has_rest = patterns
            .iter()
            .any(|p| matches!(p, Pattern::RestIdentifier(_)));
        let required_len = if has_rest {
            patterns.len() - 1
        } else {
            patterns.len()
        };

        // Dup and check size
        self.emit(OpCode::Dup);
        self.emit(OpCode::Size);
        self.emit_constant(Value::Integer(required_len as i64))?;
        if has_rest {
            self.emit(OpCode::Ge);
        } else {
            self.emit(OpCode::Eq);
        }
        let size_fail_jump = self.emit_jump(OpCode::JumpIfFalse);

        // Extract and bind each element
        let rest_pos = patterns
            .iter()
            .position(|p| matches!(p, Pattern::RestIdentifier(_)));

        for (i, pattern) in patterns.iter().enumerate() {
            match pattern {
                Pattern::Identifier(name) => {
                    self.emit(OpCode::Dup);
                    let idx = self.calc_pattern_index(i, rest_pos, patterns.len());
                    self.emit_constant(Value::Integer(idx))?;
                    self.emit(OpCode::Index);
                    self.add_local(name.clone(), false);
                }
                Pattern::Wildcard => {
                    // Skip - don't need to extract
                }
                Pattern::RestIdentifier(name) => {
                    self.emit(OpCode::Dup);
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
                Pattern::Literal(lit) => {
                    // Extract and compare
                    self.emit(OpCode::Dup);
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
                    // If not equal, fail
                    let fail_jump = self.emit_jump(OpCode::JumpIfFalse);
                    return Ok(Some(fail_jump));
                }
                Pattern::List(inner) => {
                    self.emit(OpCode::Dup);
                    let idx = self.calc_pattern_index(i, rest_pos, patterns.len());
                    self.emit_constant(Value::Integer(idx))?;
                    self.emit(OpCode::Index);
                    if let Some(jump) = self.compile_list_pattern_test(inner, span)? {
                        return Ok(Some(jump));
                    }
                }
                Pattern::Range { .. } => {
                    return Err(CompileError::new(
                        "Range patterns inside list patterns not yet supported",
                        span,
                    ));
                }
            }
        }

        Ok(Some(size_fail_jump))
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

        // Pop subject value if pattern matched
        self.emit(OpCode::Pop);

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

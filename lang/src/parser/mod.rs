pub mod ast;

#[cfg(test)]
mod tests;

pub use ast::*;

use crate::lexer::{Span, Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl ParseError {
    fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

// Precedence levels for Pratt parsing (higher = binds tighter)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None = 0,
    Assignment = 1, // =
    Or = 2,         // ||
    And = 3,        // &&
    Equality = 4,   // == !=
    Comparison = 5, // < <= > >=
    Range = 6,      // |> >> .. ..=
    Sum = 7,        // + -
    Product = 8,    // * / % `infix`
    Prefix = 9,     // ! - (unary)
    Call = 10,      // ()
    Index = 11,     // []
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut statements = Vec::new();
        let mut sections = Vec::new();

        while !self.is_at_end() {
            // Check for section syntax: identifier ":"
            if self.check_section() {
                let section = self.parse_section()?;
                sections.push(section);
            } else {
                let stmt = self.parse_statement()?;
                statements.push(stmt);
            }
        }

        Ok(Program {
            statements,
            sections,
        })
    }

    fn check_section(&self) -> bool {
        if let Some(Token {
            kind: TokenKind::Identifier(name),
            ..
        }) = self.peek()
        {
            let is_section_name =
                matches!(name.as_str(), "input" | "part_one" | "part_two" | "test");
            if is_section_name
                && matches!(
                    self.peek_next(),
                    Some(Token {
                        kind: TokenKind::Colon,
                        ..
                    })
                )
            {
                return true;
            }
        }
        false
    }

    fn parse_section(&mut self) -> Result<Section, ParseError> {
        let name_token = self.advance().unwrap();
        let name = match &name_token.kind {
            TokenKind::Identifier(s) => s.clone(),
            _ => unreachable!(),
        };

        self.expect(TokenKind::Colon)?;

        match name.as_str() {
            "test" => {
                // test section has special block structure with input:, part_one:, part_two:
                // Parse it directly instead of as a normal expression
                self.parse_test_block()
            }
            _ => {
                // For part_one/part_two, if the body starts with { it's a block, not a set
                let is_part_section = name.as_str() == "part_one" || name.as_str() == "part_two";
                let expr = if is_part_section && self.check(&TokenKind::LeftBrace) {
                    self.parse_block()?
                } else {
                    self.parse_expression()?
                };
                match name.as_str() {
                    "input" => Ok(Section::Input(expr)),
                    "part_one" => Ok(Section::PartOne(expr)),
                    "part_two" => Ok(Section::PartTwo(expr)),
                    _ => Err(ParseError::new(
                        format!("Unknown section: {name}"),
                        name_token.span,
                    )),
                }
            }
        }
    }

    fn parse_test_block(&mut self) -> Result<Section, ParseError> {
        // Test block has special syntax: test: { input: expr, part_one: expr, part_two: expr }
        // Expect a left brace
        let brace_token = self.expect(TokenKind::LeftBrace)?;

        let mut input_expr = None;
        let mut part_one_expr = None;
        let mut part_two_expr = None;

        // Parse field: expression pairs until we hit }
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            // Parse identifier
            let field_token = self.advance().ok_or_else(|| {
                ParseError::new(
                    "Expected field name in test block".to_string(),
                    brace_token.span,
                )
            })?;

            let field_name = match &field_token.kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => {
                    return Err(ParseError::new(
                        format!("Expected field name, got {:?}", field_token.kind),
                        field_token.span,
                    ));
                }
            };

            // Expect colon
            self.expect(TokenKind::Colon)?;

            // Parse expression
            let expr = self.parse_expression()?;

            // Store in appropriate field
            match field_name.as_str() {
                "input" => {
                    if input_expr.is_some() {
                        return Err(ParseError::new(
                            "Duplicate 'input' field in test block".to_string(),
                            field_token.span,
                        ));
                    }
                    input_expr = Some(expr);
                }
                "part_one" => {
                    if part_one_expr.is_some() {
                        return Err(ParseError::new(
                            "Duplicate 'part_one' field in test block".to_string(),
                            field_token.span,
                        ));
                    }
                    part_one_expr = Some(expr);
                }
                "part_two" => {
                    if part_two_expr.is_some() {
                        return Err(ParseError::new(
                            "Duplicate 'part_two' field in test block".to_string(),
                            field_token.span,
                        ));
                    }
                    part_two_expr = Some(expr);
                }
                _ => {
                    return Err(ParseError::new(
                        format!(
                            "Unknown field '{field_name}' in test block. Expected 'input', 'part_one', or 'part_two'"
                        ),
                        field_token.span,
                    ));
                }
            }

            // Optional semicolon between fields
            if self.check(&TokenKind::Semicolon) {
                self.advance();
            }
        }

        // Expect closing brace
        self.expect(TokenKind::RightBrace)?;

        // input is required
        let input = input_expr.ok_or_else(|| {
            ParseError::new(
                "Test block must have an 'input' field".to_string(),
                brace_token.span,
            )
        })?;

        Ok(Section::Test {
            input,
            part_one: part_one_expr,
            part_two: part_two_expr,
        })
    }

    pub fn parse_expression(&mut self) -> Result<SpannedExpr, ParseError> {
        self.parse_precedence(Precedence::None)
    }

    fn parse_precedence(&mut self, min_precedence: Precedence) -> Result<SpannedExpr, ParseError> {
        let mut left = self.parse_prefix()?;

        while let Some(token) = self.peek() {
            let precedence = self.get_infix_precedence(&token.kind);
            if precedence <= min_precedence {
                break;
            }

            left = self.parse_infix(left, precedence)?;
        }

        Ok(left)
    }

    fn parse_prefix(&mut self) -> Result<SpannedExpr, ParseError> {
        let token = self.peek().ok_or_else(|| {
            ParseError::new(
                "Unexpected end of input",
                self.tokens.last().map(|t| t.span).unwrap_or(Span {
                    start: 0,
                    end: 0,
                    line: 1,
                    column: 1,
                }),
            )
        })?;

        match &token.kind {
            // Literals
            TokenKind::Integer(n) => {
                let n = *n;
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Integer(n), span))
            }
            TokenKind::Decimal(n) => {
                let n = *n;
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Decimal(n), span))
            }
            TokenKind::String(s) => {
                let s = s.clone();
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::String(s), span))
            }
            TokenKind::True => {
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Boolean(true), span))
            }
            TokenKind::False => {
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Boolean(false), span))
            }
            TokenKind::Nil => {
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Nil, span))
            }

            // Identifiers and placeholders
            TokenKind::Identifier(name) => {
                let name = name.clone();
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Identifier(name), span))
            }
            TokenKind::Underscore => {
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Placeholder, span))
            }

            // Prefix operators (or operator as function reference)
            TokenKind::Minus => {
                let op_span = self.advance().unwrap().span;
                // Check if this is an operator reference (followed by , or ) or ])
                // Note: LeftParen should NOT be here - `-(expr)` is prefix negation, not operator reference
                if matches!(
                    self.peek().map(|t| &t.kind),
                    Some(TokenKind::Comma | TokenKind::RightParen | TokenKind::RightBracket)
                ) {
                    // Operator as function reference
                    Ok(Spanned::new(Expr::Identifier("-".to_string()), op_span))
                } else {
                    // Prefix negation
                    let right = self.parse_precedence(Precedence::Prefix)?;
                    let span = Span {
                        start: op_span.start,
                        end: right.span.end,
                        line: op_span.line,
                        column: op_span.column,
                    };
                    Ok(Spanned::new(
                        Expr::Prefix {
                            op: PrefixOp::Neg,
                            right: Box::new(right),
                        },
                        span,
                    ))
                }
            }
            TokenKind::Bang => {
                let op_span = self.advance().unwrap().span;
                let right = self.parse_precedence(Precedence::Prefix)?;
                let span = Span {
                    start: op_span.start,
                    end: right.span.end,
                    line: op_span.line,
                    column: op_span.column,
                };
                Ok(Spanned::new(
                    Expr::Prefix {
                        op: PrefixOp::Not,
                        right: Box::new(right),
                    },
                    span,
                ))
            }

            // Binary operators as function references
            // Arithmetic operators
            TokenKind::Plus => {
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Identifier("+".to_string()), span))
            }
            TokenKind::Star => {
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Identifier("*".to_string()), span))
            }
            TokenKind::Slash => {
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Identifier("/".to_string()), span))
            }
            TokenKind::Percent => {
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Identifier("%".to_string()), span))
            }

            // Comparison operators
            TokenKind::Less => {
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Identifier("<".to_string()), span))
            }
            TokenKind::Greater => {
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Identifier(">".to_string()), span))
            }
            TokenKind::LessEqual => {
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Identifier("<=".to_string()), span))
            }
            TokenKind::GreaterEqual => {
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Identifier(">=".to_string()), span))
            }
            TokenKind::EqualEqual => {
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Identifier("==".to_string()), span))
            }
            TokenKind::BangEqual => {
                let span = self.advance().unwrap().span;
                Ok(Spanned::new(Expr::Identifier("!=".to_string()), span))
            }

            // Grouped expression
            TokenKind::LeftParen => {
                let start_span = self.advance().unwrap().span;
                let expr = self.parse_expression()?;
                let end_token = self.expect(TokenKind::RightParen)?;
                let span = Span {
                    start: start_span.start,
                    end: end_token.span.end,
                    line: start_span.line,
                    column: start_span.column,
                };
                Ok(Spanned::new(expr.node, span))
            }

            // List literal
            TokenKind::LeftBracket => self.parse_list(),

            // Set literal or block
            TokenKind::LeftBrace => self.parse_set_or_block(),

            // Dictionary literal
            TokenKind::HashBrace => self.parse_dict(),

            // Function expression
            TokenKind::Pipe => self.parse_function(),

            // No-argument function: || body
            TokenKind::PipePipe => self.parse_no_arg_function(),

            // If expression
            TokenKind::If => self.parse_if(),

            // Match expression
            TokenKind::Match => self.parse_match(),

            // Spread expression
            TokenKind::DotDot => {
                let start_span = self.advance().unwrap().span;
                // Check if this is a rest identifier (..name) or spread expression
                if let Some(Token {
                    kind: TokenKind::Identifier(name),
                    span: id_span,
                }) = self.peek()
                {
                    // Check if this is in a context where it should be a rest identifier
                    // For now, parse as spread with identifier
                    let name = name.clone();
                    let id_span = *id_span;
                    self.advance();
                    let span = Span {
                        start: start_span.start,
                        end: id_span.end,
                        line: start_span.line,
                        column: start_span.column,
                    };
                    Ok(Spanned::new(
                        Expr::Spread(Box::new(Spanned::new(Expr::Identifier(name), id_span))),
                        span,
                    ))
                } else {
                    let right = self.parse_precedence(Precedence::Prefix)?;
                    let span = Span {
                        start: start_span.start,
                        end: right.span.end,
                        line: start_span.line,
                        column: start_span.column,
                    };
                    Ok(Spanned::new(Expr::Spread(Box::new(right)), span))
                }
            }

            _ => Err(ParseError::new(
                format!("Unexpected token: {:?}", token.kind),
                token.span,
            )),
        }
    }

    fn parse_infix(
        &mut self,
        left: SpannedExpr,
        precedence: Precedence,
    ) -> Result<SpannedExpr, ParseError> {
        let token = self.peek().unwrap().clone();

        match &token.kind {
            // Index expression
            TokenKind::LeftBracket => {
                self.advance();
                let index = self.parse_expression()?;
                let end_token = self.expect(TokenKind::RightBracket)?;
                let span = Span {
                    start: left.span.start,
                    end: end_token.span.end,
                    line: left.span.line,
                    column: left.span.column,
                };
                Ok(Spanned::new(
                    Expr::Index {
                        collection: Box::new(left),
                        index: Box::new(index),
                    },
                    span,
                ))
            }

            // Function call
            TokenKind::LeftParen => {
                self.advance();
                let args = self.parse_args()?;
                let end_token = self.expect(TokenKind::RightParen)?;
                let span = Span {
                    start: left.span.start,
                    end: end_token.span.end,
                    line: left.span.line,
                    column: left.span.column,
                };
                Ok(Spanned::new(
                    Expr::Call {
                        function: Box::new(left),
                        args,
                    },
                    span,
                ))
            }

            // Trailing lambda
            TokenKind::Pipe => {
                let lambda = self.parse_function()?;
                let span = Span {
                    start: left.span.start,
                    end: lambda.span.end,
                    line: left.span.line,
                    column: left.span.column,
                };
                // If left is already a Call (e.g., fold(0)), append the lambda to its args
                // rather than creating a nested call. This allows fold(0) |acc, x| {...}
                // to parse as fold(0, |acc, x| {...}) per LANG.txt §8.8.
                match left.node {
                    Expr::Call { function, mut args } => {
                        args.push(lambda);
                        Ok(Spanned::new(Expr::Call { function, args }, span))
                    }
                    _ => {
                        // For non-call expressions, wrap as a call with the lambda as arg
                        Ok(Spanned::new(
                            Expr::Call {
                                function: Box::new(left),
                                args: vec![lambda],
                            },
                            span,
                        ))
                    }
                }
            }

            // Infix function call with backticks
            TokenKind::Backtick => {
                self.advance();
                let func_name = match self.advance() {
                    Some(Token {
                        kind: TokenKind::Identifier(name),
                        ..
                    }) => name,
                    Some(t) => {
                        return Err(ParseError::new(
                            format!("Expected identifier after backtick, got {:?}", t.kind),
                            t.span,
                        ));
                    }
                    None => {
                        return Err(ParseError::new(
                            "Expected identifier after backtick",
                            left.span,
                        ));
                    }
                };
                self.expect(TokenKind::Backtick)?;

                // Use lower precedence so we get the right operand
                let right = self.parse_precedence(precedence)?;
                let span = Span {
                    start: left.span.start,
                    end: right.span.end,
                    line: left.span.line,
                    column: left.span.column,
                };
                Ok(Spanned::new(
                    Expr::InfixCall {
                        function: func_name,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    span,
                ))
            }

            // Range operators
            TokenKind::DotDot => {
                self.advance();
                // Check for ..= (inclusive) was handled in tokenizer as DotDotEqual
                // Check if there's a right operand (unbounded range has none)
                // An unbounded range ends if followed by: end of input, statement terminator,
                // closing delimiter, or an infix operator at same/lower precedence (like |>)
                if self.is_at_end()
                    || self.check_statement_terminator()
                    || self.check_closing_delimiter()
                    || self.check_infix_at_or_below_precedence(precedence)
                {
                    let span = Span {
                        start: left.span.start,
                        end: token.span.end,
                        line: left.span.line,
                        column: left.span.column,
                    };
                    Ok(Spanned::new(
                        Expr::Range {
                            start: Box::new(left),
                            end: None,
                            inclusive: false,
                        },
                        span,
                    ))
                } else {
                    let right = self.parse_precedence(precedence)?;
                    let span = Span {
                        start: left.span.start,
                        end: right.span.end,
                        line: left.span.line,
                        column: left.span.column,
                    };
                    Ok(Spanned::new(
                        Expr::Range {
                            start: Box::new(left),
                            end: Some(Box::new(right)),
                            inclusive: false,
                        },
                        span,
                    ))
                }
            }
            TokenKind::DotDotEqual => {
                self.advance();
                let right = self.parse_precedence(precedence)?;
                let span = Span {
                    start: left.span.start,
                    end: right.span.end,
                    line: left.span.line,
                    column: left.span.column,
                };
                Ok(Spanned::new(
                    Expr::Range {
                        start: Box::new(left),
                        end: Some(Box::new(right)),
                        inclusive: true,
                    },
                    span,
                ))
            }

            // Assignment
            TokenKind::Equal => {
                self.advance();
                let name = match &left.node {
                    Expr::Identifier(n) => n.clone(),
                    _ => {
                        return Err(ParseError::new(
                            "Left side of assignment must be an identifier",
                            left.span,
                        ));
                    }
                };
                let value = self.parse_precedence(Precedence::Assignment)?;
                let span = Span {
                    start: left.span.start,
                    end: value.span.end,
                    line: left.span.line,
                    column: left.span.column,
                };
                Ok(Spanned::new(
                    Expr::Assignment {
                        name,
                        value: Box::new(value),
                    },
                    span,
                ))
            }

            // Binary operators
            _ => {
                let op = self.get_infix_op(&token.kind).ok_or_else(|| {
                    ParseError::new(
                        format!("Expected operator, got {:?}", token.kind),
                        token.span,
                    )
                })?;
                self.advance();
                let right = self.parse_precedence(precedence)?;
                let span = Span {
                    start: left.span.start,
                    end: right.span.end,
                    line: left.span.line,
                    column: left.span.column,
                };
                Ok(Spanned::new(
                    Expr::Infix {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    },
                    span,
                ))
            }
        }
    }

    fn get_infix_precedence(&self, kind: &TokenKind) -> Precedence {
        match kind {
            TokenKind::LeftBracket => Precedence::Index,
            TokenKind::LeftParen | TokenKind::Pipe => Precedence::Call,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent | TokenKind::Backtick => {
                Precedence::Product
            }
            TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
            TokenKind::PipeGreater
            | TokenKind::GreaterGreater
            | TokenKind::DotDot
            | TokenKind::DotDotEqual => Precedence::Range,
            TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual => Precedence::Comparison,
            TokenKind::EqualEqual | TokenKind::BangEqual => Precedence::Equality,
            TokenKind::Equal => Precedence::Assignment,
            TokenKind::AmpAmp => Precedence::And,
            TokenKind::PipePipe => Precedence::Or,
            _ => Precedence::None,
        }
    }

    fn get_infix_op(&self, kind: &TokenKind) -> Option<InfixOp> {
        match kind {
            TokenKind::Plus => Some(InfixOp::Add),
            TokenKind::Minus => Some(InfixOp::Sub),
            TokenKind::Star => Some(InfixOp::Mul),
            TokenKind::Slash => Some(InfixOp::Div),
            TokenKind::Percent => Some(InfixOp::Mod),
            TokenKind::EqualEqual => Some(InfixOp::Eq),
            TokenKind::BangEqual => Some(InfixOp::Ne),
            TokenKind::Less => Some(InfixOp::Lt),
            TokenKind::LessEqual => Some(InfixOp::Le),
            TokenKind::Greater => Some(InfixOp::Gt),
            TokenKind::GreaterEqual => Some(InfixOp::Ge),
            TokenKind::AmpAmp => Some(InfixOp::And),
            TokenKind::PipePipe => Some(InfixOp::Or),
            TokenKind::PipeGreater => Some(InfixOp::Pipeline),
            TokenKind::GreaterGreater => Some(InfixOp::Compose),
            _ => None,
        }
    }

    fn parse_list(&mut self) -> Result<SpannedExpr, ParseError> {
        let start_span = self.advance().unwrap().span; // consume [

        let mut elements = Vec::new();

        while !self.check(&TokenKind::RightBracket) && !self.is_at_end() {
            // Check for spread
            if self.check(&TokenKind::DotDot) {
                let spread_span = self.advance().unwrap().span;
                let expr = self.parse_expression()?;
                let span = Span {
                    start: spread_span.start,
                    end: expr.span.end,
                    line: spread_span.line,
                    column: spread_span.column,
                };
                elements.push(Spanned::new(Expr::Spread(Box::new(expr)), span));
            } else {
                elements.push(self.parse_expression()?);
            }

            if !self.check(&TokenKind::RightBracket) {
                self.expect(TokenKind::Comma)?;
                // Allow trailing comma
            }
        }

        let end_token = self.expect(TokenKind::RightBracket)?;
        let span = Span {
            start: start_span.start,
            end: end_token.span.end,
            line: start_span.line,
            column: start_span.column,
        };

        Ok(Spanned::new(Expr::List(elements), span))
    }

    fn parse_set_or_block(&mut self) -> Result<SpannedExpr, ParseError> {
        let start_span = self.advance().unwrap().span; // consume {

        // Empty braces: in expression context = empty set, in statement context = empty block
        // For simplicity, we'll parse as empty set in expression context
        if self.check(&TokenKind::RightBrace) {
            let end_token = self.advance().unwrap();
            let span = Span {
                start: start_span.start,
                end: end_token.span.end,
                line: start_span.line,
                column: start_span.column,
            };
            return Ok(Spanned::new(Expr::Set(Vec::new()), span));
        }

        // Try to determine if this is a set or block
        // Block indicators: let, return, break, or statement-like structures
        if self.check(&TokenKind::Let)
            || self.check(&TokenKind::Return)
            || self.check(&TokenKind::Break)
        {
            return self.parse_block_contents(start_span);
        }

        // Parse first expression
        let first = self.parse_expression()?;

        // If followed by semicolon or newline, it's a block
        if self.check(&TokenKind::Semicolon) {
            self.advance();
            return self.continue_as_block(start_span, first);
        }

        // If followed by comma, it's a set
        if self.check(&TokenKind::Comma) {
            return self.continue_as_set(start_span, first);
        }

        // If followed by }, it's a single-element set
        // (blocks only appear in specific contexts like if/else bodies, not standalone)
        if self.check(&TokenKind::RightBrace) {
            let end_token = self.advance().unwrap();
            let span = Span {
                start: start_span.start,
                end: end_token.span.end,
                line: start_span.line,
                column: start_span.column,
            };
            return Ok(Spanned::new(Expr::Set(vec![first]), span));
        }

        // Otherwise, continue as block (there might be more statements)
        self.continue_as_block(start_span, first)
    }

    fn continue_as_set(
        &mut self,
        start_span: Span,
        first: SpannedExpr,
    ) -> Result<SpannedExpr, ParseError> {
        let mut elements = vec![first];

        while self.check(&TokenKind::Comma) {
            self.advance();
            if self.check(&TokenKind::RightBrace) {
                break; // trailing comma
            }
            elements.push(self.parse_expression()?);
        }

        let end_token = self.expect(TokenKind::RightBrace)?;
        let span = Span {
            start: start_span.start,
            end: end_token.span.end,
            line: start_span.line,
            column: start_span.column,
        };

        Ok(Spanned::new(Expr::Set(elements), span))
    }

    fn continue_as_block(
        &mut self,
        start_span: Span,
        first: SpannedExpr,
    ) -> Result<SpannedExpr, ParseError> {
        let first_span = first.span;
        let mut statements = vec![Spanned::new(Stmt::Expr(first), first_span)];

        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
        }

        let end_token = self.expect(TokenKind::RightBrace)?;
        let span = Span {
            start: start_span.start,
            end: end_token.span.end,
            line: start_span.line,
            column: start_span.column,
        };

        Ok(Spanned::new(Expr::Block(statements), span))
    }

    fn parse_block_contents(&mut self, start_span: Span) -> Result<SpannedExpr, ParseError> {
        let mut statements = Vec::new();

        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
        }

        let end_token = self.expect(TokenKind::RightBrace)?;
        let span = Span {
            start: start_span.start,
            end: end_token.span.end,
            line: start_span.line,
            column: start_span.column,
        };

        Ok(Spanned::new(Expr::Block(statements), span))
    }

    fn parse_dict(&mut self) -> Result<SpannedExpr, ParseError> {
        let start_span = self.advance().unwrap().span; // consume #{

        let mut entries = Vec::new();

        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            // Check for shorthand syntax: identifier alone means "name": name
            if let Some(Token {
                kind: TokenKind::Identifier(name),
                span,
            }) = self.peek()
            {
                let name = name.clone();
                let span = *span;

                // Peek ahead to see if this is shorthand (no colon)
                if let Some(Token {
                    kind: TokenKind::Comma | TokenKind::RightBrace,
                    ..
                }) = self.peek_next()
                {
                    self.advance();
                    let key_expr = Spanned::new(Expr::String(name.clone()), span);
                    let value_expr = Spanned::new(Expr::Identifier(name), span);
                    entries.push((key_expr, value_expr));
                } else if let Some(Token {
                    kind: TokenKind::Colon,
                    ..
                }) = self.peek_next()
                {
                    // Normal key: value syntax
                    let key = self.parse_expression()?;
                    self.expect(TokenKind::Colon)?;
                    let value = self.parse_expression()?;
                    entries.push((key, value));
                } else {
                    // Key followed by something else - parse as expression
                    let key = self.parse_expression()?;
                    self.expect(TokenKind::Colon)?;
                    let value = self.parse_expression()?;
                    entries.push((key, value));
                }
            } else {
                // Non-identifier key
                let key = self.parse_expression()?;
                self.expect(TokenKind::Colon)?;
                let value = self.parse_expression()?;
                entries.push((key, value));
            }

            if !self.check(&TokenKind::RightBrace) {
                self.expect(TokenKind::Comma)?;
            }
        }

        let end_token = self.expect(TokenKind::RightBrace)?;
        let span = Span {
            start: start_span.start,
            end: end_token.span.end,
            line: start_span.line,
            column: start_span.column,
        };

        Ok(Spanned::new(Expr::Dict(entries), span))
    }

    fn parse_function(&mut self) -> Result<SpannedExpr, ParseError> {
        let start_span = self.expect(TokenKind::Pipe)?.span;

        let mut params = Vec::new();

        while !self.check(&TokenKind::Pipe) && !self.is_at_end() {
            let param_token = self
                .advance()
                .ok_or_else(|| ParseError::new("Expected parameter", start_span))?;

            let param = match &param_token.kind {
                TokenKind::Identifier(name) => Param {
                    name: ParamKind::Identifier(name.clone()),
                    span: param_token.span,
                },
                TokenKind::Underscore => Param {
                    name: ParamKind::Placeholder,
                    span: param_token.span,
                },
                TokenKind::DotDot => {
                    let rest_name_token = self.advance().ok_or_else(|| {
                        ParseError::new("Expected identifier after ..", param_token.span)
                    })?;
                    match &rest_name_token.kind {
                        TokenKind::Identifier(name) => Param {
                            name: ParamKind::Rest(name.clone()),
                            span: Span {
                                start: param_token.span.start,
                                end: rest_name_token.span.end,
                                line: param_token.span.line,
                                column: param_token.span.column,
                            },
                        },
                        _ => {
                            return Err(ParseError::new(
                                "Expected identifier after ..",
                                rest_name_token.span,
                            ));
                        }
                    }
                }
                TokenKind::LeftBracket => {
                    // List pattern parameter: |[a, b]| ...
                    self.position -= 1; // Put the [ back for parse_pattern
                    let pattern = self.parse_pattern()?;
                    // The span is just the pattern token span for now
                    Param {
                        name: ParamKind::Pattern(pattern),
                        span: param_token.span,
                    }
                }
                _ => {
                    return Err(ParseError::new(
                        format!("Expected parameter, got {:?}", param_token.kind),
                        param_token.span,
                    ));
                }
            };
            params.push(param);

            if !self.check(&TokenKind::Pipe) {
                self.expect(TokenKind::Comma)?;
            }
        }

        self.expect(TokenKind::Pipe)?;

        // Body can be expression or block
        // When function body starts with {, it's always a block (not a set)
        let body = if self.check(&TokenKind::LeftBrace) {
            self.parse_block()?
        } else {
            self.parse_expression()?
        };

        let span = Span {
            start: start_span.start,
            end: body.span.end,
            line: start_span.line,
            column: start_span.column,
        };

        Ok(Spanned::new(
            Expr::Function {
                params,
                body: Box::new(body),
            },
            span,
        ))
    }

    fn parse_no_arg_function(&mut self) -> Result<SpannedExpr, ParseError> {
        // Handle || as no-argument function (lexer combines || into PipePipe)
        let start_span = self.expect(TokenKind::PipePipe)?.span;

        // Body can be expression or block
        // When function body starts with {, it's always a block (not a set)
        let body = if self.check(&TokenKind::LeftBrace) {
            self.parse_block()?
        } else {
            self.parse_expression()?
        };

        let span = Span {
            start: start_span.start,
            end: body.span.end,
            line: start_span.line,
            column: start_span.column,
        };

        Ok(Spanned::new(
            Expr::Function {
                params: Vec::new(),
                body: Box::new(body),
            },
            span,
        ))
    }

    fn parse_if(&mut self) -> Result<SpannedExpr, ParseError> {
        let start_span = self.expect(TokenKind::If)?.span;

        // Check for if-let pattern
        if self.check(&TokenKind::Let) {
            self.advance();
            let pattern = self.parse_pattern()?;
            self.expect(TokenKind::Equal)?;
            let value = self.parse_expression()?;

            // Parse then block
            let then_branch = self.parse_block()?;

            // Optional else branch
            let else_branch = if self.check(&TokenKind::Else) {
                self.advance();
                if self.check(&TokenKind::If) {
                    Some(Box::new(self.parse_if()?))
                } else {
                    Some(Box::new(self.parse_block()?))
                }
            } else {
                None
            };

            let end_span = else_branch
                .as_ref()
                .map(|e| e.span)
                .unwrap_or(then_branch.span);
            let span = Span {
                start: start_span.start,
                end: end_span.end,
                line: start_span.line,
                column: start_span.column,
            };

            return Ok(Spanned::new(
                Expr::IfLet {
                    pattern,
                    value: Box::new(value),
                    then_branch: Box::new(then_branch),
                    else_branch,
                },
                span,
            ));
        }

        // Normal if expression
        let condition = self.parse_expression()?;
        let then_branch = self.parse_block()?;

        let else_branch = if self.check(&TokenKind::Else) {
            self.advance();
            if self.check(&TokenKind::If) {
                Some(Box::new(self.parse_if()?))
            } else {
                Some(Box::new(self.parse_block()?))
            }
        } else {
            None
        };

        let end_span = else_branch
            .as_ref()
            .map(|e| e.span)
            .unwrap_or(then_branch.span);
        let span = Span {
            start: start_span.start,
            end: end_span.end,
            line: start_span.line,
            column: start_span.column,
        };

        Ok(Spanned::new(
            Expr::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch,
            },
            span,
        ))
    }

    fn parse_block(&mut self) -> Result<SpannedExpr, ParseError> {
        let start_span = self.expect(TokenKind::LeftBrace)?.span;
        self.parse_block_contents(start_span)
    }

    fn parse_match(&mut self) -> Result<SpannedExpr, ParseError> {
        let start_span = self.expect(TokenKind::Match)?.span;

        let subject = self.parse_expression()?;
        self.expect(TokenKind::LeftBrace)?;

        let mut arms = Vec::new();

        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            let arm = self.parse_match_arm()?;
            arms.push(arm);
            // Optional trailing comma between arms
            if self.check(&TokenKind::Comma) {
                self.advance();
            }
        }

        let end_token = self.expect(TokenKind::RightBrace)?;
        let span = Span {
            start: start_span.start,
            end: end_token.span.end,
            line: start_span.line,
            column: start_span.column,
        };

        Ok(Spanned::new(
            Expr::Match {
                subject: Box::new(subject),
                arms,
            },
            span,
        ))
    }

    fn parse_match_arm(&mut self) -> Result<MatchArm, ParseError> {
        let pattern_start = self.peek().map(|t| t.span).unwrap_or(Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        });
        let pattern = self.parse_pattern()?;

        // Optional guard
        let guard = if self.check(&TokenKind::If) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        // Body block
        let body = self.parse_block()?;

        let span = Span {
            start: pattern_start.start,
            end: body.span.end,
            line: pattern_start.line,
            column: pattern_start.column,
        };

        Ok(MatchArm {
            pattern,
            guard,
            body,
            span,
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let token = self.peek().cloned().ok_or_else(|| {
            ParseError::new(
                "Expected pattern",
                Span {
                    start: 0,
                    end: 0,
                    line: 1,
                    column: 1,
                },
            )
        })?;

        match &token.kind {
            TokenKind::Underscore => {
                self.advance();
                Ok(Pattern::Wildcard)
            }
            TokenKind::Identifier(name) => {
                let name = name.clone();
                self.advance();
                Ok(Pattern::Identifier(name))
            }
            TokenKind::DotDot => {
                self.advance();
                let name_token = self
                    .advance()
                    .ok_or_else(|| ParseError::new("Expected identifier after ..", token.span))?;
                match &name_token.kind {
                    TokenKind::Identifier(name) => Ok(Pattern::RestIdentifier(name.clone())),
                    _ => Err(ParseError::new(
                        "Expected identifier after ..",
                        name_token.span,
                    )),
                }
            }
            TokenKind::Integer(n) => {
                let n = *n;
                let span = self.advance().unwrap().span;

                // Check for range pattern
                if self.check(&TokenKind::DotDot) {
                    self.advance();
                    if self.check_integer()
                        && let Some(Token {
                            kind: TokenKind::Integer(end),
                            ..
                        }) = self.advance()
                    {
                        return Ok(Pattern::Range {
                            start: n,
                            end: Some(end),
                            inclusive: false,
                        });
                    }
                    return Ok(Pattern::Range {
                        start: n,
                        end: None,
                        inclusive: false,
                    });
                } else if self.check(&TokenKind::DotDotEqual) {
                    self.advance();
                    let end_token = self.advance().ok_or_else(|| {
                        ParseError::new("Expected integer in range pattern", span)
                    })?;
                    match &end_token.kind {
                        TokenKind::Integer(end) => {
                            return Ok(Pattern::Range {
                                start: n,
                                end: Some(*end),
                                inclusive: true,
                            });
                        }
                        _ => {
                            return Err(ParseError::new(
                                "Expected integer in range pattern",
                                end_token.span,
                            ));
                        }
                    }
                }

                Ok(Pattern::Literal(LiteralPattern::Integer(n)))
            }
            TokenKind::Decimal(n) => {
                let n = *n;
                self.advance();
                Ok(Pattern::Literal(LiteralPattern::Decimal(n)))
            }
            TokenKind::String(s) => {
                let s = s.clone();
                self.advance();
                Ok(Pattern::Literal(LiteralPattern::String(s)))
            }
            TokenKind::True => {
                self.advance();
                Ok(Pattern::Literal(LiteralPattern::Boolean(true)))
            }
            TokenKind::False => {
                self.advance();
                Ok(Pattern::Literal(LiteralPattern::Boolean(false)))
            }
            TokenKind::Nil => {
                self.advance();
                Ok(Pattern::Literal(LiteralPattern::Nil))
            }
            TokenKind::LeftBracket => {
                self.advance();
                let mut patterns = Vec::new();

                while !self.check(&TokenKind::RightBracket) && !self.is_at_end() {
                    patterns.push(self.parse_pattern()?);
                    if !self.check(&TokenKind::RightBracket) {
                        self.expect(TokenKind::Comma)?;
                    }
                }

                self.expect(TokenKind::RightBracket)?;
                Ok(Pattern::List(patterns))
            }
            _ => Err(ParseError::new(
                format!("Expected pattern, got {:?}", token.kind),
                token.span,
            )),
        }
    }

    fn parse_args(&mut self) -> Result<Vec<SpannedExpr>, ParseError> {
        let mut args = Vec::new();

        while !self.check(&TokenKind::RightParen) && !self.is_at_end() {
            // Check for spread
            if self.check(&TokenKind::DotDot) {
                let spread_span = self.advance().unwrap().span;
                let expr = self.parse_expression()?;
                let span = Span {
                    start: spread_span.start,
                    end: expr.span.end,
                    line: spread_span.line,
                    column: spread_span.column,
                };
                args.push(Spanned::new(Expr::Spread(Box::new(expr)), span));
            } else {
                args.push(self.parse_expression()?);
            }

            if !self.check(&TokenKind::RightParen) {
                self.expect(TokenKind::Comma)?;
            }
        }

        Ok(args)
    }

    fn parse_statement(&mut self) -> Result<SpannedStmt, ParseError> {
        let token = self.peek().ok_or_else(|| {
            ParseError::new(
                "Expected statement",
                Span {
                    start: 0,
                    end: 0,
                    line: 1,
                    column: 1,
                },
            )
        })?;

        match &token.kind {
            TokenKind::Let => {
                let start_span = self.advance().unwrap().span;

                let mutable = if self.check(&TokenKind::Mut) {
                    self.advance();
                    true
                } else {
                    false
                };

                let pattern = self.parse_pattern()?;
                self.expect(TokenKind::Equal)?;
                let value = self.parse_expression()?;

                // Optional semicolon
                if self.check(&TokenKind::Semicolon) {
                    self.advance();
                }

                let span = Span {
                    start: start_span.start,
                    end: value.span.end,
                    line: start_span.line,
                    column: start_span.column,
                };

                Ok(Spanned::new(
                    Stmt::Let {
                        mutable,
                        pattern,
                        value,
                    },
                    span,
                ))
            }
            TokenKind::Return => {
                let start_span = self.advance().unwrap().span;
                let value = self.parse_expression()?;

                if self.check(&TokenKind::Semicolon) {
                    self.advance();
                }

                let span = Span {
                    start: start_span.start,
                    end: value.span.end,
                    line: start_span.line,
                    column: start_span.column,
                };

                Ok(Spanned::new(Stmt::Return(value), span))
            }
            TokenKind::Break => {
                let start_span = self.advance().unwrap().span;
                let value = self.parse_expression()?;

                if self.check(&TokenKind::Semicolon) {
                    self.advance();
                }

                let span = Span {
                    start: start_span.start,
                    end: value.span.end,
                    line: start_span.line,
                    column: start_span.column,
                };

                Ok(Spanned::new(Stmt::Break(value), span))
            }
            _ => {
                let expr = self.parse_expression()?;
                let span = expr.span;

                if self.check(&TokenKind::Semicolon) {
                    self.advance();
                }

                Ok(Spanned::new(Stmt::Expr(expr), span))
            }
        }
    }

    // Helper methods

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    fn peek_next(&self) -> Option<&Token> {
        self.tokens.get(self.position + 1)
    }

    fn advance(&mut self) -> Option<Token> {
        if self.is_at_end() {
            None
        } else {
            let token = self.tokens[self.position].clone();
            self.position += 1;
            Some(token)
        }
    }

    fn check(&self, kind: &TokenKind) -> bool {
        self.peek().map(|t| &t.kind == kind).unwrap_or(false)
    }

    fn check_integer(&self) -> bool {
        matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Integer(_)))
    }

    fn check_statement_terminator(&self) -> bool {
        matches!(
            self.peek().map(|t| &t.kind),
            Some(TokenKind::Semicolon) | Some(TokenKind::RightBrace) | None
        )
    }

    fn check_closing_delimiter(&self) -> bool {
        matches!(
            self.peek().map(|t| &t.kind),
            Some(TokenKind::RightParen)
                | Some(TokenKind::RightBracket)
                | Some(TokenKind::RightBrace)
                | Some(TokenKind::Comma)
        )
    }

    fn check_infix_at_or_below_precedence(&self, min_precedence: Precedence) -> bool {
        if let Some(token) = self.peek() {
            let prec = self.get_infix_precedence(&token.kind);
            // If it's an infix operator at same or lower precedence, we should stop
            // This handles cases like `4.. |> find(...)` where |> has same precedence as ..
            prec > Precedence::None && prec <= min_precedence
        } else {
            false
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek()
            .map(|t| t.kind == TokenKind::Eof)
            .unwrap_or(true)
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        if self.check(&kind) {
            Ok(self.advance().unwrap())
        } else {
            let token = self.peek();
            let span = token.map(|t| t.span).unwrap_or(Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            });
            Err(ParseError::new(
                format!("Expected {:?}, got {:?}", kind, token.map(|t| &t.kind)),
                span,
            ))
        }
    }
}

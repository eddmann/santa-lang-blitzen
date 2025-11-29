use crate::lexer::LexError;
use crate::parser::ParseError;
use crate::vm::compiler::CompileError;
use crate::vm::runtime::RuntimeError;
use std::fmt;

// Re-export StackFrame from runtime module
pub use crate::vm::runtime::StackFrame;

/// Unified error type for all santa-lang errors
#[derive(Debug)]
pub enum SantaError {
    Lex(LexError),
    Parse(ParseError),
    Compile(CompileError),
    Runtime(RuntimeError),
}

impl From<LexError> for SantaError {
    fn from(err: LexError) -> Self {
        SantaError::Lex(err)
    }
}

impl From<ParseError> for SantaError {
    fn from(err: ParseError) -> Self {
        SantaError::Parse(err)
    }
}

impl From<CompileError> for SantaError {
    fn from(err: CompileError) -> Self {
        SantaError::Compile(err)
    }
}

impl From<RuntimeError> for SantaError {
    fn from(err: RuntimeError) -> Self {
        SantaError::Runtime(err)
    }
}

impl fmt::Display for SantaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SantaError::Lex(err) => write!(
                f,
                "Lexical error at line {}, column {}: {}",
                err.line, err.column, err.message
            ),
            SantaError::Parse(err) => write!(
                f,
                "Parse error at line {}, column {}: {}",
                err.span.line, err.span.column, err.message
            ),
            SantaError::Compile(err) => write!(
                f,
                "Compile error at line {}, column {}: {}",
                err.span.line, err.span.column, err.message
            ),
            SantaError::Runtime(err) => {
                if err.is_break {
                    write!(f, "break outside of loop at line {}", err.line)
                } else {
                    write!(f, "Runtime error at line {}: {}", err.line, err.message)
                }
            }
        }
    }
}

impl std::error::Error for SantaError {}

impl SantaError {
    /// Format error with source context
    pub fn format_with_source(&self, source: &str) -> String {
        let (line, column, message, kind) = match self {
            SantaError::Lex(err) => (err.line, err.column, &err.message, "Lexical error"),
            SantaError::Parse(err) => (err.span.line, err.span.column, &err.message, "Parse error"),
            SantaError::Compile(err) => (
                err.span.line,
                err.span.column,
                &err.message,
                "Compile error",
            ),
            SantaError::Runtime(err) => {
                if err.is_break {
                    return format!("Runtime error at line {}: break outside of loop", err.line);
                }
                (err.line, 0, &err.message, "Runtime error")
            }
        };

        let mut output = String::new();

        // Error header
        output.push_str(&format!("\n{} at line {}", kind, line));
        if column > 0 {
            output.push_str(&format!(", column {}", column));
        }
        output.push_str(&format!(": {}\n\n", message));

        // Source context (5 lines around error)
        let lines: Vec<&str> = source.lines().collect();
        let error_line_idx = (line as usize).saturating_sub(1);

        let start = error_line_idx.saturating_sub(2);
        let end = (error_line_idx + 3).min(lines.len());

        for (idx, line_content) in lines.iter().enumerate().take(end).skip(start) {
            let line_num = idx + 1;
            let is_error_line = idx == error_line_idx;

            if is_error_line {
                output.push_str(&format!(" → {:4} | {}\n", line_num, line_content));

                // Add caret pointing to error position
                if column > 0 {
                    output.push_str("        | ");
                    output.push_str(&" ".repeat(column as usize - 1));
                    output.push_str("^\n");
                }
            } else {
                output.push_str(&format!("   {:4} | {}\n", line_num, line_content));
            }
        }

        // Add stack trace for runtime errors
        if let SantaError::Runtime(err) = self
            && !err.stack_trace.is_empty()
        {
            output.push_str("\nStack trace:\n");
            for (i, frame) in err.stack_trace.iter().enumerate() {
                if let Some(ref name) = frame.function_name {
                    output.push_str(&format!("  {} at {} (line {})\n", i, name, frame.line));
                } else {
                    output.push_str(&format!("  {} at <anonymous> (line {})\n", i, frame.line));
                }
            }
        }

        output
    }

    /// Format error with source context and colored output
    #[cfg(feature = "colored")]
    pub fn format_colored(&self, source: &str) -> String {
        use colored::Colorize;

        let (line, column, message, kind) = match self {
            SantaError::Lex(err) => (err.line, err.column, &err.message, "Lexical error"),
            SantaError::Parse(err) => (err.span.line, err.span.column, &err.message, "Parse error"),
            SantaError::Compile(err) => (
                err.span.line,
                err.span.column,
                &err.message,
                "Compile error",
            ),
            SantaError::Runtime(err) => {
                if err.is_break {
                    return format!(
                        "\n{}: break outside of loop\n",
                        format!("Runtime error at line {}", err.line).red().bold()
                    );
                }
                (err.line, 0, &err.message, "Runtime error")
            }
        };

        let mut output = String::new();

        // Error header (red and bold)
        output.push_str("\n");
        output.push_str(
            &format!("{} at line {}", kind, line)
                .red()
                .bold()
                .to_string(),
        );
        if column > 0 {
            output.push_str(&format!(", column {}", column).red().bold().to_string());
        }
        output.push_str(&format!(": {}\n\n", message.red()));

        // Source context
        let lines: Vec<&str> = source.lines().collect();
        let error_line_idx = (line as usize).saturating_sub(1);

        let start = error_line_idx.saturating_sub(2);
        let end = (error_line_idx + 3).min(lines.len());

        for (idx, line_content) in lines.iter().enumerate().take(end).skip(start) {
            let line_num = idx + 1;
            let is_error_line = idx == error_line_idx;

            if is_error_line {
                output.push_str(
                    &format!(" → {:4} | {}\n", line_num, line_content)
                        .red()
                        .to_string(),
                );

                // Add caret pointing to error position
                if column > 0 {
                    output.push_str(&"        | ".red().to_string());
                    output.push_str(&" ".repeat(column as usize - 1));
                    output.push_str(&"^".red().bold().to_string());
                    output.push('\n');
                }
            } else {
                output.push_str(
                    &format!("   {:4} | {}\n", line_num, line_content)
                        .dimmed()
                        .to_string(),
                );
            }
        }

        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Span;

    #[test]
    fn lex_error_display() {
        let err = SantaError::Lex(LexError {
            message: "Unexpected character '&'".to_string(),
            line: 5,
            column: 10,
        });

        let display = err.to_string();
        assert!(display.contains("Lexical error"));
        assert!(display.contains("line 5"));
        assert!(display.contains("column 10"));
        assert!(display.contains("Unexpected character '&'"));
    }

    #[test]
    fn parse_error_display() {
        let err = SantaError::Parse(ParseError {
            message: "Expected ')'".to_string(),
            span: Span {
                start: 0,
                end: 1,
                line: 3,
                column: 15,
            },
        });

        let display = err.to_string();
        assert!(display.contains("Parse error"));
        assert!(display.contains("line 3"));
        assert!(display.contains("column 15"));
        assert!(display.contains("Expected ')'"));
    }

    #[test]
    fn runtime_error_display() {
        let err = SantaError::Runtime(RuntimeError::new("Division by zero", 10));

        let display = err.to_string();
        assert!(display.contains("Runtime error"));
        assert!(display.contains("line 10"));
        assert!(display.contains("Division by zero"));
    }

    #[test]
    fn error_with_source_context() {
        let source = r#"let x = 1;
let y = 2;
let z = x + y;
let w = x / 0;
let v = 5;"#;

        let err = SantaError::Runtime(RuntimeError::new("Division by zero", 4));
        let formatted = err.format_with_source(source);

        // Should show 5 lines of context (2 before, error line, 2 after)
        assert!(formatted.contains("line 4"));
        assert!(formatted.contains("Division by zero"));
        assert!(formatted.contains("let y = 2"));
        assert!(formatted.contains("let z = x + y"));
        assert!(formatted.contains("let w = x / 0"));
        assert!(formatted.contains("let v = 5"));
    }

    #[test]
    fn error_with_caret_position() {
        let source = "let x = 1 + @;";

        let err = SantaError::Lex(LexError {
            message: "Unexpected character '@'".to_string(),
            line: 1,
            column: 13,
        });

        let formatted = err.format_with_source(source);

        // Should have caret pointing to error
        assert!(formatted.contains("^"));
        // Caret should be at column 13 (12 spaces + ^)
        assert!(formatted.contains("            ^"));
    }

    #[test]
    fn error_at_file_boundaries() {
        let source = "let x = 1;";

        // Error at first line
        let err = SantaError::Runtime(RuntimeError::new("Some error", 1));
        let formatted = err.format_with_source(source);
        assert!(formatted.contains("let x = 1"));

        // Error beyond last line (should not panic)
        let err = SantaError::Runtime(RuntimeError::new("Some error", 100));
        let formatted = err.format_with_source(source);
        assert!(formatted.contains("Runtime error at line 100"));
    }

    #[test]
    fn runtime_error_with_stack_trace() {
        let source = r#"let foo = |x| {
  let bar = |y| {
    y / 0
  };
  bar(x)
};
foo(5);"#;

        let mut err = RuntimeError::new("Division by zero", 3);
        err.add_frame(Some("bar".to_string()), 3);
        err.add_frame(Some("foo".to_string()), 5);
        err.add_frame(None, 7);

        let santa_err = SantaError::Runtime(err);
        let formatted = santa_err.format_with_source(source);

        // Should show error message
        assert!(formatted.contains("Division by zero"));
        assert!(formatted.contains("line 3"));

        // Should show stack trace
        assert!(formatted.contains("Stack trace:"));
        assert!(formatted.contains("bar"));
        assert!(formatted.contains("foo"));
        assert!(formatted.contains("<anonymous>"));
    }

    #[test]
    fn runtime_error_without_stack_trace() {
        let source = "let x = 1 / 0;";

        let err = RuntimeError::new("Division by zero", 1);
        let santa_err = SantaError::Runtime(err);
        let formatted = santa_err.format_with_source(source);

        // Should show error but no stack trace section
        assert!(formatted.contains("Division by zero"));
        assert!(!formatted.contains("Stack trace:"));
    }
}

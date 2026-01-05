//! CLI output formatting for JSON and JSONL modes.
//!
//! This module implements the CLI Output Format Specification (Section 16 of lang.txt).
//! It provides machine-readable output formats for integration with editors, CI systems,
//! and other tools.

use lang::error::SantaError;
use lang::runner::{SolutionResult, TestResult};
use lang::vm::Value;
use serde::Serialize;
use std::io::{self, Write};

/// Output mode for CLI execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputMode {
    /// Human-readable output with ANSI colors (default)
    Text,
    /// Single JSON object after execution completes
    Json,
    /// Real-time streaming with JSON Lines
    Jsonl,
}

/// Console output entry from puts() calls.
#[derive(Debug, Clone, Serialize)]
pub struct ConsoleEntry {
    pub timestamp_ms: u64,
    pub message: String,
}

/// Error location with 1-indexed line and column.
#[derive(Debug, Clone, Serialize)]
pub struct ErrorLocation {
    pub line: u32,
    pub column: u32,
}

/// Stack frame for error traces.
#[derive(Debug, Clone, Serialize)]
pub struct StackFrame {
    pub function: String,
    pub line: u32,
    pub column: u32,
}

/// Part result for JSON output.
#[derive(Debug, Clone, Serialize)]
pub struct JsonPartResult {
    pub status: &'static str,
    pub value: String,
    pub duration_ms: u64,
}

/// Test part result for JSON output.
#[derive(Debug, Clone, Serialize)]
pub struct JsonTestPartResult {
    pub passed: bool,
    pub expected: String,
    pub actual: String,
}

/// Test case for JSON output.
#[derive(Debug, Clone, Serialize)]
pub struct JsonTestCase {
    pub index: u32,
    pub slow: bool,
    pub status: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub part_one: Option<JsonTestPartResult>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub part_two: Option<JsonTestPartResult>,
}

/// Test summary counts.
#[derive(Debug, Clone, Serialize)]
pub struct TestSummary {
    pub total: u32,
    pub passed: u32,
    pub failed: u32,
    pub skipped: u32,
}

/// JSON output for solution execution.
#[derive(Debug, Clone, Serialize)]
pub struct JsonSolutionOutput {
    #[serde(rename = "type")]
    pub output_type: &'static str,
    pub status: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub part_one: Option<JsonPartResult>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub part_two: Option<JsonPartResult>,
    pub console: Vec<ConsoleEntry>,
}

/// JSON output for script execution.
#[derive(Debug, Clone, Serialize)]
pub struct JsonScriptOutput {
    #[serde(rename = "type")]
    pub output_type: &'static str,
    pub status: &'static str,
    pub value: String,
    pub duration_ms: u64,
    pub console: Vec<ConsoleEntry>,
}

/// JSON output for test execution.
#[derive(Debug, Clone, Serialize)]
pub struct JsonTestOutput {
    #[serde(rename = "type")]
    pub output_type: &'static str,
    pub status: &'static str,
    pub success: bool,
    pub summary: TestSummary,
    pub tests: Vec<JsonTestCase>,
    pub console: Vec<ConsoleEntry>,
}

/// JSON output for errors.
#[derive(Debug, Clone, Serialize)]
pub struct JsonErrorOutput {
    #[serde(rename = "type")]
    pub output_type: &'static str,
    pub message: String,
    pub location: ErrorLocation,
    pub stack: Vec<StackFrame>,
}

/// Format a SantaError as JSON error output.
pub fn format_error_json(error: &SantaError) -> JsonErrorOutput {
    let (line, column, message, stack) = match error {
        SantaError::Lex(err) => (err.line, err.column, err.message.clone(), vec![]),
        SantaError::Parse(err) => (err.span.line, err.span.column, err.message.clone(), vec![]),
        SantaError::Compile(err) => (err.span.line, err.span.column, err.message.clone(), vec![]),
        SantaError::Runtime(err) => {
            let stack: Vec<StackFrame> = err
                .stack_trace
                .iter()
                .map(|frame| StackFrame {
                    function: frame
                        .function_name
                        .clone()
                        .unwrap_or_else(|| "<lambda>".to_string()),
                    line: frame.line,
                    column: 1, // RuntimeError doesn't track column
                })
                .collect();
            (err.line, 1, err.message.clone(), stack) // column=1 since RuntimeError doesn't track it
        }
    };

    JsonErrorOutput {
        output_type: "error",
        message,
        location: ErrorLocation { line, column },
        stack,
    }
}

/// Format solution result as JSON.
pub fn format_solution_json(result: &SolutionResult, console: Vec<ConsoleEntry>) -> String {
    let output = JsonSolutionOutput {
        output_type: "solution",
        status: "complete",
        part_one: result.part_one.as_ref().map(|(value, duration)| JsonPartResult {
            status: "complete",
            value: value.to_string(),
            duration_ms: *duration as u64,
        }),
        part_two: result.part_two.as_ref().map(|(value, duration)| JsonPartResult {
            status: "complete",
            value: value.to_string(),
            duration_ms: *duration as u64,
        }),
        console,
    };
    serde_json::to_string(&output).unwrap()
}

/// Format script result as JSON.
pub fn format_script_json(value: &Value, duration_ms: u64, console: Vec<ConsoleEntry>) -> String {
    let output = JsonScriptOutput {
        output_type: "script",
        status: "complete",
        value: value.to_string(),
        duration_ms,
        console,
    };
    serde_json::to_string(&output).unwrap()
}

/// Collected test info for JSON output (includes skipped tests).
pub struct CollectedTestInfo {
    pub index: usize,
    pub slow: bool,
    pub skipped: bool,
    pub result: Option<TestResult>,
}

/// Format test results as JSON.
pub fn format_test_json(
    tests: &[CollectedTestInfo],
    has_part_one: bool,
    has_part_two: bool,
    console: Vec<ConsoleEntry>,
) -> String {
    let mut passed = 0u32;
    let mut failed = 0u32;
    let mut skipped = 0u32;

    let test_cases: Vec<JsonTestCase> = tests
        .iter()
        .map(|info| {
            if info.skipped {
                skipped += 1;
                JsonTestCase {
                    index: (info.index + 1) as u32,
                    slow: info.slow,
                    status: "skipped",
                    part_one: None,
                    part_two: None,
                }
            } else if let Some(ref result) = info.result {
                // Determine if test passed (all parts must pass)
                let part_one_passed = result.part_one_passed.unwrap_or(true);
                let part_two_passed = result.part_two_passed.unwrap_or(true);
                let all_passed = part_one_passed && part_two_passed;

                if all_passed {
                    passed += 1;
                } else {
                    failed += 1;
                }

                JsonTestCase {
                    index: (info.index + 1) as u32,
                    slow: info.slow,
                    status: "complete",
                    part_one: if has_part_one && result.part_one_passed.is_some() {
                        Some(JsonTestPartResult {
                            passed: result.part_one_passed.unwrap(),
                            expected: result
                                .part_one_expected
                                .as_ref()
                                .map(|v| v.to_string())
                                .unwrap_or_default(),
                            actual: result
                                .part_one_actual
                                .as_ref()
                                .map(|v| v.to_string())
                                .unwrap_or_default(),
                        })
                    } else {
                        None
                    },
                    part_two: if has_part_two && result.part_two_passed.is_some() {
                        Some(JsonTestPartResult {
                            passed: result.part_two_passed.unwrap(),
                            expected: result
                                .part_two_expected
                                .as_ref()
                                .map(|v| v.to_string())
                                .unwrap_or_default(),
                            actual: result
                                .part_two_actual
                                .as_ref()
                                .map(|v| v.to_string())
                                .unwrap_or_default(),
                        })
                    } else {
                        None
                    },
                }
            } else {
                // No result (shouldn't happen for non-skipped)
                skipped += 1;
                JsonTestCase {
                    index: (info.index + 1) as u32,
                    slow: info.slow,
                    status: "skipped",
                    part_one: None,
                    part_two: None,
                }
            }
        })
        .collect();

    let total = tests.len() as u32;
    let success = failed == 0;

    let output = JsonTestOutput {
        output_type: "test",
        status: "complete",
        success,
        summary: TestSummary {
            total,
            passed,
            failed,
            skipped,
        },
        tests: test_cases,
        console,
    };

    serde_json::to_string(&output).unwrap()
}

// ============================================================================
// JSONL Streaming Support
// ============================================================================

/// JSONL patch operation per RFC 6902.
#[derive(Debug, Clone, Serialize)]
pub struct JsonPatch {
    pub op: &'static str,
    pub path: String,
    pub value: serde_json::Value,
}

/// Initial state for JSONL solution streaming.
#[derive(Debug, Clone, Serialize)]
pub struct JsonlSolutionInitial {
    #[serde(rename = "type")]
    pub output_type: &'static str,
    pub status: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub part_one: Option<JsonlPartInitial>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub part_two: Option<JsonlPartInitial>,
    pub console: Vec<ConsoleEntry>,
}

/// Initial state for a part in JSONL streaming.
#[derive(Debug, Clone, Serialize)]
pub struct JsonlPartInitial {
    pub status: &'static str,
    pub value: Option<String>,
    pub duration_ms: Option<u64>,
}

/// Initial state for JSONL script streaming.
#[derive(Debug, Clone, Serialize)]
pub struct JsonlScriptInitial {
    #[serde(rename = "type")]
    pub output_type: &'static str,
    pub status: &'static str,
    pub value: Option<String>,
    pub duration_ms: Option<u64>,
    pub console: Vec<ConsoleEntry>,
}

/// Initial state for JSONL test streaming.
#[derive(Debug, Clone, Serialize)]
pub struct JsonlTestInitial {
    #[serde(rename = "type")]
    pub output_type: &'static str,
    pub status: &'static str,
    pub success: Option<bool>,
    pub summary: TestSummary,
    pub tests: Vec<JsonlTestCaseInitial>,
    pub console: Vec<ConsoleEntry>,
}

/// Initial test case state for JSONL streaming.
#[derive(Debug, Clone, Serialize)]
pub struct JsonlTestCaseInitial {
    pub index: u32,
    pub slow: bool,
    pub status: &'static str,
    pub part_one: Option<JsonTestPartResult>,
    pub part_two: Option<JsonTestPartResult>,
}

/// JSONL streaming writer.
pub struct JsonlWriter<W: Write> {
    writer: W,
}

impl<W: Write> JsonlWriter<W> {
    pub fn new(writer: W) -> Self {
        Self { writer }
    }

    /// Write initial state line.
    pub fn write_initial<T: Serialize>(&mut self, state: &T) -> io::Result<()> {
        let json = serde_json::to_string(state)?;
        writeln!(self.writer, "{}", json)?;
        self.writer.flush()
    }

    /// Write a patch array.
    pub fn write_patches(&mut self, patches: &[JsonPatch]) -> io::Result<()> {
        let json = serde_json::to_string(patches)?;
        writeln!(self.writer, "{}", json)?;
        self.writer.flush()
    }

    /// Create a replace patch.
    pub fn replace_patch(path: &str, value: impl Serialize) -> JsonPatch {
        JsonPatch {
            op: "replace",
            path: path.to_string(),
            value: serde_json::to_value(value).unwrap(),
        }
    }

    /// Create an add patch (for appending to arrays).
    pub fn add_patch(path: &str, value: impl Serialize) -> JsonPatch {
        JsonPatch {
            op: "add",
            path: path.to_string(),
            value: serde_json::to_value(value).unwrap(),
        }
    }
}

/// Determine if source is a solution (has part_one/part_two) or script.
pub fn is_solution_source(source: &str) -> (bool, bool) {
    // Simple heuristic: check if source contains part_one: or part_two:
    // This matches the runner's behavior
    let has_part_one = source.contains("part_one:");
    let has_part_two = source.contains("part_two:");
    (has_part_one, has_part_two)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_solution_source() {
        assert_eq!(is_solution_source("part_one: { 42 }"), (true, false));
        assert_eq!(is_solution_source("part_two: { 42 }"), (false, true));
        assert_eq!(
            is_solution_source("part_one: { 1 }\npart_two: { 2 }"),
            (true, true)
        );
        assert_eq!(is_solution_source("1 + 2"), (false, false));
    }
}

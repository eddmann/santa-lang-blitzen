// RuntimeError is 128+ bytes but boxing would add overhead on successful paths.
// Error paths are not performance-critical for an interpreter.
#![allow(clippy::result_large_err)]

use crate::output::ConsoleEntry;
use lang::vm::{RuntimeError, Value};
use std::cell::RefCell;
use std::fs;
use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};

thread_local! {
    /// Console buffer for capturing puts() output in JSON/JSONL mode.
    /// When Some, puts() will append to this buffer instead of printing.
    static CONSOLE_BUFFER: RefCell<Option<Vec<ConsoleEntry>>> = const { RefCell::new(None) };

    /// Start time for calculating timestamps in console entries.
    static START_TIME_MS: RefCell<u128> = const { RefCell::new(0) };
}

/// Enable console capture mode. Returns any previously captured entries.
pub fn enable_console_capture() -> Vec<ConsoleEntry> {
    let start = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards")
        .as_millis();

    START_TIME_MS.with(|t| *t.borrow_mut() = start);
    CONSOLE_BUFFER.with(|buf| buf.borrow_mut().replace(Vec::new()).unwrap_or_default())
}

/// Disable console capture and return captured entries.
pub fn disable_console_capture() -> Vec<ConsoleEntry> {
    CONSOLE_BUFFER.with(|buf| buf.borrow_mut().take().unwrap_or_default())
}

/// Get current timestamp in milliseconds since capture started.
fn get_timestamp_ms() -> u64 {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards")
        .as_millis();

    START_TIME_MS.with(|t| (now - *t.borrow()) as u64)
}

/// Format a value for puts() output.
/// Strings are displayed without surrounding quotes (unlike Display trait).
fn format_puts_value(value: &Value) -> String {
    match value {
        Value::String(s) => s.to_string(),
        other => other.to_string(),
    }
}

pub fn builtin_puts(args: &[Value]) -> Result<Value, RuntimeError> {
    // Skip if no values (spec says no event for puts with no args)
    if args.is_empty() {
        return Ok(Value::Nil);
    }

    // Build the message string (space-separated values)
    // Use format_puts_value to avoid quotes around strings
    let output = args
        .iter()
        .map(format_puts_value)
        .collect::<Vec<_>>()
        .join(" ");

    // Check if we should capture or print
    let captured = CONSOLE_BUFFER.with(|buf| {
        if let Some(ref mut buffer) = *buf.borrow_mut() {
            buffer.push(ConsoleEntry {
                timestamp_ms: get_timestamp_ms(),
                message: output.clone(),
            });
            true
        } else {
            false
        }
    });

    if !captured {
        // Print to stdout as normal
        println!("{}", output);
    }

    Ok(Value::Nil)
}

pub fn builtin_read(
    path: &str,
    session_token: Option<&str>,
    script_dir: Option<&Path>,
) -> Result<Value, RuntimeError> {
    if path.starts_with("aoc://") {
        read_aoc_input(path, session_token, script_dir)
    } else if path.starts_with("http://") || path.starts_with("https://") {
        read_url(path)
    } else {
        read_file(path)
    }
}

fn read_file(path: &str) -> Result<Value, RuntimeError> {
    fs::read_to_string(path)
        .map(|content| Value::String(content.into()))
        .map_err(|e| RuntimeError::new(format!("Failed to read file '{}': {}", path, e), 0))
}

fn read_url(url: &str) -> Result<Value, RuntimeError> {
    ureq::get(url)
        .call()
        .map_err(|e| RuntimeError::new(format!("Failed to fetch URL '{}': {}", url, e), 0))?
        .into_string()
        .map(|content| Value::String(content.into()))
        .map_err(|e| RuntimeError::new(format!("Failed to read URL response: {}", e), 0))
}

fn read_aoc_input(
    url: &str,
    session_token: Option<&str>,
    script_dir: Option<&Path>,
) -> Result<Value, RuntimeError> {
    let parts: Vec<&str> = url.strip_prefix("aoc://").unwrap().split('/').collect();
    if parts.len() != 2 {
        return Err(RuntimeError::new(
            format!(
                "Invalid AOC URL format: '{}'. Expected 'aoc://YEAR/DAY'",
                url
            ),
            0,
        ));
    }

    let year = parts[0]
        .parse::<u32>()
        .map_err(|_| RuntimeError::new(format!("Invalid year in AOC URL: '{}'", parts[0]), 0))?;
    let day = parts[1]
        .parse::<u32>()
        .map_err(|_| RuntimeError::new(format!("Invalid day in AOC URL: '{}'", parts[1]), 0))?;

    let filename = format!("aoc{}_day{:02}.input", year, day);

    // Check for local .input file next to the script
    if let Some(dir) = script_dir {
        let input_path = dir.join(&filename);
        if let Ok(content) = fs::read_to_string(&input_path) {
            return Ok(Value::String(content.into()));
        }
    }

    // Fetch from AOC (requires token)
    let token = session_token.ok_or_else(|| {
        RuntimeError::new(
            "AOC session token not found. Set SANTA_CLI_SESSION_TOKEN environment variable"
                .to_string(),
            0,
        )
    })?;

    let aoc_url = format!("https://adventofcode.com/{}/day/{}/input", year, day);
    let content = ureq::get(&aoc_url)
        .set("Cookie", &format!("session={}", token))
        .call()
        .map_err(|e| RuntimeError::new(format!("Failed to fetch AOC input: {}", e), 0))?
        .into_string()
        .map_err(|e| RuntimeError::new(format!("Failed to read AOC response: {}", e), 0))?;

    // Cache locally if script_dir provided
    if let Some(dir) = script_dir {
        let input_path = dir.join(&filename);
        fs::write(&input_path, &content).ok(); // Ignore write errors
    }

    Ok(Value::String(content.into()))
}

pub fn builtin_env(vars: &[(String, Value)]) -> Result<Value, RuntimeError> {
    println!("Environment:");
    for (name, value) in vars {
        println!("  {} = {}", name, value);
    }
    Ok(Value::Nil)
}

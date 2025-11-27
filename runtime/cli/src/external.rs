// RuntimeError is 128+ bytes but boxing would add overhead on successful paths.
// Error paths are not performance-critical for an interpreter.
#![allow(clippy::result_large_err)]

use lang::vm::{RuntimeError, Value};
use std::fs;

pub fn builtin_puts(args: &[Value]) -> Result<Value, RuntimeError> {
    let output = args
        .iter()
        .map(|v| v.to_string())
        .collect::<Vec<_>>()
        .join(" ");
    println!("{}", output);
    Ok(Value::Nil)
}

pub fn builtin_read(path: &str, session_token: Option<&str>) -> Result<Value, RuntimeError> {
    if path.starts_with("aoc://") {
        read_aoc_input(path, session_token)
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

fn read_aoc_input(url: &str, session_token: Option<&str>) -> Result<Value, RuntimeError> {
    let parts: Vec<&str> = url.strip_prefix("aoc://").unwrap().split('/').collect();
    if parts.len() != 2 {
        return Err(RuntimeError::new(
            format!("Invalid AOC URL format: '{}'. Expected 'aoc://YEAR/DAY'", url),
            0,
        ));
    }

    let year = parts[0]
        .parse::<u32>()
        .map_err(|_| RuntimeError::new(format!("Invalid year in AOC URL: '{}'", parts[0]), 0))?;
    let day = parts[1]
        .parse::<u32>()
        .map_err(|_| RuntimeError::new(format!("Invalid day in AOC URL: '{}'", parts[1]), 0))?;

    if let Some(cached) = get_cached_input(year, day) {
        return Ok(Value::String(cached.into()));
    }

    let token = session_token.ok_or_else(|| {
        RuntimeError::new(
            "AOC session token not found. Set SANTA_CLI_SESSION_TOKEN environment variable".to_string(),
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

    cache_input(year, day, &content)?;
    Ok(Value::String(content.into()))
}

fn get_cache_dir() -> Option<std::path::PathBuf> {
    dirs::cache_dir().map(|mut path| {
        path.push("santa-cli");
        path.push("inputs");
        path
    })
}

fn get_cached_input(year: u32, day: u32) -> Option<String> {
    let cache_dir = get_cache_dir()?;
    let cache_file = cache_dir.join(format!("{}-{}.txt", year, day));
    fs::read_to_string(cache_file).ok()
}

fn cache_input(year: u32, day: u32, content: &str) -> Result<(), RuntimeError> {
    if let Some(cache_dir) = get_cache_dir() {
        fs::create_dir_all(&cache_dir).map_err(|e| {
            RuntimeError::new(format!("Failed to create cache directory: {}", e), 0)
        })?;
        let cache_file = cache_dir.join(format!("{}-{}.txt", year, day));
        fs::write(cache_file, content)
            .map_err(|e| RuntimeError::new(format!("Failed to cache input: {}", e), 0))?;
    }
    Ok(())
}

pub fn builtin_env(vars: &[(String, Value)]) -> Result<Value, RuntimeError> {
    println!("Environment:");
    for (name, value) in vars {
        println!("  {} = {}", name, value);
    }
    Ok(Value::Nil)
}

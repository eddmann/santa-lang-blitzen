mod external;
mod output;

use lang::{
    error::SantaError,
    lexer::Lexer,
    parser::{Parser, ast::Section},
    runner::AocRunner,
    vm::{RuntimeError, VM, Value},
};
use output::{
    CollectedTestInfo, JsonTestPartResult, JsonVersionOutput, JsonlPartInitial, JsonlScriptInitial,
    JsonlSolutionInitial, JsonlTestCaseInitial, JsonlTestInitial, JsonlWriter, OutputMode, TestSummary,
    format_error_json, format_script_json, format_solution_json, format_test_json, is_solution_source,
};
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use std::env;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process;
use std::time::Instant;

#[cfg(feature = "profile")]
use std::fs::File;

fn main() {
    let args: Vec<String> = env::args().collect();

    // Parse flags
    let mut test_mode = false;
    let mut include_slow = false;
    let mut eval_script: Option<String> = None;
    let mut script_path: Option<String> = None;
    let mut output_mode = OutputMode::Text;
    let mut show_version = false;
    #[allow(unused_variables, unused_assignments)]
    let mut profile_mode = false;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-h" | "--help" => {
                print_help();
                process::exit(0);
            }
            "-v" | "--version" => {
                show_version = true;
            }
            "-r" | "--repl" => {
                let result = run_repl();
                match result {
                    Ok(()) => process::exit(0),
                    Err(ExitCode::ArgumentError) => process::exit(1),
                    Err(ExitCode::RuntimeError) => process::exit(2),
                    Err(ExitCode::TestFailure) => process::exit(3),
                }
            }
            "-t" | "--test" => {
                test_mode = true;
            }
            "-s" | "--slow" => {
                include_slow = true;
            }
            "-p" | "--profile" => {
                #[allow(unused_assignments)]
                {
                    profile_mode = true;
                }
            }
            "-o" | "--output" => {
                i += 1;
                if i < args.len() {
                    output_mode = match args[i].as_str() {
                        "text" => OutputMode::Text,
                        "json" => OutputMode::Json,
                        "jsonl" => OutputMode::Jsonl,
                        other => {
                            eprintln!("Error: Invalid output format '{}'. Use: text, json, jsonl", other);
                            process::exit(1);
                        }
                    };
                } else {
                    eprintln!("Error: -o requires a format argument (text, json, jsonl)");
                    process::exit(1);
                }
            }
            "-e" | "--eval" => {
                i += 1;
                if i < args.len() {
                    eval_script = Some(args[i].clone());
                } else {
                    eprintln!("Error: -e requires a script argument");
                    process::exit(1);
                }
            }
            arg if !arg.starts_with('-') => {
                script_path = Some(arg.to_string());
            }
            unknown => {
                eprintln!("Unknown option: {}", unknown);
                process::exit(1);
            }
        }
        i += 1;
    }

    // Handle version after parsing (so we have output_mode)
    if show_version {
        match output_mode {
            OutputMode::Text => {
                println!("santa-lang Blitzen {}", env!("CARGO_PKG_VERSION"));
            }
            OutputMode::Json | OutputMode::Jsonl => {
                let output = JsonVersionOutput {
                    reindeer: "Blitzen",
                    version: env!("CARGO_PKG_VERSION"),
                };
                println!("{}", serde_json::to_string(&output).unwrap());
            }
        }
        process::exit(0);
    }

    // Determine source: -e > file > stdin
    let (source, source_path): (String, Option<String>) = if let Some(script) = eval_script {
        (script, None)
    } else if let Some(path) = script_path {
        match fs::read_to_string(&path) {
            Ok(content) => (content, Some(path)),
            Err(e) => {
                eprintln!("Error reading file '{}': {}", path, e);
                process::exit(1);
            }
        }
    } else if !atty::is(atty::Stream::Stdin) {
        let mut source = String::new();
        if let Err(e) = std::io::stdin().read_to_string(&mut source) {
            eprintln!("Error reading from stdin: {}", e);
            process::exit(1);
        }
        (source, None)
    } else {
        print_help();
        process::exit(1);
    };

    // Start profiler if enabled
    #[cfg(feature = "profile")]
    let _guard = if profile_mode { Some(start_profiler()) } else { None };

    let result = if test_mode {
        run_tests_from_source(&source, source_path.as_deref(), include_slow, output_mode)
    } else {
        run_script_from_source(&source, source_path.as_deref(), output_mode)
    };

    // Stop profiler and write output
    #[cfg(feature = "profile")]
    if let Some(guard) = _guard {
        stop_profiler(guard);
    }

    match result {
        Ok(()) => process::exit(0),
        Err(ExitCode::ArgumentError) => process::exit(1),
        Err(ExitCode::RuntimeError) => process::exit(2),
        Err(ExitCode::TestFailure) => process::exit(3),
    }
}

enum ExitCode {
    #[allow(dead_code)]
    ArgumentError,
    RuntimeError,
    TestFailure,
}

fn print_help() {
    println!("santa-lang CLI - Blitzen {}", env!("CARGO_PKG_VERSION"));
    println!();
    println!("USAGE:");
    println!("    santa-cli <SCRIPT>              Run solution file");
    println!("    santa-cli -e <CODE>             Evaluate inline script");
    println!("    santa-cli -t <SCRIPT>           Run test suite");
    println!("    santa-cli -t -s <SCRIPT>        Run tests including @slow");
    println!("    santa-cli -o json <SCRIPT>      Output as JSON");
    println!("    santa-cli -o jsonl <SCRIPT>     Output as JSON Lines (streaming)");
    println!("    santa-cli -r                    Start REPL");
    println!("    santa-cli -h                    Show this help");
    println!("    cat file | santa-cli            Read from stdin");
    println!();
    println!("OPTIONS:");
    println!("    -e, --eval <CODE>    Evaluate inline script");
    println!("    -o, --output FORMAT  Output format: text (default), json, jsonl");
    println!("    -t, --test           Run the solution's test suite");
    println!("    -s, --slow           Include @slow tests (use with -t)");
    println!("    -r, --repl           Start interactive REPL");
    println!("    -p, --profile        Enable CPU profiling");
    println!("    -h, --help           Show this help message");
    println!("    -v, --version        Display version information");
    println!();
    println!("ENVIRONMENT:");
    println!("    SANTA_CLI_SESSION_TOKEN    AOC session token for aoc:// URLs");
}

fn run_script_from_source(source: &str, source_path: Option<&str>, output_mode: OutputMode) -> Result<(), ExitCode> {
    // Enable console capture for JSON/JSONL modes
    if output_mode != OutputMode::Text {
        external::enable_console_capture();
    }

    let session_token = env::var("SANTA_CLI_SESSION_TOKEN").ok();
    let script_dir = source_path.and_then(|p| Path::new(p).parent().map(|d| d.to_path_buf()));

    // Lex and parse
    let mut lexer = Lexer::new(source);
    let tokens = match lexer.tokenize() {
        Ok(t) => t,
        Err(e) => {
            let santa_error = SantaError::Lex(e);
            return handle_error(santa_error, output_mode);
        }
    };

    let mut parser = Parser::new(tokens);
    let program = match parser.parse_program() {
        Ok(p) => p,
        Err(e) => {
            let santa_error = SantaError::Parse(e);
            return handle_error(santa_error, output_mode);
        }
    };

    let mut vm = create_vm(session_token.as_deref(), script_dir);
    let mut runner = AocRunner::new(program);

    match output_mode {
        OutputMode::Text => run_script_text(&mut runner, &mut vm),
        OutputMode::Json => run_script_json(source, &mut runner, &mut vm),
        OutputMode::Jsonl => run_script_jsonl(source, &mut runner, &mut vm),
    }
}

fn run_script_text(runner: &mut AocRunner, vm: &mut VM) -> Result<(), ExitCode> {
    if runner.is_script_mode() {
        match runner.run_script(vm) {
            Ok(script_result) => {
                if script_result != Value::Nil {
                    println!("{}", script_result);
                }
                Ok(())
            }
            Err(e) => {
                eprintln!("{}", e);
                Err(ExitCode::RuntimeError)
            }
        }
    } else {
        match runner.run_solution(vm) {
            Ok(result) => {
                if let Some((value, duration)) = result.part_one {
                    println!("Part 1: \x1b[32m{}\x1b[0m \x1b[90m{}ms\x1b[0m", value, duration);
                }
                if let Some((value, duration)) = result.part_two {
                    println!("Part 2: \x1b[32m{}\x1b[0m \x1b[90m{}ms\x1b[0m", value, duration);
                }
                Ok(())
            }
            Err(e) => {
                eprintln!("{}", e);
                Err(ExitCode::RuntimeError)
            }
        }
    }
}

fn run_script_json(_source: &str, runner: &mut AocRunner, vm: &mut VM) -> Result<(), ExitCode> {
    if runner.is_script_mode() {
        let start = Instant::now();
        match runner.run_script(vm) {
            Ok(result) => {
                let duration_ms = start.elapsed().as_millis() as u64;
                let console = external::disable_console_capture();
                let json = format_script_json(&result, duration_ms, console);
                println!("{}", json);
                Ok(())
            }
            Err(e) => {
                let _ = external::disable_console_capture();
                let santa_error = SantaError::Runtime(e);
                let json = serde_json::to_string(&format_error_json(&santa_error)).unwrap();
                println!("{}", json);
                Err(ExitCode::RuntimeError)
            }
        }
    } else {
        match runner.run_solution(vm) {
            Ok(result) => {
                let console = external::disable_console_capture();
                let json = format_solution_json(&result, console);
                println!("{}", json);
                Ok(())
            }
            Err(e) => {
                let _ = external::disable_console_capture();
                let santa_error = SantaError::Runtime(e);
                let json = serde_json::to_string(&format_error_json(&santa_error)).unwrap();
                println!("{}", json);
                Err(ExitCode::RuntimeError)
            }
        }
    }
}

fn run_script_jsonl(source: &str, runner: &mut AocRunner, vm: &mut VM) -> Result<(), ExitCode> {
    let mut writer = JsonlWriter::new(io::stdout());
    let (has_part_one, has_part_two) = is_solution_source(source);
    let is_solution = has_part_one || has_part_two;

    if is_solution {
        // Emit initial solution state
        let initial = JsonlSolutionInitial {
            output_type: "solution",
            status: "pending",
            part_one: if has_part_one {
                Some(JsonlPartInitial {
                    status: "pending",
                    value: None,
                    duration_ms: None,
                })
            } else {
                None
            },
            part_two: if has_part_two {
                Some(JsonlPartInitial {
                    status: "pending",
                    value: None,
                    duration_ms: None,
                })
            } else {
                None
            },
            console: vec![],
        };
        writer.write_initial(&initial).ok();
        writer
            .write_patches(&[JsonlWriter::<io::Stdout>::replace_patch("/status", "running")])
            .ok();

        match runner.run_solution(vm) {
            Ok(result) => {
                let console = external::disable_console_capture();

                // Emit console entries
                for entry in &console {
                    writer
                        .write_patches(&[JsonlWriter::<io::Stdout>::add_patch("/console/-", entry)])
                        .ok();
                }

                if let Some((value, duration)) = &result.part_one {
                    writer
                        .write_patches(&[JsonlWriter::<io::Stdout>::replace_patch("/part_one/status", "running")])
                        .ok();
                    writer
                        .write_patches(&[
                            JsonlWriter::<io::Stdout>::replace_patch("/part_one/status", "complete"),
                            JsonlWriter::<io::Stdout>::replace_patch("/part_one/value", value.to_string()),
                            JsonlWriter::<io::Stdout>::replace_patch("/part_one/duration_ms", *duration as u64),
                        ])
                        .ok();
                }

                if let Some((value, duration)) = &result.part_two {
                    writer
                        .write_patches(&[JsonlWriter::<io::Stdout>::replace_patch("/part_two/status", "running")])
                        .ok();
                    writer
                        .write_patches(&[
                            JsonlWriter::<io::Stdout>::replace_patch("/part_two/status", "complete"),
                            JsonlWriter::<io::Stdout>::replace_patch("/part_two/value", value.to_string()),
                            JsonlWriter::<io::Stdout>::replace_patch("/part_two/duration_ms", *duration as u64),
                        ])
                        .ok();
                }

                writer
                    .write_patches(&[JsonlWriter::<io::Stdout>::replace_patch("/status", "complete")])
                    .ok();
                Ok(())
            }
            Err(e) => {
                let _ = external::disable_console_capture();
                let santa_error = SantaError::Runtime(e);
                let error_output = format_error_json(&santa_error);
                writer
                    .write_patches(&[
                        JsonlWriter::<io::Stdout>::replace_patch("/status", "error"),
                        JsonlWriter::<io::Stdout>::add_patch("/error", &error_output),
                    ])
                    .ok();
                Err(ExitCode::RuntimeError)
            }
        }
    } else {
        // Script mode
        let initial = JsonlScriptInitial {
            output_type: "script",
            status: "pending",
            value: None,
            duration_ms: None,
            console: vec![],
        };
        writer.write_initial(&initial).ok();
        writer
            .write_patches(&[JsonlWriter::<io::Stdout>::replace_patch("/status", "running")])
            .ok();

        let start = Instant::now();
        match runner.run_script(vm) {
            Ok(result) => {
                let duration_ms = start.elapsed().as_millis() as u64;
                let console = external::disable_console_capture();

                // Emit console entries
                for entry in &console {
                    writer
                        .write_patches(&[JsonlWriter::<io::Stdout>::add_patch("/console/-", entry)])
                        .ok();
                }

                writer
                    .write_patches(&[
                        JsonlWriter::<io::Stdout>::replace_patch("/status", "complete"),
                        JsonlWriter::<io::Stdout>::replace_patch("/value", result.to_string()),
                        JsonlWriter::<io::Stdout>::replace_patch("/duration_ms", duration_ms),
                    ])
                    .ok();
                Ok(())
            }
            Err(e) => {
                let _ = external::disable_console_capture();
                let santa_error = SantaError::Runtime(e);
                let error_output = format_error_json(&santa_error);
                writer
                    .write_patches(&[
                        JsonlWriter::<io::Stdout>::replace_patch("/status", "error"),
                        JsonlWriter::<io::Stdout>::add_patch("/error", &error_output),
                    ])
                    .ok();
                Err(ExitCode::RuntimeError)
            }
        }
    }
}

fn handle_error(error: SantaError, output_mode: OutputMode) -> Result<(), ExitCode> {
    match output_mode {
        OutputMode::Text => {
            eprintln!("{}", error);
            Err(ExitCode::RuntimeError)
        }
        OutputMode::Json => {
            let _ = external::disable_console_capture();
            let json = serde_json::to_string(&format_error_json(&error)).unwrap();
            println!("{}", json);
            Err(ExitCode::RuntimeError)
        }
        OutputMode::Jsonl => {
            let _ = external::disable_console_capture();
            // For parse errors in JSONL, we emit a minimal initial state then error
            let mut writer = JsonlWriter::new(io::stdout());
            let initial = JsonlScriptInitial {
                output_type: "script",
                status: "pending",
                value: None,
                duration_ms: None,
                console: vec![],
            };
            writer.write_initial(&initial).ok();
            writer
                .write_patches(&[JsonlWriter::<io::Stdout>::replace_patch("/status", "running")])
                .ok();
            let error_output = format_error_json(&error);
            writer
                .write_patches(&[
                    JsonlWriter::<io::Stdout>::replace_patch("/status", "error"),
                    JsonlWriter::<io::Stdout>::add_patch("/error", &error_output),
                ])
                .ok();
            Err(ExitCode::RuntimeError)
        }
    }
}

fn run_tests_from_source(
    source: &str,
    source_path: Option<&str>,
    include_slow: bool,
    output_mode: OutputMode,
) -> Result<(), ExitCode> {
    // Enable console capture for JSON/JSONL modes
    if output_mode != OutputMode::Text {
        external::enable_console_capture();
    }

    let session_token = env::var("SANTA_CLI_SESSION_TOKEN").ok();
    let script_dir = source_path.and_then(|p| Path::new(p).parent().map(|d| d.to_path_buf()));

    // Lex and parse
    let mut lexer = Lexer::new(source);
    let tokens = match lexer.tokenize() {
        Ok(t) => t,
        Err(e) => {
            let santa_error = SantaError::Lex(e);
            return handle_test_error(santa_error, output_mode);
        }
    };

    let mut parser = Parser::new(tokens);
    let program = match parser.parse_program() {
        Ok(p) => p,
        Err(e) => {
            let santa_error = SantaError::Parse(e);
            return handle_test_error(santa_error, output_mode);
        }
    };

    // Collect test info (including @slow tests) for JSON output
    let test_infos: Vec<(usize, bool)> = program
        .sections
        .iter()
        .filter_map(|s| match s {
            Section::Test { attributes, .. } => {
                let is_slow = attributes.iter().any(|a| a.name == "slow");
                Some(is_slow)
            }
            _ => None,
        })
        .enumerate()
        .collect();

    // Determine which parts the solution defines
    let (has_part_one, has_part_two) = is_solution_source(source);

    let mut runner = AocRunner::new(program);

    // Create VM factory for tests
    let vm_factory = || create_vm(session_token.as_deref(), script_dir.clone());

    match output_mode {
        OutputMode::Text => run_tests_text(&mut runner, &vm_factory, include_slow),
        OutputMode::Json => run_tests_json(
            &mut runner,
            &vm_factory,
            include_slow,
            &test_infos,
            has_part_one,
            has_part_two,
        ),
        OutputMode::Jsonl => run_tests_jsonl(
            &mut runner,
            &vm_factory,
            include_slow,
            &test_infos,
            has_part_one,
            has_part_two,
        ),
    }
}

fn run_tests_text(runner: &mut AocRunner, vm_factory: &dyn Fn() -> VM, include_slow: bool) -> Result<(), ExitCode> {
    match runner.run_tests(vm_factory, include_slow) {
        Ok(test_results) => {
            let mut all_passed = true;

            for test_result in &test_results {
                println!("\x1b[4mTestcase #{}\x1b[0m", test_result.test_index + 1);

                if let Some(passed) = test_result.part_one_passed {
                    let actual = test_result
                        .part_one_actual
                        .as_ref()
                        .map(|v| v.to_string())
                        .unwrap_or_else(|| "N/A".to_string());
                    if passed {
                        println!("Part 1: {} \x1b[32m✔\x1b[0m", actual);
                    } else {
                        let expected = test_result
                            .part_one_expected
                            .as_ref()
                            .map(|v| v.to_string())
                            .unwrap_or_else(|| "N/A".to_string());
                        println!("Part 1: {} \x1b[31m✘ (Expected: {})\x1b[0m", actual, expected);
                        all_passed = false;
                    }
                }

                if let Some(passed) = test_result.part_two_passed {
                    let actual = test_result
                        .part_two_actual
                        .as_ref()
                        .map(|v| v.to_string())
                        .unwrap_or_else(|| "N/A".to_string());
                    if passed {
                        println!("Part 2: {} \x1b[32m✔\x1b[0m", actual);
                    } else {
                        let expected = test_result
                            .part_two_expected
                            .as_ref()
                            .map(|v| v.to_string())
                            .unwrap_or_else(|| "N/A".to_string());
                        println!("Part 2: {} \x1b[31m✘ (Expected: {})\x1b[0m", actual, expected);
                        all_passed = false;
                    }
                }

                println!();
            }

            if all_passed { Ok(()) } else { Err(ExitCode::TestFailure) }
        }
        Err(e) => {
            eprintln!("{}", e);
            Err(ExitCode::RuntimeError)
        }
    }
}

fn run_tests_json(
    runner: &mut AocRunner,
    vm_factory: &dyn Fn() -> VM,
    include_slow: bool,
    test_infos: &[(usize, bool)], // (index, is_slow)
    has_part_one: bool,
    has_part_two: bool,
) -> Result<(), ExitCode> {
    match runner.run_tests(vm_factory, include_slow) {
        Ok(test_results) => {
            let console = external::disable_console_capture();

            // Build collected test info that includes skipped tests
            let mut collected: Vec<CollectedTestInfo> = Vec::new();
            let mut result_iter = test_results.iter();

            for (index, is_slow) in test_infos {
                let skipped = *is_slow && !include_slow;
                if skipped {
                    collected.push(CollectedTestInfo {
                        index: *index,
                        slow: *is_slow,
                        skipped: true,
                        result: None,
                    });
                } else if let Some(result) = result_iter.next() {
                    collected.push(CollectedTestInfo {
                        index: *index,
                        slow: *is_slow,
                        skipped: false,
                        result: Some(result.clone()),
                    });
                }
            }

            let json = format_test_json(&collected, has_part_one, has_part_two, console);
            println!("{}", json);

            // Determine exit code
            let has_failures = collected.iter().any(|info| {
                if info.skipped {
                    return false;
                }
                if let Some(ref result) = info.result {
                    let p1_failed = result.part_one_passed == Some(false);
                    let p2_failed = result.part_two_passed == Some(false);
                    p1_failed || p2_failed
                } else {
                    false
                }
            });

            if has_failures {
                Err(ExitCode::TestFailure)
            } else {
                Ok(())
            }
        }
        Err(e) => {
            let _ = external::disable_console_capture();
            let santa_error = SantaError::Runtime(e);
            let json = serde_json::to_string(&format_error_json(&santa_error)).unwrap();
            println!("{}", json);
            Err(ExitCode::RuntimeError)
        }
    }
}

fn run_tests_jsonl(
    runner: &mut AocRunner,
    vm_factory: &dyn Fn() -> VM,
    include_slow: bool,
    test_infos: &[(usize, bool)], // (index, is_slow)
    has_part_one: bool,
    has_part_two: bool,
) -> Result<(), ExitCode> {
    let mut writer = JsonlWriter::new(io::stdout());
    let total = test_infos.len() as u32;

    // Emit initial state
    let initial_tests: Vec<JsonlTestCaseInitial> = test_infos
        .iter()
        .map(|(index, is_slow)| JsonlTestCaseInitial {
            index: (*index + 1) as u32,
            slow: *is_slow,
            status: "pending",
            part_one: None,
            part_two: None,
        })
        .collect();

    let initial = JsonlTestInitial {
        output_type: "test",
        status: "pending",
        success: None,
        summary: TestSummary {
            total,
            passed: 0,
            failed: 0,
            skipped: 0,
        },
        tests: initial_tests,
        console: vec![],
    };
    writer.write_initial(&initial).ok();
    writer
        .write_patches(&[JsonlWriter::<io::Stdout>::replace_patch("/status", "running")])
        .ok();

    match runner.run_tests(vm_factory, include_slow) {
        Ok(test_results) => {
            let console = external::disable_console_capture();

            // Emit console entries
            for entry in &console {
                writer
                    .write_patches(&[JsonlWriter::<io::Stdout>::add_patch("/console/-", entry)])
                    .ok();
            }

            let mut passed = 0u32;
            let mut failed = 0u32;
            let mut skipped = 0u32;
            let mut result_iter = test_results.iter();

            for (i, (_, is_slow)) in test_infos.iter().enumerate() {
                let path_prefix = format!("/tests/{}", i);
                let test_skipped = *is_slow && !include_slow;

                if test_skipped {
                    skipped += 1;
                    writer
                        .write_patches(&[
                            JsonlWriter::<io::Stdout>::replace_patch(&format!("{}/status", path_prefix), "skipped"),
                            JsonlWriter::<io::Stdout>::replace_patch("/summary/skipped", skipped),
                        ])
                        .ok();
                } else if let Some(result) = result_iter.next() {
                    // Emit running
                    writer
                        .write_patches(&[JsonlWriter::<io::Stdout>::replace_patch(
                            &format!("{}/status", path_prefix),
                            "running",
                        )])
                        .ok();

                    // Determine result
                    let part_one_passed = result.part_one_passed.unwrap_or(true);
                    let part_two_passed = result.part_two_passed.unwrap_or(true);
                    let all_passed = part_one_passed && part_two_passed;

                    if all_passed {
                        passed += 1;
                    } else {
                        failed += 1;
                    }

                    // Build patches
                    let mut patches = vec![JsonlWriter::<io::Stdout>::replace_patch(
                        &format!("{}/status", path_prefix),
                        "complete",
                    )];

                    if has_part_one && let Some(p1_passed) = result.part_one_passed {
                        patches.push(JsonlWriter::<io::Stdout>::replace_patch(
                            &format!("{}/part_one", path_prefix),
                            JsonTestPartResult {
                                passed: p1_passed,
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
                            },
                        ));
                    }

                    if has_part_two && let Some(p2_passed) = result.part_two_passed {
                        patches.push(JsonlWriter::<io::Stdout>::replace_patch(
                            &format!("{}/part_two", path_prefix),
                            JsonTestPartResult {
                                passed: p2_passed,
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
                            },
                        ));
                    }

                    if all_passed {
                        patches.push(JsonlWriter::<io::Stdout>::replace_patch("/summary/passed", passed));
                    } else {
                        patches.push(JsonlWriter::<io::Stdout>::replace_patch("/summary/failed", failed));
                    }

                    writer.write_patches(&patches).ok();
                }
            }

            // Emit completion
            let success = failed == 0;
            writer
                .write_patches(&[
                    JsonlWriter::<io::Stdout>::replace_patch("/status", "complete"),
                    JsonlWriter::<io::Stdout>::replace_patch("/success", success),
                ])
                .ok();

            if !success { Err(ExitCode::TestFailure) } else { Ok(()) }
        }
        Err(e) => {
            let _ = external::disable_console_capture();
            let santa_error = SantaError::Runtime(e);
            let error_output = format_error_json(&santa_error);
            writer
                .write_patches(&[
                    JsonlWriter::<io::Stdout>::replace_patch("/status", "error"),
                    JsonlWriter::<io::Stdout>::add_patch("/error", &error_output),
                ])
                .ok();
            Err(ExitCode::RuntimeError)
        }
    }
}

fn handle_test_error(error: SantaError, output_mode: OutputMode) -> Result<(), ExitCode> {
    match output_mode {
        OutputMode::Text => {
            eprintln!("{}", error);
            Err(ExitCode::RuntimeError)
        }
        OutputMode::Json => {
            let _ = external::disable_console_capture();
            let json = serde_json::to_string(&format_error_json(&error)).unwrap();
            println!("{}", json);
            Err(ExitCode::RuntimeError)
        }
        OutputMode::Jsonl => {
            let _ = external::disable_console_capture();
            let mut writer = JsonlWriter::new(io::stdout());
            let initial = JsonlTestInitial {
                output_type: "test",
                status: "pending",
                success: None,
                summary: TestSummary {
                    total: 0,
                    passed: 0,
                    failed: 0,
                    skipped: 0,
                },
                tests: vec![],
                console: vec![],
            };
            writer.write_initial(&initial).ok();
            writer
                .write_patches(&[JsonlWriter::<io::Stdout>::replace_patch("/status", "running")])
                .ok();
            let error_output = format_error_json(&error);
            writer
                .write_patches(&[
                    JsonlWriter::<io::Stdout>::replace_patch("/status", "error"),
                    JsonlWriter::<io::Stdout>::add_patch("/error", &error_output),
                ])
                .ok();
            Err(ExitCode::RuntimeError)
        }
    }
}

fn run_repl() -> Result<(), ExitCode> {
    println!(
        "   ,--.\n  ()   \\\n   /    \\\n _/______\\_\n(__________)\n(/  @  @  \\)\n(`._,()._,')  Santa REPL\n(  `-'`-'  )\n \\        /\n  \\,,,,,,/\n"
    );

    let session_token = env::var("SANTA_CLI_SESSION_TOKEN").ok();
    let mut vm = create_vm(session_token.as_deref(), None);
    let mut editor = DefaultEditor::new().map_err(|e| {
        eprintln!("Failed to initialize REPL: {}", e);
        ExitCode::RuntimeError
    })?;

    loop {
        match editor.readline(">> ") {
            Ok(line) => {
                if line.trim().is_empty() {
                    continue;
                }

                editor.add_history_entry(&line).ok();

                // Lex and parse
                let mut lexer = Lexer::new(&line);
                let tokens = match lexer.tokenize() {
                    Ok(t) => t,
                    Err(e) => {
                        eprintln!("{}", SantaError::Lex(e));
                        continue;
                    }
                };

                let mut parser = Parser::new(tokens);
                match parser.parse_program() {
                    Ok(program) => {
                        let runner = AocRunner::new(program);
                        match runner.run_script(&mut vm) {
                            Ok(result) => {
                                if result != Value::Nil {
                                    println!("{}", result);
                                }
                            }
                            Err(e) => eprintln!("{}", e),
                        }
                    }
                    Err(e) => eprintln!("{}", SantaError::Parse(e)),
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                println!("Goodbye!");
                break;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return Err(ExitCode::RuntimeError);
            }
        }
    }

    Ok(())
}

fn create_vm(session_token: Option<&str>, script_dir: Option<PathBuf>) -> VM {
    let mut vm = VM::new();

    let session_token_owned = session_token.map(|s| s.to_string());

    vm.register_external("puts", move |args, _| external::builtin_puts(args));

    let session_token_for_read = session_token_owned.clone();
    vm.register_external("read", move |args, _| {
        if args.len() != 1 {
            return Err(RuntimeError::new(
                format!("read expects 1 argument, got {}", args.len()),
                0,
            ));
        }
        match &args[0] {
            Value::String(path) => {
                external::builtin_read(path, session_token_for_read.as_deref(), script_dir.as_deref())
            }
            _ => Err(RuntimeError::new(
                format!("read expects String, got {}", args[0].type_name()),
                0,
            )),
        }
    });

    vm.register_external("env", move |args, vm| {
        if !args.is_empty() {
            return Err(RuntimeError::new(
                format!("env expects 0 arguments, got {}", args.len()),
                0,
            ));
        }
        let vars: Vec<_> = vm.globals().iter().map(|(k, v)| (k.clone(), v.clone())).collect();
        external::builtin_env(&vars)
    });

    vm
}

#[cfg(feature = "profile")]
fn start_profiler() -> pprof::ProfilerGuard<'static> {
    pprof::ProfilerGuardBuilder::default()
        .frequency(1000)
        .blocklist(&["libc", "libgcc", "pthread", "vdso"])
        .build()
        .unwrap()
}

#[cfg(feature = "profile")]
fn stop_profiler(guard: pprof::ProfilerGuard<'static>) {
    if let Ok(report) = guard.report().build() {
        // Write flamegraph
        let flamegraph_file = File::create("flamegraph.svg").expect("Failed to create flamegraph.svg");
        report.flamegraph(flamegraph_file).expect("Failed to write flamegraph");
        eprintln!("Wrote flamegraph.svg");

        // Write protobuf profile
        let profile_file = File::create("profile.pb").expect("Failed to create profile.pb");
        let profile = report.pprof().expect("Failed to generate pprof");
        use pprof::protos::Message;
        profile
            .write_to_writer(&mut std::io::BufWriter::new(profile_file))
            .expect("Failed to write profile.pb");
        eprintln!("Wrote profile.pb");
    }
}

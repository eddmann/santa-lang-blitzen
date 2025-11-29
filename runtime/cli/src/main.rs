mod external;

use lang::{
    error::SantaError,
    lexer::Lexer,
    parser::Parser,
    runner::AocRunner,
    vm::{RuntimeError, Value, VM},
};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_help();
        process::exit(1);
    }

    let result = match args[1].as_str() {
        "-h" | "--help" => {
            print_help();
            process::exit(0);
        }
        "-r" | "--repl" => run_repl(),
        "-t" | "--test" => {
            if args.len() < 3 {
                eprintln!("Error: --test requires a script file");
                process::exit(1);
            }
            run_tests(&args[2])
        }
        script => run_script(script),
    };

    match result {
        Ok(()) => process::exit(0),
        Err(ExitCode::ArgumentError) => process::exit(1),
        Err(ExitCode::RuntimeError) => process::exit(2),
        Err(ExitCode::TestFailure) => process::exit(3),
    }
}

enum ExitCode {
    ArgumentError,
    RuntimeError,
    TestFailure,
}

fn print_help() {
    println!("Santa Language CLI - Blitzen VM");
    println!();
    println!("USAGE:");
    println!("    santa-cli <SCRIPT>        Run solution file");
    println!("    santa-cli -t <SCRIPT>     Run test suite");
    println!("    santa-cli -r              Start REPL");
    println!("    santa-cli -h              Show this help");
    println!();
    println!("ENVIRONMENT:");
    println!("    SANTA_CLI_SESSION_TOKEN   AOC session token for aoc:// URLs");
}

fn run_script(path: &str) -> Result<(), ExitCode> {
    let source = fs::read_to_string(path).map_err(|e| {
        eprintln!("Error reading file '{}': {}", path, e);
        ExitCode::ArgumentError
    })?;

    let session_token = env::var("SANTA_CLI_SESSION_TOKEN").ok();
    let script_dir = Path::new(path).parent().map(|p| p.to_path_buf());

    // Lex and parse
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize().map_err(|e| {
        eprintln!("{}", SantaError::Lex(e));
        ExitCode::RuntimeError
    })?;

    let mut parser = Parser::new(tokens);
    let program = parser.parse_program().map_err(|e| {
        eprintln!("{}", SantaError::Parse(e));
        ExitCode::RuntimeError
    })?;

    let mut vm = create_vm(session_token.as_deref(), script_dir);
    let mut runner = AocRunner::new(program);

    // Check if this is a script (no AOC sections) or an AOC solution
    if runner.is_script_mode() {
        // Run as script
        match runner.run_script(&mut vm) {
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
        // Run as AOC solution
        match runner.run_solution(&mut vm) {
            Ok(result) => {
                if let Some((value, duration)) = result.part_one {
                    println!("Part 1: {} ({}ms)", value, duration);
                }
                if let Some((value, duration)) = result.part_two {
                    println!("Part 2: {} ({}ms)", value, duration);
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

fn run_tests(path: &str) -> Result<(), ExitCode> {
    let source = fs::read_to_string(path).map_err(|e| {
        eprintln!("Error reading file '{}': {}", path, e);
        ExitCode::ArgumentError
    })?;

    let session_token = env::var("SANTA_CLI_SESSION_TOKEN").ok();
    let script_dir = Path::new(path).parent().map(|p| p.to_path_buf());

    // Lex and parse
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize().map_err(|e| {
        eprintln!("{}", SantaError::Lex(e));
        ExitCode::RuntimeError
    })?;

    let mut parser = Parser::new(tokens);
    let program = parser.parse_program().map_err(|e| {
        eprintln!("{}", SantaError::Parse(e));
        ExitCode::RuntimeError
    })?;

    let mut runner = AocRunner::new(program);

    // Create VM factory for tests
    let vm_factory = || create_vm(session_token.as_deref(), script_dir.clone());

    match runner.run_tests(&vm_factory) {
        Ok(test_results) => {
            let mut all_passed = true;

            for test_result in &test_results {
                println!("Test #{}:", test_result.test_index + 1);

                if let Some(passed) = test_result.part_one_passed {
                    let status = if passed { "✓" } else { "✗" };
                    let expected = test_result
                        .part_one_expected
                        .as_ref()
                        .map(|v| v.to_string())
                        .unwrap_or_else(|| "N/A".to_string());
                    let actual = test_result
                        .part_one_actual
                        .as_ref()
                        .map(|v| v.to_string())
                        .unwrap_or_else(|| "N/A".to_string());
                    println!(
                        "  Part 1: {} (expected: {}, got: {})",
                        status, expected, actual
                    );
                    if !passed {
                        all_passed = false;
                    }
                }

                if let Some(passed) = test_result.part_two_passed {
                    let status = if passed { "✓" } else { "✗" };
                    let expected = test_result
                        .part_two_expected
                        .as_ref()
                        .map(|v| v.to_string())
                        .unwrap_or_else(|| "N/A".to_string());
                    let actual = test_result
                        .part_two_actual
                        .as_ref()
                        .map(|v| v.to_string())
                        .unwrap_or_else(|| "N/A".to_string());
                    println!(
                        "  Part 2: {} (expected: {}, got: {})",
                        status, expected, actual
                    );
                    if !passed {
                        all_passed = false;
                    }
                }
            }

            if all_passed {
                println!("\nAll tests passed!");
                Ok(())
            } else {
                println!("\nSome tests failed!");
                Err(ExitCode::TestFailure)
            }
        }
        Err(e) => {
            eprintln!("{}", e);
            Err(ExitCode::RuntimeError)
        }
    }
}

fn run_repl() -> Result<(), ExitCode> {
    println!("Santa Language REPL - Blitzen VM");
    println!("Type 'env()' to see environment, Ctrl+C or Ctrl+D to exit");
    println!();

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
            Value::String(path) => external::builtin_read(
                path,
                session_token_for_read.as_deref(),
                script_dir.as_deref(),
            ),
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
        let vars: Vec<_> = vm
            .globals()
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        external::builtin_env(&vars)
    });

    vm
}

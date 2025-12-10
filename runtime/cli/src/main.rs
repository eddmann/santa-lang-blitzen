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
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process;

#[cfg(feature = "profile")]
use std::fs::File;

fn main() {
    let args: Vec<String> = env::args().collect();

    // Parse flags
    let mut test_mode = false;
    let mut include_slow = false;
    let mut eval_script: Option<String> = None;
    let mut script_path: Option<String> = None;
    #[allow(unused_variables, unused_assignments)]
    let mut profile_mode = false;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-h" | "--help" => {
                print_help();
                process::exit(0);
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
    let _guard = if profile_mode {
        Some(start_profiler())
    } else {
        None
    };

    let result = if test_mode {
        run_tests_from_source(&source, source_path.as_deref(), include_slow)
    } else {
        run_script_from_source(&source, source_path.as_deref())
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
    println!("Santa Language CLI - Blitzen VM");
    println!();
    println!("USAGE:");
    println!("    santa-cli <SCRIPT>        Run solution file");
    println!("    santa-cli -e <CODE>       Evaluate inline script");
    println!("    santa-cli -t <SCRIPT>     Run test suite");
    println!("    santa-cli -t -s <SCRIPT>  Run test suite including @slow tests");
    println!("    santa-cli -p <SCRIPT>     Run with CPU profiling (requires --features profile)");
    println!("    santa-cli -r              Start REPL");
    println!("    santa-cli -h              Show this help");
    println!("    cat file | santa-cli      Read script from stdin");
    println!();
    println!("OPTIONS:");
    println!("    -e, --eval                Evaluate inline script");
    println!("    -s, --slow                Include @slow tests (use with -t)");
    println!("    -p, --profile             Enable CPU profiling (outputs flamegraph.svg)");
    println!();
    println!("ENVIRONMENT:");
    println!("    SANTA_CLI_SESSION_TOKEN   AOC session token for aoc:// URLs");
}

fn run_script_from_source(source: &str, source_path: Option<&str>) -> Result<(), ExitCode> {
    let session_token = env::var("SANTA_CLI_SESSION_TOKEN").ok();
    let script_dir = source_path.and_then(|p| Path::new(p).parent().map(|d| d.to_path_buf()));

    // Lex and parse
    let mut lexer = Lexer::new(source);
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
                    println!(
                        "Part 1: \x1b[32m{}\x1b[0m \x1b[90m{}ms\x1b[0m",
                        value, duration
                    );
                }
                if let Some((value, duration)) = result.part_two {
                    println!(
                        "Part 2: \x1b[32m{}\x1b[0m \x1b[90m{}ms\x1b[0m",
                        value, duration
                    );
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

fn run_tests_from_source(
    source: &str,
    source_path: Option<&str>,
    include_slow: bool,
) -> Result<(), ExitCode> {
    let session_token = env::var("SANTA_CLI_SESSION_TOKEN").ok();
    let script_dir = source_path.and_then(|p| Path::new(p).parent().map(|d| d.to_path_buf()));

    // Lex and parse
    let mut lexer = Lexer::new(source);
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

    match runner.run_tests(&vm_factory, include_slow) {
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
                        println!(
                            "Part 1: {} \x1b[31m✘ (Expected: {})\x1b[0m",
                            actual, expected
                        );
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
                        println!(
                            "Part 2: {} \x1b[31m✘ (Expected: {})\x1b[0m",
                            actual, expected
                        );
                        all_passed = false;
                    }
                }

                println!();
            }

            if all_passed {
                Ok(())
            } else {
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
        let flamegraph_file =
            File::create("flamegraph.svg").expect("Failed to create flamegraph.svg");
        report
            .flamegraph(flamegraph_file)
            .expect("Failed to write flamegraph");
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

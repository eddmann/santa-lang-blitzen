use assert_cmd::cargo::cargo_bin_cmd;
use assert_cmd::Command;
use predicates::prelude::*;
use std::io::Write;
use tempfile::NamedTempFile;

fn santa_cli() -> Command {
    cargo_bin_cmd!("santa-cli")
}

#[test]
fn cli_help() {
    santa_cli()
        .arg("-h")
        .assert()
        .success()
        .stdout(predicate::str::contains("Santa Language CLI"))
        .stdout(predicate::str::contains("USAGE:"));
}

#[test]
fn cli_run_script_simple() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "1 + 2 * 3").unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("7"));
}

#[test]
fn cli_run_script_with_pipeline() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "[1, 2, 3, 4, 5] |> map(|x| x * 2) |> sum").unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("30"));
}

#[test]
fn cli_run_aoc_solution() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, r#"
        input: "10\n20\n30"
        part_one: input |> lines |> map(|x| int(x)) |> sum
        part_two: input |> lines |> map(|x| int(x) * 2) |> sum
    "#).unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("Part 1: 60"))
        .stdout(predicate::str::contains("Part 2: 120"));
}

#[test]
fn cli_run_tests_passing() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, r#"
        input: "1\n2\n3"
        part_one: input |> lines |> map(|x| int(x)) |> sum

        test: {{
            input: "1\n2\n3"
            part_one: 6
        }}
    "#).unwrap();

    santa_cli()
        .arg("-t")
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("All tests passed!"));
}

#[test]
fn cli_run_tests_failing() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, r#"
        input: "1\n2\n3"
        part_one: input |> lines |> map(|x| int(x)) |> sum

        test: {{
            input: "1\n2\n3"
            part_one: 100
        }}
    "#).unwrap();

    santa_cli()
        .arg("-t")
        .arg(file.path())
        .assert()
        .code(3)  // Test failure exit code
        .stdout(predicate::str::contains("Some tests failed!"));
}

#[test]
fn cli_error_file_not_found() {
    santa_cli()
        .arg("/nonexistent/file.santa")
        .assert()
        .code(1)  // Argument error
        .stderr(predicate::str::contains("Error reading file"));
}

#[test]
fn cli_error_parse_error() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "let x = ").unwrap();  // Incomplete expression

    santa_cli()
        .arg(file.path())
        .assert()
        .code(2)  // Runtime/parse error
        .stderr(predicate::str::contains("error"));
}

#[test]
fn cli_error_runtime_error() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "1 / 0").unwrap();  // Division by zero

    santa_cli()
        .arg(file.path())
        .assert()
        .code(2)  // Runtime error
        .stderr(predicate::str::contains("Division by zero"));
}

#[test]
fn cli_puts_external_function() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, r#"puts("Hello", "World")"#).unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        // puts prints values with their Display format (strings include quotes)
        .stdout(predicate::str::contains(r#""Hello" "World""#));
}

#[test]
fn cli_read_file() {
    let mut data_file = NamedTempFile::new().unwrap();
    writeln!(data_file, "test data").unwrap();

    let mut script_file = NamedTempFile::new().unwrap();
    writeln!(script_file, r#"read("{}")"#, data_file.path().display()).unwrap();

    santa_cli()
        .arg(script_file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("test data"));
}

#[test]
fn cli_no_args_shows_help() {
    santa_cli()
        .assert()
        .code(1)
        .stdout(predicate::str::contains("USAGE:"));
}

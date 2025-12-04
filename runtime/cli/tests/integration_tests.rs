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
    writeln!(
        file,
        r#"
        input: "10\n20\n30"
        part_one: input |> lines |> map(|x| int(x)) |> sum
        part_two: input |> lines |> map(|x| int(x) * 2) |> sum
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("Part 1: \x1b[32m60\x1b[0m"))
        .stdout(predicate::str::contains("Part 2: \x1b[32m120\x1b[0m"));
}

#[test]
fn cli_run_tests_passing() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        input: "1\n2\n3"
        part_one: input |> lines |> map(|x| int(x)) |> sum

        test: {{
            input: "1\n2\n3"
            part_one: 6
        }}
    "#
    )
    .unwrap();

    santa_cli()
        .arg("-t")
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("Part 1: 6 \x1b[32m✔\x1b[0m"));
}

#[test]
fn cli_run_tests_failing() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        input: "1\n2\n3"
        part_one: input |> lines |> map(|x| int(x)) |> sum

        test: {{
            input: "1\n2\n3"
            part_one: 100
        }}
    "#
    )
    .unwrap();

    santa_cli()
        .arg("-t")
        .arg(file.path())
        .assert()
        .code(3) // Test failure exit code
        .stdout(predicate::str::contains("Part 1: 6 \x1b[31m✘ (Expected: 100)\x1b[0m"));
}

#[test]
fn cli_error_file_not_found() {
    santa_cli()
        .arg("/nonexistent/file.santa")
        .assert()
        .code(1) // Argument error
        .stderr(predicate::str::contains("Error reading file"));
}

#[test]
fn cli_error_parse_error() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "let x = ").unwrap(); // Incomplete expression

    santa_cli()
        .arg(file.path())
        .assert()
        .code(2) // Runtime/parse error
        .stderr(predicate::str::contains("error"));
}

#[test]
fn cli_error_runtime_error() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "1 / 0").unwrap(); // Division by zero

    santa_cli()
        .arg(file.path())
        .assert()
        .code(2) // Runtime error
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
fn cli_help_flag() {
    santa_cli()
        .arg("-h")
        .assert()
        .success()
        .stdout(predicate::str::contains("USAGE:"));
}

#[test]
fn cli_stdin_empty() {
    // Empty stdin should run empty program (returns nil, no output)
    santa_cli()
        .write_stdin("")
        .assert()
        .success();
}

#[test]
fn cli_eval_simple_expression() {
    santa_cli()
        .arg("-e")
        .arg("1 + 2")
        .assert()
        .success()
        .stdout("3\n");
}

#[test]
fn cli_eval_complex_expression() {
    santa_cli()
        .arg("-e")
        .arg("map(|x| x * 2, [1, 2, 3])")
        .assert()
        .success()
        .stdout("[2, 4, 6]\n");
}

#[test]
fn cli_eval_aoc_solution() {
    santa_cli()
        .arg("-e")
        .arg("part_one: { 42 }")
        .assert()
        .success()
        .stdout(predicate::str::contains("Part 1: \x1b[32m42\x1b[0m"));
}

#[test]
fn cli_stdin_simple_expression() {
    santa_cli()
        .write_stdin("1 + 2")
        .assert()
        .success()
        .stdout("3\n");
}

#[test]
fn cli_stdin_aoc_solution() {
    santa_cli()
        .write_stdin("part_one: { 42 }")
        .assert()
        .success()
        .stdout(predicate::str::contains("Part 1: \x1b[32m42\x1b[0m"));
}

// ============================================================================
// LANG.txt Appendix D Integration Tests
// ============================================================================

// Example 1: Fibonacci Sequence (requires memoize - skipped)
// The memoize builtin is not yet implemented. Once implemented, enable this test:
// ```santa
// let fib = memoize |n| {
//   if n > 1 { fib(n - 1) + fib(n - 2) } else { n }
// };
// fib(50)
// ```

// Alternative: Non-memoized fibonacci for small n (to test recursion)
#[test]
fn integration_fibonacci_recursive() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let fib = |n| {{
            if n > 1 {{ fib(n - 1) + fib(n - 2) }}
            else {{ n }}
        }};
        fib(20)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("6765"));
}

// Example 2: AOC 2022 Day 1 pattern (simplified - no read() for aoc://)
#[test]
fn integration_aoc_day1_style() {
    let mut file = NamedTempFile::new().unwrap();
    // Input has 3 groups: (1000+2000+3000=6000), (4000), (5000+6000=11000)
    // Part 1: max = 11000
    // Part 2: top 3 sums = 11000 + 6000 + 4000 = 21000
    writeln!(
        file,
        r#"
        input: "1000\n2000\n3000\n\n4000\n\n5000\n6000"

        let parse_inventories = split("\n\n") >> map(ints >> sum);

        part_one: {{
            parse_inventories(input) |> max;
        }}

        part_two: {{
            parse_inventories(input)
                |> sort(<)
                |> reverse
                |> take(3)
                |> sum;
        }}

        test: {{
            input: "1000\n2000\n3000\n\n4000\n\n5000\n6000"
            part_one: 11000
            part_two: 21000
        }}
    "#
    )
    .unwrap();

    santa_cli()
        .arg("-t")
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("Part 1: 11000 \x1b[32m✔\x1b[0m"))
        .stdout(predicate::str::contains("Part 2: 21000 \x1b[32m✔\x1b[0m"));
}

// Example 3: Word Frequency Counter (simplified - inline input)
#[test]
fn integration_word_frequency() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let text = "hello world hello santa hello world";

        text
            |> split(" ")
            |> fold(#{{}}) |freq, word| {{
                update_d(word, 0, _ + 1, freq)
            }}
            |> list
            |> sort(|[_, a], [_, b]| a > b)
            |> take(3)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        // "hello" appears 3 times, "world" 2 times
        .stdout(predicate::str::contains("hello"))
        .stdout(predicate::str::contains("3"));
}

// Example 4: Prime Numbers (lazy infinite sequences)
#[test]
fn integration_prime_numbers() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let is_prime = |n| {{
            if n < 2 {{ return false }};
            (2..n) |> all?(|d| n % d != 0)
        }};

        (1..) |> filter(is_prime) |> take(10) |> list
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        // First 10 primes: 2, 3, 5, 7, 11, 13, 17, 19, 23, 29
        .stdout(predicate::str::contains("2"))
        .stdout(predicate::str::contains("29"));
}

// Example 5: Recursive List Sum (pattern matching)
#[test]
fn integration_recursive_list_sum() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let sum_list = |list| match list {{
            [] {{ 0 }}
            [head, ..tail] {{ head + sum_list(tail) }}
        }};

        sum_list([1, 2, 3, 4, 5])
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("15"));
}

// Additional integration tests for LANG.txt compliance

#[test]
fn integration_lazy_range_unbounded() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        (1..) |> take(5) |> list
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("[1, 2, 3, 4, 5]"));
}

#[test]
fn integration_operator_as_function() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        reduce(+, [1, 2, 3, 4, 5])
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("15"));
}

#[test]
fn integration_function_composition() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let double = |x| x * 2;
        let add10 = |x| x + 10;
        let composed = double >> add10;
        composed(5)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("20"));
}

#[test]
fn integration_partial_application() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let add10 = _ + 10;
        add10(5)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("15"));
}

#[test]
fn integration_tail_call_optimization() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let sum_to = |n, acc| if n == 0 {{ acc }} else {{ sum_to(n - 1, acc + n) }};
        sum_to(10000, 0)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("50005000"));
}

#[test]
fn integration_pattern_destructuring() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let [a, b, ..rest] = [1, 2, 3, 4, 5];
        a + b + sum(rest)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("15"));
}

#[test]
fn integration_string_grapheme_indexing() {
    let mut file = NamedTempFile::new().unwrap();
    // Test grapheme cluster indexing per LANG.txt §3.3
    writeln!(
        file,
        r#"
        let s = "hello";
        s[0]
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("h"));
}

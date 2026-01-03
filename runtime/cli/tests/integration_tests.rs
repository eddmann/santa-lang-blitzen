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
        .stdout(predicate::str::contains("santa-lang CLI - Blitzen"))
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
        .stdout(predicate::str::contains(
            "Part 1: 6 \x1b[31m✘ (Expected: 100)\x1b[0m",
        ));
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
    santa_cli().write_stdin("").assert().success();
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

// =========================================================================
// Partial Application with Higher-Order Builtins
// Tests that partial application works with all higher-order builtins
// =========================================================================

#[test]
fn partial_application_with_count() {
    // count with a partially applied predicate
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let is_in_range = |lo, hi, x| x >= lo && x <= hi;
        let in_5_to_10 = is_in_range(5, 10);
        [3, 5, 7, 9, 11, 13] |> count(in_5_to_10)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("3")); // 5, 7, 9 are in range
}

#[test]
fn partial_application_with_any() {
    // any? with a partially applied predicate
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let greater_than = |threshold, x| x > threshold;
        let gt_10 = greater_than(10);
        [1, 5, 15, 3] |> any?(gt_10)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("true"));
}

#[test]
fn partial_application_with_all() {
    // all? with a partially applied predicate
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let less_than = |threshold, x| x < threshold;
        let lt_100 = less_than(100);
        [10, 20, 30, 40] |> all?(lt_100)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("true"));
}

#[test]
fn partial_application_with_find() {
    // find with a partially applied predicate
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let divisible_by = |d, n| n % d == 0;
        let div_by_3 = divisible_by(3);
        [4, 7, 9, 11, 15] |> find(div_by_3)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("9"));
}

#[test]
fn partial_application_with_flat_map() {
    // flat_map with a partially applied function
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let repeat_n = |n, x| 1..=n |> map(|_| x) |> list;
        let repeat_twice = repeat_n(2);
        [1, 2, 3] |> flat_map(repeat_twice)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("[1, 1, 2, 2, 3, 3]"));
}

#[test]
fn partial_application_with_filter_map() {
    // filter_map with a partially applied function
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let scale_if_positive = |factor, x| if x > 0 {{ x * factor }} else {{ nil }};
        let double_positive = scale_if_positive(2);
        [-1, 2, -3, 4, 0] |> filter_map(double_positive)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("[4, 8]"));
}

#[test]
fn partial_application_with_find_map() {
    // find_map with a partially applied function
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let try_divide = |divisor, n| if n % divisor == 0 {{ n / divisor }} else {{ nil }};
        let try_div_by_3 = try_divide(3);
        [4, 7, 9, 11] |> find_map(try_div_by_3)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("3")); // 9 / 3 = 3
}

#[test]
fn partial_application_with_reduce() {
    // reduce with a partially applied reducer
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let weighted_add = |weight, acc, x| acc + x * weight;
        let add_doubled = weighted_add(2);
        [1, 2, 3, 4] |> reduce(add_doubled)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("19")); // 1 + 2*2 + 3*2 + 4*2 = 1 + 4 + 6 + 8 = 19
}

#[test]
fn partial_application_with_fold_s() {
    // fold_s with a partially applied folder
    // fold_s(initial, folder, collection) -> Value
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let add_scaled = |scale, acc, x| acc + x * scale;
        let add_tripled = add_scaled(3);
        fold_s(0, add_tripled, [1, 2, 3])
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("18")); // 0 + 1*3 + 2*3 + 3*3 = 18
}

#[test]
fn partial_application_with_scan() {
    // scan with a partially applied folder
    // scan(initial, folder, collection) -> List
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let add_scaled = |scale, acc, x| acc + x * scale;
        let add_tripled = add_scaled(3);
        scan(0, add_tripled, [1, 2, 3])
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("[0, 3, 9, 18]")); // 0, 0+1*3=3, 3+2*3=9, 9+3*3=18
}

#[test]
fn partial_application_with_each() {
    // each with a partially applied side effect
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let mut total = 0;
        let add_to_total = |scale, x| {{ total = total + x * scale }};
        let add_doubled = add_to_total(2);
        [1, 2, 3] |> each(add_doubled);
        total
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("12")); // 1*2 + 2*2 + 3*2 = 12
}

#[test]
fn partial_application_with_update() {
    // update with a partially applied updater
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let scale_by = |factor, x| x * factor;
        let triple = scale_by(3);
        [10, 20, 30] |> update(1, triple)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("[10, 60, 30]"));
}

#[test]
fn partial_application_with_update_d() {
    // update_d with a partially applied updater
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let add_n = |n, x| x + n;
        let add_10 = add_n(10);
        #{{"a": 1, "b": 2}} |> update_d("c", 0, add_10)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("\"c\": 10")); // default 0 + 10 = 10
}

// Self-recursive closure tests

#[test]
fn self_recursive_closure_direct() {
    // Direct self-recursive closure: let f = |x| f(x)
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let factorial = |n| if n <= 1 {{ 1 }} else {{ n * factorial(n - 1) }};
        factorial(5)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("120"));
}

#[test]
fn self_recursive_closure_with_memoize() {
    // Self-recursive closure wrapped in memoize: let f = memoize |x| f(x)
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let fib = memoize |n| if n <= 1 {{ n }} else {{ fib(n - 1) + fib(n - 2) }};
        fib(10)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("55"));
}

#[test]
fn self_recursive_closure_with_memoize_large() {
    // Memoized recursive fibonacci should handle larger values efficiently
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let fib = memoize |n| if n <= 1 {{ n }} else {{ fib(n - 1) + fib(n - 2) }};
        fib(30)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("832040"));
}

#[test]
fn self_recursive_closure_with_memoize_multiple_args() {
    // Memoized recursive function with multiple arguments
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let binomial = memoize |n, k| {{
            if k == 0 || k == n {{ 1 }}
            else {{ binomial(n - 1, k - 1) + binomial(n - 1, k) }}
        }};
        binomial(10, 5)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("252")); // C(10,5) = 252
}

#[test]
fn self_recursive_closure_nested_in_call() {
    // Self-recursive closure inside another function call
    let mut file = NamedTempFile::new().unwrap();
    writeln!(
        file,
        r#"
        let apply_twice = |f, x| f(f(x));
        let countdown = memoize |n| if n <= 0 {{ 0 }} else {{ countdown(n - 1) }};
        apply_twice(countdown, 10)
    "#
    )
    .unwrap();

    santa_cli()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("0"));
}

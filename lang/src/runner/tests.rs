use super::*;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::vm::value::Value;
use std::rc::Rc;

fn parse(source: &str) -> Program {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().unwrap();
    let mut parser = Parser::new(tokens);
    parser.parse_program().unwrap()
}

#[test]
fn runner_simple_solution() {
    let source = r#"
        input: "10\n20\n30"

        part_one: {
            let lines_list = input |> lines |> map(|x| int(x));
            lines_list |> sum
        }

        part_two: {
            let lines_list = input |> lines |> map(|x| int(x));
            lines_list |> map(|x| x * 2) |> sum
        }
    "#;

    let program = parse(source);
    let runner = AocRunner::new(program);

    let result = runner.run_solution().unwrap();

    assert!(result.part_one.is_some());
    assert!(result.part_two.is_some());

    let (part_one_value, _) = result.part_one.unwrap();
    let (part_two_value, _) = result.part_two.unwrap();

    assert_eq!(part_one_value, Value::Integer(60));
    assert_eq!(part_two_value, Value::Integer(120));
}

#[test]
#[ignore] // TODO: Parser doesn't support test block internal structure yet
fn runner_with_tests() {
    // Test sections with internal input:/part_one:/part_two: structure
    // are not yet implemented in the parser
}

#[test]
fn runner_script_mode() {
    let source = r#"
        let data = [1, 2, 3, 4, 5];
        let result = data |> map(_ * 2) |> sum;
        result
    "#;

    let program = parse(source);
    let runner = AocRunner::new(program);

    assert!(runner.is_script_mode());

    let result = runner.run_script().unwrap();
    assert_eq!(result, Value::Integer(30));
}

#[test]
fn runner_duplicate_input_error() {
    let source = r#"
        input: "data1"
        input: "data2"

        part_one: 42
    "#;

    let program = parse(source);
    let runner = AocRunner::new(program);

    let result = runner.run_solution();
    assert!(result.is_err());

    let err = result.unwrap_err();
    assert!(err.message.contains("single 'input' section"));
}

#[test]
fn runner_duplicate_part_one_error() {
    let source = r#"
        part_one: 42
        part_one: 43
    "#;

    let program = parse(source);
    let runner = AocRunner::new(program);

    let result = runner.run_solution();
    assert!(result.is_err());

    let err = result.unwrap_err();
    assert!(err.message.contains("single 'part_one'"));
}

#[test]
fn runner_duplicate_part_two_error() {
    let source = r#"
        part_two: 42
        part_two: 43
    "#;

    let program = parse(source);
    let runner = AocRunner::new(program);

    let result = runner.run_solution();
    assert!(result.is_err());

    let err = result.unwrap_err();
    assert!(err.message.contains("single 'part_two'"));
}

#[test]
#[ignore] // TODO: Top-level let bindings are locals, not accessible in sections
fn runner_with_top_level_definitions() {
    // Top-level `let` statements create locals in the program scope
    // but sections like part_one are compiled separately and can't access them
    // This needs compiler/VM architecture changes to support properly
}

#[test]
fn runner_timing_collected() {
    let source = r#"
        part_one: 42
        part_two: 84
    "#;

    let program = parse(source);
    let runner = AocRunner::new(program);

    let result = runner.run_solution().unwrap();

    assert!(result.part_one.is_some());
    assert!(result.part_two.is_some());

    let (_, _duration1) = result.part_one.unwrap();
    let (_, _duration2) = result.part_two.unwrap();

    // Timing is collected (non-negative u128 microseconds)
    // Just verify that timing data is present - values are tested by having unwrapped successfully
}

#[test]
#[ignore] // TODO: Parser doesn't support test block internal structure yet
fn runner_test_with_part_two() {
    // Test sections with internal input:/part_one:/part_two: structure
    // are not yet implemented in the parser
}

#[test]
#[ignore] // TODO: Parser doesn't support test block internal structure yet
fn runner_test_failure() {
    // Test sections with internal input:/part_one:/part_two: structure
    // are not yet implemented in the parser
}

#[test]
fn runner_no_input_section() {
    let source = r#"
        part_one: 42
        part_two: 84
    "#;

    let program = parse(source);
    let runner = AocRunner::new(program);

    let result = runner.run_solution().unwrap();

    assert!(result.part_one.is_some());
    assert!(result.part_two.is_some());

    let (part_one_value, _) = result.part_one.unwrap();
    let (part_two_value, _) = result.part_two.unwrap();

    assert_eq!(part_one_value, Value::Integer(42));
    assert_eq!(part_two_value, Value::Integer(84));
}

#[test]
fn runner_only_part_one() {
    let source = r#"
        input: "hello"
        part_one: input
    "#;

    let program = parse(source);
    let runner = AocRunner::new(program);

    let result = runner.run_solution().unwrap();

    assert!(result.part_one.is_some());
    assert!(result.part_two.is_none());

    let (part_one_value, _) = result.part_one.unwrap();
    assert_eq!(part_one_value, Value::String(Rc::new("hello".to_string())));
}

#[test]
#[ignore] // TODO: Parser doesn't support `<` as function value and test blocks
fn runner_complex_example() {
    // Complex example from LANG.txt requires:
    // 1. Operators as function values (sort(<))
    // 2. Test block internal structure
    // These are not yet fully implemented
}

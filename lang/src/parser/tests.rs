use super::*;
use crate::lexer::Lexer;
use expect_test::{expect, Expect};

fn check(input: &str, expect: Expect) {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().unwrap();
    let mut parser = Parser::new(tokens);
    let result = parser.parse_expression();
    let output = match result {
        Ok(expr) => format_expr(&expr),
        Err(e) => format!("Error: {} at {}:{}", e.message, e.span.line, e.span.column),
    };
    expect.assert_eq(&output);
}

fn check_program(input: &str, expect: Expect) {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().unwrap();
    let mut parser = Parser::new(tokens);
    let result = parser.parse_program();
    let output = match result {
        Ok(program) => format_program(&program),
        Err(e) => format!("Error: {} at {}:{}", e.message, e.span.line, e.span.column),
    };
    expect.assert_eq(&output);
}

fn format_expr(expr: &SpannedExpr) -> String {
    format_expr_node(&expr.node)
}

fn format_expr_node(expr: &Expr) -> String {
    match expr {
        Expr::Integer(n) => n.to_string(),
        Expr::Decimal(n) => format!("{n:?}"),
        Expr::String(s) => format!("\"{s}\""),
        Expr::Boolean(b) => b.to_string(),
        Expr::Nil => "nil".to_string(),
        Expr::Identifier(name) => name.clone(),
        Expr::Placeholder => "_".to_string(),
        Expr::Prefix { op, right } => {
            format!("({op}{})", format_expr(right))
        }
        Expr::Infix { left, op, right } => {
            format!("({} {op} {})", format_expr(left), format_expr(right))
        }
        Expr::Index { collection, index } => {
            format!("({}[{}])", format_expr(collection), format_expr(index))
        }
        Expr::List(elements) => {
            let elems: Vec<_> = elements.iter().map(format_expr).collect();
            format!("[{}]", elems.join(", "))
        }
        Expr::Set(elements) => {
            let elems: Vec<_> = elements.iter().map(format_expr).collect();
            format!("{{{}}}", elems.join(", "))
        }
        Expr::Dict(entries) => {
            let pairs: Vec<_> = entries
                .iter()
                .map(|(k, v)| format!("{}: {}", format_expr(k), format_expr(v)))
                .collect();
            format!("#{{{}}}", pairs.join(", "))
        }
        Expr::Range { start, end, inclusive } => {
            let op = if *inclusive { "..=" } else { ".." };
            match end {
                Some(e) => format!("({}{op}{})", format_expr(start), format_expr(e)),
                None => format!("({}{op})", format_expr(start)),
            }
        }
        Expr::Function { params, body } => {
            let ps: Vec<_> = params
                .iter()
                .map(|p| match &p.name {
                    ParamKind::Identifier(n) => n.clone(),
                    ParamKind::Placeholder => "_".to_string(),
                    ParamKind::Rest(n) => format!("..{n}"),
                })
                .collect();
            format!("|{}| {}", ps.join(", "), format_expr(body))
        }
        Expr::Call { function, args } => {
            let arg_strs: Vec<_> = args.iter().map(format_expr).collect();
            format!("({}({}))", format_expr(function), arg_strs.join(", "))
        }
        Expr::InfixCall {
            function,
            left,
            right,
        } => {
            format!("({} `{function}` {})", format_expr(left), format_expr(right))
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let else_str = else_branch
                .as_ref()
                .map(|e| format!(" else {}", format_expr(e)))
                .unwrap_or_default();
            format!(
                "(if {} {}{})",
                format_expr(condition),
                format_expr(then_branch),
                else_str
            )
        }
        Expr::IfLet {
            pattern,
            value,
            then_branch,
            else_branch,
        } => {
            let else_str = else_branch
                .as_ref()
                .map(|e| format!(" else {}", format_expr(e)))
                .unwrap_or_default();
            format!(
                "(if let {} = {} {}{})",
                format_pattern(pattern),
                format_expr(value),
                format_expr(then_branch),
                else_str
            )
        }
        Expr::Match { subject, arms } => {
            let arm_strs: Vec<_> = arms
                .iter()
                .map(|arm| {
                    let guard_str = arm
                        .guard
                        .as_ref()
                        .map(|g| format!(" if {}", format_expr(g)))
                        .unwrap_or_default();
                    format!(
                        "{}{guard_str} => {}",
                        format_pattern(&arm.pattern),
                        format_expr(&arm.body)
                    )
                })
                .collect();
            format!("(match {} {{ {} }})", format_expr(subject), arm_strs.join(", "))
        }
        Expr::Block(stmts) => {
            let stmt_strs: Vec<_> = stmts.iter().map(format_stmt).collect();
            format!("{{ {} }}", stmt_strs.join("; "))
        }
        Expr::Assignment { name, value } => {
            format!("({name} = {})", format_expr(value))
        }
        Expr::Spread(inner) => {
            format!("(..{})", format_expr(inner))
        }
    }
}

fn format_pattern(pattern: &Pattern) -> String {
    match pattern {
        Pattern::Wildcard => "_".to_string(),
        Pattern::Identifier(name) => name.clone(),
        Pattern::RestIdentifier(name) => format!("..{name}"),
        Pattern::Literal(lit) => match lit {
            LiteralPattern::Integer(n) => n.to_string(),
            LiteralPattern::Decimal(n) => format!("{n:?}"),
            LiteralPattern::String(s) => format!("\"{s}\""),
            LiteralPattern::Boolean(b) => b.to_string(),
            LiteralPattern::Nil => "nil".to_string(),
        },
        Pattern::List(patterns) => {
            let ps: Vec<_> = patterns.iter().map(format_pattern).collect();
            format!("[{}]", ps.join(", "))
        }
        Pattern::Range {
            start,
            end,
            inclusive,
        } => {
            let op = if *inclusive { "..=" } else { ".." };
            match end {
                Some(e) => format!("{start}{op}{e}"),
                None => format!("{start}{op}"),
            }
        }
    }
}

fn format_stmt(stmt: &SpannedStmt) -> String {
    match &stmt.node {
        Stmt::Let { mutable, pattern, value } => {
            let mut_str = if *mutable { "mut " } else { "" };
            format!("let {mut_str}{} = {}", format_pattern(pattern), format_expr(value))
        }
        Stmt::Return(expr) => format!("return {}", format_expr(expr)),
        Stmt::Break(expr) => format!("break {}", format_expr(expr)),
        Stmt::Expr(expr) => format_expr(expr),
    }
}

fn format_program(program: &Program) -> String {
    let mut parts = Vec::new();

    for stmt in &program.statements {
        parts.push(format_stmt(stmt));
    }

    for section in &program.sections {
        match section {
            Section::Input(expr) => parts.push(format!("input: {}", format_expr(expr))),
            Section::PartOne(expr) => parts.push(format!("part_one: {}", format_expr(expr))),
            Section::PartTwo(expr) => parts.push(format!("part_two: {}", format_expr(expr))),
            Section::Test { input, .. } => parts.push(format!("test: {}", format_expr(input))),
        }
    }

    parts.join("\n")
}

// Tests

#[test]
fn parse_integer_literal() {
    check("42", expect!["42"]);
}

#[test]
fn parse_decimal_literal() {
    check("3.14", expect!["3.14"]);
}

#[test]
fn parse_string_literal() {
    check(r#""hello""#, expect![[r#""hello""#]]);
}

#[test]
fn parse_boolean_literals() {
    check("true", expect!["true"]);
    check("false", expect!["false"]);
}

#[test]
fn parse_nil_literal() {
    check("nil", expect!["nil"]);
}

#[test]
fn parse_identifier() {
    check("foo", expect!["foo"]);
}

#[test]
fn parse_placeholder() {
    check("_", expect!["_"]);
}

#[test]
fn parse_unary_negation() {
    check("-42", expect!["(-42)"]);
}

#[test]
fn parse_unary_not() {
    check("!true", expect!["(!true)"]);
}

#[test]
fn parse_binary_addition() {
    check("1 + 2", expect!["(1 + 2)"]);
}

#[test]
fn parse_binary_subtraction() {
    check("5 - 3", expect!["(5 - 3)"]);
}

#[test]
fn parse_binary_multiplication() {
    check("2 * 3", expect!["(2 * 3)"]);
}

#[test]
fn parse_binary_division() {
    check("10 / 2", expect!["(10 / 2)"]);
}

#[test]
fn parse_binary_modulo() {
    check("7 % 3", expect!["(7 % 3)"]);
}

#[test]
fn parse_comparison_operators() {
    check("1 < 2", expect!["(1 < 2)"]);
    check("1 <= 2", expect!["(1 <= 2)"]);
    check("1 > 2", expect!["(1 > 2)"]);
    check("1 >= 2", expect!["(1 >= 2)"]);
}

#[test]
fn parse_equality_operators() {
    check("1 == 2", expect!["(1 == 2)"]);
    check("1 != 2", expect!["(1 != 2)"]);
}

#[test]
fn parse_logical_operators() {
    check("true && false", expect!["(true && false)"]);
    check("true || false", expect!["(true || false)"]);
}

#[test]
fn parse_operator_precedence() {
    // Multiplication binds tighter than addition
    check("1 + 2 * 3", expect!["(1 + (2 * 3))"]);
    check("1 * 2 + 3", expect!["((1 * 2) + 3)"]);
}

#[test]
fn parse_operator_precedence_complex() {
    // Multiple precedence levels
    check("1 + 2 * 3 - 4 / 2", expect!["((1 + (2 * 3)) - (4 / 2))"]);
}

#[test]
fn parse_comparison_precedence() {
    check("1 + 2 < 3 + 4", expect!["((1 + 2) < (3 + 4))"]);
}

#[test]
fn parse_logical_precedence() {
    // && binds tighter than ||
    check("a || b && c", expect!["(a || (b && c))"]);
    check("a && b || c", expect!["((a && b) || c)"]);
}

#[test]
fn parse_grouped_expression() {
    check("(1 + 2) * 3", expect!["((1 + 2) * 3)"]);
}

#[test]
fn parse_nested_groups() {
    check("((1 + 2) * (3 + 4))", expect!["((1 + 2) * (3 + 4))"]);
}

#[test]
fn parse_list_literal() {
    check("[1, 2, 3]", expect!["[1, 2, 3]"]);
}

#[test]
fn parse_empty_list() {
    check("[]", expect!["[]"]);
}

#[test]
fn parse_list_with_trailing_comma() {
    check("[1, 2, 3,]", expect!["[1, 2, 3]"]);
}

#[test]
fn parse_set_literal() {
    check("{1, 2, 3}", expect!["{1, 2, 3}"]);
}

#[test]
fn parse_empty_set() {
    check("{}", expect!["{}"]);
}

#[test]
fn parse_dict_literal() {
    check(
        r#"#{"a": 1, "b": 2}"#,
        expect![[r#"#{"a": 1, "b": 2}"#]],
    );
}

#[test]
fn parse_empty_dict() {
    check("#{}", expect!["#{}"]);
}

#[test]
fn parse_dict_shorthand() {
    check("#{name, age}", expect![[r#"#{"name": name, "age": age}"#]]);
}

#[test]
fn parse_index_expression() {
    check("list[0]", expect!["(list[0])"]);
}

#[test]
fn parse_nested_index() {
    check("matrix[0][1]", expect!["((matrix[0])[1])"]);
}

#[test]
fn parse_negative_index() {
    check("list[-1]", expect!["(list[(-1)])"]);
}

#[test]
fn parse_function_expression() {
    check("|x| x + 1", expect!["|x| (x + 1)"]);
}

#[test]
fn parse_function_multiple_params() {
    check("|a, b| a + b", expect!["|a, b| (a + b)"]);
}

#[test]
fn parse_function_no_params() {
    check("|| 42", expect!["|| 42"]);
}

#[test]
fn parse_function_with_block() {
    check("|x| { x + 1 }", expect!["|x| { (x + 1) }"]);
}

#[test]
fn parse_function_rest_param() {
    check("|first, ..rest| rest", expect!["|first, ..rest| rest"]);
}

#[test]
fn parse_function_placeholder_param() {
    check("|_, b| b", expect!["|_, b| b"]);
}

#[test]
fn parse_function_call() {
    check("f(1, 2)", expect!["(f(1, 2))"]);
}

#[test]
fn parse_function_call_no_args() {
    check("f()", expect!["(f())"]);
}

#[test]
fn parse_chained_calls() {
    check("f(1)(2)", expect!["((f(1))(2))"]);
}

#[test]
fn parse_partial_application() {
    check("_ + 1", expect!["(_ + 1)"]);
}

#[test]
fn parse_partial_application_both_sides() {
    check("_ + _", expect!["(_ + _)"]);
}

#[test]
fn parse_pipeline_operator() {
    check("x |> f", expect!["(x |> f)"]);
}

#[test]
fn parse_pipeline_chained() {
    check("x |> f |> g", expect!["((x |> f) |> g)"]);
}

#[test]
fn parse_composition_operator() {
    check("f >> g", expect!["(f >> g)"]);
}

#[test]
fn parse_composition_chained() {
    check("f >> g >> h", expect!["((f >> g) >> h)"]);
}

#[test]
fn parse_exclusive_range() {
    check("1..10", expect!["(1..10)"]);
}

#[test]
fn parse_inclusive_range() {
    check("1..=10", expect!["(1..=10)"]);
}

#[test]
fn parse_unbounded_range() {
    check_program("let x = 1..", expect!["let x = (1..)"]);
}

#[test]
fn parse_infix_call() {
    check("list `includes?` 2", expect!["(list `includes?` 2)"]);
}

#[test]
fn parse_if_expression() {
    check("if x { 1 }", expect!["(if x { 1 })"]);
}

#[test]
fn parse_if_else() {
    check("if x { 1 } else { 2 }", expect!["(if x { 1 } else { 2 })"]);
}

#[test]
fn parse_if_else_if() {
    check(
        "if a { 1 } else if b { 2 } else { 3 }",
        expect!["(if a { 1 } else (if b { 2 } else { 3 }))"],
    );
}

#[test]
fn parse_if_let() {
    check(
        "if let x = get_value() { x }",
        expect!["(if let x = (get_value()) { x })"],
    );
}

#[test]
fn parse_match_expression() {
    check(
        "match x { 1 { a } 2 { b } _ { c } }",
        expect!["(match x { 1 => { a }, 2 => { b }, _ => { c } })"],
    );
}

#[test]
fn parse_match_with_guard() {
    check(
        "match x { n if n > 0 { n } _ { 0 } }",
        expect!["(match x { n if (n > 0) => { n }, _ => { 0 } })"],
    );
}

#[test]
fn parse_match_list_pattern() {
    check(
        "match list { [a, b] { a + b } _ { 0 } }",
        expect!["(match list { [a, b] => { (a + b) }, _ => { 0 } })"],
    );
}

#[test]
fn parse_match_rest_pattern() {
    check(
        "match list { [first, ..rest] { first } _ { nil } }",
        expect!["(match list { [first, ..rest] => { first }, _ => { nil } })"],
    );
}

#[test]
fn parse_block_expression() {
    check("{ let x = 1; x + 1 }", expect!["{ let x = 1; (x + 1) }"]);
}

#[test]
fn parse_spread_in_list() {
    check("[0, ..list, 4]", expect!["[0, (..list), 4]"]);
}

#[test]
fn parse_spread_in_call() {
    check("f(1, ..args)", expect!["(f(1, (..args)))"]);
}

#[test]
fn parse_let_statement() {
    check_program("let x = 42", expect!["let x = 42"]);
}

#[test]
fn parse_let_mut_statement() {
    check_program("let mut x = 42", expect!["let mut x = 42"]);
}

#[test]
fn parse_let_destructuring() {
    check_program("let [a, b] = list", expect!["let [a, b] = list"]);
}

#[test]
fn parse_return_statement() {
    check_program("return 42", expect!["return 42"]);
}

#[test]
fn parse_break_statement() {
    check_program("break 42", expect!["break 42"]);
}

#[test]
fn parse_aoc_sections() {
    check_program(
        r#"input: "test"
part_one: |input| input
part_two: |input| input"#,
        expect![[r#"
            input: "test"
            part_one: |input| input
            part_two: |input| input"#]],
    );
}

#[test]
fn parse_assignment() {
    check("x = 42", expect!["(x = 42)"]);
}

#[test]
fn parse_trailing_lambda() {
    check("each(list) |x| x + 1", expect!["((each(list))(|x| (x + 1)))"]);
}

#[test]
fn parse_complex_expression() {
    check(
        "[1, 2, 3] |> map(_ * 2) |> filter(_ > 2) |> sum",
        expect!["((([1, 2, 3] |> (map((_ * 2)))) |> (filter((_ > 2)))) |> sum)"],
    );
}

#[test]
fn parse_nested_function_calls() {
    check("sum(map(list, f))", expect!["(sum((map(list, f))))"]);
}

#[test]
fn parse_method_chain() {
    check(
        "data |> parse |> transform |> output",
        expect!["(((data |> parse) |> transform) |> output)"],
    );
}

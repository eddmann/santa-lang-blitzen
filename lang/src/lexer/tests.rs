use super::*;
use expect_test::{Expect, expect};

fn check(input: &str, expect: Expect) {
    let mut lexer = Lexer::new(input);
    let result = lexer.tokenize();
    let output = match result {
        Ok(tokens) => {
            let mut lines = Vec::new();
            for token in &tokens {
                lines.push(format!(
                    "{:?} @ {}:{} [{}-{}]",
                    token.kind,
                    token.span.line,
                    token.span.column,
                    token.span.start,
                    token.span.end
                ));
            }
            lines.join("\n")
        }
        Err(e) => format!("Error: {} at {}:{}", e.message, e.line, e.column),
    };
    expect.assert_eq(&output);
}

#[test]
fn lex_integer_literals() {
    check(
        "42 -17 0 1_000_000",
        expect![[r#"
            Integer(42) @ 1:1 [0-2]
            Minus @ 1:4 [3-4]
            Integer(17) @ 1:5 [4-6]
            Integer(0) @ 1:8 [7-8]
            Integer(1000000) @ 1:10 [9-18]
            Eof @ 1:19 [18-18]"#]],
    );
}

#[test]
fn lex_decimal_literals() {
    check(
        "3.14 -0.5 1_000.50",
        expect![[r#"
            Decimal(3.14) @ 1:1 [0-4]
            Minus @ 1:6 [5-6]
            Decimal(0.5) @ 1:7 [6-9]
            Decimal(1000.5) @ 1:11 [10-18]
            Eof @ 1:19 [18-18]"#]],
    );
}

#[test]
fn lex_string_literals() {
    check(
        r#""hello" "world""#,
        expect![[r#"
            String("hello") @ 1:1 [0-7]
            String("world") @ 1:9 [8-15]
            Eof @ 1:16 [15-15]"#]],
    );
}

#[test]
fn lex_string_with_escapes() {
    check(
        r#""Line 1\nLine 2" "Tab\tseparated" "Quote: \"text\"" "Back\\slash""#,
        expect![[r#"
            String("Line 1\nLine 2") @ 1:1 [0-16]
            String("Tab\tseparated") @ 1:18 [17-33]
            String("Quote: \"text\"") @ 1:35 [34-51]
            String("Back\\slash") @ 1:53 [52-65]
            Eof @ 1:66 [65-65]"#]],
    );
}

#[test]
fn lex_keywords() {
    check(
        "let mut if else match return break nil true false",
        expect![[r#"
            Let @ 1:1 [0-3]
            Mut @ 1:5 [4-7]
            If @ 1:9 [8-10]
            Else @ 1:12 [11-15]
            Match @ 1:17 [16-21]
            Return @ 1:23 [22-28]
            Break @ 1:30 [29-34]
            Nil @ 1:36 [35-38]
            True @ 1:40 [39-43]
            False @ 1:45 [44-49]
            Eof @ 1:50 [49-49]"#]],
    );
}

#[test]
fn lex_identifiers() {
    check(
        "x counter parse_input is_valid? my_var_123",
        expect![[r#"
            Identifier("x") @ 1:1 [0-1]
            Identifier("counter") @ 1:3 [2-9]
            Identifier("parse_input") @ 1:11 [10-21]
            Identifier("is_valid?") @ 1:23 [22-31]
            Identifier("my_var_123") @ 1:33 [32-42]
            Eof @ 1:43 [42-42]"#]],
    );
}

#[test]
fn lex_keywords_vs_identifiers() {
    check(
        "let letter if iffy true truthy",
        expect![[r#"
            Let @ 1:1 [0-3]
            Identifier("letter") @ 1:5 [4-10]
            If @ 1:12 [11-13]
            Identifier("iffy") @ 1:15 [14-18]
            True @ 1:20 [19-23]
            Identifier("truthy") @ 1:25 [24-30]
            Eof @ 1:31 [30-30]"#]],
    );
}

#[test]
fn lex_operators() {
    check(
        "+ - * / % ! = == != < <= > >= && || |> >>",
        expect![[r#"
            Plus @ 1:1 [0-1]
            Minus @ 1:3 [2-3]
            Star @ 1:5 [4-5]
            Slash @ 1:7 [6-7]
            Percent @ 1:9 [8-9]
            Bang @ 1:11 [10-11]
            Equal @ 1:13 [12-13]
            EqualEqual @ 1:15 [14-16]
            BangEqual @ 1:18 [17-19]
            Less @ 1:21 [20-21]
            LessEqual @ 1:23 [22-24]
            Greater @ 1:26 [25-26]
            GreaterEqual @ 1:28 [27-29]
            AmpAmp @ 1:31 [30-32]
            PipePipe @ 1:34 [33-35]
            PipeGreater @ 1:37 [36-38]
            GreaterGreater @ 1:40 [39-41]
            Eof @ 1:42 [41-41]"#]],
    );
}

#[test]
fn lex_range_operators() {
    check(
        "1..5 1..=5 1..",
        expect![[r#"
            Integer(1) @ 1:1 [0-1]
            DotDot @ 1:2 [1-3]
            Integer(5) @ 1:4 [3-4]
            Integer(1) @ 1:6 [5-6]
            DotDotEqual @ 1:7 [6-9]
            Integer(5) @ 1:10 [9-10]
            Integer(1) @ 1:12 [11-12]
            DotDot @ 1:13 [12-14]
            Eof @ 1:15 [14-14]"#]],
    );
}

#[test]
fn lex_delimiters() {
    check(
        "( ) [ ] { } #{ , : ; | `",
        expect![[r#"
            LeftParen @ 1:1 [0-1]
            RightParen @ 1:3 [2-3]
            LeftBracket @ 1:5 [4-5]
            RightBracket @ 1:7 [6-7]
            LeftBrace @ 1:9 [8-9]
            RightBrace @ 1:11 [10-11]
            HashBrace @ 1:13 [12-14]
            Comma @ 1:16 [15-16]
            Colon @ 1:18 [17-18]
            Semicolon @ 1:20 [19-20]
            Pipe @ 1:22 [21-22]
            Backtick @ 1:24 [23-24]
            Eof @ 1:25 [24-24]"#]],
    );
}

#[test]
fn lex_underscore_placeholder() {
    check(
        "_ _x x_ _foo",
        expect![[r#"
            Underscore @ 1:1 [0-1]
            Identifier("_x") @ 1:3 [2-4]
            Identifier("x_") @ 1:6 [5-7]
            Identifier("_foo") @ 1:9 [8-12]
            Eof @ 1:13 [12-12]"#]],
    );
}

#[test]
fn lex_comments() {
    check(
        "let x = 42; // This is a comment\nlet y = 10;",
        expect![[r#"
            Let @ 1:1 [0-3]
            Identifier("x") @ 1:5 [4-5]
            Equal @ 1:7 [6-7]
            Integer(42) @ 1:9 [8-10]
            Semicolon @ 1:11 [10-11]
            Let @ 2:1 [33-36]
            Identifier("y") @ 2:5 [37-38]
            Equal @ 2:7 [39-40]
            Integer(10) @ 2:9 [41-43]
            Semicolon @ 2:11 [43-44]
            Eof @ 2:12 [44-44]"#]],
    );
}

#[test]
fn lex_full_line_comment() {
    check(
        "// Full line comment\nlet x = 1;",
        expect![[r#"
            Let @ 2:1 [21-24]
            Identifier("x") @ 2:5 [25-26]
            Equal @ 2:7 [27-28]
            Integer(1) @ 2:9 [29-30]
            Semicolon @ 2:10 [30-31]
            Eof @ 2:11 [31-31]"#]],
    );
}

#[test]
fn lex_multiline() {
    check(
        "let x = 1;\nlet y = 2;\nlet z = 3;",
        expect![[r#"
            Let @ 1:1 [0-3]
            Identifier("x") @ 1:5 [4-5]
            Equal @ 1:7 [6-7]
            Integer(1) @ 1:9 [8-9]
            Semicolon @ 1:10 [9-10]
            Let @ 2:1 [11-14]
            Identifier("y") @ 2:5 [15-16]
            Equal @ 2:7 [17-18]
            Integer(2) @ 2:9 [19-20]
            Semicolon @ 2:10 [20-21]
            Let @ 3:1 [22-25]
            Identifier("z") @ 3:5 [26-27]
            Equal @ 3:7 [28-29]
            Integer(3) @ 3:9 [30-31]
            Semicolon @ 3:10 [31-32]
            Eof @ 3:11 [32-32]"#]],
    );
}

#[test]
fn lex_function_expression() {
    check(
        "|x| x + 1",
        expect![[r#"
            Pipe @ 1:1 [0-1]
            Identifier("x") @ 1:2 [1-2]
            Pipe @ 1:3 [2-3]
            Identifier("x") @ 1:5 [4-5]
            Plus @ 1:7 [6-7]
            Integer(1) @ 1:9 [8-9]
            Eof @ 1:10 [9-9]"#]],
    );
}

#[test]
fn lex_list_literal() {
    check(
        "[1, 2, 3, 4]",
        expect![[r#"
            LeftBracket @ 1:1 [0-1]
            Integer(1) @ 1:2 [1-2]
            Comma @ 1:3 [2-3]
            Integer(2) @ 1:5 [4-5]
            Comma @ 1:6 [5-6]
            Integer(3) @ 1:8 [7-8]
            Comma @ 1:9 [8-9]
            Integer(4) @ 1:11 [10-11]
            RightBracket @ 1:12 [11-12]
            Eof @ 1:13 [12-12]"#]],
    );
}

#[test]
fn lex_dict_literal() {
    check(
        r#"#{"name": "Alice", "age": 30}"#,
        expect![[r#"
            HashBrace @ 1:1 [0-2]
            String("name") @ 1:3 [2-8]
            Colon @ 1:9 [8-9]
            String("Alice") @ 1:11 [10-17]
            Comma @ 1:18 [17-18]
            String("age") @ 1:20 [19-24]
            Colon @ 1:25 [24-25]
            Integer(30) @ 1:27 [26-28]
            RightBrace @ 1:29 [28-29]
            Eof @ 1:30 [29-29]"#]],
    );
}

#[test]
fn lex_set_literal() {
    check(
        "{1, 2, 3}",
        expect![[r#"
            LeftBrace @ 1:1 [0-1]
            Integer(1) @ 1:2 [1-2]
            Comma @ 1:3 [2-3]
            Integer(2) @ 1:5 [4-5]
            Comma @ 1:6 [5-6]
            Integer(3) @ 1:8 [7-8]
            RightBrace @ 1:9 [8-9]
            Eof @ 1:10 [9-9]"#]],
    );
}

#[test]
fn lex_pipeline() {
    check(
        "[1, 2, 3] |> map(_ * 2) |> sum",
        expect![[r#"
            LeftBracket @ 1:1 [0-1]
            Integer(1) @ 1:2 [1-2]
            Comma @ 1:3 [2-3]
            Integer(2) @ 1:5 [4-5]
            Comma @ 1:6 [5-6]
            Integer(3) @ 1:8 [7-8]
            RightBracket @ 1:9 [8-9]
            PipeGreater @ 1:11 [10-12]
            Identifier("map") @ 1:14 [13-16]
            LeftParen @ 1:17 [16-17]
            Underscore @ 1:18 [17-18]
            Star @ 1:20 [19-20]
            Integer(2) @ 1:22 [21-22]
            RightParen @ 1:23 [22-23]
            PipeGreater @ 1:25 [24-26]
            Identifier("sum") @ 1:28 [27-30]
            Eof @ 1:31 [30-30]"#]],
    );
}

#[test]
fn lex_infix_call() {
    check(
        "[1, 2, 3] `includes?` 2",
        expect![[r#"
            LeftBracket @ 1:1 [0-1]
            Integer(1) @ 1:2 [1-2]
            Comma @ 1:3 [2-3]
            Integer(2) @ 1:5 [4-5]
            Comma @ 1:6 [5-6]
            Integer(3) @ 1:8 [7-8]
            RightBracket @ 1:9 [8-9]
            Backtick @ 1:11 [10-11]
            Identifier("includes?") @ 1:12 [11-20]
            Backtick @ 1:21 [20-21]
            Integer(2) @ 1:23 [22-23]
            Eof @ 1:24 [23-23]"#]],
    );
}

#[test]
fn lex_error_unterminated_string() {
    check(
        r#""hello"#,
        expect![[r#"Error: Unterminated string literal at 1:1"#]],
    );
}

#[test]
fn lex_error_invalid_escape() {
    check(
        r#""hello\x""#,
        expect![[r#"Error: Invalid escape sequence: \x at 1:8"#]],
    );
}

#[test]
fn lex_error_unexpected_character() {
    check(
        "let x = $42;",
        expect![[r#"Error: Unexpected character: '$' at 1:9"#]],
    );
}

#[test]
fn lex_match_expression() {
    check(
        "match x { 1 { } _ { } }",
        expect![[r#"
            Match @ 1:1 [0-5]
            Identifier("x") @ 1:7 [6-7]
            LeftBrace @ 1:9 [8-9]
            Integer(1) @ 1:11 [10-11]
            LeftBrace @ 1:13 [12-13]
            RightBrace @ 1:15 [14-15]
            Underscore @ 1:17 [16-17]
            LeftBrace @ 1:19 [18-19]
            RightBrace @ 1:21 [20-21]
            RightBrace @ 1:23 [22-23]
            Eof @ 1:24 [23-23]"#]],
    );
}

#[test]
fn lex_if_expression() {
    check(
        "if x > 0 { x } else { -x }",
        expect![[r#"
            If @ 1:1 [0-2]
            Identifier("x") @ 1:4 [3-4]
            Greater @ 1:6 [5-6]
            Integer(0) @ 1:8 [7-8]
            LeftBrace @ 1:10 [9-10]
            Identifier("x") @ 1:12 [11-12]
            RightBrace @ 1:14 [13-14]
            Else @ 1:16 [15-19]
            LeftBrace @ 1:21 [20-21]
            Minus @ 1:23 [22-23]
            Identifier("x") @ 1:24 [23-24]
            RightBrace @ 1:26 [25-26]
            Eof @ 1:27 [26-26]"#]],
    );
}

#[test]
fn lex_composition_operator() {
    check(
        "lines >> map(int) >> sum",
        expect![[r#"
            Identifier("lines") @ 1:1 [0-5]
            GreaterGreater @ 1:7 [6-8]
            Identifier("map") @ 1:10 [9-12]
            LeftParen @ 1:13 [12-13]
            Identifier("int") @ 1:14 [13-16]
            RightParen @ 1:17 [16-17]
            GreaterGreater @ 1:19 [18-20]
            Identifier("sum") @ 1:22 [21-24]
            Eof @ 1:25 [24-24]"#]],
    );
}

#[test]
fn lex_spread_in_list() {
    check(
        "[0, ..list, 4]",
        expect![[r#"
            LeftBracket @ 1:1 [0-1]
            Integer(0) @ 1:2 [1-2]
            Comma @ 1:3 [2-3]
            DotDot @ 1:5 [4-6]
            Identifier("list") @ 1:7 [6-10]
            Comma @ 1:11 [10-11]
            Integer(4) @ 1:13 [12-13]
            RightBracket @ 1:14 [13-14]
            Eof @ 1:15 [14-14]"#]],
    );
}

#[test]
fn lex_rest_parameter() {
    check(
        "|first, ..rest| rest",
        expect![[r#"
            Pipe @ 1:1 [0-1]
            Identifier("first") @ 1:2 [1-6]
            Comma @ 1:7 [6-7]
            DotDot @ 1:9 [8-10]
            Identifier("rest") @ 1:11 [10-14]
            Pipe @ 1:15 [14-15]
            Identifier("rest") @ 1:17 [16-20]
            Eof @ 1:21 [20-20]"#]],
    );
}

#[test]
fn lex_empty_source() {
    check("", expect![[r#"Eof @ 1:1 [0-0]"#]]);
}

#[test]
fn lex_only_whitespace() {
    check("   \n  \t  ", expect![[r#"Eof @ 2:6 [9-9]"#]]);
}

#[test]
fn lex_unicode_string() {
    check(
        r#""❤🍕" "👨‍👩‍👧‍👦""#,
        expect![[r#"
            String("❤🍕") @ 1:1 [0-9]
            String("👨\u{200d}👩\u{200d}👧\u{200d}👦") @ 1:6 [10-37]
            Eof @ 1:15 [37-37]"#]],
    );
}

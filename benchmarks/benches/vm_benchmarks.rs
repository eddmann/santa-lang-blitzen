use criterion::{BenchmarkId, Criterion, black_box, criterion_group, criterion_main};
use lang::lexer::Lexer;
use lang::parser::Parser;
use lang::vm::compiler::Compiler;
use lang::vm::runtime::VM;
use std::rc::Rc;

// ============================================================================
// Lexer Benchmarks
// ============================================================================

fn benchmark_lexer(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer");

    // Simple expression
    let simple = "1 + 2 * 3";
    group.bench_with_input(BenchmarkId::new("simple_expr", "1+2*3"), &simple, |b, src| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(src));
            lexer.tokenize().unwrap()
        });
    });

    // Medium complexity - function with control flow
    let medium = r#"
        let factorial = |n| {
            if n <= 1 { 1 }
            else { n * factorial(n - 1) }
        };
        factorial(10)
    "#;
    group.bench_with_input(BenchmarkId::new("function_def", "factorial"), &medium, |b, src| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(src));
            lexer.tokenize().unwrap()
        });
    });

    // Complex - AOC-style solution
    let complex = r#"
        let parse = |input| input |> lines |> map(int);
        let solve = |numbers| {
            numbers
                |> filter(|n| n > 0)
                |> map(|n| n * 2)
                |> fold(0, +)
        };

        input: read("aoc://2023/1")
        part_one: parse(input) |> solve
        part_two: parse(input) |> map(|n| n + 1) |> solve
    "#;
    group.bench_with_input(BenchmarkId::new("aoc_solution", "complex"), &complex, |b, src| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(src));
            lexer.tokenize().unwrap()
        });
    });

    // Large input - many tokens
    let large: String = (0..1000).map(|i| format!("let x{i} = {i};\n")).collect();
    group.bench_with_input(BenchmarkId::new("large_input", "1000_vars"), &large, |b, src| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(src));
            lexer.tokenize().unwrap()
        });
    });

    group.finish();
}

// ============================================================================
// Parser Benchmarks
// ============================================================================

fn benchmark_parser(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser");

    // Simple expression
    let simple = "1 + 2 * 3 - 4 / 2";
    let simple_tokens = Lexer::new(simple).tokenize().unwrap();
    group.bench_with_input(
        BenchmarkId::new("simple_expr", "arithmetic"),
        &simple_tokens,
        |b, tokens| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(tokens.clone()));
                parser.parse_program().unwrap()
            });
        },
    );

    // Nested expressions
    let nested = "((((1 + 2) * 3) - 4) / 2)";
    let nested_tokens = Lexer::new(nested).tokenize().unwrap();
    group.bench_with_input(
        BenchmarkId::new("nested_expr", "parens"),
        &nested_tokens,
        |b, tokens| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(tokens.clone()));
                parser.parse_program().unwrap()
            });
        },
    );

    // Function with pattern matching
    let pattern_match = r#"
        match value {
            [head, ..tail] { head + sum(tail) }
            [] { 0 }
            n if n > 0 { n }
            _ { nil }
        }
    "#;
    let pattern_tokens = Lexer::new(pattern_match).tokenize().unwrap();
    group.bench_with_input(
        BenchmarkId::new("pattern_match", "complex"),
        &pattern_tokens,
        |b, tokens| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(tokens.clone()));
                parser.parse_program().unwrap()
            });
        },
    );

    // Collections
    let collections = r#"
        let list = [1, 2, 3, 4, 5];
        let set = {1, 2, 3,};
        let dict = #{"a": 1, "b": 2, "c": 3};
        let range = 1..100;
    "#;
    let collection_tokens = Lexer::new(collections).tokenize().unwrap();
    group.bench_with_input(
        BenchmarkId::new("collections", "all_types"),
        &collection_tokens,
        |b, tokens| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(tokens.clone()));
                parser.parse_program().unwrap()
            });
        },
    );

    group.finish();
}

// ============================================================================
// VM Arithmetic Benchmarks
// ============================================================================

fn benchmark_vm_arithmetic(c: &mut Criterion) {
    let mut group = c.benchmark_group("vm_arithmetic");

    // Simple addition
    let add_code = "1 + 2";
    let add_compiled = compile_program(add_code);
    group.bench_function("addition", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&add_compiled))).unwrap()
        });
    });

    // Chained operations
    let chain_code = "1 + 2 * 3 - 4 / 2 + 5 % 3";
    let chain_compiled = compile_program(chain_code);
    group.bench_function("chained_ops", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&chain_compiled))).unwrap()
        });
    });

    // Integer operations loop (via fold)
    let loop_code = "fold(0, |acc, n| acc + n * 2, 1..100)";
    let loop_compiled = compile_program(loop_code);
    group.bench_function("fold_100_iterations", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&loop_compiled))).unwrap()
        });
    });

    // Large arithmetic
    let large_code = "fold(0, +, 1..1000)";
    let large_compiled = compile_program(large_code);
    group.bench_function("sum_1000", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&large_compiled))).unwrap()
        });
    });

    group.finish();
}

// ============================================================================
// VM Collections Benchmarks
// ============================================================================

fn benchmark_vm_collections(c: &mut Criterion) {
    let mut group = c.benchmark_group("vm_collections");

    // List creation
    let list_code = "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]";
    let list_compiled = compile_program(list_code);
    group.bench_function("list_create_10", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&list_compiled))).unwrap()
        });
    });

    // List from range
    let list_range_code = "list(1..100)";
    let list_range_compiled = compile_program(list_range_code);
    group.bench_function("list_from_range_100", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&list_range_compiled))).unwrap()
        });
    });

    // Set operations
    let set_code = "{1, 2, 3, 4, 5} + {4, 5, 6, 7, 8}";
    let set_compiled = compile_program(set_code);
    group.bench_function("set_union", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&set_compiled))).unwrap()
        });
    });

    // Dict creation and access
    let dict_code = r#"
        let d = #{"a": 1, "b": 2, "c": 3, "d": 4, "e": 5};
        d["c"]
    "#;
    let dict_compiled = compile_program(dict_code);
    group.bench_function("dict_create_access", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&dict_compiled))).unwrap()
        });
    });

    // List map operation
    let map_code = "map(|x| x * 2, list(1..=100))";
    let map_compiled = compile_program(map_code);
    group.bench_function("map_100", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&map_compiled))).unwrap()
        });
    });

    // List filter operation
    let filter_code = "filter(|x| x % 2 == 0, list(1..=100))";
    let filter_compiled = compile_program(filter_code);
    group.bench_function("filter_100", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&filter_compiled))).unwrap()
        });
    });

    // Chained operations
    let chain_code = r#"
        1..1000
            |> filter(|x| x % 2 == 0)
            |> map(|x| x * 2)
            |> take(100)
            |> fold(0, +)
    "#;
    let chain_compiled = compile_program(chain_code);
    group.bench_function("chain_filter_map_fold", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&chain_compiled))).unwrap()
        });
    });

    group.finish();
}

// ============================================================================
// VM Functions Benchmarks
// ============================================================================

fn benchmark_vm_functions(c: &mut Criterion) {
    let mut group = c.benchmark_group("vm_functions");

    // Simple function call
    let simple_fn_code = r#"
        let add = |a, b| a + b;
        add(1, 2)
    "#;
    let simple_fn_compiled = compile_program(simple_fn_code);
    group.bench_function("simple_call", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&simple_fn_compiled))).unwrap()
        });
    });

    // Recursive function (factorial)
    let factorial_code = r#"
        let factorial = |n| if n <= 1 { 1 } else { n * factorial(n - 1) };
        factorial(10)
    "#;
    let factorial_compiled = compile_program(factorial_code);
    group.bench_function("factorial_10", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&factorial_compiled))).unwrap()
        });
    });

    // Tail-recursive (with TCO)
    let tail_rec_code = r#"
        let sum_to = |n, acc| if n == 0 { acc } else { sum_to(n - 1, acc + n) };
        sum_to(1000, 0)
    "#;
    let tail_rec_compiled = compile_program(tail_rec_code);
    group.bench_function("tail_recursion_1000", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&tail_rec_compiled))).unwrap()
        });
    });

    // Closure capture
    let closure_code = r#"
        let make_adder = |x| |y| x + y;
        let add5 = make_adder(5);
        add5(10)
    "#;
    let closure_compiled = compile_program(closure_code);
    group.bench_function("closure_capture", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&closure_compiled))).unwrap()
        });
    });

    // Partial application (operator-based)
    let partial_code = r#"
        let add10 = _ + 10;
        add10(5)
    "#;
    let partial_compiled = compile_program(partial_code);
    group.bench_function("partial_application", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&partial_compiled))).unwrap()
        });
    });

    // Higher-order functions
    let hof_code = r#"
        let twice = |f| |x| f(f(x));
        let inc = |x| x + 1;
        twice(inc)(0)
    "#;
    let hof_compiled = compile_program(hof_code);
    group.bench_function("higher_order", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&hof_compiled))).unwrap()
        });
    });

    // Function composition
    let compose_code = r#"
        let double = |x| x * 2;
        let inc = |x| x + 1;
        let composed = double >> inc >> double;
        composed(5)
    "#;
    let compose_compiled = compile_program(compose_code);
    group.bench_function("composition", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&compose_compiled))).unwrap()
        });
    });

    group.finish();
}

// ============================================================================
// AOC Solution Benchmarks
// ============================================================================

fn benchmark_aoc_solutions(c: &mut Criterion) {
    let mut group = c.benchmark_group("aoc_solutions");

    // AOC 2015 Day 1 style (counting parens)
    let day1_code = r#"
        let input = "(((()()((((()(";
        let count_floors = |s| {
            fold(0, |acc, c| {
                if c == "(" { acc + 1 }
                else if c == ")" { acc - 1 }
                else { acc }
            }, s)
        };
        count_floors(input)
    "#;
    let day1_compiled = compile_program(day1_code);
    group.bench_function("day1_style_parens", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&day1_compiled))).unwrap()
        });
    });

    // Sum of numbers (common AOC pattern)
    let sum_code = r#"
        let input = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
        fold(0, +, input)
    "#;
    let sum_compiled = compile_program(sum_code);
    group.bench_function("sum_numbers", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&sum_compiled))).unwrap()
        });
    });

    // Finding max (common AOC pattern)
    let max_code = r#"
        let input = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5];
        reduce(|a, b| if a > b { a } else { b }, input)
    "#;
    let max_compiled = compile_program(max_code);
    group.bench_function("find_max", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&max_compiled))).unwrap()
        });
    });

    // Grouping/chunking (common AOC pattern)
    let chunk_code = r#"
        let input = 1..100 |> list;
        chunk(10, input) |> map(|c| fold(0, +, c)) |> list
    "#;
    let chunk_compiled = compile_program(chunk_code);
    group.bench_function("chunk_and_sum", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&chunk_compiled))).unwrap()
        });
    });

    // Complex pipeline (typical AOC solution shape)
    let pipeline_code = r#"
        let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20];
        numbers
            |> filter(|n| n % 2 == 0)
            |> map(|n| n * n)
            |> filter(|n| n > 10)
            |> fold(0, +)
    "#;
    let pipeline_compiled = compile_program(pipeline_code);
    group.bench_function("complex_pipeline", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&pipeline_compiled))).unwrap()
        });
    });

    // Pattern matching (common in AOC)
    let pattern_code = r#"
        let classify = |n| match n {
            0 { "zero" }
            1..10 { "small" }
            10..100 { "medium" }
            _ { "large" }
        };
        map(classify, [0, 5, 15, 150]) |> list
    "#;
    let pattern_compiled = compile_program(pattern_code);
    group.bench_function("pattern_matching", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            vm.run(black_box(Rc::clone(&pattern_compiled))).unwrap()
        });
    });

    group.finish();
}

// ============================================================================
// End-to-End Benchmarks (lex + parse + compile + run)
// ============================================================================

fn benchmark_e2e(c: &mut Criterion) {
    let mut group = c.benchmark_group("e2e");

    // Simple expression end-to-end
    let simple_code = "1 + 2 * 3";
    group.bench_function("simple_expr", |b| {
        b.iter(|| run_program(black_box(simple_code)));
    });

    // Medium complexity
    let medium_code = r#"
        let factorial = |n| if n <= 1 { 1 } else { n * factorial(n - 1) };
        factorial(10)
    "#;
    group.bench_function("factorial", |b| {
        b.iter(|| run_program(black_box(medium_code)));
    });

    // Complex pipeline
    let complex_code = r#"
        1..100
            |> filter(|x| x % 2 == 0)
            |> map(|x| x * 2)
            |> fold(0, +)
    "#;
    group.bench_function("pipeline", |b| {
        b.iter(|| run_program(black_box(complex_code)));
    });

    group.finish();
}

// ============================================================================
// Helper Functions
// ============================================================================

fn compile_program(source: &str) -> Rc<lang::vm::bytecode::CompiledFunction> {
    let tokens = Lexer::new(source).tokenize().unwrap();
    let program = Parser::new(tokens).parse_program().unwrap();
    let compiled = Compiler::compile_statements(&program.statements).unwrap();
    Rc::new(compiled)
}

fn run_program(source: &str) {
    let tokens = Lexer::new(source).tokenize().unwrap();
    let program = Parser::new(tokens).parse_program().unwrap();
    let compiled = Compiler::compile_statements(&program.statements).unwrap();
    let mut vm = VM::new();
    vm.run(Rc::new(compiled)).unwrap();
}

criterion_group!(
    benches,
    benchmark_lexer,
    benchmark_parser,
    benchmark_vm_arithmetic,
    benchmark_vm_collections,
    benchmark_vm_functions,
    benchmark_aoc_solutions,
    benchmark_e2e,
);
criterion_main!(benches);

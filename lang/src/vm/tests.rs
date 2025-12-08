#[cfg(test)]
mod value_tests {
    use crate::vm::value::Value;
    use im_rc::{HashMap, HashSet, Vector};
    use ordered_float::OrderedFloat;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    use std::rc::Rc;

    fn hash_value(v: &Value) -> u64 {
        let mut hasher = DefaultHasher::new();
        v.hash(&mut hasher);
        hasher.finish()
    }

    // §3.1-3.2 Integer and Decimal equality
    #[test]
    fn integer_equality() {
        assert_eq!(Value::Integer(42), Value::Integer(42));
        assert_ne!(Value::Integer(42), Value::Integer(43));
        assert_ne!(Value::Integer(-1), Value::Integer(1));
    }

    #[test]
    fn decimal_equality() {
        assert_eq!(
            Value::Decimal(OrderedFloat(3.15)),
            Value::Decimal(OrderedFloat(3.15))
        );
        assert_ne!(
            Value::Decimal(OrderedFloat(3.15)),
            Value::Decimal(OrderedFloat(2.71))
        );
    }

    #[test]
    fn integer_decimal_not_equal() {
        // Different types are not equal
        assert_ne!(Value::Integer(3), Value::Decimal(OrderedFloat(3.0)));
    }

    // §3.4 Boolean equality
    #[test]
    fn boolean_equality() {
        assert_eq!(Value::Boolean(true), Value::Boolean(true));
        assert_eq!(Value::Boolean(false), Value::Boolean(false));
        assert_ne!(Value::Boolean(true), Value::Boolean(false));
    }

    // §3.3 String equality
    #[test]
    fn string_equality() {
        assert_eq!(
            Value::String(Rc::new("hello".to_string())),
            Value::String(Rc::new("hello".to_string()))
        );
        assert_ne!(
            Value::String(Rc::new("hello".to_string())),
            Value::String(Rc::new("world".to_string()))
        );
    }

    // §3.10 Nil equality
    #[test]
    fn nil_equality() {
        assert_eq!(Value::Nil, Value::Nil);
        assert_ne!(Value::Nil, Value::Integer(0));
        assert_ne!(Value::Nil, Value::Boolean(false));
    }

    // §3.5 List equality (structural)
    #[test]
    fn list_equality() {
        let list1 = Value::List(Vector::from(vec![Value::Integer(1), Value::Integer(2)]));
        let list2 = Value::List(Vector::from(vec![Value::Integer(1), Value::Integer(2)]));
        let list3 = Value::List(Vector::from(vec![Value::Integer(1), Value::Integer(3)]));

        assert_eq!(list1, list2);
        assert_ne!(list1, list3);
    }

    #[test]
    fn nested_list_equality() {
        let inner1 = Value::List(Vector::from(vec![Value::Integer(1)]));
        let inner2 = Value::List(Vector::from(vec![Value::Integer(1)]));
        let outer1 = Value::List(Vector::from(vec![inner1]));
        let outer2 = Value::List(Vector::from(vec![inner2]));

        assert_eq!(outer1, outer2);
    }

    // §3.6 Set equality (unordered)
    #[test]
    fn set_equality() {
        let mut set1 = HashSet::new();
        set1.insert(Value::Integer(1));
        set1.insert(Value::Integer(2));

        let mut set2 = HashSet::new();
        set2.insert(Value::Integer(2));
        set2.insert(Value::Integer(1));

        assert_eq!(Value::Set(set1), Value::Set(set2));
    }

    // §3.7 Dictionary equality (unordered)
    #[test]
    fn dict_equality() {
        let mut dict1 = HashMap::new();
        dict1.insert(Value::String(Rc::new("a".to_string())), Value::Integer(1));
        dict1.insert(Value::String(Rc::new("b".to_string())), Value::Integer(2));

        let mut dict2 = HashMap::new();
        dict2.insert(Value::String(Rc::new("b".to_string())), Value::Integer(2));
        dict2.insert(Value::String(Rc::new("a".to_string())), Value::Integer(1));

        assert_eq!(Value::Dict(dict1), Value::Dict(dict2));
    }

    // §3.11 Hashability tests
    #[test]
    fn integer_hashable() {
        let v1 = Value::Integer(42);
        let v2 = Value::Integer(42);
        assert_eq!(hash_value(&v1), hash_value(&v2));
    }

    #[test]
    fn decimal_hashable() {
        let v1 = Value::Decimal(OrderedFloat(3.15));
        let v2 = Value::Decimal(OrderedFloat(3.15));
        assert_eq!(hash_value(&v1), hash_value(&v2));
    }

    #[test]
    fn string_hashable() {
        let v1 = Value::String(Rc::new("test".to_string()));
        let v2 = Value::String(Rc::new("test".to_string()));
        assert_eq!(hash_value(&v1), hash_value(&v2));
    }

    #[test]
    fn boolean_hashable() {
        let v1 = Value::Boolean(true);
        let v2 = Value::Boolean(true);
        assert_eq!(hash_value(&v1), hash_value(&v2));
    }

    #[test]
    fn nil_hashable() {
        let v1 = Value::Nil;
        let v2 = Value::Nil;
        assert_eq!(hash_value(&v1), hash_value(&v2));
    }

    #[test]
    fn list_hashable_when_elements_hashable() {
        let v1 = Value::List(Vector::from(vec![Value::Integer(1), Value::Integer(2)]));
        let v2 = Value::List(Vector::from(vec![Value::Integer(1), Value::Integer(2)]));
        assert_eq!(hash_value(&v1), hash_value(&v2));
    }

    #[test]
    fn set_hashable() {
        let mut set1 = HashSet::new();
        set1.insert(Value::Integer(1));
        let mut set2 = HashSet::new();
        set2.insert(Value::Integer(1));
        assert_eq!(hash_value(&Value::Set(set1)), hash_value(&Value::Set(set2)));
    }

    // Values in sets must be hashable
    #[test]
    fn integers_can_be_set_elements() {
        let mut set = HashSet::new();
        set.insert(Value::Integer(1));
        set.insert(Value::Integer(2));
        set.insert(Value::Integer(1)); // Duplicate
        assert_eq!(set.len(), 2);
    }

    #[test]
    fn lists_can_be_set_elements() {
        let mut set = HashSet::new();
        let list = Value::List(Vector::from(vec![Value::Integer(1), Value::Integer(2)]));
        set.insert(list);
        assert_eq!(set.len(), 1);
    }

    // §14.1 Truthiness tests
    #[test]
    fn truthiness_integer() {
        assert!(Value::Integer(1).is_truthy());
        assert!(Value::Integer(-1).is_truthy());
        assert!(!Value::Integer(0).is_truthy());
    }

    #[test]
    fn truthiness_decimal() {
        assert!(Value::Decimal(OrderedFloat(1.0)).is_truthy());
        assert!(Value::Decimal(OrderedFloat(0.1)).is_truthy());
        assert!(!Value::Decimal(OrderedFloat(0.0)).is_truthy());
    }

    #[test]
    fn truthiness_string() {
        assert!(Value::String(Rc::new("hello".to_string())).is_truthy());
        assert!(!Value::String(Rc::new("".to_string())).is_truthy());
    }

    #[test]
    fn truthiness_boolean() {
        assert!(Value::Boolean(true).is_truthy());
        assert!(!Value::Boolean(false).is_truthy());
    }

    #[test]
    fn truthiness_nil() {
        assert!(!Value::Nil.is_truthy());
    }

    #[test]
    fn truthiness_list() {
        assert!(Value::List(Vector::from(vec![Value::Integer(1)])).is_truthy());
        assert!(!Value::List(Vector::new()).is_truthy());
    }

    #[test]
    fn truthiness_set() {
        let mut set = HashSet::new();
        set.insert(Value::Integer(1));
        assert!(Value::Set(set).is_truthy());
        assert!(!Value::Set(HashSet::new()).is_truthy());
    }

    #[test]
    fn truthiness_dict() {
        let mut dict = HashMap::new();
        dict.insert(Value::String(Rc::new("a".to_string())), Value::Integer(1));
        assert!(Value::Dict(dict).is_truthy());
        assert!(!Value::Dict(HashMap::new()).is_truthy());
    }

    // Range tests (§3.4)
    #[test]
    fn range_equality() {
        assert_eq!(
            Value::Range {
                start: 1,
                end: Some(5),
                inclusive: false
            },
            Value::Range {
                start: 1,
                end: Some(5),
                inclusive: false
            }
        );
        assert_ne!(
            Value::Range {
                start: 1,
                end: Some(5),
                inclusive: false
            },
            Value::Range {
                start: 1,
                end: Some(5),
                inclusive: true
            }
        );
    }

    #[test]
    fn unbounded_range_equality() {
        assert_eq!(
            Value::Range {
                start: 1,
                end: None,
                inclusive: false
            },
            Value::Range {
                start: 1,
                end: None,
                inclusive: false
            }
        );
    }

    // Display tests
    #[test]
    fn display_integer() {
        assert_eq!(format!("{}", Value::Integer(42)), "42");
    }

    #[test]
    fn display_decimal() {
        assert_eq!(format!("{}", Value::Decimal(OrderedFloat(3.15))), "3.15");
    }

    #[test]
    fn display_string() {
        assert_eq!(
            format!("{}", Value::String(Rc::new("hello".to_string()))),
            "\"hello\""
        );
    }

    #[test]
    fn display_boolean() {
        assert_eq!(format!("{}", Value::Boolean(true)), "true");
        assert_eq!(format!("{}", Value::Boolean(false)), "false");
    }

    #[test]
    fn display_nil() {
        assert_eq!(format!("{}", Value::Nil), "nil");
    }

    #[test]
    fn display_list() {
        let list = Value::List(Vector::from(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]));
        assert_eq!(format!("{}", list), "[1, 2, 3]");
    }

    #[test]
    fn display_empty_list() {
        assert_eq!(format!("{}", Value::List(Vector::new())), "[]");
    }

    #[test]
    fn display_range_exclusive() {
        let range = Value::Range {
            start: 1,
            end: Some(5),
            inclusive: false,
        };
        assert_eq!(format!("{}", range), "1..5");
    }

    #[test]
    fn display_range_inclusive() {
        let range = Value::Range {
            start: 1,
            end: Some(5),
            inclusive: true,
        };
        assert_eq!(format!("{}", range), "1..=5");
    }

    #[test]
    fn display_range_unbounded() {
        let range = Value::Range {
            start: 1,
            end: None,
            inclusive: false,
        };
        assert_eq!(format!("{}", range), "1..");
    }

    // Type name tests
    #[test]
    fn type_name() {
        assert_eq!(Value::Nil.type_name(), "Nil");
        assert_eq!(Value::Integer(0).type_name(), "Integer");
        assert_eq!(Value::Decimal(OrderedFloat(0.0)).type_name(), "Decimal");
        assert_eq!(Value::Boolean(true).type_name(), "Boolean");
        assert_eq!(Value::String(Rc::new("".to_string())).type_name(), "String");
        assert_eq!(Value::List(Vector::new()).type_name(), "List");
        assert_eq!(Value::Set(HashSet::new()).type_name(), "Set");
        assert_eq!(Value::Dict(HashMap::new()).type_name(), "Dictionary");
        assert_eq!(
            Value::Range {
                start: 0,
                end: None,
                inclusive: false
            }
            .type_name(),
            "Range"
        );
    }
}

#[cfg(test)]
mod bytecode_tests {
    use crate::vm::bytecode::{Chunk, OpCode};
    use crate::vm::value::Value;

    #[test]
    fn chunk_write_and_read() {
        let mut chunk = Chunk::new();
        chunk.write(OpCode::Constant, 1);
        chunk.write_operand(0);
        chunk.write(OpCode::Return, 1);

        assert_eq!(chunk.code.len(), 3);
        assert_eq!(chunk.code[0], OpCode::Constant as u8);
        assert_eq!(chunk.code[1], 0);
        assert_eq!(chunk.code[2], OpCode::Return as u8);
    }

    #[test]
    fn chunk_add_constant() {
        let mut chunk = Chunk::new();
        let idx = chunk.add_constant(Value::Integer(42));
        assert_eq!(idx, 0);
        assert_eq!(chunk.constants[0], Value::Integer(42));

        let idx2 = chunk.add_constant(Value::Integer(100));
        assert_eq!(idx2, 1);
    }

    #[test]
    fn chunk_line_tracking() {
        let mut chunk = Chunk::new();
        chunk.write(OpCode::Constant, 10);
        chunk.write_operand(0);
        chunk.write(OpCode::Return, 10);

        assert_eq!(chunk.get_line(0), 10);
        assert_eq!(chunk.get_line(1), 10);
        assert_eq!(chunk.get_line(2), 10);
    }

    #[test]
    fn opcode_to_u8_roundtrip() {
        let opcodes = vec![
            OpCode::Constant,
            OpCode::Pop,
            OpCode::Add,
            OpCode::Sub,
            OpCode::Mul,
            OpCode::Div,
            OpCode::Mod,
            OpCode::Neg,
            OpCode::Not,
            OpCode::Eq,
            OpCode::Ne,
            OpCode::Lt,
            OpCode::Le,
            OpCode::Gt,
            OpCode::Ge,
            OpCode::Return,
            OpCode::Jump,
            OpCode::JumpIfFalse,
            OpCode::JumpIfTrue,
        ];

        for op in opcodes {
            let byte = op as u8;
            let back = OpCode::try_from(byte).expect("Should convert back");
            assert_eq!(op, back);
        }
    }

    #[test]
    fn chunk_disassemble_constant() {
        let mut chunk = Chunk::new();
        let idx = chunk.add_constant(Value::Integer(42));
        chunk.write(OpCode::Constant, 1);
        chunk.write_operand(idx as u8);

        let disasm = chunk.disassemble_instruction(0);
        assert!(disasm.contains("Constant"));
        assert!(disasm.contains("42"));
    }
}

#[cfg(test)]
mod compiler_tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::vm::bytecode::OpCode;
    use crate::vm::compiler::Compiler;
    use expect_test::{Expect, expect};

    fn check_bytecode(source: &str, expected: Expect) {
        let tokens = Lexer::new(source).tokenize().expect("Should tokenize");
        let program = Parser::new(tokens).parse_program().expect("Should parse");

        // Get first statement expression
        let expr = match &program.statements[0].node {
            crate::parser::ast::Stmt::Expr(e) => e,
            _ => panic!("Expected expression statement"),
        };

        let compiled = Compiler::compile_expression(expr).expect("Should compile");
        let disasm = compiled.chunk.disassemble("test");
        expected.assert_eq(&disasm);
    }

    fn compile_expr(source: &str) -> crate::vm::bytecode::CompiledFunction {
        let tokens = Lexer::new(source).tokenize().expect("Should tokenize");
        let program = Parser::new(tokens).parse_program().expect("Should parse");

        let expr = match &program.statements[0].node {
            crate::parser::ast::Stmt::Expr(e) => e,
            _ => panic!("Expected expression statement"),
        };

        Compiler::compile_expression(expr).expect("Should compile")
    }

    // §2.5 Literal compilation tests
    #[test]
    fn compile_integer_literal() {
        check_bytecode(
            "42",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (42)
                0002 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_decimal_literal() {
        check_bytecode(
            "3.15",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (3.15)
                0002 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_string_literal() {
        check_bytecode(
            r#""hello""#,
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 ("hello")
                0002 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_boolean_literals() {
        check_bytecode(
            "true",
            expect![[r#"
                == test ==
                0000 [   1] True
                0001 [   1] Return
            "#]],
        );

        check_bytecode(
            "false",
            expect![[r#"
                == test ==
                0000 [   1] False
                0001 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_nil_literal() {
        check_bytecode(
            "nil",
            expect![[r#"
                == test ==
                0000 [   1] Nil
                0001 [   1] Return
            "#]],
        );
    }

    // §4 Binary expression tests
    #[test]
    fn compile_binary_add() {
        check_bytecode(
            "1 + 2",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] Add
                0005 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_binary_sub() {
        check_bytecode(
            "10 - 3",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (10)
                0002 [   1] Constant 1 (3)
                0004 [   1] Sub
                0005 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_binary_mul() {
        check_bytecode(
            "4 * 5",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (4)
                0002 [   1] Constant 1 (5)
                0004 [   1] Mul
                0005 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_binary_div() {
        check_bytecode(
            "20 / 4",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (20)
                0002 [   1] Constant 1 (4)
                0004 [   1] Div
                0005 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_binary_mod() {
        check_bytecode(
            "17 % 5",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (17)
                0002 [   1] Constant 1 (5)
                0004 [   1] Mod
                0005 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_nested_expression() {
        // Test operator precedence: 1 + 2 * 3 should be 1 + (2 * 3)
        check_bytecode(
            "1 + 2 * 3",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] Constant 2 (3)
                0006 [   1] Mul
                0007 [   1] Add
                0008 [   1] Return
            "#]],
        );
    }

    // Comparison operators
    #[test]
    fn compile_comparison_operators() {
        check_bytecode(
            "1 == 2",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] Eq
                0005 [   1] Return
            "#]],
        );

        check_bytecode(
            "1 != 2",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] Ne
                0005 [   1] Return
            "#]],
        );

        check_bytecode(
            "1 < 2",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] Lt
                0005 [   1] Return
            "#]],
        );

        check_bytecode(
            "1 <= 2",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] Le
                0005 [   1] Return
            "#]],
        );

        check_bytecode(
            "1 > 2",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] Gt
                0005 [   1] Return
            "#]],
        );

        check_bytecode(
            "1 >= 2",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] Ge
                0005 [   1] Return
            "#]],
        );
    }

    // Prefix operators
    #[test]
    fn compile_negation() {
        check_bytecode(
            "-42",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (42)
                0002 [   1] Neg
                0003 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_not() {
        check_bytecode(
            "!true",
            expect![[r#"
                == test ==
                0000 [   1] True
                0001 [   1] Not
                0002 [   1] Return
            "#]],
        );
    }

    // §4.4 Short-circuit logical operators
    #[test]
    fn compile_and_short_circuit() {
        check_bytecode(
            "true && false",
            expect![[r#"
                == test ==
                0000 [   1] True
                0001 [   1] PopJumpIfFalse -> 5
                0004 [   1] False
                0005 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_or_short_circuit() {
        check_bytecode(
            "false || true",
            expect![[r#"
                == test ==
                0000 [   1] False
                0001 [   1] PopJumpIfTrue -> 5
                0004 [   1] True
                0005 [   1] Return
            "#]],
        );
    }

    // §10 Collection compilation
    #[test]
    fn compile_empty_list() {
        check_bytecode(
            "[]",
            expect![[r#"
                == test ==
                0000 [   1] MakeList 0
                0002 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_list_with_elements() {
        check_bytecode(
            "[1, 2, 3]",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] Constant 2 (3)
                0006 [   1] MakeList 3
                0008 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_empty_set() {
        check_bytecode(
            "{}",
            expect![[r#"
                == test ==
                0000 [   1] MakeSet 0
                0002 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_set_with_elements() {
        check_bytecode(
            "{1, 2, 3}",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] Constant 2 (3)
                0006 [   1] MakeSet 3
                0008 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_empty_dict() {
        check_bytecode(
            "#{}",
            expect![[r#"
                == test ==
                0000 [   1] MakeDict 0
                0002 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_dict_with_entries() {
        check_bytecode(
            r#"#{"a": 1, "b": 2}"#,
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 ("a")
                0002 [   1] Constant 1 (1)
                0004 [   1] Constant 2 ("b")
                0006 [   1] Constant 3 (2)
                0008 [   1] MakeDict 2
                0010 [   1] Return
            "#]],
        );
    }

    // §3.4 Range compilation
    #[test]
    fn compile_exclusive_range() {
        check_bytecode(
            "1..10",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (10)
                0004 [   1] False
                0005 [   1] MakeRange
                0006 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_inclusive_range() {
        check_bytecode(
            "1..=10",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (10)
                0004 [   1] True
                0005 [   1] MakeRange
                0006 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_unbounded_range() {
        check_bytecode(
            "1..",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Nil
                0003 [   1] False
                0004 [   1] MakeRange
                0005 [   1] Return
            "#]],
        );
    }

    // §4.6 Index operation
    #[test]
    fn compile_index() {
        check_bytecode(
            "[1, 2, 3][1]",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] Constant 2 (3)
                0006 [   1] MakeList 3
                0008 [   1] Constant 3 (1)
                0010 [   1] Index
                0011 [   1] Return
            "#]],
        );
    }

    // §8.1 Function expressions
    #[test]
    fn compile_function() {
        let compiled = compile_expr("|x| x + 1");
        // The outer chunk should have a MakeClosure instruction
        assert_eq!(compiled.chunk.code[0], OpCode::MakeClosure as u8);
        // And there should be one function in the functions list
        assert_eq!(compiled.chunk.functions.len(), 1);
    }

    #[test]
    fn compile_function_body() {
        let compiled = compile_expr("|x| x + 1");
        // Get the inner function
        let inner_fn = &compiled.chunk.functions[0];
        assert_eq!(inner_fn.arity, 1);

        // The body should have: GetLocal 0, Constant, Add, Return
        let disasm = inner_fn.chunk.disassemble("inner");
        assert!(disasm.contains("GetLocal"));
        assert!(disasm.contains("Add"));
        assert!(disasm.contains("Return"));
    }

    #[test]
    fn compile_function_multiple_params() {
        let compiled = compile_expr("|a, b, c| a + b + c");
        let inner_fn = &compiled.chunk.functions[0];
        assert_eq!(inner_fn.arity, 3);
    }

    // §8.4 Partial application
    #[test]
    fn compile_partial_add_right() {
        // _ + 1 should compile to a function with one parameter
        let compiled = compile_expr("_ + 1");
        assert_eq!(compiled.chunk.code[0], OpCode::MakeClosure as u8);
        let inner_fn = &compiled.chunk.functions[0];
        assert_eq!(inner_fn.arity, 1);
    }

    #[test]
    fn compile_partial_add_left() {
        // 10 - _ should compile to a function with one parameter
        let compiled = compile_expr("10 - _");
        let inner_fn = &compiled.chunk.functions[0];
        assert_eq!(inner_fn.arity, 1);
    }

    #[test]
    fn compile_partial_both() {
        // _ / _ should compile to a function with two parameters
        let compiled = compile_expr("_ / _");
        let inner_fn = &compiled.chunk.functions[0];
        assert_eq!(inner_fn.arity, 2);
    }

    // §8.2 Function calls
    #[test]
    fn compile_function_call() {
        check_bytecode(
            "f(1, 2)",
            expect![[r#"
                == test ==
                0000 [   1] GetGlobal 0 ("f")
                0002 [   1] Constant 1 (1)
                0004 [   1] Constant 2 (2)
                0006 [   1] Call 2
                0008 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_function_call_no_args() {
        check_bytecode(
            "f()",
            expect![[r#"
                == test ==
                0000 [   1] GetGlobal 0 ("f")
                0002 [   1] Call 0
                0004 [   1] Return
            "#]],
        );
    }

    // §4.6a Self-recursive closures
    // Self-recursive closures need pre-declaration so the closure can capture itself
    #[test]
    fn compile_self_recursive_closure_direct() {
        // Direct self-recursive closure: let f = |x| f(x)
        // Should use Nil + SetLocal pattern to allow self-reference
        check_bytecode(
            "{ let f = |x| f(x); f(1) }",
            expect![[r#"
                == test ==
                0000 [   1] Nil
                0001 [   1] MakeClosure 0
                0003 [   1] SetLocal 0
                0005 [   1] Pop
                0006 [   1] GetLocal 0
                0008 [   1] Constant 0 (1)
                0010 [   1] Call 1
                0012 [   1] PopN 1
                0014 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_self_recursive_closure_with_wrapper() {
        // Self-recursive closure wrapped in a function call (like memoize)
        // Should also use Nil + SetLocal pattern
        check_bytecode(
            "{ let f = wrapper(|x| f(x)); f(1) }",
            expect![[r#"
                == test ==
                0000 [   1] Nil
                0001 [   1] GetGlobal 0 ("wrapper")
                0003 [   1] MakeClosure 0
                0005 [   1] Call 1
                0007 [   1] SetLocal 0
                0009 [   1] Pop
                0010 [   1] GetLocal 0
                0012 [   1] Constant 1 (1)
                0014 [   1] Call 1
                0016 [   1] PopN 1
                0018 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_non_recursive_closure_no_predeclaration() {
        // Non-recursive closure also uses pre-declaration since it contains a function
        // The contains_function check is conservative - it doesn't check if the closure
        // actually references the bound name
        check_bytecode(
            "{ let f = |x| x + 1; f(1) }",
            expect![[r#"
                == test ==
                0000 [   1] Nil
                0001 [   1] MakeClosure 0
                0003 [   1] SetLocal 0
                0005 [   1] Pop
                0006 [   1] GetLocal 0
                0008 [   1] Constant 0 (1)
                0010 [   1] Call 1
                0012 [   1] PopN 1
                0014 [   1] Return
            "#]],
        );
    }

    // §4.7 Pipeline operator
    #[test]
    fn compile_pipeline_simple() {
        // x |> f compiles to f(x)
        check_bytecode(
            "5 |> double",
            expect![[r#"
                == test ==
                0000 [   1] GetGlobal 0 ("double")
                0002 [   1] Constant 1 (5)
                0004 [   1] Call 1
                0006 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_pipeline_with_args() {
        // x |> f(a) compiles to (f(a))(x) - call f(a) first, then call result with x
        check_bytecode(
            "5 |> add(10)",
            expect![[r#"
                == test ==
                0000 [   1] GetGlobal 0 ("add")
                0002 [   1] Constant 1 (10)
                0004 [   1] Call 1
                0006 [   1] Constant 2 (5)
                0008 [   1] Call 1
                0010 [   1] Return
            "#]],
        );
    }

    // §7.1 If expression
    #[test]
    fn compile_if_else() {
        check_bytecode(
            "if true { 1 } else { 2 }",
            expect![[r#"
                == test ==
                0000 [   1] True
                0001 [   1] JumpIfFalse -> 10
                0004 [   1] Pop
                0005 [   1] Constant 0 (1)
                0007 [   1] Jump -> 13
                0010 [   1] Pop
                0011 [   1] Constant 1 (2)
                0013 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_if_without_else() {
        check_bytecode(
            "if false { 1 }",
            expect![[r#"
                == test ==
                0000 [   1] False
                0001 [   1] JumpIfFalse -> 10
                0004 [   1] Pop
                0005 [   1] Constant 0 (1)
                0007 [   1] Jump -> 12
                0010 [   1] Pop
                0011 [   1] Nil
                0012 [   1] Return
            "#]],
        );
    }

    // §6.5 Infix function call
    #[test]
    fn compile_infix_call() {
        check_bytecode(
            r#"[1, 2, 3] `contains?` 2"#,
            expect![[r#"
                == test ==
                0000 [   1] GetGlobal 0 ("contains?")
                0002 [   1] Constant 1 (1)
                0004 [   1] Constant 2 (2)
                0006 [   1] Constant 3 (3)
                0008 [   1] MakeList 3
                0010 [   1] Constant 4 (2)
                0012 [   1] Call 2
                0014 [   1] Return
            "#]],
        );
    }

    // Phase 6: Statements & Control Flow tests

    // §5.1 Let binding
    #[test]
    fn compile_let_binding() {
        check_bytecode(
            "{ let x = 42; x }",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (42)
                0002 [   1] Dup
                0003 [   1] Pop
                0004 [   1] GetLocal 0
                0006 [   1] PopN 1
                0008 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_let_multiple() {
        check_bytecode(
            "{ let x = 1; let y = 2; x + y }",
            expect![[r#"
                == test ==
                0000 [   1] Nil
                0001 [   1] Nil
                0002 [   1] Constant 0 (1)
                0004 [   1] SetLocal 0
                0006 [   1] Pop
                0007 [   1] Constant 1 (2)
                0009 [   1] SetLocal 1
                0011 [   1] Pop
                0012 [   1] GetLocal 0
                0014 [   1] GetLocal 1
                0016 [   1] Add
                0017 [   1] PopN 2
                0019 [   1] Return
            "#]],
        );
    }

    // §5.3 Mutable variables
    #[test]
    fn compile_let_mut_and_assign() {
        // SetLocal uses peek (not pop), so the value stays on the stack after assignment.
        // Dup is emitted for let so the value can be used as implicit return.
        check_bytecode(
            "{ let mut x = 1; x = 2; x }",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Dup
                0003 [   1] Pop
                0004 [   1] Constant 1 (2)
                0006 [   1] SetLocal 0
                0008 [   1] Pop
                0009 [   1] GetLocal 0
                0011 [   1] PopN 1
                0013 [   1] Return
            "#]],
        );
    }

    // §5.4 Destructuring - list
    #[test]
    fn compile_destructuring_list() {
        check_bytecode(
            "{ let [a, b] = [1, 2]; a + b }",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] MakeList 2
                0006 [   1] GetLocal 0
                0008 [   1] Constant 2 (0)
                0010 [   1] Index
                0011 [   1] GetLocal 0
                0013 [   1] Constant 3 (1)
                0015 [   1] Index
                0016 [   1] GetLocal 1
                0018 [   1] GetLocal 2
                0020 [   1] Add
                0021 [   1] PopN 3
                0023 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_destructuring_with_rest() {
        check_bytecode(
            "{ let [first, ..rest] = [1, 2, 3]; first }",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] Constant 2 (3)
                0006 [   1] MakeList 3
                0008 [   1] GetLocal 0
                0010 [   1] Constant 3 (0)
                0012 [   1] Index
                0013 [   1] GetLocal 0
                0015 [   1] Constant 4 (1)
                0017 [   1] Nil
                0018 [   1] Slice
                0019 [   1] GetLocal 1
                0021 [   1] PopN 3
                0023 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_destructuring_with_wildcard() {
        check_bytecode(
            "{ let [x, _] = [1, 2]; x }",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] MakeList 2
                0006 [   1] GetLocal 0
                0008 [   1] Constant 2 (0)
                0010 [   1] Index
                0011 [   1] GetLocal 1
                0013 [   1] PopN 2
                0015 [   1] Return
            "#]],
        );
    }

    // §5.5 Shadowing
    #[test]
    fn compile_shadowing() {
        check_bytecode(
            "{ let x = 1; { let x = 2; x }; x }",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Dup
                0003 [   1] Pop
                0004 [   1] Constant 1 (2)
                0006 [   1] Dup
                0007 [   1] Pop
                0008 [   1] GetLocal 1
                0010 [   1] PopN 1
                0012 [   1] Pop
                0013 [   1] GetLocal 0
                0015 [   1] PopN 1
                0017 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_shadowing_with_different_types() {
        check_bytecode(
            r#"{ let x = 1; let x = "hello"; x }"#,
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Dup
                0003 [   1] Pop
                0004 [   1] Constant 1 ("hello")
                0006 [   1] Dup
                0007 [   1] Pop
                0008 [   1] GetLocal 1
                0010 [   1] PopN 2
                0012 [   1] Return
            "#]],
        );
    }

    // §6.3 Block expressions
    #[test]
    fn compile_block_returns_last() {
        check_bytecode(
            "{ 1; 2; 3 }",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Pop
                0003 [   1] Constant 1 (2)
                0005 [   1] Pop
                0006 [   1] Constant 2 (3)
                0008 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_empty_block() {
        check_bytecode(
            "{ }",
            expect![[r#"
                == test ==
                0000 [   1] MakeSet 0
                0002 [   1] Return
            "#]],
        );
    }

    // §7.2 Match expression
    #[test]
    fn compile_match_literals() {
        check_bytecode(
            r#"match 1 { 0 { "zero" } 1 { "one" } _ { "other" } }"#,
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Dup
                0003 [   1] Constant 1 (0)
                0005 [   1] Eq
                0006 [   1] JumpIfFalse -> 16
                0009 [   1] Pop
                0010 [   1] Pop
                0011 [   1] Constant 2 ("zero")
                0013 [   1] Jump -> 35
                0016 [   1] Pop
                0017 [   1] Dup
                0018 [   1] Constant 3 (1)
                0020 [   1] Eq
                0021 [   1] JumpIfFalse -> 31
                0024 [   1] Pop
                0025 [   1] Pop
                0026 [   1] Constant 4 ("one")
                0028 [   1] Jump -> 35
                0031 [   1] Pop
                0032 [   1] Pop
                0033 [   1] Constant 5 ("other")
                0035 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_match_with_identifier() {
        check_bytecode(
            "match 42 { x { x + 1 } }",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (42)
                0002 [   1] Dup
                0003 [   1] Nil
                0004 [   1] Ne
                0005 [   1] JumpIfFalse -> 19
                0008 [   1] Pop
                0009 [   1] GetLocal 0
                0011 [   1] Constant 1 (1)
                0013 [   1] Add
                0014 [   1] PopN 1
                0016 [   1] Jump -> 22
                0019 [   1] Pop
                0020 [   1] Pop
                0021 [   1] Nil
                0022 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_match_with_guard() {
        check_bytecode(
            "match 5 { x if x > 3 { x } _ { 0 } }",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (5)
                0002 [   1] Dup
                0003 [   1] Nil
                0004 [   1] Ne
                0005 [   1] JumpIfFalse -> 28
                0008 [   1] Pop
                0009 [   1] GetLocal 0
                0011 [   1] Constant 1 (3)
                0013 [   1] Gt
                0014 [   1] JumpIfFalse -> 24
                0017 [   1] GetLocal 0
                0019 [   1] PopN 1
                0021 [   1] Jump -> 32
                0024 [   1] Pop
                0025 [   1] Jump -> 29
                0028 [   1] Pop
                0029 [   1] Pop
                0030 [   1] Constant 2 (0)
                0032 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_match_list_pattern() {
        check_bytecode(
            "match [1, 2] { [a, b] { a + b } _ { 0 } }",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] MakeList 2
                0006 [   1] GetLocal 0
                0008 [   1] Size
                0009 [   1] Constant 2 (2)
                0011 [   1] Eq
                0012 [   1] JumpIfFalse -> 36
                0015 [   1] Pop
                0016 [   1] GetLocal 0
                0018 [   1] Constant 3 (0)
                0020 [   1] Index
                0021 [   1] GetLocal 0
                0023 [   1] Constant 4 (1)
                0025 [   1] Index
                0026 [   1] GetLocal 1
                0028 [   1] GetLocal 2
                0030 [   1] Add
                0031 [   1] PopN 3
                0033 [   1] Jump -> 40
                0036 [   1] Pop
                0037 [   1] Pop
                0038 [   1] Constant 5 (0)
                0040 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_match_list_pattern_single_arm() {
        // Single arm, no fallback - tests bytecode generation
        check_bytecode(
            "match [1, 2] { [a, b] { a + b } }",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] MakeList 2
                0006 [   1] GetLocal 0
                0008 [   1] Size
                0009 [   1] Constant 2 (2)
                0011 [   1] Eq
                0012 [   1] JumpIfFalse -> 36
                0015 [   1] Pop
                0016 [   1] GetLocal 0
                0018 [   1] Constant 3 (0)
                0020 [   1] Index
                0021 [   1] GetLocal 0
                0023 [   1] Constant 4 (1)
                0025 [   1] Index
                0026 [   1] GetLocal 1
                0028 [   1] GetLocal 2
                0030 [   1] Add
                0031 [   1] PopN 3
                0033 [   1] Jump -> 39
                0036 [   1] Pop
                0037 [   1] Pop
                0038 [   1] Nil
                0039 [   1] Return
            "#]],
        );
    }

    #[test]
    fn compile_match_range_pattern() {
        check_bytecode(
            r#"match 5 { 1..=3 { "low" } 4..10 { "mid" } _ { "high" } }"#,
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (5)
                0002 [   1] Dup
                0003 [   1] RangeCheck 1 3 true
                0009 [   1] JumpIfFalse -> 19
                0012 [   1] Pop
                0013 [   1] Pop
                0014 [   1] Constant 1 ("low")
                0016 [   1] Jump -> 41
                0019 [   1] Pop
                0020 [   1] Dup
                0021 [   1] RangeCheck 4 10 false
                0027 [   1] JumpIfFalse -> 37
                0030 [   1] Pop
                0031 [   1] Pop
                0032 [   1] Constant 2 ("mid")
                0034 [   1] Jump -> 41
                0037 [   1] Pop
                0038 [   1] Pop
                0039 [   1] Constant 3 ("high")
                0041 [   1] Return
            "#]],
        );
    }

    // §7.2 If-let expression
    #[test]
    fn compile_if_let() {
        check_bytecode(
            "if let [x, y] = [1, 2] { x + y } else { 0 }",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (1)
                0002 [   1] Constant 1 (2)
                0004 [   1] MakeList 2
                0006 [   1] GetLocal 0
                0008 [   1] Size
                0009 [   1] Constant 2 (2)
                0011 [   1] Eq
                0012 [   1] JumpIfFalse -> 36
                0015 [   1] Pop
                0016 [   1] GetLocal 0
                0018 [   1] Constant 3 (0)
                0020 [   1] Index
                0021 [   1] GetLocal 0
                0023 [   1] Constant 4 (1)
                0025 [   1] Index
                0026 [   1] GetLocal 1
                0028 [   1] GetLocal 2
                0030 [   1] Add
                0031 [   1] PopN 3
                0033 [   1] Jump -> 40
                0036 [   1] Pop
                0037 [   1] Pop
                0038 [   1] Constant 5 (0)
                0040 [   1] Return
            "#]],
        );
    }

    // §7.3 Return statement
    #[test]
    fn compile_return_in_function() {
        let compiled = compile_expr("|x| { if x < 0 { return 0 }; x }");
        let inner_fn = &compiled.chunk.functions[0];
        let disasm = inner_fn.chunk.disassemble("inner");
        assert!(disasm.contains("Return"));
    }

    // §7.4 Break statement
    #[test]
    fn compile_break() {
        check_bytecode(
            "{ break 42 }",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (42)
                0002 [   1] Break
                0003 [   1] Return
            "#]],
        );
    }
}

#[cfg(test)]
mod runtime_tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::vm::compiler::Compiler;
    use crate::vm::runtime::VM;
    use crate::vm::value::Value;
    use im_rc::{HashMap, HashSet, Vector};
    use ordered_float::OrderedFloat;
    use std::rc::Rc;

    /// Helper to evaluate source code and return the result
    fn eval(source: &str) -> Result<Value, String> {
        let tokens = Lexer::new(source).tokenize().map_err(|e| e.message)?;
        let program = Parser::new(tokens).parse_program().map_err(|e| e.message)?;

        // Get first statement expression
        let expr = match &program.statements[0].node {
            crate::parser::ast::Stmt::Expr(e) => e,
            _ => return Err("Expected expression statement".to_string()),
        };

        let compiled = Compiler::compile_expression(expr).map_err(|e| e.message)?;

        let mut vm = VM::new();
        vm.run(Rc::new(compiled)).map_err(|e| e.message)
    }

    // ============================================================
    // §2.5 Literal evaluation tests
    // ============================================================

    #[test]
    fn eval_integer_literal() {
        assert_eq!(eval("42"), Ok(Value::Integer(42)));
        assert_eq!(eval("-17"), Ok(Value::Integer(-17)));
        assert_eq!(eval("0"), Ok(Value::Integer(0)));
    }

    #[test]
    fn eval_decimal_literal() {
        assert_eq!(eval("3.15"), Ok(Value::Decimal(OrderedFloat(3.15))));
        assert_eq!(eval("-0.5"), Ok(Value::Decimal(OrderedFloat(-0.5))));
    }

    #[test]
    fn eval_string_literal() {
        assert_eq!(
            eval(r#""hello""#),
            Ok(Value::String(Rc::new("hello".to_string())))
        );
        assert_eq!(
            eval(r#""hello world""#),
            Ok(Value::String(Rc::new("hello world".to_string())))
        );
    }

    #[test]
    fn eval_boolean_literals() {
        assert_eq!(eval("true"), Ok(Value::Boolean(true)));
        assert_eq!(eval("false"), Ok(Value::Boolean(false)));
    }

    #[test]
    fn eval_nil_literal() {
        assert_eq!(eval("nil"), Ok(Value::Nil));
    }

    // ============================================================
    // §4.1 Arithmetic operations
    // ============================================================

    #[test]
    fn eval_integer_addition() {
        assert_eq!(eval("1 + 2"), Ok(Value::Integer(3)));
        assert_eq!(eval("100 + 200"), Ok(Value::Integer(300)));
        assert_eq!(eval("-5 + 3"), Ok(Value::Integer(-2)));
    }

    #[test]
    fn eval_integer_subtraction() {
        assert_eq!(eval("5 - 3"), Ok(Value::Integer(2)));
        assert_eq!(eval("3 - 5"), Ok(Value::Integer(-2)));
        assert_eq!(eval("0 - 10"), Ok(Value::Integer(-10)));
    }

    #[test]
    fn eval_integer_multiplication() {
        assert_eq!(eval("3 * 4"), Ok(Value::Integer(12)));
        assert_eq!(eval("-3 * 4"), Ok(Value::Integer(-12)));
        assert_eq!(eval("0 * 100"), Ok(Value::Integer(0)));
    }

    #[test]
    fn eval_integer_division() {
        // Python-style floored division (floors toward negative infinity)
        assert_eq!(eval("7 / 2"), Ok(Value::Integer(3)));
        assert_eq!(eval("-7 / 2"), Ok(Value::Integer(-4)));
        assert_eq!(eval("7 / -2"), Ok(Value::Integer(-4)));
        assert_eq!(eval("-7 / -2"), Ok(Value::Integer(3)));
    }

    #[test]
    fn eval_integer_modulo() {
        // Floored modulo (Python-style) - result has same sign as divisor
        assert_eq!(eval("7 % 3"), Ok(Value::Integer(1)));
        assert_eq!(eval("-7 % 3"), Ok(Value::Integer(2))); // floor: -7 = -3*3 + 2
        assert_eq!(eval("7 % -3"), Ok(Value::Integer(-2))); // floor: 7 = -2*-3 + (-2)
        assert_eq!(eval("-7 % -3"), Ok(Value::Integer(-1)));
    }

    #[test]
    fn eval_decimal_arithmetic() {
        assert_eq!(eval("1.5 + 2.5"), Ok(Value::Decimal(OrderedFloat(4.0))));
        assert_eq!(eval("5.0 - 2.5"), Ok(Value::Decimal(OrderedFloat(2.5))));
        assert_eq!(eval("2.5 * 4.0"), Ok(Value::Decimal(OrderedFloat(10.0))));
        assert_eq!(eval("7.0 / 2.0"), Ok(Value::Decimal(OrderedFloat(3.5))));
    }

    #[test]
    fn eval_negation() {
        assert_eq!(eval("-42"), Ok(Value::Integer(-42)));
        assert_eq!(eval("--42"), Ok(Value::Integer(42)));
        assert_eq!(eval("-3.15"), Ok(Value::Decimal(OrderedFloat(-3.15))));
    }

    #[test]
    fn eval_complex_arithmetic() {
        assert_eq!(eval("1 + 2 * 3"), Ok(Value::Integer(7))); // Precedence: 1 + (2 * 3)
        assert_eq!(eval("(1 + 2) * 3"), Ok(Value::Integer(9)));
        assert_eq!(eval("10 - 3 - 2"), Ok(Value::Integer(5))); // Left associative
        assert_eq!(eval("2 * 3 + 4 * 5"), Ok(Value::Integer(26)));
    }

    #[test]
    fn eval_division_by_zero() {
        assert!(eval("1 / 0").is_err());
        assert!(eval("1 % 0").is_err());
    }

    // ============================================================
    // §4.1 Type coercion tests
    // ============================================================

    #[test]
    fn eval_type_coercion_left_integer() {
        // Left operand is Integer, result is Integer
        assert_eq!(eval("1 + 2.5"), Ok(Value::Integer(3)));
        assert_eq!(eval("10 - 2.5"), Ok(Value::Integer(7)));
        assert_eq!(eval("4 * 2.5"), Ok(Value::Integer(10)));
    }

    #[test]
    fn eval_type_coercion_left_decimal() {
        // Left operand is Decimal, result is Decimal
        assert_eq!(eval("1.5 + 2"), Ok(Value::Decimal(OrderedFloat(3.5))));
        assert_eq!(eval("10.0 - 3"), Ok(Value::Decimal(OrderedFloat(7.0))));
        assert_eq!(eval("2.5 * 4"), Ok(Value::Decimal(OrderedFloat(10.0))));
        assert_eq!(eval("7.0 / 2"), Ok(Value::Decimal(OrderedFloat(3.5))));
    }

    // ============================================================
    // §4.2-4.3 Comparison operators
    // ============================================================

    #[test]
    fn eval_integer_comparison() {
        assert_eq!(eval("1 < 2"), Ok(Value::Boolean(true)));
        assert_eq!(eval("2 < 1"), Ok(Value::Boolean(false)));
        assert_eq!(eval("1 <= 1"), Ok(Value::Boolean(true)));
        assert_eq!(eval("1 <= 2"), Ok(Value::Boolean(true)));
        assert_eq!(eval("2 > 1"), Ok(Value::Boolean(true)));
        assert_eq!(eval("1 > 2"), Ok(Value::Boolean(false)));
        assert_eq!(eval("1 >= 1"), Ok(Value::Boolean(true)));
        assert_eq!(eval("2 >= 1"), Ok(Value::Boolean(true)));
    }

    #[test]
    fn eval_decimal_comparison() {
        assert_eq!(eval("1.5 < 2.5"), Ok(Value::Boolean(true)));
        assert_eq!(eval("2.5 > 1.5"), Ok(Value::Boolean(true)));
    }

    #[test]
    fn eval_mixed_numeric_comparison() {
        assert_eq!(eval("1 < 1.5"), Ok(Value::Boolean(true)));
        assert_eq!(eval("1.5 > 1"), Ok(Value::Boolean(true)));
    }

    #[test]
    fn eval_string_comparison() {
        assert_eq!(eval(r#""abc" < "def""#), Ok(Value::Boolean(true)));
        assert_eq!(eval(r#""abc" < "abc""#), Ok(Value::Boolean(false)));
        assert_eq!(eval(r#""abc" <= "abc""#), Ok(Value::Boolean(true)));
    }

    #[test]
    fn eval_equality() {
        assert_eq!(eval("1 == 1"), Ok(Value::Boolean(true)));
        assert_eq!(eval("1 == 2"), Ok(Value::Boolean(false)));
        assert_eq!(eval("1 != 2"), Ok(Value::Boolean(true)));
        assert_eq!(eval("1 != 1"), Ok(Value::Boolean(false)));
        assert_eq!(eval(r#""a" == "a""#), Ok(Value::Boolean(true)));
        assert_eq!(eval(r#""a" == "b""#), Ok(Value::Boolean(false)));
        assert_eq!(eval("true == true"), Ok(Value::Boolean(true)));
        assert_eq!(eval("true == false"), Ok(Value::Boolean(false)));
        assert_eq!(eval("nil == nil"), Ok(Value::Boolean(true)));
    }

    // ============================================================
    // §4.4 Logical operators
    // ============================================================

    #[test]
    fn eval_logical_not() {
        assert_eq!(eval("!true"), Ok(Value::Boolean(false)));
        assert_eq!(eval("!false"), Ok(Value::Boolean(true)));
        assert_eq!(eval("!0"), Ok(Value::Boolean(true))); // 0 is falsy
        assert_eq!(eval("!1"), Ok(Value::Boolean(false))); // 1 is truthy
        assert_eq!(eval("!nil"), Ok(Value::Boolean(true)));
        assert_eq!(eval(r#"!"""#), Ok(Value::Boolean(true))); // Empty string is falsy
        assert_eq!(eval(r#"!"hello""#), Ok(Value::Boolean(false)));
    }

    #[test]
    fn eval_logical_and() {
        // Returns first falsy value, or last value if all truthy
        assert_eq!(eval("true && true"), Ok(Value::Boolean(true)));
        assert_eq!(eval("true && false"), Ok(Value::Boolean(false)));
        assert_eq!(eval("false && true"), Ok(Value::Boolean(false)));
        assert_eq!(eval("1 && 2"), Ok(Value::Integer(2))); // Both truthy, return last
        assert_eq!(eval("0 && 2"), Ok(Value::Integer(0))); // First is falsy
        assert_eq!(eval("1 && nil"), Ok(Value::Nil)); // Second is falsy
    }

    #[test]
    fn eval_logical_or() {
        // Returns first truthy value, or last value if all falsy
        assert_eq!(eval("true || false"), Ok(Value::Boolean(true)));
        assert_eq!(eval("false || true"), Ok(Value::Boolean(true)));
        assert_eq!(eval("false || false"), Ok(Value::Boolean(false)));
        assert_eq!(eval("1 || 0"), Ok(Value::Integer(1))); // First is truthy
        assert_eq!(eval("0 || 2"), Ok(Value::Integer(2))); // First is falsy, return second
        assert_eq!(eval("false || nil"), Ok(Value::Nil)); // Both falsy, return last
    }

    #[test]
    fn eval_short_circuit_and() {
        // Second operand should not be evaluated if first is falsy
        // We can test this by using side effects if we had them, but for now
        // just verify behavior is correct
        assert_eq!(eval("false && true"), Ok(Value::Boolean(false)));
    }

    #[test]
    fn eval_short_circuit_or() {
        // Second operand should not be evaluated if first is truthy
        assert_eq!(eval("true || false"), Ok(Value::Boolean(true)));
    }

    // ============================================================
    // §3.5 List operations
    // ============================================================

    #[test]
    fn eval_empty_list() {
        assert_eq!(eval("[]"), Ok(Value::List(Vector::new())));
    }

    #[test]
    fn eval_list_with_elements() {
        let result = eval("[1, 2, 3]").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 3);
                assert_eq!(v[0], Value::Integer(1));
                assert_eq!(v[1], Value::Integer(2));
                assert_eq!(v[2], Value::Integer(3));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_list_concatenation() {
        let result = eval("[1, 2] + [3, 4]").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 4);
                assert_eq!(v[0], Value::Integer(1));
                assert_eq!(v[3], Value::Integer(4));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_list_repetition() {
        let result = eval("[1, 2] * 3").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 6);
                assert_eq!(v[0], Value::Integer(1));
                assert_eq!(v[2], Value::Integer(1));
                assert_eq!(v[5], Value::Integer(2));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_list_index() {
        assert_eq!(eval("[10, 20, 30][0]"), Ok(Value::Integer(10)));
        assert_eq!(eval("[10, 20, 30][1]"), Ok(Value::Integer(20)));
        assert_eq!(eval("[10, 20, 30][2]"), Ok(Value::Integer(30)));
    }

    #[test]
    fn eval_list_negative_index() {
        assert_eq!(eval("[10, 20, 30][-1]"), Ok(Value::Integer(30)));
        assert_eq!(eval("[10, 20, 30][-2]"), Ok(Value::Integer(20)));
        assert_eq!(eval("[10, 20, 30][-3]"), Ok(Value::Integer(10)));
    }

    #[test]
    fn eval_list_out_of_bounds() {
        assert_eq!(eval("[1, 2, 3][10]"), Ok(Value::Nil));
        assert_eq!(eval("[1, 2, 3][-10]"), Ok(Value::Nil));
    }

    // ============================================================
    // §3.6 Set operations
    // ============================================================

    #[test]
    fn eval_empty_set() {
        assert_eq!(eval("{}"), Ok(Value::Set(HashSet::new())));
    }

    #[test]
    fn eval_set_with_elements() {
        let result = eval("{1, 2, 3}").unwrap();
        match result {
            Value::Set(s) => {
                assert_eq!(s.len(), 3);
                assert!(s.contains(&Value::Integer(1)));
                assert!(s.contains(&Value::Integer(2)));
                assert!(s.contains(&Value::Integer(3)));
            }
            _ => panic!("Expected set"),
        }
    }

    #[test]
    fn eval_set_deduplication() {
        let result = eval("{1, 2, 2, 1, 3}").unwrap();
        match result {
            Value::Set(s) => {
                assert_eq!(s.len(), 3);
            }
            _ => panic!("Expected set"),
        }
    }

    #[test]
    fn eval_single_element_set() {
        // Single-element sets require trailing comma per LANG.txt spec
        let result = eval("{42,}").unwrap();
        match result {
            Value::Set(s) => {
                assert_eq!(s.len(), 1);
                assert!(s.contains(&Value::Integer(42)));
            }
            _ => panic!("Expected set, got {:?}", result),
        }
    }

    #[test]
    fn eval_single_element_set_vs_block() {
        // {42,} is a set, {42} is also a set (matching reference implementation)
        let set_result = eval("{42,}").unwrap();
        let also_set_result = eval("{42}").unwrap();

        assert!(matches!(set_result, Value::Set(_)));
        assert!(matches!(also_set_result, Value::Set(_)));
    }

    #[test]
    fn eval_set_union() {
        let result = eval("{1, 2} + {2, 3}").unwrap();
        match result {
            Value::Set(s) => {
                assert_eq!(s.len(), 3);
                assert!(s.contains(&Value::Integer(1)));
                assert!(s.contains(&Value::Integer(2)));
                assert!(s.contains(&Value::Integer(3)));
            }
            _ => panic!("Expected set"),
        }
    }

    #[test]
    fn eval_set_difference() {
        // Note: Using {2, 2} instead of {2} because single-element {n} parses as block
        let result = eval("{1, 2, 3} - {2, 2}").unwrap();
        match result {
            Value::Set(s) => {
                assert_eq!(s.len(), 2);
                assert!(s.contains(&Value::Integer(1)));
                assert!(s.contains(&Value::Integer(3)));
                assert!(!s.contains(&Value::Integer(2)));
            }
            _ => panic!("Expected set"),
        }
    }

    // ============================================================
    // §3.7 Dictionary operations
    // ============================================================

    #[test]
    fn eval_empty_dict() {
        assert_eq!(eval("#{}"), Ok(Value::Dict(HashMap::new())));
    }

    #[test]
    fn eval_dict_with_entries() {
        let result = eval(r#"#{"a": 1, "b": 2}"#).unwrap();
        match result {
            Value::Dict(d) => {
                assert_eq!(d.len(), 2);
                assert_eq!(
                    d.get(&Value::String(Rc::new("a".to_string()))),
                    Some(&Value::Integer(1))
                );
                assert_eq!(
                    d.get(&Value::String(Rc::new("b".to_string()))),
                    Some(&Value::Integer(2))
                );
            }
            _ => panic!("Expected dict"),
        }
    }

    #[test]
    fn eval_dict_merge() {
        let result = eval(r#"#{"a": 1} + #{"b": 2}"#).unwrap();
        match result {
            Value::Dict(d) => {
                assert_eq!(d.len(), 2);
            }
            _ => panic!("Expected dict"),
        }
    }

    #[test]
    fn eval_dict_merge_right_precedence() {
        // Right dict values override left
        let result = eval(r#"#{"a": 1} + #{"a": 2}"#).unwrap();
        match result {
            Value::Dict(d) => {
                assert_eq!(d.len(), 1);
                assert_eq!(
                    d.get(&Value::String(Rc::new("a".to_string()))),
                    Some(&Value::Integer(2))
                );
            }
            _ => panic!("Expected dict"),
        }
    }

    #[test]
    fn eval_dict_access() {
        assert_eq!(eval(r#"#{"a": 42}["a"]"#), Ok(Value::Integer(42)));
    }

    #[test]
    fn eval_dict_missing_key() {
        assert_eq!(eval(r#"#{"a": 1}["b"]"#), Ok(Value::Nil));
    }

    // ============================================================
    // §3.3 String operations
    // ============================================================

    #[test]
    fn eval_string_concatenation() {
        assert_eq!(
            eval(r#""hello" + " " + "world""#),
            Ok(Value::String(Rc::new("hello world".to_string())))
        );
    }

    #[test]
    fn eval_string_repetition() {
        assert_eq!(
            eval(r#""ab" * 3"#),
            Ok(Value::String(Rc::new("ababab".to_string())))
        );
    }

    #[test]
    fn eval_string_index() {
        assert_eq!(
            eval(r#""hello"[0]"#),
            Ok(Value::String(Rc::new("h".to_string())))
        );
        assert_eq!(
            eval(r#""hello"[4]"#),
            Ok(Value::String(Rc::new("o".to_string())))
        );
    }

    #[test]
    fn eval_string_negative_index() {
        assert_eq!(
            eval(r#""hello"[-1]"#),
            Ok(Value::String(Rc::new("o".to_string())))
        );
        assert_eq!(
            eval(r#""hello"[-5]"#),
            Ok(Value::String(Rc::new("h".to_string())))
        );
    }

    // ============================================================
    // §3.4 Range creation
    // ============================================================

    #[test]
    fn eval_exclusive_range() {
        assert_eq!(
            eval("1..5"),
            Ok(Value::Range {
                start: 1,
                end: Some(5),
                inclusive: false
            })
        );
    }

    #[test]
    fn eval_inclusive_range() {
        assert_eq!(
            eval("1..=5"),
            Ok(Value::Range {
                start: 1,
                end: Some(5),
                inclusive: true
            })
        );
    }

    #[test]
    fn eval_unbounded_range() {
        assert_eq!(
            eval("1.."),
            Ok(Value::Range {
                start: 1,
                end: None,
                inclusive: false
            })
        );
    }

    // ============================================================
    // §7.1 Control flow - if expression
    // ============================================================

    #[test]
    fn eval_if_true_branch() {
        assert_eq!(eval("if true { 1 } else { 2 }"), Ok(Value::Integer(1)));
    }

    #[test]
    fn eval_if_false_branch() {
        assert_eq!(eval("if false { 1 } else { 2 }"), Ok(Value::Integer(2)));
    }

    #[test]
    fn eval_if_without_else() {
        assert_eq!(eval("if false { 1 }"), Ok(Value::Nil));
        assert_eq!(eval("if true { 1 }"), Ok(Value::Integer(1)));
    }

    #[test]
    fn eval_if_truthy_conditions() {
        assert_eq!(eval("if 1 { 42 } else { 0 }"), Ok(Value::Integer(42)));
        assert_eq!(eval("if 0 { 42 } else { 0 }"), Ok(Value::Integer(0)));
        assert_eq!(
            eval(r#"if "hello" { 42 } else { 0 }"#),
            Ok(Value::Integer(42))
        );
        assert_eq!(eval(r#"if "" { 42 } else { 0 }"#), Ok(Value::Integer(0)));
    }

    #[test]
    fn eval_nested_if() {
        assert_eq!(
            eval("if true { if false { 1 } else { 2 } } else { 3 }"),
            Ok(Value::Integer(2))
        );
    }

    // ============================================================
    // §8 Functions
    // ============================================================

    #[test]
    fn eval_function_call_simple() {
        assert_eq!(eval("(|x| x + 1)(5)"), Ok(Value::Integer(6)));
    }

    #[test]
    fn eval_function_call_multiple_args() {
        assert_eq!(eval("(|a, b| a + b)(3, 4)"), Ok(Value::Integer(7)));
    }

    #[test]
    fn eval_function_call_no_args() {
        assert_eq!(eval("(|| 42)()"), Ok(Value::Integer(42)));
    }

    #[test]
    fn eval_function_nested_call() {
        // Moved to closure tests section
        assert_eq!(eval("(|x| (|y| x + y)(3))(2)"), Ok(Value::Integer(5)));
    }

    #[test]
    fn eval_function_returning_function() {
        // Moved to closure tests section
        assert_eq!(eval("((|x| |y| x + y)(10))(5)"), Ok(Value::Integer(15)));
    }

    #[test]
    fn eval_function_wrong_arity() {
        // Too many arguments is still an error
        assert!(eval("(|x| x)(1, 2)").is_err());
        // Too few arguments now returns a partial application (auto-currying)
        // So this is no longer an error - test moved to auto-currying tests
    }

    // ============================================================
    // §8.4 Partial application
    // ============================================================

    #[test]
    fn eval_partial_application_right() {
        // _ + 1 creates a function that adds 1 to its argument
        assert_eq!(eval("(_ + 1)(5)"), Ok(Value::Integer(6)));
    }

    #[test]
    fn eval_partial_application_left() {
        // 10 - _ creates a function that subtracts its argument from 10
        assert_eq!(eval("(10 - _)(3)"), Ok(Value::Integer(7)));
    }

    #[test]
    fn eval_partial_application_both() {
        // _ / _ creates a function taking two arguments
        assert_eq!(eval("(_ / _)(10, 2)"), Ok(Value::Integer(5)));
    }

    #[test]
    fn eval_partial_application_mul() {
        assert_eq!(eval("(_ * 2)(5)"), Ok(Value::Integer(10)));
    }

    // ============================================================
    // §4.7 Pipeline operator
    // ============================================================

    #[test]
    fn eval_pipeline_simple() {
        // 5 |> (|x| x + 1) === (|x| x + 1)(5)
        assert_eq!(eval("5 |> (|x| x + 1)"), Ok(Value::Integer(6)));
    }

    #[test]
    fn eval_pipeline_chain() {
        // 5 |> (|x| x + 1) |> (|x| x * 2) === (|x| x * 2)((|x| x + 1)(5))
        assert_eq!(
            eval("5 |> (|x| x + 1) |> (|x| x * 2)"),
            Ok(Value::Integer(12))
        );
    }

    #[test]
    fn eval_pipeline_with_partial() {
        // 5 |> (_ + 1) === (_ + 1)(5)
        assert_eq!(eval("5 |> (_ + 1)"), Ok(Value::Integer(6)));
    }

    #[test]
    fn eval_pipeline_result_in_expression() {
        // Regression test: a pipeline result should be usable in outer expressions
        // without the outer expression becoming a partial application.
        // (3 |> _ + 1) evaluates to 4, then 4 + 0 should evaluate to 4, not <function>
        assert_eq!(eval("(3 |> _ + 1) + 0"), Ok(Value::Integer(4)));
        assert_eq!(eval("5 > (3 |> _ + 1)"), Ok(Value::Boolean(true)));
        assert_eq!(eval("(3 |> _ + 1) == 4"), Ok(Value::Boolean(true)));
    }

    // ============================================================
    // §5 Variables and bindings
    // ============================================================

    #[test]
    fn eval_let_binding() {
        assert_eq!(eval("{ let x = 42; x }"), Ok(Value::Integer(42)));
    }

    #[test]
    fn eval_let_multiple() {
        assert_eq!(
            eval("{ let x = 10; let y = 20; x + y }"),
            Ok(Value::Integer(30))
        );
    }

    #[test]
    fn eval_let_shadowing() {
        assert_eq!(eval("{ let x = 1; let x = 2; x }"), Ok(Value::Integer(2)));
    }

    #[test]
    fn eval_let_nested_scope() {
        assert_eq!(
            eval("{ let x = 1; { let x = 2; x }; x }"),
            Ok(Value::Integer(1))
        );
    }

    #[test]
    fn eval_let_mut_assignment() {
        assert_eq!(eval("{ let mut x = 1; x = 2; x }"), Ok(Value::Integer(2)));
    }

    // ============================================================
    // §5.4 Destructuring
    // ============================================================

    #[test]
    fn eval_destructuring_list() {
        assert_eq!(
            eval("{ let [a, b] = [1, 2]; a + b }"),
            Ok(Value::Integer(3))
        );
    }

    #[test]
    fn eval_destructuring_nested() {
        assert_eq!(
            eval("{ let [[a, b], c] = [[1, 2], 3]; a + b + c }"),
            Ok(Value::Integer(6))
        );
    }

    // ============================================================
    // §7.2 Match expression
    // ============================================================

    #[test]
    fn eval_match_literal() {
        assert_eq!(
            eval(r#"match 1 { 1 { "one" } _ { "other" } }"#),
            Ok(Value::String(Rc::new("one".to_string())))
        );
        assert_eq!(
            eval(r#"match 2 { 1 { "one" } _ { "other" } }"#),
            Ok(Value::String(Rc::new("other".to_string())))
        );
    }

    #[test]
    fn eval_match_binding() {
        assert_eq!(eval("match 42 { x { x + 1 } }"), Ok(Value::Integer(43)));
    }

    #[test]
    fn eval_match_with_guard() {
        assert_eq!(
            eval("match 5 { x if x > 3 { x * 2 } _ { 0 } }"),
            Ok(Value::Integer(10))
        );
        assert_eq!(
            eval("match 2 { x if x > 3 { x * 2 } _ { 0 } }"),
            Ok(Value::Integer(0))
        );
    }

    #[test]
    fn eval_match_list_pattern_with_guard() {
        // Guard succeeds - binds from first arm
        assert_eq!(
            eval("match [10, 20] { [start, end] if start > 5 { start + end } _ { 0 } }"),
            Ok(Value::Integer(30))
        );
        // Guard fails - falls through to second arm with list pattern
        assert_eq!(
            eval("match [10, 20] { [start, end] if start > 100 { start } [a, b] { a + b } }"),
            Ok(Value::Integer(30))
        );
        // Guard fails - falls through to wildcard
        assert_eq!(
            eval("match [10, 20] { [start, end] if start > 100 { start } x { x } }"),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(10),
                Value::Integer(20)
            ])))
        );
    }

    #[test]
    fn eval_match_nested_list_pattern_size_mismatch() {
        // Outer size doesn't match - fall through to second arm
        assert_eq!(
            eval("match [[1, 2], [3, 4], [5, 6]] { [[a, b], _] { a } [[x, y], ..rest] { x + y } }"),
            Ok(Value::Integer(3))
        );
    }

    #[test]
    fn eval_match_wildcard() {
        assert_eq!(eval("match 999 { _ { 42 } }"), Ok(Value::Integer(42)));
    }

    #[test]
    fn eval_match_list_pattern() {
        // Simple list pattern with two elements
        assert_eq!(
            eval("match [1, 2] { [a, b] { a + b } _ { 0 } }"),
            Ok(Value::Integer(3))
        );
    }

    #[test]
    fn eval_match_list_pattern_via_statements() {
        // Test via compile_statements (as CLI does)
        let tokens = Lexer::new("match [1, 2] { [a, b] { a + b } _ { 0 } }")
            .tokenize()
            .unwrap();
        let program = Parser::new(tokens).parse_program().unwrap();
        let compiled = Compiler::compile_statements(&program.statements).unwrap();
        let mut vm = VM::new();
        let result = vm.run(Rc::new(compiled));
        assert_eq!(result.map_err(|e| e.message), Ok(Value::Integer(3)));
    }

    #[test]
    fn eval_match_list_pattern_no_fallback() {
        // Test without wildcard fallback
        let tokens = Lexer::new("match [1, 2] { [a, b] { a + b } }")
            .tokenize()
            .unwrap();
        let program = Parser::new(tokens).parse_program().unwrap();
        let compiled = Compiler::compile_statements(&program.statements).unwrap();
        let mut vm = VM::new();
        let result = vm.run(Rc::new(compiled));
        assert_eq!(result.map_err(|e| e.message), Ok(Value::Integer(3)));
    }

    #[test]
    fn eval_match_list_pattern_literal_and_identifier() {
        // Test list pattern with literal followed by identifier binding
        assert_eq!(
            eval(r#"match ["+", 42] { ["+", v] { v } }"#),
            Ok(Value::Integer(42))
        );
    }

    #[test]
    fn eval_match_list_pattern_closure_capture() {
        // Test that identifiers bound in list patterns can be captured by closures
        assert_eq!(
            eval(r#"(|x| match x { [a, v] { _ + v } })(["+", 10])(5)"#),
            Ok(Value::Integer(15))
        );
    }

    // ============================================================
    // §6.3 Block expressions
    // ============================================================

    #[test]
    fn eval_block_returns_last() {
        assert_eq!(eval("{ 1; 2; 3 }"), Ok(Value::Integer(3)));
    }

    #[test]
    fn eval_block_single_expr() {
        // { 42 } is parsed as a set (matching reference implementation)
        assert!(matches!(eval("{ 42 }").unwrap(), Value::Set(_)));
    }

    // ============================================================
    // Edge cases and error handling
    // ============================================================

    #[test]
    fn eval_string_coercion() {
        // String coercion: only String + any produces String (not any + String)
        // This matches the Rust and TypeScript implementations
        assert_eq!(
            eval(r#""hello" + 1"#),
            Ok(Value::String(Rc::new("hello1".to_string())))
        );
        assert_eq!(
            eval(r#""" + 42"#),
            Ok(Value::String(Rc::new("42".to_string())))
        );
        assert_eq!(
            eval(r#""hello" + 1.5"#),
            Ok(Value::String(Rc::new("hello1.5".to_string())))
        );
        // Integer + String is NOT supported - produces an error
        assert!(eval(r#"1 + "hello""#).is_err());
    }

    #[test]
    fn eval_type_error_subtract_string() {
        assert!(eval(r#""hello" - "h""#).is_err());
    }

    #[test]
    fn eval_type_error_multiply_strings() {
        assert!(eval(r#""a" * "b""#).is_err());
    }

    #[test]
    fn eval_type_error_compare_list() {
        assert!(eval("[1, 2] < [1, 3]").is_err());
    }

    #[test]
    fn eval_call_non_function() {
        assert!(eval("42(1)").is_err());
        assert!(eval(r#""hello"()"#).is_err());
    }

    #[test]
    fn eval_undefined_variable() {
        assert!(eval("undefined_var").is_err());
    }

    // ============================================================
    // §8.3 Closures - Phase 8
    // ============================================================

    #[test]
    fn eval_closure_capture_local() {
        // Basic closure capturing a variable from enclosing scope
        assert_eq!(eval("(|x| (|y| x + y)(3))(2)"), Ok(Value::Integer(5)));
    }

    #[test]
    fn eval_closure_make_adder() {
        // The make_adder example from LANG.txt §8.3
        assert_eq!(eval("((|x| |y| x + y)(5))(3)"), Ok(Value::Integer(8)));
    }

    #[test]
    fn eval_closure_nested() {
        // Nested closures with multiple captures
        assert_eq!(
            eval("((|a| (|b| (|c| a + b + c)(3))(2))(1))"),
            Ok(Value::Integer(6))
        );
    }

    #[test]
    fn eval_closure_capture_upvalue() {
        // Capture from grandparent scope
        assert_eq!(
            eval("((|x| |y| |z| x + y + z)(1))(2)(3)"),
            Ok(Value::Integer(6))
        );
    }

    #[test]
    fn eval_closure_mutable_capture_debug() {
        // Debug: what does make_counter() return?
        let result = eval(
            r#"{
                let make_counter = || {
                    let mut count = 0;
                    || {
                        count = count + 1;
                        count
                    }
                };
                make_counter()
            }"#,
        );
        match &result {
            Ok(Value::Function(_)) => { /* expected */ }
            other => panic!("Expected Function, got {:?}", other),
        }
    }

    #[test]
    fn eval_closure_mutable_capture_simple() {
        // Simpler version: just create the closure and call it once
        assert_eq!(
            eval(
                r#"{
                    let make_counter = || {
                        let mut count = 0;
                        || {
                            count = count + 1;
                            count
                        }
                    };
                    let counter = make_counter();
                    counter()
                }"#
            ),
            Ok(Value::Integer(1))
        );
    }

    #[test]
    fn eval_closure_mutable_capture() {
        // Mutable variable capture - counter example from LANG.txt §8.3
        assert_eq!(
            eval(
                r#"{
                    let counter = (|| {
                        let mut count = 0;
                        || {
                            count = count + 1;
                            count
                        }
                    })();
                    let a = counter();
                    let b = counter();
                    let c = counter();
                    [a, b, c]
                }"#
            ),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3)
            ])))
        );
    }

    #[test]
    fn eval_closure_upvalue_close_over() {
        // Ensure upvalues are closed over when the enclosing scope exits
        assert_eq!(
            eval(
                r#"{
                    let make_counter = || {
                        let mut n = 0;
                        || { n = n + 1; n }
                    };
                    let c1 = make_counter();
                    let c2 = make_counter();
                    [c1(), c1(), c2(), c1()]
                }"#
            ),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(1),
                Value::Integer(3)
            ])))
        );
    }

    #[test]
    fn eval_closure_in_pipeline() {
        // Closures used in pipeline - LANG.txt §4.7 specifies
        // x |> f(a) compiles to f(a, x), not (f(a))(x)
        // So we use a 2-argument function
        assert_eq!(
            eval(
                r#"{
                    let add = |x, y| x + y;
                    5 |> add(10)
                }"#
            ),
            Ok(Value::Integer(15))
        );
    }

    #[test]
    fn eval_closure_curried_call() {
        // Curried function call (not pipeline)
        assert_eq!(
            eval(
                r#"{
                    let add = |x| |y| x + y;
                    add(10)(5)
                }"#
            ),
            Ok(Value::Integer(15))
        );
    }

    // ============================================================
    // §11.1 Built-in Type Conversion Functions - Phase 9
    // ============================================================

    #[test]
    fn eval_builtin_int_from_integer() {
        assert_eq!(eval("int(5)"), Ok(Value::Integer(5)));
        assert_eq!(eval("int(-42)"), Ok(Value::Integer(-42)));
    }

    #[test]
    fn eval_builtin_int_from_decimal() {
        // Rounds to nearest, half away from zero
        assert_eq!(eval("int(3.7)"), Ok(Value::Integer(4)));
        assert_eq!(eval("int(3.5)"), Ok(Value::Integer(4)));
        assert_eq!(eval("int(3.2)"), Ok(Value::Integer(3)));
        assert_eq!(eval("int(-3.5)"), Ok(Value::Integer(-4)));
        assert_eq!(eval("int(-3.7)"), Ok(Value::Integer(-4)));
    }

    #[test]
    fn eval_builtin_int_from_string() {
        assert_eq!(eval(r#"int("42")"#), Ok(Value::Integer(42)));
        assert_eq!(eval(r#"int("-17")"#), Ok(Value::Integer(-17)));
        assert_eq!(eval(r#"int("abc")"#), Ok(Value::Integer(0)));
    }

    #[test]
    fn eval_builtin_int_from_boolean() {
        assert_eq!(eval("int(true)"), Ok(Value::Integer(1)));
        assert_eq!(eval("int(false)"), Ok(Value::Integer(0)));
    }

    #[test]
    fn eval_builtin_ints() {
        let result = eval(r#"ints("1,2,3")"#).unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 3);
                assert_eq!(v[0], Value::Integer(1));
                assert_eq!(v[1], Value::Integer(2));
                assert_eq!(v[2], Value::Integer(3));
            }
            _ => panic!("Expected list"),
        }

        let result = eval(r#"ints("x: 10, y: -5")"#).unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 2);
                assert_eq!(v[0], Value::Integer(10));
                assert_eq!(v[1], Value::Integer(-5));
            }
            _ => panic!("Expected list"),
        }

        assert_eq!(
            eval(r#"ints("no numbers")"#),
            Ok(Value::List(Vector::new()))
        );
    }

    #[test]
    fn eval_builtin_list_from_list() {
        let result = eval("list([1, 2, 3])").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 3);
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_list_from_set() {
        let result = eval("list({1, 2, 3})").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 3);
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_list_from_string() {
        let result = eval(r#"list("ab")"#).unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 2);
                assert_eq!(v[0], Value::String(Rc::new("a".to_string())));
                assert_eq!(v[1], Value::String(Rc::new("b".to_string())));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_list_from_range() {
        let result = eval("list(1..5)").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 4);
                assert_eq!(v[0], Value::Integer(1));
                assert_eq!(v[3], Value::Integer(4));
            }
            _ => panic!("Expected list"),
        }

        let result = eval("list(1..=5)").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 5);
                assert_eq!(v[4], Value::Integer(5));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_set_from_list() {
        let result = eval("set([1, 2, 2, 3])").unwrap();
        match result {
            Value::Set(s) => {
                assert_eq!(s.len(), 3);
            }
            _ => panic!("Expected set"),
        }
    }

    #[test]
    fn eval_builtin_set_from_string() {
        let result = eval(r#"set("aab")"#).unwrap();
        match result {
            Value::Set(s) => {
                assert_eq!(s.len(), 2);
            }
            _ => panic!("Expected set"),
        }
    }

    #[test]
    fn eval_builtin_dict_from_list() {
        let result = eval("dict([[1, 2], [3, 4]])").unwrap();
        match result {
            Value::Dict(d) => {
                assert_eq!(d.len(), 2);
                assert_eq!(d.get(&Value::Integer(1)), Some(&Value::Integer(2)));
                assert_eq!(d.get(&Value::Integer(3)), Some(&Value::Integer(4)));
            }
            _ => panic!("Expected dict"),
        }
    }

    // ============================================================
    // §11.2 Built-in Collection Access Functions - Phase 9
    // ============================================================

    #[test]
    fn eval_builtin_get_list() {
        assert_eq!(eval("get(1, [1, 2, 3])"), Ok(Value::Integer(2)));
        assert_eq!(eval("get(5, [1, 2])"), Ok(Value::Nil));
        assert_eq!(eval("get(-1, [1, 2, 3])"), Ok(Value::Integer(3)));
    }

    #[test]
    fn eval_builtin_get_dict() {
        assert_eq!(
            eval(r#"get("a", #{"a": 1, "b": 2})"#),
            Ok(Value::Integer(1))
        );
        assert_eq!(eval(r#"get("c", #{"a": 1})"#), Ok(Value::Nil));
    }

    #[test]
    fn eval_builtin_get_string() {
        assert_eq!(
            eval(r#"get(1, "ab")"#),
            Ok(Value::String(Rc::new("b".to_string())))
        );
    }

    #[test]
    fn eval_builtin_get_set() {
        assert_eq!(eval("get(1, {1, 2})"), Ok(Value::Integer(1)));
        assert_eq!(eval("get(3, {1, 2})"), Ok(Value::Nil));
    }

    #[test]
    fn eval_builtin_size_list() {
        assert_eq!(eval("size([1, 2, 3])"), Ok(Value::Integer(3)));
        assert_eq!(eval("size([])"), Ok(Value::Integer(0)));
    }

    #[test]
    fn eval_builtin_size_set() {
        assert_eq!(eval("size({1, 2, 3})"), Ok(Value::Integer(3)));
    }

    #[test]
    fn eval_builtin_size_dict() {
        assert_eq!(eval(r#"size(#{"a": 1, "b": 2})"#), Ok(Value::Integer(2)));
    }

    #[test]
    fn eval_builtin_size_string() {
        assert_eq!(eval(r#"size("hello")"#), Ok(Value::Integer(5)));
    }

    #[test]
    fn eval_builtin_size_range() {
        assert_eq!(eval("size(1..5)"), Ok(Value::Integer(4)));
        assert_eq!(eval("size(1..=5)"), Ok(Value::Integer(5)));
    }

    #[test]
    fn eval_builtin_size_lazy_sequence() {
        // size on a LazySequence (from range function) - range is inclusive
        assert_eq!(eval("range(1, 10, 1) |> size"), Ok(Value::Integer(10)));
    }

    #[test]
    fn eval_builtin_size_lazy_sequence_with_filter() {
        // size on a filtered LazySequence
        assert_eq!(
            eval("range(1, 11, 1) |> filter(|x| x % 2 == 0) |> size"),
            Ok(Value::Integer(5)) // 2, 4, 6, 8, 10 = 5 elements
        );
    }

    #[test]
    fn eval_builtin_first() {
        assert_eq!(eval("first([1, 2, 3])"), Ok(Value::Integer(1)));
        assert_eq!(eval("first([])"), Ok(Value::Nil));
        assert_eq!(
            eval(r#"first("hello")"#),
            Ok(Value::String(Rc::new("h".to_string())))
        );
        assert_eq!(eval("first(1..5)"), Ok(Value::Integer(1)));
    }

    #[test]
    fn eval_builtin_second() {
        assert_eq!(eval("second([1, 2, 3])"), Ok(Value::Integer(2)));
        assert_eq!(eval("second([1])"), Ok(Value::Nil));
        assert_eq!(
            eval(r#"second("hello")"#),
            Ok(Value::String(Rc::new("e".to_string())))
        );
        assert_eq!(eval("second(1..5)"), Ok(Value::Integer(2)));
    }

    #[test]
    fn eval_builtin_rest() {
        let result = eval("rest([1, 2, 3])").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 2);
                assert_eq!(v[0], Value::Integer(2));
                assert_eq!(v[1], Value::Integer(3));
            }
            _ => panic!("Expected list"),
        }

        assert_eq!(eval("rest([])"), Ok(Value::List(Vector::new())));
    }

    #[test]
    fn eval_builtin_rest_string() {
        assert_eq!(
            eval(r#"rest("hello")"#),
            Ok(Value::String(Rc::new("ello".to_string())))
        );
    }

    #[test]
    fn eval_builtin_keys() {
        let result = eval(r#"keys(#{"a": 1, "b": 2})"#).unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 2);
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_values() {
        let result = eval(r#"values(#{"a": 1, "b": 2})"#).unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 2);
            }
            _ => panic!("Expected list"),
        }
    }

    // ============================================================
    // §11.3 Built-in Collection Modification Functions - Phase 9
    // ============================================================

    #[test]
    fn eval_builtin_push_list() {
        let result = eval("push(3, [1, 2])").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 3);
                assert_eq!(v[2], Value::Integer(3));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_push_set() {
        let result = eval("push(3, {1, 2})").unwrap();
        match result {
            Value::Set(s) => {
                assert_eq!(s.len(), 3);
                assert!(s.contains(&Value::Integer(3)));
            }
            _ => panic!("Expected set"),
        }
    }

    #[test]
    fn eval_builtin_assoc_list() {
        let result = eval("assoc(0, 10, [1, 2])").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v[0], Value::Integer(10));
                assert_eq!(v[1], Value::Integer(2));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_assoc_dict() {
        let result = eval(r#"assoc("c", 3, #{"a": 1, "b": 2})"#).unwrap();
        match result {
            Value::Dict(d) => {
                assert_eq!(d.len(), 3);
                assert_eq!(
                    d.get(&Value::String(Rc::new("c".to_string()))),
                    Some(&Value::Integer(3))
                );
            }
            _ => panic!("Expected dict"),
        }
    }

    // ============================================================
    // §11.4 Built-in Transformation Functions - Phase 10
    // ============================================================

    #[test]
    fn eval_builtin_map_list() {
        // map(_ + 1, [1, 2]) => [2, 3]
        let result = eval("map(_ + 1, [1, 2])").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 2);
                assert_eq!(v[0], Value::Integer(2));
                assert_eq!(v[1], Value::Integer(3));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_map_set() {
        // map(_ + 1, {1, 2}) => {2, 3}
        let result = eval("map(_ + 1, {1, 2})").unwrap();
        match result {
            Value::Set(s) => {
                assert_eq!(s.len(), 2);
                assert!(s.contains(&Value::Integer(2)));
                assert!(s.contains(&Value::Integer(3)));
            }
            _ => panic!("Expected set"),
        }
    }

    #[test]
    fn eval_builtin_map_dict_value_only() {
        // map(_ + 1, #{1: 2, 3: 4}) => #{1: 3, 3: 5}
        let result = eval("map(_ + 1, #{1: 2, 3: 4})").unwrap();
        match result {
            Value::Dict(d) => {
                assert_eq!(d.len(), 2);
                assert_eq!(d.get(&Value::Integer(1)), Some(&Value::Integer(3)));
                assert_eq!(d.get(&Value::Integer(3)), Some(&Value::Integer(5)));
            }
            _ => panic!("Expected dict"),
        }
    }

    #[test]
    fn eval_builtin_map_dict_with_key() {
        // map(|v, k| k + v, #{1: 2, 3: 4}) => #{1: 3, 3: 7}
        let result = eval("map(|v, k| k + v, #{1: 2, 3: 4})").unwrap();
        match result {
            Value::Dict(d) => {
                assert_eq!(d.len(), 2);
                assert_eq!(d.get(&Value::Integer(1)), Some(&Value::Integer(3)));
                assert_eq!(d.get(&Value::Integer(3)), Some(&Value::Integer(7)));
            }
            _ => panic!("Expected dict"),
        }
    }

    #[test]
    fn eval_builtin_map_string() {
        // map(_ * 2, "ab") => ["aa", "bb"]
        let result = eval(r#"map(_ * 2, "ab")"#).unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 2);
                assert_eq!(v[0], Value::String(Rc::new("aa".to_string())));
                assert_eq!(v[1], Value::String(Rc::new("bb".to_string())));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_map_range() {
        // map(_ + 1, 1..5) returns List for bounded ranges (eagerly evaluated)
        let result = eval("map(_ + 1, 1..5)").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 4);
                assert_eq!(v[0], Value::Integer(2));
                assert_eq!(v[1], Value::Integer(3));
                assert_eq!(v[2], Value::Integer(4));
                assert_eq!(v[3], Value::Integer(5));
            }
            _ => panic!("Expected List for bounded range"),
        }
    }

    #[test]
    fn eval_builtin_map_empty_non_inclusive_range() {
        // map over empty non-inclusive range (3..3) returns empty list
        assert_eq!(eval("map(_ + 1, 3..3)"), Ok(Value::List(Vector::new())));
    }

    #[test]
    fn eval_builtin_map_descending_range() {
        // Descending non-inclusive range: 5..3 yields [5, 4]
        assert_eq!(
            eval("map(|x| x, 5..3)"),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(5),
                Value::Integer(4)
            ])))
        );
        // Descending inclusive range: 5..=3 yields [5, 4, 3]
        assert_eq!(
            eval("map(|x| x, 5..=3)"),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(5),
                Value::Integer(4),
                Value::Integer(3)
            ])))
        );
    }

    #[test]
    fn eval_builtin_map_range_comprehensive() {
        // Ascending non-inclusive: 1..3 yields [1, 2]
        assert_eq!(
            eval("map(|x| x, 1..3)"),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(1),
                Value::Integer(2)
            ])))
        );
        // Ascending inclusive: 1..=3 yields [1, 2, 3]
        assert_eq!(
            eval("map(|x| x, 1..=3)"),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3)
            ])))
        );
        // Descending non-inclusive: 3..1 yields [3, 2]
        assert_eq!(
            eval("map(|x| x, 3..1)"),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(3),
                Value::Integer(2)
            ])))
        );
        // Descending inclusive: 3..=1 yields [3, 2, 1]
        assert_eq!(
            eval("map(|x| x, 3..=1)"),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(3),
                Value::Integer(2),
                Value::Integer(1)
            ])))
        );
        // Crossing zero non-inclusive: 1..-1 yields [1, 0]
        assert_eq!(
            eval("map(|x| x, 1..-1)"),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(1),
                Value::Integer(0)
            ])))
        );
        // Crossing zero inclusive: 1..=-1 yields [1, 0, -1]
        assert_eq!(
            eval("map(|x| x, 1..=-1)"),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(1),
                Value::Integer(0),
                Value::Integer(-1)
            ])))
        );
        // Negative ascending non-inclusive: -1..1 yields [-1, 0]
        assert_eq!(
            eval("map(|x| x, -1..1)"),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(-1),
                Value::Integer(0)
            ])))
        );
        // Negative ascending inclusive: -1..=1 yields [-1, 0, 1]
        assert_eq!(
            eval("map(|x| x, -1..=1)"),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(-1),
                Value::Integer(0),
                Value::Integer(1)
            ])))
        );
    }

    #[test]
    fn eval_builtin_map_unbounded_range() {
        // map(|x| x + 1, 1..) returns LazySequence for unbounded ranges
        let result = eval("map(|x| x + 1, 1..)").unwrap();
        match result {
            Value::LazySequence(_) => {
                // LazySequence returned as expected
            }
            _ => panic!("Expected LazySequence for unbounded range"),
        }
    }

    #[test]
    fn eval_builtin_map_range_sum() {
        // map on bounded Range literal returns List, allowing sum to work
        assert_eq!(
            eval("1..=4 |> map(|x| x * 2) |> sum"),
            Ok(Value::Integer(20)) // 2 + 4 + 6 + 8 = 20
        );
    }

    #[test]
    fn eval_builtin_filter_list() {
        // filter(_ % 2, [1, 2, 3, 4]) => [1, 3]
        let result = eval("filter(|v| v % 2, [1, 2, 3, 4])").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 2);
                assert_eq!(v[0], Value::Integer(1));
                assert_eq!(v[1], Value::Integer(3));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_filter_set() {
        // filter(_ == 1, {1, 2}) => {1}
        let result = eval("filter(|v| v == 1, {1, 2})").unwrap();
        match result {
            Value::Set(s) => {
                assert_eq!(s.len(), 1);
                assert!(s.contains(&Value::Integer(1)));
            }
            _ => panic!("Expected set"),
        }
    }

    #[test]
    fn eval_builtin_filter_dict_value_only() {
        // filter(_ == 2, #{1: 2, 3: 4}) => #{1: 2}
        let result = eval("filter(|v| v == 2, #{1: 2, 3: 4})").unwrap();
        match result {
            Value::Dict(d) => {
                assert_eq!(d.len(), 1);
                assert_eq!(d.get(&Value::Integer(1)), Some(&Value::Integer(2)));
            }
            _ => panic!("Expected dict"),
        }
    }

    #[test]
    fn eval_builtin_filter_dict_with_key() {
        // filter(|_, k| k == 3, #{1: 2, 3: 4}) => #{3: 4}
        let result = eval("filter(|_, k| k == 3, #{1: 2, 3: 4})").unwrap();
        match result {
            Value::Dict(d) => {
                assert_eq!(d.len(), 1);
                assert_eq!(d.get(&Value::Integer(3)), Some(&Value::Integer(4)));
            }
            _ => panic!("Expected dict"),
        }
    }

    #[test]
    fn eval_builtin_filter_string() {
        // filter(_ == "a", "ab") => ["a"]
        let result = eval(r#"filter(|c| c == "a", "ab")"#).unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 1);
                assert_eq!(v[0], Value::String(Rc::new("a".to_string())));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_filter_range() {
        // filter(_ % 2, 1..5) returns LazySequence
        // We test by verifying the result is a LazySequence
        let result = eval("filter(|v| v % 2, 1..5)").unwrap();
        match result {
            Value::LazySequence(_) => {
                // LazySequence returned as expected
            }
            _ => panic!("Expected LazySequence"),
        }
    }

    #[test]
    fn eval_builtin_flat_map() {
        // flat_map(|x| [x, x * 2], [1, 2]) => [1, 2, 2, 4]
        let result = eval("flat_map(|x| [x, x * 2], [1, 2])").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 4);
                assert_eq!(v[0], Value::Integer(1));
                assert_eq!(v[1], Value::Integer(2));
                assert_eq!(v[2], Value::Integer(2));
                assert_eq!(v[3], Value::Integer(4));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_flat_map_nested() {
        // flat_map applies mapper to each element, then flattens results
        // flat_map(|x| [x, x * 2], [1, 2]) => [1, 2, 2, 4]
        let result = eval("flat_map(|x| [x, x * 2], [1, 2])").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 4);
                assert_eq!(v[0], Value::Integer(1));
                assert_eq!(v[1], Value::Integer(2));
                assert_eq!(v[2], Value::Integer(2));
                assert_eq!(v[3], Value::Integer(4));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_flat_map_with_pairs() {
        // flat_map should work with zip pairs without unwrapping them
        let result =
            eval("zip(0..3, [\"a\", \"b\", \"c\"]) |> flat_map(|[i, v]| [[i, v]])").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 3);
                // First element is [0, "a"]
                match &v[0] {
                    Value::List(pair) => {
                        assert_eq!(pair[0], Value::Integer(0));
                        assert_eq!(pair[1], Value::String(std::rc::Rc::new("a".to_string())));
                    }
                    _ => panic!("Expected list pair"),
                }
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_filter_map_list() {
        // [1, 2, 3, 4] |> filter_map(|v| if v % 2 { v * 2 }) => [2, 6]
        let result = eval("[1, 2, 3, 4] |> filter_map(|v| if v % 2 { v * 2 })").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 2);
                assert_eq!(v[0], Value::Integer(2));
                assert_eq!(v[1], Value::Integer(6));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_filter_map_set() {
        // {1, 2, 3, 4} |> filter_map(|v| if v % 2 { v * 2 }) => {2, 6}
        let result = eval("{1, 2, 3, 4} |> filter_map(|v| if v % 2 { v * 2 })").unwrap();
        match result {
            Value::Set(s) => {
                assert_eq!(s.len(), 2);
                assert!(s.contains(&Value::Integer(2)));
                assert!(s.contains(&Value::Integer(6)));
            }
            _ => panic!("Expected set"),
        }
    }

    #[test]
    fn eval_builtin_find_map_list() {
        // [1, 2] |> find_map(|v| if v % 2 { v * 2 }) => 2
        assert_eq!(
            eval("[1, 2] |> find_map(|v| if v % 2 { v * 2 })"),
            Ok(Value::Integer(2))
        );
    }

    #[test]
    fn eval_builtin_find_map_not_found() {
        // [2, 4] |> find_map(|v| if v % 2 { v }) => nil
        assert_eq!(
            eval("[2, 4] |> find_map(|v| if v % 2 { v })"),
            Ok(Value::Nil)
        );
    }

    // ============================================================
    // §11.5 Built-in Reduction Functions - Phase 10
    // ============================================================

    #[test]
    fn eval_builtin_reduce_list() {
        // reduce(|a, b| a + b, [1, 2, 3]) => 6
        assert_eq!(
            eval("reduce(|a, b| a + b, [1, 2, 3])"),
            Ok(Value::Integer(6))
        );
    }

    #[test]
    fn eval_builtin_reduce_list_custom_fn() {
        // reduce(|a, b| a * b, [1, 2, 3, 4]) => 24
        assert_eq!(
            eval("reduce(|a, b| a * b, [1, 2, 3, 4])"),
            Ok(Value::Integer(24))
        );
    }

    #[test]
    fn eval_builtin_reduce_string() {
        // reduce(|acc, ch| ch + acc, "ab") => "ba"
        assert_eq!(
            eval(r#"reduce(|acc, ch| ch + acc, "ab")"#),
            Ok(Value::String(Rc::new("ba".to_string())))
        );
    }

    #[test]
    fn eval_builtin_reduce_range() {
        // reduce(|a, b| a + b, 1..5) => 10
        assert_eq!(eval("reduce(|a, b| a + b, 1..5)"), Ok(Value::Integer(10)));
        // reduce(|a, b| a + b, 1..=5) => 15
        assert_eq!(eval("reduce(|a, b| a + b, 1..=5)"), Ok(Value::Integer(15)));
    }

    #[test]
    fn eval_builtin_reduce_empty_error() {
        // reduce on empty collection throws error
        assert!(eval("reduce(|a, b| a + b, [])").is_err());
    }

    #[test]
    fn eval_operator_as_function_add() {
        // Operators as function values per LANG.txt and Phase 15
        // reduce(+, [1, 2, 3]) => 6
        assert_eq!(eval("reduce(+, [1, 2, 3])"), Ok(Value::Integer(6)));
    }

    #[test]
    fn eval_operator_as_function_multiply() {
        // reduce(*, [2, 3, 4]) => 24
        assert_eq!(eval("reduce(*, [2, 3, 4])"), Ok(Value::Integer(24)));
    }

    #[test]
    fn eval_operator_as_function_subtract() {
        // reduce(-, [10, 2, 1]) => 7 (10 - 2 - 1)
        assert_eq!(eval("reduce(-, [10, 2, 1])"), Ok(Value::Integer(7)));
    }

    #[test]
    fn eval_operator_as_function_divide() {
        // reduce(/, [100, 2, 5]) => 10 (100 / 2 / 5)
        assert_eq!(eval("reduce(/, [100, 2, 5])"), Ok(Value::Integer(10)));
    }

    #[test]
    fn eval_operator_as_function_modulo() {
        // Can use % as a function value
        let code = r#"{ let mod = %; mod(10, 3) }"#;
        assert_eq!(eval(code), Ok(Value::Integer(1)));
    }

    #[test]
    fn eval_operator_as_function_direct_call() {
        // Can call operators directly as functions (with parentheses)
        assert_eq!(eval("(+)(5, 3)"), Ok(Value::Integer(8)));
        assert_eq!(eval("(*)(4, 7)"), Ok(Value::Integer(28)));
        assert_eq!(eval("(-)(10, 3)"), Ok(Value::Integer(7)));
        assert_eq!(eval("(/)(20, 4)"), Ok(Value::Integer(5)));
        assert_eq!(eval("(%)(10, 3)"), Ok(Value::Integer(1)));

        // Some operators can be called without parentheses (those that aren't prefix unary)
        assert_eq!(eval("+(1, 2)"), Ok(Value::Integer(3)));
        assert_eq!(eval("*(6, 7)"), Ok(Value::Integer(42)));
        // Note: -(10, 3) doesn't work because - is prefix negation
    }

    #[test]
    fn eval_operator_as_function_assigned() {
        // Can assign operator to variable
        let code = r#"{ let add = +; add(10, 20) }"#;
        assert_eq!(eval(code), Ok(Value::Integer(30)));
    }

    #[test]
    fn eval_builtin_fold_list() {
        // fold(0, |a, b| a + b, [1, 2, 3]) => 6
        assert_eq!(
            eval("fold(0, |a, b| a + b, [1, 2, 3])"),
            Ok(Value::Integer(6))
        );
    }

    #[test]
    fn eval_builtin_fold_empty() {
        // fold(0, |a, b| a + b, []) => 0 (returns initial if empty)
        assert_eq!(eval("fold(0, |a, b| a + b, [])"), Ok(Value::Integer(0)));
    }

    #[test]
    fn eval_builtin_fold_custom_fn() {
        // fold(1, |acc, v| acc * v, [1, 2, 3, 4]) => 24
        assert_eq!(
            eval("fold(1, |acc, v| acc * v, [1, 2, 3, 4])"),
            Ok(Value::Integer(24))
        );
    }

    #[test]
    fn eval_builtin_fold_range() {
        // fold(0, |a, b| a + b, 1..5) => 10
        assert_eq!(eval("fold(0, |a, b| a + b, 1..5)"), Ok(Value::Integer(10)));
        // fold(0, |a, b| a + b, 1..=5) => 15
        assert_eq!(eval("fold(0, |a, b| a + b, 1..=5)"), Ok(Value::Integer(15)));
    }

    #[test]
    fn eval_builtin_fold_string() {
        // fold(0, |acc, _| acc + 1, "ab") => 2
        assert_eq!(
            eval(r#"fold(0, |acc, _| acc + 1, "ab")"#),
            Ok(Value::Integer(2))
        );
    }

    #[test]
    fn eval_builtin_fold_s() {
        // fold_s returns first element of final accumulator
        // Test with simple list accumulator (without destructuring syntax)
        // 1..=10 gives 10 iterations, producing Fibonacci F(10)=55
        assert_eq!(
            eval("fold_s([0, 1], |acc, _| [get(1, acc), get(0, acc) + get(1, acc)], 1..=10)"),
            Ok(Value::Integer(55))
        );
    }

    #[test]
    fn eval_builtin_scan_list() {
        // scan(0, |a, b| a + b, [1, 2, 3]) => [0, 1, 3, 6] (includes initial value)
        let result = eval("scan(0, |a, b| a + b, [1, 2, 3])").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 4);
                assert_eq!(v[0], Value::Integer(0));
                assert_eq!(v[1], Value::Integer(1));
                assert_eq!(v[2], Value::Integer(3));
                assert_eq!(v[3], Value::Integer(6));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_scan_string() {
        // scan("", |a, b| a + b, "ab") => ["", "a", "ab"] (includes initial value)
        let result = eval(r#"scan("", |a, b| a + b, "ab")"#).unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 3);
                assert_eq!(v[0], Value::String(Rc::new("".to_string())));
                assert_eq!(v[1], Value::String(Rc::new("a".to_string())));
                assert_eq!(v[2], Value::String(Rc::new("ab".to_string())));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn eval_builtin_scan_range() {
        // scan(0, |a, b| a + b, 1..5) => [0, 1, 3, 6, 10] (includes initial value)
        let result = eval("scan(0, |a, b| a + b, 1..5)").unwrap();
        match result {
            Value::List(v) => {
                assert_eq!(v.len(), 5);
                assert_eq!(v[0], Value::Integer(0));
                assert_eq!(v[1], Value::Integer(1));
                assert_eq!(v[2], Value::Integer(3));
                assert_eq!(v[3], Value::Integer(6));
                assert_eq!(v[4], Value::Integer(10));
            }
            _ => panic!("Expected list"),
        }
    }

    // ============================================================
    // §11.6 Built-in Iteration Functions - Phase 10
    // ============================================================

    #[test]
    fn eval_builtin_each_list() {
        // each should return nil and execute side effects
        // let mut acc = 0; each(|v| acc = acc + v, [1, 2, 3]); acc => 6
        assert_eq!(
            eval("{ let mut acc = 0; each(|v| { acc = acc + v }, [1, 2, 3]); acc }"),
            Ok(Value::Integer(6))
        );
    }

    #[test]
    fn eval_builtin_each_returns_nil() {
        // each always returns nil
        assert_eq!(eval("each(|v| v, [1, 2, 3])"), Ok(Value::Nil));
    }

    #[test]
    fn eval_builtin_each_range() {
        // let mut acc = 0; each(|v| acc = acc + v, 1..5); acc => 10
        assert_eq!(
            eval("{ let mut acc = 0; each(|v| { acc = acc + v }, 1..5); acc }"),
            Ok(Value::Integer(10))
        );
    }

    #[test]
    fn eval_builtin_each_dict_value_only() {
        // let mut acc = 0; each(|v| acc = acc + v, #{1: 2, 3: 4}); acc => 6
        assert_eq!(
            eval("{ let mut acc = 0; each(|v| { acc = acc + v }, #{1: 2, 3: 4}); acc }"),
            Ok(Value::Integer(6))
        );
    }

    #[test]
    fn eval_builtin_each_dict_with_key() {
        // let mut acc = 0; each(|_, k| acc = acc + k, #{1: 2, 3: 4}); acc => 4
        assert_eq!(
            eval("{ let mut acc = 0; each(|_, k| { acc = acc + k }, #{1: 2, 3: 4}); acc }"),
            Ok(Value::Integer(4))
        );
    }

    // =========================================================================
    // Phase 11: Search Functions (§11.7)
    // =========================================================================

    #[test]
    fn eval_builtin_find_list() {
        // find(|x| x > 2, [1, 2, 3, 4]) => 3
        assert_eq!(eval("find(|x| x > 2, [1, 2, 3, 4])"), Ok(Value::Integer(3)));
    }

    #[test]
    fn eval_builtin_find_not_found() {
        // find(|x| x > 10, [1, 2, 3]) => nil
        assert_eq!(eval("find(|x| x > 10, [1, 2, 3])"), Ok(Value::Nil));
    }

    #[test]
    fn eval_builtin_find_range() {
        // find(|x| x > 5, 1..10) => 6
        assert_eq!(eval("find(|x| x > 5, 1..10)"), Ok(Value::Integer(6)));
    }

    #[test]
    fn eval_builtin_count_list() {
        // count(|x| x > 2, [1, 2, 3, 4, 5]) => 3
        assert_eq!(
            eval("count(|x| x > 2, [1, 2, 3, 4, 5])"),
            Ok(Value::Integer(3))
        );
    }

    #[test]
    fn eval_builtin_count_range() {
        // count(|x| x % 2 == 0, 1..11) => 5
        assert_eq!(eval("count(|x| x % 2 == 0, 1..11)"), Ok(Value::Integer(5)));
    }

    // =========================================================================
    // Phase 11: Aggregation Functions (§11.8)
    // =========================================================================

    #[test]
    fn eval_builtin_sum_list() {
        // sum([1, 2, 3, 4]) => 10
        assert_eq!(eval("sum([1, 2, 3, 4])"), Ok(Value::Integer(10)));
    }

    #[test]
    fn eval_builtin_sum_empty() {
        // sum([]) => 0
        assert_eq!(eval("sum([])"), Ok(Value::Integer(0)));
    }

    #[test]
    fn eval_builtin_sum_range() {
        // sum(1..=5) => 15
        assert_eq!(eval("sum(1..=5)"), Ok(Value::Integer(15)));
    }

    #[test]
    fn eval_builtin_sum_mixed_decimal() {
        // sum([1, 2.5, 3]) => 6.5
        assert_eq!(
            eval("sum([1, 2.5, 3])"),
            Ok(Value::Decimal(OrderedFloat(6.5)))
        );
    }

    #[test]
    fn eval_builtin_sum_lazy_sequence() {
        // sum on a LazySequence (from range function) - range is inclusive
        assert_eq!(
            eval("range(1, 5, 1) |> sum"),
            Ok(Value::Integer(15)) // 1 + 2 + 3 + 4 + 5 = 15
        );
    }

    #[test]
    fn eval_builtin_sum_lazy_sequence_with_map() {
        // sum on a mapped LazySequence - range is inclusive
        assert_eq!(
            eval("range(1, 5, 1) |> map(|x| x * 2) |> sum"),
            Ok(Value::Integer(30)) // 2 + 4 + 6 + 8 + 10 = 30
        );
    }

    #[test]
    fn eval_builtin_max_list() {
        // max([3, 1, 4, 1, 5]) => 5
        assert_eq!(eval("max([3, 1, 4, 1, 5])"), Ok(Value::Integer(5)));
    }

    #[test]
    fn eval_builtin_max_variadic() {
        // max(3, 1, 4, 1, 5) => 5
        assert_eq!(eval("max(3, 1, 4, 1, 5)"), Ok(Value::Integer(5)));
    }

    #[test]
    fn eval_builtin_max_empty() {
        // max([]) => nil
        assert_eq!(eval("max([])"), Ok(Value::Nil));
    }

    #[test]
    fn eval_builtin_max_range() {
        // max(1..=10) => 10
        assert_eq!(eval("max(1..=10)"), Ok(Value::Integer(10)));
    }

    #[test]
    fn eval_builtin_min_list() {
        // min([3, 1, 4, 1, 5]) => 1
        assert_eq!(eval("min([3, 1, 4, 1, 5])"), Ok(Value::Integer(1)));
    }

    #[test]
    fn eval_builtin_min_variadic() {
        // min(3, 1, 4, 1, 5) => 1
        assert_eq!(eval("min(3, 1, 4, 1, 5)"), Ok(Value::Integer(1)));
    }

    #[test]
    fn eval_builtin_min_range() {
        // min(1..=10) => 1
        assert_eq!(eval("min(1..=10)"), Ok(Value::Integer(1)));
    }

    // =========================================================================
    // Phase 11: Sequence Manipulation (§11.9)
    // =========================================================================

    #[test]
    fn eval_builtin_skip_list() {
        // skip(2, [1, 2, 3, 4, 5]) => [3, 4, 5]
        let expected = Value::List(
            vec![Value::Integer(3), Value::Integer(4), Value::Integer(5)]
                .into_iter()
                .collect(),
        );
        assert_eq!(eval("skip(2, [1, 2, 3, 4, 5])"), Ok(expected));
    }

    #[test]
    fn eval_builtin_take_list() {
        // take(3, [1, 2, 3, 4, 5]) => [1, 2, 3]
        let expected = Value::List(
            vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]
                .into_iter()
                .collect(),
        );
        assert_eq!(eval("take(3, [1, 2, 3, 4, 5])"), Ok(expected));
    }

    #[test]
    fn eval_builtin_take_range() {
        // take(3, 1..) => [1, 2, 3]
        let expected = Value::List(
            vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]
                .into_iter()
                .collect(),
        );
        assert_eq!(eval("take(3, 1..)"), Ok(expected));
    }

    #[test]
    fn eval_builtin_sort_list() {
        // sort(|a, b| a - b, [3, 1, 4, 1, 5]) => [1, 1, 3, 4, 5]
        let expected = Value::List(
            vec![
                Value::Integer(1),
                Value::Integer(1),
                Value::Integer(3),
                Value::Integer(4),
                Value::Integer(5),
            ]
            .into_iter()
            .collect(),
        );
        assert_eq!(eval("sort(|a, b| a - b, [3, 1, 4, 1, 5])"), Ok(expected));
    }

    #[test]
    fn eval_builtin_sort_descending() {
        // sort(|a, b| b - a, [3, 1, 4]) => [4, 3, 1]
        let expected = Value::List(
            vec![Value::Integer(4), Value::Integer(3), Value::Integer(1)]
                .into_iter()
                .collect(),
        );
        assert_eq!(eval("sort(|a, b| b - a, [3, 1, 4])"), Ok(expected));
    }

    #[test]
    fn eval_builtin_reverse_list() {
        // reverse([1, 2, 3]) => [3, 2, 1]
        let expected = Value::List(
            vec![Value::Integer(3), Value::Integer(2), Value::Integer(1)]
                .into_iter()
                .collect(),
        );
        assert_eq!(eval("reverse([1, 2, 3])"), Ok(expected));
    }

    #[test]
    fn eval_builtin_reverse_string() {
        // reverse("hello") => "olleh"
        assert_eq!(
            eval("reverse(\"hello\")"),
            Ok(Value::String(Rc::new("olleh".to_string())))
        );
    }

    #[test]
    fn eval_builtin_rotate_list() {
        // rotate(1, [1, 2, 3, 4]) => [4, 1, 2, 3]
        let expected = Value::List(
            vec![
                Value::Integer(4),
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3),
            ]
            .into_iter()
            .collect(),
        );
        assert_eq!(eval("rotate(1, [1, 2, 3, 4])"), Ok(expected));
    }

    #[test]
    fn eval_builtin_rotate_negative() {
        // rotate(-1, [1, 2, 3, 4]) => [2, 3, 4, 1]
        let expected = Value::List(
            vec![
                Value::Integer(2),
                Value::Integer(3),
                Value::Integer(4),
                Value::Integer(1),
            ]
            .into_iter()
            .collect(),
        );
        assert_eq!(eval("rotate(-1, [1, 2, 3, 4])"), Ok(expected));
    }

    #[test]
    fn eval_builtin_chunk_list() {
        // chunk(2, [1, 2, 3, 4, 5]) => [[1, 2], [3, 4], [5]]
        let expected = Value::List(
            vec![
                Value::List(
                    vec![Value::Integer(1), Value::Integer(2)]
                        .into_iter()
                        .collect(),
                ),
                Value::List(
                    vec![Value::Integer(3), Value::Integer(4)]
                        .into_iter()
                        .collect(),
                ),
                Value::List(vec![Value::Integer(5)].into_iter().collect()),
            ]
            .into_iter()
            .collect(),
        );
        assert_eq!(eval("chunk(2, [1, 2, 3, 4, 5])"), Ok(expected));
    }

    // =========================================================================
    // Phase 11: Set Operations (§11.10)
    // =========================================================================

    #[test]
    fn eval_builtin_union_sets() {
        // union({1, 2}, {2, 3}) => {1, 2, 3}
        let result = eval("union({1, 2}, {2, 3})");
        assert!(result.is_ok());
        if let Value::Set(s) = result.unwrap() {
            assert_eq!(s.len(), 3);
            assert!(s.contains(&Value::Integer(1)));
            assert!(s.contains(&Value::Integer(2)));
            assert!(s.contains(&Value::Integer(3)));
        } else {
            panic!("Expected Set");
        }
    }

    #[test]
    fn eval_builtin_intersection_sets() {
        // intersection({1, 2, 3}, {2, 3, 4}) => {2, 3}
        let result = eval("intersection({1, 2, 3}, {2, 3, 4})");
        assert!(result.is_ok());
        if let Value::Set(s) = result.unwrap() {
            assert_eq!(s.len(), 2);
            assert!(s.contains(&Value::Integer(2)));
            assert!(s.contains(&Value::Integer(3)));
        } else {
            panic!("Expected Set");
        }
    }

    // =========================================================================
    // Phase 11: Predicates (§11.11)
    // =========================================================================

    #[test]
    fn eval_builtin_includes_list() {
        // includes?([1, 2, 3], 2) => true
        assert_eq!(eval("includes?([1, 2, 3], 2)"), Ok(Value::Boolean(true)));
    }

    #[test]
    fn eval_builtin_includes_list_not_found() {
        // includes?([1, 2, 3], 5) => false
        assert_eq!(eval("includes?([1, 2, 3], 5)"), Ok(Value::Boolean(false)));
    }

    #[test]
    fn eval_builtin_includes_string() {
        // includes?("hello", "ell") => true
        assert_eq!(
            eval("includes?(\"hello\", \"ell\")"),
            Ok(Value::Boolean(true))
        );
    }

    #[test]
    fn eval_builtin_includes_range() {
        // includes?(1..10, 5) => true
        assert_eq!(eval("includes?(1..10, 5)"), Ok(Value::Boolean(true)));
    }

    #[test]
    fn eval_builtin_excludes_list() {
        // excludes?([1, 2, 3], 5) => true
        assert_eq!(eval("excludes?([1, 2, 3], 5)"), Ok(Value::Boolean(true)));
    }

    #[test]
    fn eval_builtin_any_list() {
        // any?(|x| x > 3, [1, 2, 3, 4]) => true
        assert_eq!(
            eval("any?(|x| x > 3, [1, 2, 3, 4])"),
            Ok(Value::Boolean(true))
        );
    }

    #[test]
    fn eval_builtin_any_list_false() {
        // any?(|x| x > 10, [1, 2, 3]) => false
        assert_eq!(
            eval("any?(|x| x > 10, [1, 2, 3])"),
            Ok(Value::Boolean(false))
        );
    }

    #[test]
    fn eval_builtin_all_list() {
        // all?(|x| x > 0, [1, 2, 3]) => true
        assert_eq!(eval("all?(|x| x > 0, [1, 2, 3])"), Ok(Value::Boolean(true)));
    }

    #[test]
    fn eval_builtin_all_list_false() {
        // all?(|x| x > 2, [1, 2, 3]) => false
        assert_eq!(
            eval("all?(|x| x > 2, [1, 2, 3])"),
            Ok(Value::Boolean(false))
        );
    }

    #[test]
    fn eval_builtin_all_range() {
        // all?(|x| x > 0, 1..=5) => true
        assert_eq!(eval("all?(|x| x > 0, 1..=5)"), Ok(Value::Boolean(true)));
    }

    // =========================================================================
    // Phase 12: Lazy Sequences (§11.12, §11.13)
    // =========================================================================

    #[test]
    fn eval_repeat_take() {
        // take(5, repeat(1)) => [1, 1, 1, 1, 1]
        assert_eq!(
            eval("take(5, repeat(1))"),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(1),
                Value::Integer(1),
                Value::Integer(1),
                Value::Integer(1),
                Value::Integer(1)
            ])))
        );
    }

    #[test]
    fn eval_repeat_first() {
        // first(repeat("x")) => "x"
        assert_eq!(
            eval("first(repeat(\"x\"))"),
            Ok(Value::String(Rc::new("x".to_string())))
        );
    }

    #[test]
    fn eval_cycle_take() {
        // take(7, cycle([1, 2, 3])) => [1, 2, 3, 1, 2, 3, 1]
        assert_eq!(
            eval("take(7, cycle([1, 2, 3]))"),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3),
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3),
                Value::Integer(1)
            ])))
        );
    }

    #[test]
    fn eval_iterate_take() {
        // take(5, iterate(|x| x * 2, 1)) => [1, 2, 4, 8, 16]
        assert_eq!(
            eval("take(5, iterate(|x| x * 2, 1))"),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(4),
                Value::Integer(8),
                Value::Integer(16)
            ])))
        );
    }

    #[test]
    fn eval_unbounded_range_take() {
        // take(5, 1..) => [1, 2, 3, 4, 5]
        assert_eq!(
            eval("take(5, 1..)"),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3),
                Value::Integer(4),
                Value::Integer(5)
            ])))
        );
    }

    #[test]
    fn eval_unbounded_range_find() {
        // find(|x| x > 10, 1..) => 11
        assert_eq!(eval("find(|x| x > 10, 1..)"), Ok(Value::Integer(11)));
    }

    #[test]
    fn eval_combinations_basic() {
        // combinations(2, [1, 2, 3]) => [[1, 2], [1, 3], [2, 3]]
        // (as lazy sequence, materialize with take)
        let result = eval("list(take(3, combinations(2, [1, 2, 3])))");
        match result {
            Ok(Value::List(list)) => {
                assert_eq!(list.len(), 3);
                // First combination should be [1, 2]
                assert_eq!(
                    list[0],
                    Value::List(Vector::from(vec![Value::Integer(1), Value::Integer(2)]))
                );
            }
            other => panic!("Expected list, got {:?}", other),
        }
    }

    #[test]
    fn eval_zip_finite() {
        // zip([1, 2], ["a", "b"]) => [[1, "a"], [2, "b"]]
        let result = eval("zip([1, 2], [\"a\", \"b\"])");
        match result {
            Ok(Value::List(list)) => {
                assert_eq!(list.len(), 2);
            }
            other => panic!("Expected list, got {:?}", other),
        }
    }

    #[test]
    fn eval_zip_mixed_finite_infinite() {
        // zip(0.., ["a", "b", "c"]) => List (because one is finite)
        // Per LANG.txt: If ANY collection has finite size → returns List
        let result = eval("zip(0.., [\"a\", \"b\", \"c\"])");
        match result {
            Ok(Value::List(list)) => {
                assert_eq!(list.len(), 3);
                // Check first element is [0, "a"]
                match &list[0] {
                    Value::List(inner) => {
                        assert_eq!(inner[0], Value::Integer(0));
                    }
                    _ => panic!("Expected inner list"),
                }
            }
            other => panic!("Expected List, got {:?}", other),
        }
    }

    #[test]
    fn eval_zip_all_infinite() {
        // zip(0.., 1..) with two infinite ranges returns LazySequence
        // Per LANG.txt: If ALL collections are infinite → returns LazySequence
        let result = eval("take(3, zip(0.., 1..))");
        match result {
            Ok(Value::List(list)) => {
                assert_eq!(list.len(), 3);
            }
            other => panic!("Expected list from take, got {:?}", other),
        }
    }

    #[test]
    fn eval_range_function() {
        // range(0, 10, 2) => [0, 2, 4, 6, 8]
        assert_eq!(
            eval("take(5, range(0, 10, 2))"),
            Ok(Value::List(Vector::from(vec![
                Value::Integer(0),
                Value::Integer(2),
                Value::Integer(4),
                Value::Integer(6),
                Value::Integer(8)
            ])))
        );
    }

    #[test]
    fn eval_fold_unbounded_break() {
        // fold(0, |acc, x| if x > 5 { break acc } else { acc + x }, 1..) => 15 (1+2+3+4+5)
        assert_eq!(
            eval("fold(0, |acc, x| if x > 5 { break acc } else { acc + x }, 1..)"),
            Ok(Value::Integer(15))
        );
    }

    #[test]
    fn eval_fold_list_break() {
        // fold with break on list
        assert_eq!(
            eval("fold(0, |acc, x| if x > 3 { break acc } else { acc + x }, [1, 2, 3, 4, 5])"),
            Ok(Value::Integer(6)) // 1+2+3
        );
    }

    #[test]
    fn eval_fold_dict_break() {
        // fold with break on dict - dict iteration order is undefined so we test the break mechanism
        assert_eq!(
            eval(
                r#"fold(0, |acc, v| if v > 2 { break 99 } else { acc + v }, #{"a": 1, "b": 2, "c": 3})"#
            ),
            Ok(Value::Integer(99)) // break returns 99 when we hit v=3
        );
    }

    #[test]
    fn eval_fold_string_break() {
        // fold with break on string
        assert_eq!(
            eval(r#"fold("", |acc, c| if c == "c" { break acc } else { acc + c }, "abcde")"#),
            Ok(Value::String(Rc::new("ab".to_string())))
        );
    }

    #[test]
    fn eval_each_bounded_range() {
        // Tests that each works on bounded range and returns nil
        assert_eq!(eval("each(|x| x, 1..5)"), Ok(Value::Nil));
    }

    #[test]
    fn eval_lazy_first_iterate() {
        // first(iterate(|x| x + 1, 0)) => 0
        assert_eq!(eval("first(iterate(|x| x + 1, 0))"), Ok(Value::Integer(0)));
    }

    #[test]
    fn eval_reduce_lazy_sequence() {
        // reduce(|a, b| a + b, take(5, repeat(1))) => 5
        // First converts lazy to list with take, then reduces
        assert_eq!(
            eval("reduce(|a, b| a + b, take(5, repeat(1)))"),
            Ok(Value::Integer(5))
        );
    }

    #[test]
    fn eval_fold_lazy_sequence() {
        // fold(0, |acc, x| acc + x, take(5, cycle([1, 2, 3])))
        assert_eq!(
            eval("fold(0, |acc, x| acc + x, take(5, cycle([1, 2, 3])))"),
            Ok(Value::Integer(9)) // 1+2+3+1+2 = 9
        );
    }

    #[test]
    fn eval_find_lazy_sequence() {
        // find(|x| x > 5, iterate(|x| x + 1, 1)) => 6
        assert_eq!(
            eval("find(|x| x > 5, iterate(|x| x + 1, 1))"),
            Ok(Value::Integer(6))
        );
    }

    // Lazy composition tests - map/filter/skip should work on LazySequence
    #[test]
    fn eval_map_on_lazy_sequence() {
        // map on lazy sequence returns lazy sequence
        // take(3, map(|x| x * 2, repeat(5))) => [10, 10, 10]
        assert_eq!(
            eval("take(3, map(|x| x * 2, repeat(5)))"),
            Ok(Value::List(
                vec![Value::Integer(10), Value::Integer(10), Value::Integer(10)]
                    .into_iter()
                    .collect()
            ))
        );
    }

    #[test]
    fn eval_map_chain_lazy() {
        // Chained maps on lazy sequence
        // take(3, map(|x| x + 1, map(|x| x * 2, 1..))) => [3, 5, 7]
        assert_eq!(
            eval("take(3, map(|x| x + 1, map(|x| x * 2, 1..)))"),
            Ok(Value::List(
                vec![
                    Value::Integer(3), // 1*2+1 = 3
                    Value::Integer(5), // 2*2+1 = 5
                    Value::Integer(7)  // 3*2+1 = 7
                ]
                .into_iter()
                .collect()
            ))
        );
    }

    #[test]
    fn eval_filter_on_lazy_sequence() {
        // filter on lazy sequence returns lazy sequence
        // take(3, filter(|x| x % 2 == 0, iterate(|x| x + 1, 1))) => [2, 4, 6]
        assert_eq!(
            eval("take(3, filter(|x| x % 2 == 0, iterate(|x| x + 1, 1)))"),
            Ok(Value::List(
                vec![Value::Integer(2), Value::Integer(4), Value::Integer(6)]
                    .into_iter()
                    .collect()
            ))
        );
    }

    #[test]
    fn eval_filter_map_chain_lazy() {
        // filter then map on lazy sequence
        // take(3, map(|x| x * 10, filter(|x| x % 2 == 0, 1..))) => [20, 40, 60]
        assert_eq!(
            eval("take(3, map(|x| x * 10, filter(|x| x % 2 == 0, 1..)))"),
            Ok(Value::List(
                vec![Value::Integer(20), Value::Integer(40), Value::Integer(60)]
                    .into_iter()
                    .collect()
            ))
        );
    }

    #[test]
    fn eval_skip_on_lazy_sequence() {
        // skip on lazy sequence returns lazy sequence
        // take(3, skip(2, repeat(7))) => [7, 7, 7]
        assert_eq!(
            eval("take(3, skip(2, repeat(7)))"),
            Ok(Value::List(
                vec![Value::Integer(7), Value::Integer(7), Value::Integer(7)]
                    .into_iter()
                    .collect()
            ))
        );
    }

    #[test]
    fn eval_skip_on_iterate() {
        // skip on iterate lazy sequence
        // take(3, skip(5, iterate(|x| x + 1, 0))) => [5, 6, 7]
        assert_eq!(
            eval("take(3, skip(5, iterate(|x| x + 1, 0)))"),
            Ok(Value::List(
                vec![Value::Integer(5), Value::Integer(6), Value::Integer(7)]
                    .into_iter()
                    .collect()
            ))
        );
    }

    // second() and rest() on LazySequence
    #[test]
    fn eval_second_lazy_sequence() {
        // second(repeat(5)) => 5
        assert_eq!(eval("second(repeat(5))"), Ok(Value::Integer(5)));
        // second(iterate(|x| x + 1, 0)) => 1
        assert_eq!(eval("second(iterate(|x| x + 1, 0))"), Ok(Value::Integer(1)));
    }

    #[test]
    fn eval_rest_lazy_sequence() {
        // rest(repeat(5)) should return a lazy sequence, take 3 from it
        assert_eq!(
            eval("take(3, rest(repeat(5)))"),
            Ok(Value::List(
                vec![Value::Integer(5), Value::Integer(5), Value::Integer(5)]
                    .into_iter()
                    .collect()
            ))
        );
    }

    #[test]
    fn eval_rest_iterate() {
        // rest(iterate(|x| x + 1, 0)) skips first, take 3 => [1, 2, 3]
        assert_eq!(
            eval("take(3, rest(iterate(|x| x + 1, 0)))"),
            Ok(Value::List(
                vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]
                    .into_iter()
                    .collect()
            ))
        );
    }

    // ========================================================================
    // Phase 13: String Functions Tests
    // ========================================================================

    #[test]
    fn eval_builtin_lines() {
        assert_eq!(
            eval("lines(\"a\\nb\\nc\")"),
            Ok(Value::List(
                vec![
                    Value::String(Rc::new("a".to_string())),
                    Value::String(Rc::new("b".to_string())),
                    Value::String(Rc::new("c".to_string()))
                ]
                .into_iter()
                .collect()
            ))
        );
    }

    #[test]
    fn eval_builtin_lines_single() {
        assert_eq!(
            eval("lines(\"single line\")"),
            Ok(Value::List(
                vec![Value::String(Rc::new("single line".to_string()))]
                    .into_iter()
                    .collect()
            ))
        );
    }

    #[test]
    fn eval_builtin_split() {
        assert_eq!(
            eval("split(\",\", \"a,b,c\")"),
            Ok(Value::List(
                vec![
                    Value::String(Rc::new("a".to_string())),
                    Value::String(Rc::new("b".to_string())),
                    Value::String(Rc::new("c".to_string()))
                ]
                .into_iter()
                .collect()
            ))
        );
    }

    #[test]
    fn eval_builtin_split_empty_separator() {
        assert_eq!(
            eval("split(\"\", \"abc\")"),
            Ok(Value::List(
                vec![
                    Value::String(Rc::new("a".to_string())),
                    Value::String(Rc::new("b".to_string())),
                    Value::String(Rc::new("c".to_string()))
                ]
                .into_iter()
                .collect()
            ))
        );
    }

    #[test]
    fn eval_builtin_regex_match() {
        assert_eq!(
            eval("regex_match(\"(\\\\d+)\", \"abc123\")"),
            Ok(Value::List(
                vec![Value::String(Rc::new("123".to_string()))]
                    .into_iter()
                    .collect()
            ))
        );
    }

    #[test]
    fn eval_builtin_regex_match_multiple_groups() {
        assert_eq!(
            eval("regex_match(\"(\\\\w+):(\\\\d+)\", \"port:8080\")"),
            Ok(Value::List(
                vec![
                    Value::String(Rc::new("port".to_string())),
                    Value::String(Rc::new("8080".to_string()))
                ]
                .into_iter()
                .collect()
            ))
        );
    }

    #[test]
    fn eval_builtin_regex_match_no_match() {
        assert_eq!(
            eval("regex_match(\"(\\\\d+)\", \"no numbers\")"),
            Ok(Value::List(Vector::new()))
        );
    }

    #[test]
    fn eval_builtin_regex_match_all() {
        assert_eq!(
            eval("regex_match_all(\"\\\\d+\", \"a1b2c3\")"),
            Ok(Value::List(
                vec![
                    Value::String(Rc::new("1".to_string())),
                    Value::String(Rc::new("2".to_string())),
                    Value::String(Rc::new("3".to_string()))
                ]
                .into_iter()
                .collect()
            ))
        );
    }

    #[test]
    fn eval_builtin_md5_empty() {
        assert_eq!(
            eval("md5(\"\")"),
            Ok(Value::String(Rc::new(
                "d41d8cd98f00b204e9800998ecf8427e".to_string()
            )))
        );
    }

    #[test]
    fn eval_builtin_md5_hello() {
        assert_eq!(
            eval("md5(\"hello\")"),
            Ok(Value::String(Rc::new(
                "5d41402abc4b2a76b9719d911017c592".to_string()
            )))
        );
    }

    #[test]
    fn eval_builtin_md5_hello_world() {
        assert_eq!(
            eval("md5(\"Hello, World!\")"),
            Ok(Value::String(Rc::new(
                "65a8e27d8879283831b664bd8b7f0ad4".to_string()
            )))
        );
    }

    // ========================================================================
    // Phase 13: Math Functions Tests
    // ========================================================================

    #[test]
    fn eval_builtin_abs_integer() {
        assert_eq!(eval("abs(-5)"), Ok(Value::Integer(5)));
        assert_eq!(eval("abs(5)"), Ok(Value::Integer(5)));
        assert_eq!(eval("abs(0)"), Ok(Value::Integer(0)));
    }

    #[test]
    fn eval_builtin_abs_decimal() {
        assert_eq!(eval("abs(-3.7)"), Ok(Value::Decimal(OrderedFloat(3.7))));
        assert_eq!(eval("abs(3.7)"), Ok(Value::Decimal(OrderedFloat(3.7))));
    }

    #[test]
    fn eval_builtin_signum_integer() {
        assert_eq!(eval("signum(5)"), Ok(Value::Integer(1)));
        assert_eq!(eval("signum(0)"), Ok(Value::Integer(0)));
        assert_eq!(eval("signum(-3)"), Ok(Value::Integer(-1)));
    }

    #[test]
    fn eval_builtin_signum_decimal() {
        assert_eq!(eval("signum(5.5)"), Ok(Value::Integer(1)));
        assert_eq!(eval("signum(-5.5)"), Ok(Value::Integer(-1)));
        assert_eq!(eval("signum(0.0)"), Ok(Value::Integer(0)));
    }

    #[test]
    fn eval_builtin_vec_add() {
        assert_eq!(
            eval("vec_add([1, 2], [3, 4])"),
            Ok(Value::List(
                vec![Value::Integer(4), Value::Integer(6)]
                    .into_iter()
                    .collect()
            ))
        );
    }

    #[test]
    fn eval_builtin_vec_add_shorter_list() {
        assert_eq!(
            eval("vec_add([1, 2, 3], [10, 20])"),
            Ok(Value::List(
                vec![Value::Integer(11), Value::Integer(22)]
                    .into_iter()
                    .collect()
            ))
        );
    }

    // ========================================================================
    // Phase 13: Bitwise Functions Tests
    // ========================================================================

    #[test]
    fn eval_builtin_bit_and() {
        assert_eq!(eval("bit_and(12, 10)"), Ok(Value::Integer(8)));
    }

    #[test]
    fn eval_builtin_bit_or() {
        assert_eq!(eval("bit_or(12, 10)"), Ok(Value::Integer(14)));
    }

    #[test]
    fn eval_builtin_bit_xor() {
        assert_eq!(eval("bit_xor(12, 10)"), Ok(Value::Integer(6)));
    }

    #[test]
    fn eval_builtin_bit_not() {
        assert_eq!(eval("bit_not(12)"), Ok(Value::Integer(-13)));
    }

    #[test]
    fn eval_builtin_bit_shift_left() {
        assert_eq!(eval("bit_shift_left(1, 3)"), Ok(Value::Integer(8)));
    }

    #[test]
    fn eval_builtin_bit_shift_right() {
        assert_eq!(eval("bit_shift_right(8, 2)"), Ok(Value::Integer(2)));
    }

    // ========================================================================
    // Phase 13: Utility Functions Tests
    // ========================================================================

    #[test]
    fn eval_builtin_id() {
        assert_eq!(eval("id(42)"), Ok(Value::Integer(42)));
        assert_eq!(
            eval("id(\"hello\")"),
            Ok(Value::String(Rc::new("hello".to_string())))
        );
    }

    #[test]
    fn eval_builtin_type() {
        assert_eq!(
            eval("type(nil)"),
            Ok(Value::String(Rc::new("Nil".to_string())))
        );
        assert_eq!(
            eval("type(42)"),
            Ok(Value::String(Rc::new("Integer".to_string())))
        );
        assert_eq!(
            eval("type(3.15)"),
            Ok(Value::String(Rc::new("Decimal".to_string())))
        );
        assert_eq!(
            eval("type(true)"),
            Ok(Value::String(Rc::new("Boolean".to_string())))
        );
        assert_eq!(
            eval("type(\"hello\")"),
            Ok(Value::String(Rc::new("String".to_string())))
        );
        assert_eq!(
            eval("type([1, 2])"),
            Ok(Value::String(Rc::new("List".to_string())))
        );
        assert_eq!(
            eval("type({1, 2})"),
            Ok(Value::String(Rc::new("Set".to_string())))
        );
        assert_eq!(
            eval("type(#{\"a\": 1})"),
            Ok(Value::String(Rc::new("Dictionary".to_string())))
        );
    }

    #[test]
    fn eval_builtin_or() {
        assert_eq!(eval("or(true, false)"), Ok(Value::Boolean(true)));
        assert_eq!(eval("or(false, false)"), Ok(Value::Boolean(false)));
        assert_eq!(eval("or(nil, 5)"), Ok(Value::Integer(5)));
        assert_eq!(eval("or(0, 10)"), Ok(Value::Integer(10)));
    }

    #[test]
    fn eval_builtin_and() {
        assert_eq!(eval("and(true, true)"), Ok(Value::Boolean(true)));
        assert_eq!(eval("and(true, false)"), Ok(Value::Boolean(false)));
        assert_eq!(eval("and(5, 10)"), Ok(Value::Integer(10)));
        assert_eq!(eval("and(nil, 5)"), Ok(Value::Nil));
    }

    #[test]
    fn eval_builtin_evaluate() {
        assert_eq!(eval("evaluate(\"1 + 2\")"), Ok(Value::Integer(3)));
        assert_eq!(
            eval("evaluate(\"[1, 2, 3]\")"),
            Ok(Value::List(
                vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]
                    .into_iter()
                    .collect()
            ))
        );
    }

    #[test]
    fn eval_builtin_evaluate_with_let() {
        // Test evaluate with let statements
        assert_eq!(
            eval("evaluate(\"let x = 10; x * 2\")"),
            Ok(Value::Integer(20))
        );
    }

    #[test]
    fn eval_builtin_evaluate_multiple_statements() {
        // Test multiple statements where only the last value is returned
        assert_eq!(
            eval("evaluate(\"let x = 5; let y = 10; x + y\")"),
            Ok(Value::Integer(15))
        );
    }

    #[test]
    fn eval_builtin_evaluate_with_block() {
        // Test evaluate with a block expression
        assert_eq!(
            eval("evaluate(\"{ let a = 3; let b = 4; a * b }\")"),
            Ok(Value::Integer(12))
        );
    }

    // §8.9 Tail-Call Optimization Tests (Phase 14)

    #[test]
    fn tco_simple_factorial() {
        // Tail-recursive factorial - should not overflow stack
        let code = r#"{
            let factorial = |n| {
                let recur = |acc, n| {
                    if n == 0 { acc }
                    else { recur(acc * n, n - 1) }
                };
                recur(1, n)
            };
            factorial(10)
        }"#;
        assert_eq!(eval(code), Ok(Value::Integer(3628800)));
    }

    #[test]
    fn tco_deep_recursion() {
        // Test that deep tail recursion doesn't overflow
        // This would overflow without TCO (typically ~1000-10000 calls max)
        let code = r#"{
            let count_down = |n| {
                if n == 0 { "done" }
                else { count_down(n - 1) }
            };
            count_down(100000)
        }"#;
        assert_eq!(eval(code), Ok(Value::String("done".to_string().into())));
    }

    #[test]
    fn tco_sum_tail_recursive() {
        // Tail-recursive sum using accumulator
        let code = r#"{
            let sum = |n| {
                let recur = |acc, n| {
                    if n == 0 { acc }
                    else { recur(acc + n, n - 1) }
                };
                recur(0, n)
            };
            sum(1000)
        }"#;
        // Sum from 1 to 1000 = 1000 * 1001 / 2 = 500500
        assert_eq!(eval(code), Ok(Value::Integer(500500)));
    }

    #[test]
    fn tco_not_tail_position() {
        // Non-tail recursive call (operation after the call)
        // This should NOT be optimized and may overflow for large inputs
        let code = r#"{
            let factorial = |n| {
                if n == 0 { 1 }
                else { n * factorial(n - 1) }
            };
            factorial(5)
        }"#;
        assert_eq!(eval(code), Ok(Value::Integer(120)));
    }

    #[test]
    fn tco_tail_call_in_if_branches() {
        // Tail calls in both if branches
        let code = r#"{
            let is_even = |n| {
                if n == 0 { true }
                else if n == 1 { false }
                else { is_even(n - 2) }
            };
            is_even(10000)
        }"#;
        assert_eq!(eval(code), Ok(Value::Boolean(true)));
    }

    #[test]
    fn tco_tail_call_in_match() {
        // Tail call in match expression
        let code = r#"{
            let count_down = |n| {
                match n {
                    0 { "done" }
                    1 { "done" }
                    _ { count_down(n - 1) }
                }
            };
            count_down(10000)
        }"#;
        assert_eq!(eval(code), Ok(Value::String("done".to_string().into())));
    }

    #[test]
    fn tco_mutual_recursion_not_optimized() {
        // Mutual recursion should NOT be optimized (per spec)
        // but should still work for small inputs
        let code = r#"{
            let is_even = |n| {
                if n == 0 { true }
                else { is_odd(n - 1) }
            };
            let is_odd = |n| {
                if n == 0 { false }
                else { is_even(n - 1) }
            };
            is_even(6)
        }"#;
        assert_eq!(eval(code), Ok(Value::Boolean(true)));
    }

    #[test]
    fn tco_fibonacci_accumulator() {
        // Tail-recursive fibonacci with accumulators
        let code = r#"{
            let fib = |n| {
                let recur = |a, b, n| {
                    if n == 0 { a }
                    else { recur(b, a + b, n - 1) }
                };
                recur(0, 1, n)
            };
            fib(20)
        }"#;
        // 20th fibonacci number
        assert_eq!(eval(code), Ok(Value::Integer(6765)));
    }

    // §9 Function Parameter Destructuring (Phase 15)

    #[test]
    fn eval_function_param_destructuring_basic() {
        // Basic list pattern parameter
        let code = r#"{ let add = |[a, b]| a + b; add([1, 2]) }"#;
        assert_eq!(eval(code), Ok(Value::Integer(3)));
    }

    #[test]
    fn eval_function_param_destructuring_multiple_params() {
        // Mix of pattern and regular parameters
        let code = r#"{ let f = |[x, y], z| x + y + z; f([1, 2], 3) }"#;
        assert_eq!(eval(code), Ok(Value::Integer(6)));
    }

    #[test]
    fn eval_function_param_destructuring_inline() {
        // Inline lambda with pattern parameter
        let code = r#"(|[a, b]| a * b)([3, 4])"#;
        assert_eq!(eval(code), Ok(Value::Integer(12)));
    }

    // ============================================================
    // §15.5 Error Handling - RuntimeErr Conditions (Phase 17)
    // ============================================================

    #[test]
    fn error_invalid_regex_pattern() {
        // Invalid regex patterns should throw RuntimeErr
        assert!(eval(r#"regex_match("(unclosed", "test")"#).is_err());
        assert!(eval(r#"regex_match("[invalid", "test")"#).is_err());
    }

    #[test]
    fn error_type_mismatch_operations() {
        // Type mismatches in operations should throw RuntimeErr
        assert!(eval(r#""hello" * "world""#).is_err());
        assert!(eval(r#""hello" / "world""#).is_err());
        assert!(eval(r#""hello" - "world""#).is_err());
        assert!(eval(r#"true + false"#).is_err());
    }

    #[test]
    fn error_division_by_zero() {
        // Division by zero should throw RuntimeErr
        assert!(eval("10 / 0").is_err());
        assert!(eval("10 % 0").is_err());
        assert!(eval("5.5 / 0.0").is_err());
    }

    #[test]
    fn error_reduce_empty_collection() {
        // reduce on empty collection should throw RuntimeErr
        assert!(eval("reduce(+, [])").is_err());
        assert!(eval("reduce(|a, b| a + b, [])").is_err());
    }

    #[test]
    fn error_identifier_not_found() {
        // Undefined variables should throw RuntimeErr
        assert!(eval("unknown_var").is_err());
        assert!(eval("x + 1").is_err());
    }

    #[test]
    fn error_non_hashable_in_set() {
        // Functions, dicts, and lazy sequences cannot be in sets
        // Single-element sets need trailing comma per LANG.txt
        assert!(eval("{|x| x,}").is_err());
        assert!(eval("{#{a: 1},}").is_err());
    }

    #[test]
    fn error_non_hashable_dict_key() {
        // Functions cannot be dict keys
        assert!(eval("#{(|x| x): 1}").is_err());
        // Dicts cannot be dict keys
        assert!(eval("#{#{a: 1}: 2}").is_err());
    }

    #[test]
    fn error_wrong_function_arity() {
        // Calling function with too many arguments is an error
        assert!(eval("{ let f = |x| x * 2; f(1, 2) }").is_err());
        // Calling with too few now returns a partial application (auto-currying)
        // so f(1) is not an error
    }

    #[test]
    fn error_call_non_function() {
        // Calling non-functions should throw RuntimeErr
        assert!(eval("[1, 2, 3](5)").is_err());
        assert!(eval("42(1)").is_err());
        assert!(eval(r#""hello"()"#).is_err());
    }

    #[test]
    fn error_return_invalid_context() {
        // return outside function should be caught (compile or runtime error)
        // This might be a compile error depending on implementation
        let result = eval("return 42");
        assert!(result.is_err());
    }

    #[test]
    fn error_break_invalid_context() {
        // break outside iteration should throw RuntimeErr
        assert!(eval("break 42").is_err());
        assert!(eval("{ let x = 1; break x }").is_err());
    }

    #[test]
    fn error_shadow_builtin() {
        // Shadowing built-in functions should throw RuntimeErr
        assert!(eval("let map = 42").is_err());
        assert!(eval("let reduce = |x| x").is_err());
        assert!(eval("let filter = 123").is_err());
    }

    #[test]
    fn error_invalid_evaluate_syntax() {
        // Invalid syntax in evaluate() should throw RuntimeErr
        assert!(eval(r#"evaluate("1 +")"#).is_err());
        assert!(eval(r#"evaluate("let x = ")"#).is_err());
        assert!(eval(r#"evaluate("(((")"#).is_err());
    }

    // ===== Partial Application and Currying Tests =====

    #[test]
    fn partial_application_placeholder_in_call() {
        // Placeholder in function call argument creates partial application
        assert_eq!(
            eval(r#"{ let d = #{"a": 1}; let f = get(_, d); f("a") }"#),
            Ok(Value::Integer(1))
        );
    }

    #[test]
    fn partial_application_placeholder_in_infix_call() {
        // Placeholder in backtick infix call
        assert_eq!(
            eval(r#"{ let add = |a, b| a + b; let inc = 1 `add` _; inc(5) }"#),
            Ok(Value::Integer(6))
        );
    }

    #[test]
    fn partial_application_composition_with_placeholder() {
        // Composition with placeholder fills piped value into placeholder position
        assert_eq!(
            eval(
                r#"{ let d = #{"a": 1}; let f = |x| x; let composed = f >> get(_, d); composed("a") }"#
            ),
            Ok(Value::Integer(1))
        );
    }

    #[test]
    fn partial_application_composition_both_sides_placeholders() {
        // (_ + 1) >> (_ * 2) should create |x| (x + 1) * 2
        assert_eq!(
            eval("{ let f = (_ + 1) >> (_ * 2); f(5) }"),
            Ok(Value::Integer(12)) // (5 + 1) * 2 = 12
        );
    }

    #[test]
    fn builtin_as_first_class_value() {
        // Builtin functions can be assigned to variables and passed around
        assert_eq!(
            eval(r#"{ let f = ints; f("abc123") }"#),
            Ok(Value::List(vec![Value::Integer(123)].into_iter().collect()))
        );
        assert_eq!(
            eval(r#"{ let f = size; f([1, 2, 3]) }"#),
            Ok(Value::Integer(3))
        );
    }

    #[test]
    fn builtin_auto_curry() {
        // Builtin functions auto-curry when called with fewer args
        assert_eq!(
            eval(r#"{ let splitComma = split(","); splitComma("a,b,c") }"#),
            Ok(Value::List(
                vec![
                    Value::String(Rc::new("a".to_string())),
                    Value::String(Rc::new("b".to_string())),
                    Value::String(Rc::new("c".to_string()))
                ]
                .into_iter()
                .collect()
            ))
        );
    }

    #[test]
    fn builtin_curry_in_map() {
        // Auto-curried builtin used in map
        assert_eq!(
            eval(r#"["a,b", "c,d"] |> map(split(","))"#),
            Ok(Value::List(
                vec![
                    Value::List(
                        vec![
                            Value::String(Rc::new("a".to_string())),
                            Value::String(Rc::new("b".to_string()))
                        ]
                        .into_iter()
                        .collect()
                    ),
                    Value::List(
                        vec![
                            Value::String(Rc::new("c".to_string())),
                            Value::String(Rc::new("d".to_string()))
                        ]
                        .into_iter()
                        .collect()
                    )
                ]
                .into_iter()
                .collect()
            ))
        );
    }

    #[test]
    fn builtin_composition_with_ints() {
        // Composition with builtin as first-class value
        assert_eq!(
            eval(r#"{ let parse = lines >> map(ints); parse("abc123\ndef456") }"#),
            Ok(Value::List(
                vec![
                    Value::List(vec![Value::Integer(123)].into_iter().collect()),
                    Value::List(vec![Value::Integer(456)].into_iter().collect())
                ]
                .into_iter()
                .collect()
            ))
        );
    }

    #[test]
    fn partial_placeholder_not_in_lambda_body() {
        // Placeholder in lambda body doesn't affect outer call
        assert_eq!(
            eval(r#"[1, 2, 3] |> fold(0, |acc, x| acc + x)"#),
            Ok(Value::Integer(6))
        );
    }

    // ===== Range Indexing Tests =====

    #[test]
    fn list_range_indexing_exclusive() {
        // List[start..end] returns sublist (exclusive end)
        assert_eq!(
            eval("[1, 2, 3, 4, 5][1..3]"),
            Ok(Value::List(
                vec![Value::Integer(2), Value::Integer(3)]
                    .into_iter()
                    .collect()
            ))
        );
    }

    #[test]
    fn list_range_indexing_inclusive() {
        // List[start..=end] returns sublist (inclusive end)
        assert_eq!(
            eval("[1, 2, 3, 4, 5][1..=3]"),
            Ok(Value::List(
                vec![Value::Integer(2), Value::Integer(3), Value::Integer(4)]
                    .into_iter()
                    .collect()
            ))
        );
    }

    #[test]
    fn list_range_indexing_open_end() {
        // List[start..] returns from start to end
        assert_eq!(
            eval("[1, 2, 3, 4, 5][2..]"),
            Ok(Value::List(
                vec![Value::Integer(3), Value::Integer(4), Value::Integer(5)]
                    .into_iter()
                    .collect()
            ))
        );
    }

    #[test]
    fn string_range_indexing_exclusive() {
        // String[start..end] returns substring (exclusive end)
        assert_eq!(
            eval(r#""hello"[1..3]"#),
            Ok(Value::String(Rc::new("el".to_string())))
        );
    }

    #[test]
    fn string_range_indexing_inclusive() {
        // String[start..=end] returns substring (inclusive end)
        assert_eq!(
            eval(r#""hello"[1..=3]"#),
            Ok(Value::String(Rc::new("ell".to_string())))
        );
    }

    #[test]
    fn string_range_indexing_open_end() {
        // String[start..] returns from start to end
        assert_eq!(
            eval(r#""hello"[2..]"#),
            Ok(Value::String(Rc::new("llo".to_string())))
        );
    }

    #[test]
    fn range_indexing_empty_result() {
        // Empty range returns empty list/string
        assert_eq!(eval("[1, 2, 3][2..2]"), Ok(Value::List(Vector::new())));
        assert_eq!(
            eval(r#""hello"[3..3]"#),
            Ok(Value::String(Rc::new(String::new())))
        );
    }

    // ===== Nested Pattern Destructuring Tests =====

    #[test]
    fn nested_pattern_in_lambda_params() {
        // Nested list pattern like |[[x1, y1], [x2, y2]]|
        assert_eq!(
            eval(r#"{ let f = |[[x1, y1], [x2, y2]]| x1 + x2 + y1 + y2; f([[1, 2], [3, 4]]) }"#),
            Ok(Value::Integer(10))
        );
    }

    #[test]
    fn nested_pattern_in_map() {
        // Nested patterns with map
        assert_eq!(
            eval(
                r#"[[[1, 2], [3, 4]], [[5, 6], [7, 8]]] |> map(|[[a, b], [c, d]]| a + b + c + d)"#
            ),
            Ok(Value::List(
                vec![Value::Integer(10), Value::Integer(26)]
                    .into_iter()
                    .collect()
            ))
        );
    }

    #[test]
    fn nested_pattern_three_levels() {
        // Three levels of nesting
        assert_eq!(
            eval(r#"{ let f = |[[[a]]]| a; f([[[42]]]) }"#),
            Ok(Value::Integer(42))
        );
    }

    #[test]
    fn nested_pattern_mixed_with_identifier() {
        // Mix of nested patterns and identifiers
        assert_eq!(
            eval(r#"{ let f = |[a, [b, c]]| a + b + c; f([1, [2, 3]]) }"#),
            Ok(Value::Integer(6))
        );
    }

    // ===== Unbounded Range with Pipeline Tests =====

    #[test]
    fn unbounded_range_with_pipeline() {
        // Unbounded range followed by pipeline operator
        assert_eq!(eval("4.. |> find(|x| x > 5)"), Ok(Value::Integer(6)));
    }

    #[test]
    fn unbounded_range_with_pipeline_multiline() {
        // Multiline version
        assert_eq!(eval("0..\n    |> find(|x| x > 10)"), Ok(Value::Integer(11)));
    }

    #[test]
    fn unbounded_range_in_block_with_pipeline() {
        // { expr } is parsed as a set, so this becomes {15} (matching reference)
        let result = eval("{ 1.. |> take(5) |> sum }").unwrap();
        assert!(matches!(result, Value::Set(_)));
    }

    // ===== Auto-Currying User Functions Tests =====

    #[test]
    fn user_function_auto_curry_one_arg() {
        // User-defined function called with fewer args creates partial application
        assert_eq!(
            eval(r#"{ let add = |a, b| a + b; let inc = add(1); inc(5) }"#),
            Ok(Value::Integer(6))
        );
    }

    #[test]
    fn user_function_auto_curry_in_map() {
        // Partial application used in map
        assert_eq!(
            eval(r#"{ let mult = |a, b| a * b; [1, 2, 3] |> map(mult(2)) }"#),
            Ok(Value::List(
                vec![Value::Integer(2), Value::Integer(4), Value::Integer(6)]
                    .into_iter()
                    .collect()
            ))
        );
    }

    #[test]
    fn user_function_chained_curry() {
        // Multiple levels of currying
        assert_eq!(
            eval(
                r#"{ let add3 = |a, b, c| a + b + c; let add2 = add3(1); let add1 = add2(2); add1(3) }"#
            ),
            Ok(Value::Integer(6))
        );
    }

    #[test]
    fn partial_application_preserves_order() {
        // First args applied first, then remaining args
        assert_eq!(
            eval(r#"{ let sub = |a, b| a - b; let subFrom10 = sub(10); subFrom10(3) }"#),
            Ok(Value::Integer(7)) // 10 - 3 = 7
        );
    }

    #[test]
    fn eval_let_destructuring_nested_lists_global() {
        // Test that destructuring at global scope correctly extracts nested elements
        let source = r#"let [first, second] = [[1, 2], [3, 4]];
first"#;
        let tokens = Lexer::new(source).tokenize().unwrap();
        let program = Parser::new(tokens).parse_program().unwrap();
        let compiled = Compiler::compile_statements(&program.statements).unwrap();
        let mut vm = VM::new();
        let result = vm.run(Rc::new(compiled));

        let expected = Value::List(Vector::from(vec![Value::Integer(1), Value::Integer(2)]));
        assert_eq!(result.map_err(|e| e.message), Ok(expected));
    }

    #[test]
    fn eval_let_destructuring_deeply_nested_literal() {
        // Deeply nested destructuring with direct literal
        let source = r#"let [[a, b], ..rest] = [[1, 2], [3, 4]];
b"#;
        let tokens = Lexer::new(source).tokenize().unwrap();
        let program = Parser::new(tokens).parse_program().unwrap();
        let compiled = Compiler::compile_statements(&program.statements).unwrap();
        let mut vm = VM::new();
        let result = vm.run(Rc::new(compiled));

        assert_eq!(result.map_err(|e| e.message), Ok(Value::Integer(2)));
    }

    #[test]
    fn eval_let_destructuring_deeply_nested_variable() {
        // Deeply nested destructuring with variable - tests that globals don't
        // leave extra values on stack that interfere with local slot calculations
        let source = r#"let sorted = [[1, 2], [3, 4]];
let [[a, b], ..rest] = sorted;
[a, b, rest]"#;
        let tokens = Lexer::new(source).tokenize().unwrap();
        let program = Parser::new(tokens).parse_program().unwrap();
        let compiled = Compiler::compile_statements(&program.statements).unwrap();
        let mut vm = VM::new();
        let result = vm.run(Rc::new(compiled));

        let expected = Value::List(Vector::from(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::List(Vector::from(vec![Value::List(Vector::from(vec![
                Value::Integer(3),
                Value::Integer(4),
            ]))])),
        ]));
        assert_eq!(result.map_err(|e| e.message), Ok(expected));
    }
}

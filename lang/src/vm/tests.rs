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
            Value::Decimal(OrderedFloat(3.14)),
            Value::Decimal(OrderedFloat(3.14))
        );
        assert_ne!(
            Value::Decimal(OrderedFloat(3.14)),
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
        let v1 = Value::Decimal(OrderedFloat(3.14));
        let v2 = Value::Decimal(OrderedFloat(3.14));
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
        assert_eq!(format!("{}", Value::Decimal(OrderedFloat(3.14))), "3.14");
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
        assert_eq!(
            Value::String(Rc::new("".to_string())).type_name(),
            "String"
        );
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
    use expect_test::{expect, Expect};

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
            "3.14",
            expect![[r#"
                == test ==
                0000 [   1] Constant 0 (3.14)
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
        // x |> f(a) compiles to f(a, x)
        check_bytecode(
            "5 |> add(10)",
            expect![[r#"
                == test ==
                0000 [   1] GetGlobal 0 ("add")
                0002 [   1] Constant 1 (10)
                0004 [   1] Constant 2 (5)
                0006 [   1] Call 2
                0008 [   1] Return
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
}

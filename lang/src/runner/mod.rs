// RuntimeError is 128+ bytes but boxing would add overhead on successful paths.
// Error paths are not performance-critical for an interpreter.
#![allow(clippy::result_large_err)]

use crate::parser::ast::{Program, Section, SpannedExpr, SpannedStmt};
use crate::vm::compiler::Compiler;
use crate::vm::runtime::{RuntimeError, VM};
use crate::vm::value::Value;
use std::rc::Rc;
use std::time::Instant;

#[cfg(test)]
mod tests;

pub struct AocRunner {
    program: Program,
}

#[derive(Debug, Clone)]
pub struct TestResult {
    pub test_index: usize,
    pub part_one_passed: Option<bool>,
    pub part_two_passed: Option<bool>,
    pub part_one_expected: Option<Value>,
    pub part_one_actual: Option<Value>,
    pub part_two_expected: Option<Value>,
    pub part_two_actual: Option<Value>,
}

#[derive(Debug, Clone)]
pub struct SolutionResult {
    pub part_one: Option<(Value, u128)>,
    pub part_two: Option<(Value, u128)>,
}

impl AocRunner {
    pub fn new(program: Program) -> Self {
        Self { program }
    }

    pub fn run_solution(&self, vm: &mut VM) -> Result<SolutionResult, RuntimeError> {

        // Check for duplicate sections
        self.check_duplicate_sections()?;

        // Execute top-level statements (all together to preserve globals)
        if !self.program.statements.is_empty() {
            self.compile_and_execute_stmts(vm, &self.program.statements)?;
        }

        // Get input section if present
        let input_value = self.get_input_section(vm)?;

        // Execute part_one
        let part_one = self.execute_part(vm, "part_one", input_value.clone())?;

        // Execute part_two
        let part_two = self.execute_part(vm, "part_two", input_value)?;

        Ok(SolutionResult {
            part_one,
            part_two,
        })
    }

    pub fn run_tests(&self, vm_factory: &dyn Fn() -> VM) -> Result<Vec<TestResult>, RuntimeError> {
        let mut results = Vec::new();

        // Check for duplicate sections
        self.check_duplicate_sections()?;

        // Get test sections
        let test_sections: Vec<_> = self.program.sections.iter()
            .filter_map(|s| match s {
                Section::Test { input, part_one, part_two } => {
                    Some((input.clone(), part_one.clone(), part_two.clone()))
                }
                _ => None
            })
            .collect();

        // Run each test
        for (index, (test_input_expr, expected_part_one, expected_part_two)) in test_sections.iter().enumerate() {
            let result = self.run_single_test(
                vm_factory,
                index,
                test_input_expr,
                expected_part_one.as_ref(),
                expected_part_two.as_ref(),
            )?;
            results.push(result);
        }

        Ok(results)
    }

    pub fn run_script(&self, vm: &mut VM) -> Result<Value, RuntimeError> {

        // Execute all statements and return the last value
        if self.program.statements.is_empty() {
            Ok(Value::Nil)
        } else {
            self.compile_and_execute_stmts(vm, &self.program.statements)
        }
    }

    pub fn is_script_mode(&self) -> bool {
        !self.program.sections.iter().any(|s| matches!(s, Section::PartOne(_) | Section::PartTwo(_)))
    }

    fn check_duplicate_sections(&self) -> Result<(), RuntimeError> {
        let mut input_count = 0;
        let mut part_one_count = 0;
        let mut part_two_count = 0;

        for section in &self.program.sections {
            match section {
                Section::Input(_) => input_count += 1,
                Section::PartOne(_) => part_one_count += 1,
                Section::PartTwo(_) => part_two_count += 1,
                Section::Test { .. } => {}
            }
        }

        if input_count > 1 {
            return Err(RuntimeError::new("Expected a single 'input' section".to_string(), 0));
        }
        if part_one_count > 1 {
            return Err(RuntimeError::new("Expected single 'part_one' solution".to_string(), 0));
        }
        if part_two_count > 1 {
            return Err(RuntimeError::new("Expected single 'part_two' solution".to_string(), 0));
        }

        Ok(())
    }

    fn get_input_section(&self, vm: &mut VM) -> Result<Option<Value>, RuntimeError> {
        for section in &self.program.sections {
            if let Section::Input(expr) = section {
                let value = self.compile_and_execute_expr(vm, expr)?;
                return Ok(Some(value));
            }
        }
        Ok(None)
    }

    fn execute_part(
        &self,
        vm: &mut VM,
        part_name: &str,
        input_value: Option<Value>,
    ) -> Result<Option<(Value, u128)>, RuntimeError> {
        // Find the part section
        let part_expr = self.program.sections.iter().find_map(|s| match s {
            Section::PartOne(expr) if part_name == "part_one" => Some(expr),
            Section::PartTwo(expr) if part_name == "part_two" => Some(expr),
            _ => None,
        });

        let Some(expr) = part_expr else {
            return Ok(None);
        };

        // Bind input variable if present
        if let Some(input) = input_value {
            vm.define_global("input", input);
        }

        // Execute with timing
        let start = Instant::now();
        let result = self.compile_and_execute_expr(vm, expr)?;
        let duration = start.elapsed().as_micros();

        Ok(Some((result, duration)))
    }

    fn run_single_test(
        &self,
        vm_factory: &dyn Fn() -> VM,
        test_index: usize,
        test_input_expr: &SpannedExpr,
        expected_part_one: Option<&SpannedExpr>,
        expected_part_two: Option<&SpannedExpr>,
    ) -> Result<TestResult, RuntimeError> {
        // Create fresh VM for test
        let mut vm = vm_factory();

        // Execute top-level statements (all together to preserve globals)
        if !self.program.statements.is_empty() {
            self.compile_and_execute_stmts(&mut vm, &self.program.statements)?;
        }

        // Evaluate test input
        let test_input = self.compile_and_execute_expr(&mut vm, test_input_expr)?;

        // Run part_one if expected
        let (part_one_passed, part_one_expected_val, part_one_actual_val) = if let Some(expected_expr) = expected_part_one {
            let expected = self.compile_and_execute_expr(&mut vm, expected_expr)?;

            // Find and execute part_one
            if let Some((actual, _)) = self.execute_part(&mut vm, "part_one", Some(test_input.clone()))? {
                (Some(actual == expected), Some(expected.clone()), Some(actual))
            } else {
                (None, Some(expected), None)
            }
        } else {
            (None, None, None)
        };

        // Run part_two if expected
        let (part_two_passed, part_two_expected_val, part_two_actual_val) = if let Some(expected_expr) = expected_part_two {
            let expected = self.compile_and_execute_expr(&mut vm, expected_expr)?;

            // Find and execute part_two
            if let Some((actual, _)) = self.execute_part(&mut vm, "part_two", Some(test_input.clone()))? {
                (Some(actual == expected), Some(expected.clone()), Some(actual))
            } else {
                (None, Some(expected), None)
            }
        } else {
            (None, None, None)
        };

        Ok(TestResult {
            test_index,
            part_one_passed,
            part_two_passed,
            part_one_expected: part_one_expected_val,
            part_one_actual: part_one_actual_val,
            part_two_expected: part_two_expected_val,
            part_two_actual: part_two_actual_val,
        })
    }

    fn compile_and_execute_stmts(&self, vm: &mut VM, stmts: &[SpannedStmt]) -> Result<Value, RuntimeError> {
        let compiled = Compiler::compile_statements(stmts)
            .map_err(|e| RuntimeError::new(e.message, e.span.start as u32))?;
        vm.run(Rc::new(compiled))
    }

    fn compile_and_execute_expr(&self, vm: &mut VM, expr: &SpannedExpr) -> Result<Value, RuntimeError> {
        let compiled = Compiler::compile_expression(expr)
            .map_err(|e| RuntimeError::new(e.message, e.span.start as u32))?;
        vm.run(Rc::new(compiled))
    }
}

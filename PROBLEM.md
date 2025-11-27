# santa-lang Blitzen Implementation Problems

This document tracks all issues found when running the example test suite.

**Summary:** 6 passed, 44 errors, 4 skipped out of 54 tests

---

## Category 1: Missing Built-in Functions

### [ ] `ints` function - Extract integers from string
- **Affected files:** aoc2018_day01, aoc2018_day08, aoc2018_day09, aoc2023_day06, aoc2023_day09, aoc2023_day12
- **Error:** `Undefined variable 'ints'`
- **Expected behavior:** `ints("abc123def456")` → `[123, 456]`
- **Usage example:**
  ```
  let parse_deltas = ints;
  parse_deltas(input) |> sum;
  ```

### [ ] `upper` function - Convert string to uppercase
- **Affected files:** aoc2018_day05
- **Error:** `Undefined variable 'upper'`
- **Expected behavior:** `upper("abc")` → `"ABC"`

### [ ] `reverse` function - Reverse a list or string
- **Affected files:** aoc2022_day05
- **Error:** `Undefined variable 'reverse'`
- **Expected behavior:** `reverse([1, 2, 3])` → `[3, 2, 1]`

---

## Category 2: String Indexing with Ranges

### [ ] String slicing with ranges
- **Affected files:** aoc2018_day12, aoc2022_day25
- **Error:** `Cannot index String with Range`
- **Expected behavior:** `"hello"[1..3]` → `"el"`, `"hello"[2..]` → `llo`
- **Usage examples:**
  ```
  initial[15..]                    // substring from index 15 to end
  springs[0..group_size]           // substring from 0 to group_size
  springs[group_size+1..]          // substring from index to end
  ```

---

## Category 3: Nested Patterns in Function Parameters

### [ ] Support destructuring nested patterns in lambda parameters
- **Affected files:** aoc2018_day04, aoc2022_day04, aoc2022_day13, aoc2022_day22, aoc2022_day24, aoc2023_day07
- **Error:** `Nested patterns in function parameters not yet supported`
- **Expected behavior:**
  ```
  |[[x1, y1], [x2, y2]]| x1 + x2    // nested array destructuring
  |[a, [b, c]]| a + b + c           // mixed destructuring
  ```
- **Usage example from aoc2022_day04:**
  ```
  count(|[[x1, y1], [x2, y2]]| (x1 <= x2 && y1 >= y2) || (x2 <= x1 && y2 >= y1))
  ```

---

## Category 4: Partial Application with Placeholder `_`

### [ ] Placeholder `_` in various expression contexts
- **Affected files:** aoc2022_day03, aoc2022_day12, aoc2022_day14, aoc2022_day16, aoc2022_day17, aoc2022_day18, aoc2022_day20, aoc2022_day23
- **Error:** `Placeholder '_' can only be used in expressions for partial application`
- **Expected behavior:** `_` should work in:
  - Function arguments: `get(_, priorities)`
  - Operators: `_ + 1`, `_ != "#"`
  - Nested calls: `update("ore", _ + 1)`
- **Usage examples:**
  ```
  get(_, priorities)                    // partial: dict lookup
  filter(_ != next_step)                // partial: inequality check
  all?(_ == 0)                          // partial: equality check
  update("robots", update("ore", _ + 1)) // nested partial application
  ```

### [ ] Expression type not supported in partial application
- **Affected files:** aoc2018_day06
- **Error:** `Expression type not supported in partial application`
- **Need to investigate what expression type is causing this**

---

## Category 5: Parse Errors

### [ ] Trailing lambda with `|>` pipe operator
- **Affected files:** aoc2018_day07, aoc2022_day06, aoc2022_day15, aoc2023_day05
- **Error:** `Unexpected token: PipeGreater`
- **Expected behavior:** Support trailing lambda after `|>` pipe
- **Usage examples:**
  ```
  remaining_steps |> filter(_ != next_step)    // trailing lambda with _
  0.. |> find |duration| { ... }               // trailing lambda block
  ```
- **Context from aoc2018_day07 line 46:**
  ```
  remaining_steps |> filter(_ != next_step),
  ```

### [ ] Pattern parsing with trailing lambda in map
- **Affected files:** aoc2018_day13
- **Error:** `Expected pattern, got Comma`
- **Usage example:**
  ```
  |> map |path| match path {
    "^" { "|" },
    ...
  }
  ```

---

## Category 6: Type Operation Errors

### [ ] String + Integer concatenation
- **Affected files:** aoc2018_day14
- **Error:** `Cannot add String and Integer`
- **Expected behavior:** `"abc" + 123` → `"abc123"` (string coercion)
- **Need to check:** What exactly is being concatenated

### [ ] Integer indexing (likely grid/2D array access)
- **Affected files:** aoc2022_day08, aoc2023_day03, aoc2023_day10, aoc2023_day11
- **Error:** `Cannot index Integer with Integer`
- **Need to investigate:** These might be cases where a list lookup returns an integer, then trying to index into that

### [ ] `take` function type handling
- **Affected files:** aoc2022_day09_2
- **Error:** `take expects Integer, got String`
- **Need to investigate the actual usage**

### [ ] Lazy sequence sum
- **Affected files:** aoc2022_day10
- **Error:** `Cannot sum lazy sequence; use take() to create a finite list first`
- **Expected behavior:** Should be able to sum finite lazy sequences or auto-materialize

---

## Category 7: Built-in Function Signature Issues

### [ ] `split` with single argument (default delimiter)
- **Affected files:** aoc2022_day02, aoc2022_day21
- **Error:** `split expects 2 argument(s), got 1`
- **Expected behavior:** `split(" ")` should work as partial application or `split(str)` with default delimiter
- **Usage example:**
  ```
  let parse_strategy = lines >> map(split(" "));
  ```

### [ ] `fold` with 2 arguments (collection piped)
- **Affected files:** aoc2022_day11, aoc2023_day13, aoc2023_day14
- **Error:** `Expected 2 arguments but got 1`
- **Expected behavior:** `fold(initial)` or `fold(initial, fn)` with collection piped in
- **Usage examples:**
  ```
  0..size(monkeys) |> fold(monkeys) |monkeys, position| { ... }
  |> fold(0) |position, [steps, instruction]| { ... }
  ```

### [ ] `fold` / other function with different argument pattern
- **Affected files:** aoc2023_day08
- **Error:** `Expected 3 arguments but got 2`
- **Usage example:**
  ```
  zip(1.., instructions |> cycle)
    |> fold(initial, |position, [steps, instruction]| { ... })
  ```

### [ ] `filter` on function composition results
- **Affected files:** aoc2022_day19
- **Error:** `filter does not support Function`
- **This might be a cascading error from partial application issues**

---

## Category 8: Callback/Decorator Built-ins

### [ ] `memoize` as function decorator
- **Affected files:** aoc2023_day12
- **Error:** `memoize is not a callback builtin`
- **Expected behavior:** `memoize` should work as a decorator before lambda
- **Usage example:**
  ```
  let arrangements = memoize |springs, criteria| {
    // function body
  }
  ```

---

## Priority Order for Fixes

Based on impact (number of affected files):

1. **Placeholder `_` partial application** (8+ files) - Category 4
2. **Nested patterns in function params** (6 files) - Category 3
3. **`ints` built-in function** (6 files) - Category 1
4. **Trailing lambda with `|>`** (4 files) - Category 5
5. **Integer indexing issues** (4 files) - Category 6
6. **String range indexing** (2 files) - Category 2
7. **`split` single argument** (2 files) - Category 7
8. **`fold` argument patterns** (4 files) - Category 7
9. **`memoize` decorator** (1 file) - Category 8
10. **Other missing functions** (upper, reverse) - Category 1
11. **Other type operations** - Category 6

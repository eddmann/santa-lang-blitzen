# Performance Implementation

Implement a performance optimization based on an issue file.

## Arguments

Path to a performance issue file, e.g.: `perf-issues/DICT_HASH_OVERHEAD.txt`

## Instructions

1. **Read the issue file** specified in $ARGUMENTS:
   - Parse the analysis section to understand the problem
   - Note the location (file, line, function)
   - Review the proposed optimization approach

2. **Update the issue file status**:
   - Change `STATUS: IDENTIFIED` to `STATUS: IN_PROGRESS`
   - Add entry to PROGRESS LOG:
     ```
     [date] IN_PROGRESS - Beginning implementation
     ```

3. **Understand the optimization target**:
   - Read the source file at the location specified
   - Understand the current implementation
   - Check `flamegraph.svg` if available for visual context

4. **Write a failing benchmark first** (if not already covered):
   - Check existing benchmarks in `benchmarks/benches/vm_benchmarks.rs`
   - Add a targeted micro-benchmark if needed

5. **Implement the optimization**:
   - Make minimal, focused changes
   - Preserve correctness - do not change behavior
   - Follow Rust idioms and project conventions
   - Target the specific hotspot identified

6. **Verify correctness**:
   ```bash
   cargo test -p lang
   ```

7. **Run clippy**:
   ```bash
   cargo clippy -- -D warnings
   ```

8. **Update the issue file** with implementation details:
   - Add a new section after PROPOSED OPTIMIZATION:

```
--------------------------------------------------------------------------------
IMPLEMENTATION
--------------------------------------------------------------------------------

Files Modified:
  - [file1.rs]: [brief description of change]
  - [file2.rs]: [brief description of change]

Changes Made:
  [Detailed description of what was implemented]

Tests Added/Modified:
  [List any new or modified tests]
```

9. **Update the PROGRESS LOG**:
   ```
   [date] IMPLEMENTED - [Brief summary of changes made]
   ```

10. **Update status**:
    - Change `STATUS: IN_PROGRESS` to `STATUS: IMPLEMENTED`

## Output

The issue file is updated with implementation details and ready for `/perf-verify`.

# Performance Identification

Identify performance concerns in the Blitzen VM and create individual issue files for each hotspot.

## Arguments

Provide the santa file to analyze, e.g.: `examples/aoc2018_day05.santa`

## Instructions

1. **Build and run the analysis** for the provided file:
   ```bash
   make analyze FILE=$ARGUMENTS
   ```

2. **Generate a flamegraph** for visual hotspot analysis:
   ```bash
   make profile FILE=$ARGUMENTS
   ```

3. **Analyze the profile data**:
   - Check `flamegraph.svg` was generated
   - Read the Rust source in `lang/src/vm/` to understand the hot paths
   - Identify which VM operations correlate with the profile data

4. **Identify the top hotspots** by examining:
   - Which VM operations are taking the most time
   - Any unexpected O(n) or O(n^2) patterns in collection operations
   - Repeated allocations or cloning in hot paths
   - Hash computation overhead in Dict/Set operations
   - Bytecode dispatch overhead
   - String operations and allocations

5. **Create a directory for performance issues**:
   ```bash
   mkdir -p perf-issues
   ```

6. **Create one file per identified issue** in `perf-issues/` directory.

   Filename format: `UPPERCASE_DESCRIPTIVE_NAME.txt`

   Examples:
   - `perf-issues/DICT_HASH_OVERHEAD.txt`
   - `perf-issues/STRING_CLONE_IN_LOOP.txt`
   - `perf-issues/LIST_APPEND_REALLOCATION.txt`
   - `perf-issues/BYTECODE_DISPATCH_COST.txt`

   Each file must have this structure:

```
================================================================================
PERFORMANCE ISSUE: [DESCRIPTIVE NAME]
================================================================================

BENCHMARK FILE: $ARGUMENTS
IDENTIFIED: [date]
STATUS: IDENTIFIED

--------------------------------------------------------------------------------
ANALYSIS
--------------------------------------------------------------------------------

Runtime: [X]ms
Hotspot Rank: [1/2/3 etc]
Time Contribution: [X]% of total execution

Location:
  File: [lang/src/vm/file.rs]
  Line: [line number]
  Function: [function_name]

Description:
  [What is happening in this code path]

Root Cause:
  [Why this is slow - be specific about the algorithmic or implementation issue]

--------------------------------------------------------------------------------
PROPOSED OPTIMIZATION
--------------------------------------------------------------------------------

Approach:
  [Specific change to make]

Expected Impact:
  [Estimated improvement percentage and reasoning]

Risk Assessment:
  [Any correctness concerns or edge cases to watch for]

--------------------------------------------------------------------------------
PROGRESS LOG
--------------------------------------------------------------------------------

[date] IDENTIFIED - Issue discovered during profiling of $ARGUMENTS
```

7. **List created files** at the end for the user:
   ```bash
   ls -la perf-issues/
   ```

## Output

After running this command:
- `flamegraph.svg` - Visual profile
- `profile.pb` - Protocol buffer profile data
- `perf-issues/*.txt` - One file per identified hotspot

Use `/perf-implement perf-issues/ISSUE_NAME.txt` to implement a fix.

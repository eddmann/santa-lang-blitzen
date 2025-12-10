# Performance Verification

Verify that an implemented optimization achieved the expected improvement.

## Arguments

Path to a performance issue file, e.g.: `perf-issues/DICT_HASH_OVERHEAD.txt`

## Instructions

1. **Read the issue file** specified in $ARGUMENTS:
   - Confirm status is `IMPLEMENTED`
   - Get the benchmark file from `BENCHMARK FILE:` field
   - Review the expected impact

2. **Update status**:
   - Change `STATUS: IMPLEMENTED` to `STATUS: VERIFYING`
   - Add to PROGRESS LOG:
     ```
     [date] VERIFYING - Running verification benchmarks
     ```

3. **Run correctness tests**:
   ```bash
   cargo test -p lang
   ```
   Record pass/fail in the issue file.

4. **Run the benchmark** on the original file:
   ```bash
   cargo build --release -p santa-cli
   hyperfine --warmup 3 --runs 10 "./target/release/santa-cli [BENCHMARK_FILE]"
   ```

5. **Compare against baseline** (if available):
   ```bash
   make bench/compare BASELINE=benchmark/results/baseline.json CURRENT=/tmp/current.json
   ```

6. **Add VERIFICATION section** to the issue file:

```
--------------------------------------------------------------------------------
VERIFICATION
--------------------------------------------------------------------------------

Date: [date]
Tests: [PASS/FAIL]

Benchmark Results:
  Before: [X]ms (from original analysis)
  After: [Y]ms
  Change: [+/-Z%]

Meets Expected Impact: [YES/NO]
  Expected: [from PROPOSED OPTIMIZATION]
  Actual: [measured change]

Verdict: [CONFIRMED / INSUFFICIENT / REGRESSION]
```

7. **Update final status** based on results:

   If improvement confirmed (>5% faster):
   - Change `STATUS: VERIFYING` to `STATUS: VERIFIED`
   - Add to PROGRESS LOG:
     ```
     [date] VERIFIED - Performance improvement confirmed ([X]% faster)
     ```

   If insufficient improvement (-5% to +5%):
   - Change `STATUS: VERIFYING` to `STATUS: MARGINAL`
   - Add to PROGRESS LOG:
     ```
     [date] MARGINAL - Change within noise threshold ([X]%)
     ```

   If regression (>5% slower):
   - Change `STATUS: VERIFYING` to `STATUS: REGRESSION`
   - Add to PROGRESS LOG:
     ```
     [date] REGRESSION - Performance degraded ([X]% slower), needs investigation
     ```

8. **Update baseline** if improvement is confirmed:
   ```bash
   # Only if VERIFIED
   hyperfine --warmup 3 --runs 10 --export-json benchmark/results/baseline.json \
     "./target/release/santa-cli [BENCHMARK_FILE]"
   ```

## Status Lifecycle

```
IDENTIFIED -> IN_PROGRESS -> IMPLEMENTED -> VERIFYING -> VERIFIED
                                                      -> MARGINAL
                                                      -> REGRESSION
```

## Output

The issue file contains the complete journey from identification through verification, with all measurements and decisions documented.

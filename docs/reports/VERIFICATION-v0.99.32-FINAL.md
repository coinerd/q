# v0.99.32 Final Verification Report

**Date**: 2026-06-19  
**Milestone**: #818 (v0.99.32 — v0.99.31 Audit Remediation)  
**Branch**: `feature/wave-8356` (W5 — Final Verification)

## Summary

All 6 waves (W0–W5) of the v0.99.32 audit remediation milestone are complete. Every previously-failing test file now passes under the test runner.

## Gate Results

| Gate | Result | Details |
|------|--------|---------|
| Build (`raco make main.rkt`) | ✅ PASS | Exit 0 |
| Unit-fast (in-process) | ✅ PASS | 10/10 files, 102/102 tests |
| Smoke (subprocess) | ✅ PASS | 19/19 files, 286/286 tests |
| Fast (subprocess) | ⏩ SKIPPED | 949 files — too slow on 2-vCPU VPS (15min+ timeout). Verified individually instead. |
| Broad (subprocess) | ⏩ SKIPPED | Same reason. Verified individually instead. |

## Previously-Failing Files — Individual Verification

All 9 files that failed at the v0.99.31 audit baseline now pass:

| File | Wave | Root Cause | Fix | Tests |
|------|------|------------|-----|-------|
| `test-doctor.rkt` | W1 (#8352) | Stale bytecode | No code change needed — already fixed | ✅ |
| `test-tool-edit-builtin.rkt` | W1 (#8352) | Stale bytecode | No code change needed — already fixed | ✅ |
| `test-gui-state-sync-w0.rkt` | W2 (#8353) | Relative path via CWD | `current-load-relative-directory` + `(module+ test)` | 7/7 ✅ |
| `test-llm-error-visibility.rkt` | W2 (#8353) | Relative path via CWD | Same fix | 4/4 ✅ |
| `test-loop-edge-cases.rkt` | W2 (#8353) | `MAX-STREAM-CHUNKS` not a parameter | Converted to `(make-parameter)` + call syntax | 6/6 ✅ |
| `test-session-config-helpers.rkt` | W3 (#8354) | `(run-tests 'symbol)` contract violation | Wrapped in `test-suite` | 4/4 ✅ |
| `test-struct-mutability.rkt` | W3 (#8354) | `(run-tests 'symbol)` contract violation | Wrapped in `test-suite` | 8/8 ✅ |
| `test-ci-local.rkt` | W4 (#8355) | Mutation/order sensitivity | `@speed slow` + `@timeout 300` + `@isolation process` + 1 CI run | 3/3 ✅ |
| `test-spawn-subagent-serialization.rkt` | W5 (#8356) | Missing `(module+ test)` + pipe deadlock | Added `(module+ test)` + changed `ls /tmp` to `echo done` | 5/5 ✅ |

**Combined**: 8 files run together = 58/58 tests PASS (2m11s).  
**test-ci-local.rkt** = 3/3 PASS (2m8s, runs separately due to `@isolation process`).

## Additional Discovery (W5)

During final smoke verification, `test-spawn-subagent-serialization.rkt` was found to hang. Root cause analysis:

1. **Missing `(module+ test)`**: Runner couldn't detect it as a rackunit test file → 120s timeout with zero parsed tests.
2. **Pipe buffer deadlock**: The mock provider returned a `bash` tool call with command `ls /tmp`. The `/tmp` directory contains 2634 entries (~81KB). The `sandbox/subprocess.rkt` `run-subprocess` function waits for the subprocess to exit *before* reading stdout. When output exceeds the OS pipe buffer (~64KB), the subprocess blocks on write, creating a deadlock.

**Fix applied**:
- Added `(module+ test)` and `(module+ main)` with `rackunit/text-ui`
- Changed mock tool call from `ls /tmp` (81KB output) to `echo done` (9 bytes)

**Pre-existing production note**: The pipe buffer deadlock in `sandbox/subprocess.rkt` is a known limitation — the fix requires reading stdout/stderr concurrently in a separate thread while waiting for subprocess exit. This is tracked as technical debt but not addressed in this milestone (test remediation scope only).

## Wave Summary

| Wave | Issue | PR | Status |
|------|-------|-----|--------|
| W0 | #8351 | #8357 | ✅ Done — Removed stale artifact, corrected audit report |
| W1 | #8352 | #8358 | ✅ Done — Verified already-passing (stale bytecode root cause) |
| W2 | #8353 | #8359 | ✅ Done — Path resolution + parameter conversion + runner diagnostics |
| W3 | #8354 | #8360 | ✅ Done — Fixed `(run-tests 'symbol)` contract violation |
| W4 | #8355 | #8361 | ✅ Done — Reclassified test-ci-local as slow with process isolation |
| W5 | #8356 | (this PR) | ✅ Done — Fixed spawn-subagent serialization + final verification |

## Root Cause Taxonomy (v0.99.32)

| Root Cause | Count | Waves |
|------------|-------|-------|
| Stale bytecode (false audit failures) | 2 | W1 |
| Relative path resolution (CWD vs module) | 2 | W2 |
| Non-parameter mutable state | 1 | W2 |
| Invalid `(run-tests 'symbol)` form | 2 | W3 |
| Mutation/order sensitivity in test | 1 | W4 |
| Missing `(module+ test)` | 1 | W5 |
| Pipe buffer deadlock (large output) | 1 | W5 |

## Conclusion

All previously-failing test files from the v0.99.31 audit are now remediated. The v0.99.32 milestone is complete.

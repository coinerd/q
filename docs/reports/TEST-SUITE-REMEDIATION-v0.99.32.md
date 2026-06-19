# v0.99.32 — Test Suite Remediation Tracking

**Date**: 2026-06-19  
**Milestone**: #818  
**Base**: `91ed1060` (v0.99.31 final)

## Baseline (from main@91ed1060)

| Gate | Result | Details |
|------|--------|---------|
| Build (`raco make main.rkt`) | PASS | Fresh bytecode |
| Unit-fast (in-process, local) | PASS | 10 files, 102 tests |
| Smoke (subprocess, local) | PASS | 19 files, 286 tests |
| Fast (subprocess, local) | **FAIL** | 7 failed files, 3 failed tests |
| Broad+ledger (subprocess, local) | **FAIL** | 6 new/unclassified failures |

## Remaining Failures

### Fast-gate (7 files)

| File | Category | Direct `raco test` | Wave |
|------|----------|-------------------|------|
| `tests/test-ci-local.rkt` | ASSERTION_FAILURE | PASS | W4 |
| `tests/test-doctor.rkt` | not fast failure | FAIL | W1 |
| `tests/test-gui-state-sync-w0.rkt` | MODULE_LOAD_FAILURE | PASS | W2 |
| `tests/test-llm-error-visibility.rkt` | MODULE_LOAD_FAILURE | PASS | W2 |
| `tests/test-loop-edge-cases.rkt` | ASSERTION_FAILURE | PASS | W2 |
| `tests/test-session-config-helpers.rkt` | ASSERTION_FAILURE | PASS | W3 |
| `tests/test-struct-mutability.rkt` | ASSERTION_FAILURE | PASS | W3 |
| `tests/test-tool-edit-builtin.rkt` | ASSERTION_FAILURE | FAIL | W1 |

### Broad-gate (6 files)

Same as fast minus `test-ci-local.rkt` and `test-tool-edit-builtin.rkt`, plus `test-doctor.rkt`.

## Artifact Correction

- Removed committed `tmp/v09931/unit-fast-local.json` (gate output should not be tracked).
- Corrected `AUDIT-v0.99.31-FINAL.md` to annotate incomplete claims.

## Wave Progress

## W1 Verification: Doctor + Edit Builtin (2026-06-19)

Both target tests pass under fresh-bytecode direct `raco test` and `scripts/run-tests.rkt`:

- `tests/test-doctor.rkt`: 17/17 PASS — `lookup-credential` already normalizes symbol→string in `runtime/auth/auth-store.rkt:125-128`
- `tests/test-tool-edit-builtin.rkt`: 7/7 PASS — no-match returns error with "0" count in message
- Adjacent: `tests/test-auth-store.rkt`: 51/51 PASS
- Unit-fast: 10/10 PASS (102 tests)

**Root cause of audit failure**: Stale bytecode at audit time (`91ed1060`). The fix was present in the codebase but compiled artifacts did not reflect it.

## Wave Progress

| Wave | Issue | Status | PR |
|------|-------|--------|-----|
| W0 | #8351 | ✅ Done | #8357 |
| W1 | #8352 | ✅ Done (verified, no code change needed) | #8358 |
| W2 | #8353 | ✅ Done | — |
| W3 | #8354 | Todo | — |
| W4 | #8355 | Todo | — |
| W5 | #8356 | Todo | — |

## W2 Verification: Runner Diagnostics + Module-load Fixes (2026-06-19)

### Root Causes Fixed

1. **test-gui-state-sync-w0.rkt** (MODULE_LOAD_FAILURE): File used relative paths `"../gui/main.rkt"` via `with-input-from-file`, which resolve relative to CWD. Runner runs from `q/`, so the path resolved incorrectly. Fix: Added `this-dir` via `current-load-relative-directory` and `build-path` for path resolution. Also added `(module+ test ...)` so runner detects it as rackunit test file.

2. **test-llm-error-visibility.rkt** (MODULE_LOAD_FAILURE): Same relative path issue with `with-input-from-file "../util/error/error-helpers.rkt"`. Same fix applied.

3. **test-loop-edge-cases.rkt** (ASSERTION_FAILURE): `(parameterize ([MAX-STREAM-CHUNKS 100]) ...)` failed because `MAX-STREAM-CHUNKS` was a plain `define`, not a parameter. Production fix: Changed to `(make-parameter 10000)` in `agent/stream-reducer.rkt`, updated call site in `agent/stream-runner.rkt` to `(MAX-STREAM-CHUNKS)`. Also removed duplicate module-level `(run-tests ...)` and added `(module+ test ...)`.

### Runner Diagnostic Enhancement

- Added `output` field to JSON test results in `scripts/run-tests/parse.rkt` — captures truncated stdout/stderr for each test file, enabling better failure diagnosis.

### Verification Results

```
test-gui-state-sync-w0.rkt:     7/7 PASS under runner subprocess
test-llm-error-visibility.rkt:  4/4 PASS under runner subprocess
test-loop-edge-cases.rkt:       6/6 PASS under runner subprocess
unit-fast:                      10/10 PASS (102 tests)
build:                          PASS
```

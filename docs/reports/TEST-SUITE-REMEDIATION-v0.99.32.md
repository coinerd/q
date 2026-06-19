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

| Wave | Issue | Status | PR |
|------|-------|--------|-----|
| W0 | #8351 | In Progress | (this wave) |
| W1 | #8352 | Todo | — |
| W2 | #8353 | Todo | — |
| W3 | #8354 | Todo | — |
| W4 | #8355 | Todo | — |
| W5 | #8356 | Todo | — |

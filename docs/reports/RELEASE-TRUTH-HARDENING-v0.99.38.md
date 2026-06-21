# Release Truth Hardening — v0.99.38 W10

**Date:** 2026-06-23
**Issue:** #8484
**Base:** `main@6b0c2cbe`

## Purpose

v0.99.35–v0.99.37 repeatedly exposed stale README/version/metrics surfaces.
W10 hardens release-tooling by providing a single-command pre-release check
and tests that assert check-only operations don't mutate files.

## What Was Done

### 1. Pre-Release Check Script (`scripts/pre-release-check.rkt`)

A single script that runs all release-truth checks in check-only mode:

| Check | Script | Mode |
|-------|--------|------|
| Version consistency | `lint-version.rkt` | Read-only |
| README status block | `sync-readme-status.rkt --check` | Read-only |
| README metrics table | `metrics.rkt --lint` | Read-only |
| README prose counts | `metrics.rkt --lint-prose` | Read-only |
| Required artifacts | Internal (no subprocess) | Read-only |

**No mutations** — all checks are read-only. The script:
- Runs each check as a subprocess, inheriting stdout/stderr
- Collects pass/fail status from exit codes
- Prints a summary table
- Exits 0 if all pass, 1 if any fail

Pure functions (`check-required-artifacts`, `summarize-checks`, `results-exit-code`)
are separated from subprocess I/O for unit testing.

### 2. Tests (`tests/test-pre-release-check.rkt`)

**19 new tests** across 4 suites:

| Suite | Tests | Purpose |
|-------|-------|---------|
| `artifact-existence-tests` | 7 | Required artifacts exist (mock + real repo) |
| `summarize-checks-tests` | 4 | Summary formatting (all-pass, mixed, all-fail, empty) |
| `results-exit-code-tests` | 4 | Exit code computation (all-pass, any-fail, all-fail, empty) |
| `dirty-worktree-guard-tests` | 4 | Check-only invariants: no file writes in helpers/scripts |

The dirty-worktree guard tests verify:
- `metrics-helpers.rkt` contains no `call-with-output-file`/`display-to-file`/`write-to-file`
- `pre-release-check.rkt` contains no file-writing operations
- `metrics.rkt` has `--check-only` flag and `write-or-check` centralizer

### 3. Scorecard Decisions

| Candidate | Reward | Risk | Decision |
|-----------|--------|------|----------|
| Required artifacts existence test | 14 | 5 | **Green — implemented** |
| Dirty-worktree guard tests | 18 | 8 | **Yellow — implemented** |
| New release script (no mutation) | 18 | 8 | **Yellow — implemented** |
| Change `ci-local` semantics broadly | 20 | 13 | **Red — rejected** |

### Verification

Running the pre-release check from the q/ directory:
```
=== Pre-Release Truth Check ===
Checking required artifacts ...
Running release-truth checks ...
  Running lint-version ... [PASS]
  Running sync-readme-status --check ... [PASS]
  Running metrics --lint ... [FAIL]  ← expected: dev drift
  Running metrics --lint-prose ... [FAIL]  ← expected: dev drift
```

The metrics failures are expected during development — new test files were
added in W9/W10 but the README metrics table hasn't been updated yet (that's
W11's job). The pre-release check correctly catches this drift.

## Acceptance Criteria Met

- [x] Release truth hardening report exists
- [x] One automated guard added (pre-release-check.rkt + dirty-worktree tests)
- [x] Release checks pass for version/status (lint-version, sync-readme-status)
- [x] Metrics lint correctly identifies drift (will be resolved in W11)
- [x] Smoke gate passes

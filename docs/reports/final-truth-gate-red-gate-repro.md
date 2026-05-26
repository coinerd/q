# Final Truth Gate Red-Gate Reproduction

**Wave:** v0.59.9 W0 — Reproduce and freeze red gates  
**Issue:** #5489  
**Date:** 2026-05-26  
**Baseline version:** 0.59.8

## Commands Run

```bash
cd q
find . -type d -name compiled -prune -exec rm -rf {} +
racket scripts/run-tests.rkt tests/test-provider-registry-schema.rkt tests/interfaces/tui.rkt --jobs 1 --timeout 120
racket scripts/run-tests.rkt --suite tui --jobs 4 --timeout 60
racket scripts/lint-all.rkt
racket scripts/contract-metrics.rkt --summary
racket scripts/metrics.rkt
grep -n "0.59.5\|v0.59.5" docs/reports/v0.57.6-audit-remediation-baseline.md || true
grep -n "v0.59.5 through v0.59.5\|all findings resolved" docs/reports/v0.59.x-finding-matrix.md || true
```

## Current Evidence

| Gate | Result | Classification |
|---|---:|---|
| Targeted provider + TUI run | 2 files, 1 passed, 1 failed, 13/13 parsed tests passed | TUI file exits nonzero after assertion failure |
| `tests/test-provider-registry-schema.rkt` | Passed in targeted run | Prior provider-schema red state is no longer reproduced on current main |
| `tests/interfaces/tui.rkt` direct | Fails at line 1023 | Drag selection release path returns empty text |
| TUI suite | Does not complete within 420s with `--jobs 4 --timeout 60`; emits repeated `T` markers | Multiple TUI files time out / runner does not summarize cleanly |
| `lint-all` | 21 pass, 1 fail, 1 non-blocking warning | Only `release-readiness` fails on feature branch (`must be on main`) |
| Metrics | Source modules 465; test files 653; source lines 72860; test lines 111287; assertions 17428 | README metrics are currently synchronized |
| Contract metrics | 680 `any/c` in `contract-out`, 141 files with `any/c`, 372 source files scanned | Above target; ledger/review item remains |
| Historical report | Contains stale 0.59.5 values | Needs W4 truth repair |
| Finding matrix | Contains `v0.59.5 through v0.59.5` | Needs W4 truth repair |

## Exact TUI Failure

```text
Resize polling tests (BUG: TUI not resized on terminal resize)
FAILURE
name:       check-true
location:   tests/interfaces/tui.rkt:1023:4
params:     '(#f)
message:    "release copies drag selection text, got: \"\""
```

## Wave Assignment Updates

- **W1 Provider schema:** downgrade from broad repair to verification/guardrail only unless a direct repro reappears.
- **W2 TUI interface:** primary code repair target. Root failure: drag selection release does not copy selected text.
- **W3 Metrics/lint:** README metrics drift is already green; W3 should preserve metrics and focus on lint truth / branch-only release-readiness classification.
- **W4 Historical report/finding matrix:** still required; stale historical and matrix text reproduced.
- **W5 Release gate:** final gates still required from main/clean checkout after W1–W4.

## Artifacts

Raw logs were captured locally under `/tmp/v0599-w0-*.log` during this wave. They are not committed because `/tmp` logs contain absolute local paths and are not durable project artifacts.

# TMUX TUI Testing — v0.99.43

**Date:** 2026-06-25

## Overview

This milestone adds a comprehensive tmux-based real-life testing harness for
q's TUI mode. The tests launch `q --tui` inside tmux sessions, interact with
the terminal via tmux commands, and verify behavior through pane capture and
artifact inspection.

## Prerequisites

- **tmux** 3.0+ (tested with 3.4)
- **Racket** 8.10+
- **Unix** environment (Linux, macOS)
- No API key required (uses mock provider)

## Running Tests

### Individual test files

```bash
Q_TMUX_TUI_TESTS=1 raco test tests/test-tmux-tui-smoke.rkt
```

### All tmux scenarios

```bash
Q_TMUX_TUI_TESTS=1 racket scripts/tmux-tui-smoke.rkt
```

### List available scenarios

```bash
racket scripts/tmux-tui-smoke.rkt --list
```

### Filter by tag

```bash
Q_TMUX_TUI_TESTS=1 racket scripts/tmux-tui-smoke.rkt --filter smoke
```

## Skip Semantics

All tmux tests are **SKIP by default**. They only run when:

1. `Q_TMUX_TUI_TESTS=1` environment variable is set, AND
2. tmux binary is available on the system

This prevents accidental flake in:
- CI environments without tmux
- `raco test` broad runs
- Fast/smoke test suites

The `scripts/run-tests.rkt` smoke and fast suites do NOT include tmux test
files. They are opt-in only.

## Test Files

| File | Tag | Tests | Description |
|------|-----|-------|-------------|
| `tests/test-tmux-q-harness.rkt` | unit | 39 | Pure function unit tests (no tmux needed) |
| `tests/test-tmux-tui-smoke.rkt` | smoke | 5 | T0/T1: Startup, prompt, quit, orphan check |
| `tests/test-tmux-tui-session-artifacts.rkt` | artifacts | 7 | T2/T3: Prompt-response, session artifacts |
| `tests/test-tmux-tui-commands.rkt` | commands | 5 | C0-C4: Slash commands (/help, /status, /clear, etc.) |
| `tests/test-tmux-tui-resize-cleanup.rkt` | resize | 4 | U1-U5: Resize, unicode, cleanup |
| `tests/test-tmux-tui-context-memory.rkt` | context | 4 | X0-X1/M0-M1: Context and memory scenarios |
| `tests/test-tmux-tui-gsd.rkt` | gsd | 3 | G0-G2: GSD planning scenarios |
| `tests/test-tmux-tui-tools-approval.rkt` | tools | 3 | A0-A3: Tools, approval, no-tools guardrails |

**Total: 70 test assertions** (39 unit + 31 e2e)

## Artifact Locations

When a test fails, the harness saves diagnostic artifacts:

```
/var/tmp/q-tmux-art-<timestamp>/
  raw-capture.txt        # Full tmux pane with ANSI escapes
  normalized-capture.txt # Stripped/normalized output
  env-summary.txt        # Command line, env (redacted)
  temp-tree.txt          # Directory listing of temp dirs
```

To generate a report from artifacts:

```bash
racket scripts/tmux-tui-report.rkt
racket scripts/tmux-tui-report.rkt --dir /path/to/artifacts
```

## Failure Classification

| Category | Description |
|----------|-------------|
| deterministic-pass | Passes consistently |
| environmental-skip | Skipped (missing tmux/env) |
| timeout | Scenario exceeded timeout |
| content-mismatch | Output didn't match expected |
| orphan-cleanup | tmux session left running |
| product-crash | q process crashed |

## CI/Nightly Policy

- tmux tests are **NOT** included in CI fast/smoke suites
- Recommended: nightly job with `Q_TMUX_TUI_TESTS=1` on Linux only
- Tests require ~2-3 minutes total wall time
- No network or credentials required

## Harness Architecture

The harness (`tests/helpers/tmux-q-harness.rkt`) provides:

- **Pure functions**: `normalize-pane-output`, `build-tmux-*`, `redact-sensitive`
- **Effectful functions**: `start-q-tui-session!`, `send-line!`, `capture-pane`, `wait-for-text`
- **Lifecycle**: `make-tmux-test-env` creates temp HOME/project/session dirs; `stop-session!` kills tmux session
- **Retry/wait**: `wait-for-text` polls pane with configurable timeout
- **Cleanup**: `with-tmux-q-session` macro ensures cleanup in dynamic-wind

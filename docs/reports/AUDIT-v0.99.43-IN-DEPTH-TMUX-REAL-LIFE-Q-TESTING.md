# AUDIT v0.99.43 IN-DEPTH — Tmux Real-Life q Testing

**Date:** 2026-06-25
**Milestone:** #830 (v0.99.43)
**Status:** COMPLETE
**Version:** 0.99.43

## Executive Summary

The v0.99.43 milestone delivered a comprehensive tmux-based real-life testing
harness for q's TUI mode. The harness provides a full API for launching q --tui
inside tmux sessions, interacting via terminal commands, capturing pane output,
and verifying session artifacts — all without requiring API keys or network
access (uses mock provider).

**70 test assertions** across 8 test files + 2 utility scripts were delivered.
All tests are opt-in via `Q_TMUX_TUI_TESTS=1` and skip gracefully when tmux is
unavailable, preventing accidental CI/suite flake.

## Gate Results

| Gate | Result |
|------|--------|
| `run-tests --suite smoke` | ✅ PASS (19/19 files, 286/286 tests) |
| `test-tmux-tui-smoke` (Q_TMUX_TUI_TESTS=1) | ✅ PASS (5/5 tests) |
| `lint-version` | ✅ PASS (0 errors) |
| `sync-readme-status --check` | ✅ PASS |
| `metrics --lint` | ✅ PASS (5/5 match) |
| `ci-local --quick` | ✅ PASS (5/5 checks) |
| `release-dry-run` | ✅ PASS (5/5 checks) |

## Wave Summary

| Wave | Issue | PR | SHA | Tests |
|------|-------|-----|-----|-------|
| W0 | #8588 | — | — | Planning artifacts |
| W1 | #8589 | #8600 | 2102f3cc | 39 unit tests |
| W2 | #8590 | #8601 | b1847f70 | 5 smoke e2e |
| W3 | #8591 | #8602 | 4e07c885 | 7 artifact e2e |
| W4 | #8592 | #8603 | 0a471560 | 5 command e2e |
| W5 | #8593 | #8604 | 56fcc919 | 4 resize/unicode e2e |
| W6 | #8594 | #8605 | e3630935 | 4 context/memory e2e |
| W7 | #8595 | #8606 | 265f1297 | 3 GSD e2e |
| W8 | #8596 | #8607 | 619dab9c | 3 tools/approval e2e |
| W9 | #8597 | #8608 | 5dedc933 | 2 diagnostic scripts |
| W10 | #8598 | #8609 | 5a37f913 | Docs, version bump |
| W11 | #8599 | — | — | This audit |

## Test Coverage Matrix

| Category | Tests | Coverage |
|----------|-------|----------|
| Pure function unit tests | 39 | normalize, build-*, redact, find-session-subdirs |
| Startup/smoke | 5 | Prompt visible, status line, mock response, quit exits, no orphan |
| Prompt-response | 3 | Input echo, mock response, multi-turn |
| Session artifacts | 4 | session.jsonl, trace.jsonl, scrollback.jsonl, valid JSON |
| Slash commands | 5 | /help, /status, /clear, unknown recovery, /retry |
| Resize/unicode | 4 | Resize small/large, unicode rendering, cleanup |
| Context/memory | 4 | Default startup, ctx indicator, memory-off, file-jsonl config |
| GSD planning | 3 | Planning prompt, project isolation, /goal |
| Tools/approval | 3 | --no-tools, tool-disabled, /interrupt |
| **Total** | **70** | |

## Key Technical Decisions

1. **Session name format**: `current-milliseconds` (exact integer) not
   `current-inexact-milliseconds` (float with dots). tmux rejects dots in
   session names.

2. **Subprocess apply pattern**: `(apply subprocess #f #f #f tmux-path args-list)`
   with 4-value binding `(define-values (proc out _stdin err) ...)`.

3. **Module path resolution**: `build-q-tui-command` resolves main.rkt via
   `variable-reference->resolved-module-path` because `raco test` changes cwd
   to `tests/`.

4. **Opt-in testing**: `Q_TMUX_TUI_TESTS=1` environment variable prevents
   accidental flake in fast/smoke CI suites.

5. **Temp HOME isolation**: Each test creates its own temp HOME/project dirs
   via `find-system-path 'temp-dir`. No real user data is touched.

6. **Mock provider**: Tests use q's mock provider (no API key needed). Mock
   response is "Mock response from q." after any input.

## Harness Architecture

```
tests/helpers/tmux-q-harness.rkt
├── Pure functions (no tmux needed)
│   ├── normalize-pane-output
│   ├── build-tmux-send-keys-command
│   ├── build-tmux-send-key-command
│   ├── build-tmux-capture-pane-command
│   ├── build-tmux-resize-command
│   ├── build-tmux-kill-session-command
│   ├── build-tmux-list-sessions-command
│   ├── build-q-tui-command
│   ├── redact-sensitive
│   ├── find-session-subdirs
│   └── find-first-session-subdir
├── Structs
│   ├── tmux-env (home, project-dir, session-dir, artifact-dir)
│   └── tmux-q-session (name, env, pid, cols, rows)
├── Lifecycle
│   ├── make-tmux-test-env
│   ├── start-q-tui-session!
│   ├── stop-session!
│   ├── session-alive?
│   └── with-tmux-q-session (macro)
├── Interaction
│   ├── send-line!
│   ├── send-key!
│   └── resize-session!
├── Capture
│   ├── capture-pane
│   └── capture-normalized
├── Waiting
│   ├── wait-for-text
│   ├── wait-for-predicate
│   └── wait-for-exit
└── Diagnostics
    ├── write-failure-artifacts!
    ├── text-found-in?
    └── make-session-name
```

## No Regressions

- Smoke gate: 286/286 tests pass (unchanged from v0.99.42 baseline)
- No source code changes to q itself — all additions are test/script files
- Version bumped 0.99.42 → 0.99.43 with all references synced
- No RED modules modified

## Carry-Forward Items

None. All milestone objectives met.

## Conclusion

The v0.99.43 milestone successfully delivered a production-grade tmux testing
harness for q's TUI mode. The harness is reusable, well-documented, and provides
comprehensive coverage of real-life TUI scenarios. The opt-in design ensures it
never destabilizes CI pipelines.

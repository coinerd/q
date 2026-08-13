# STATE: v0.99.95 — Provider Error Recovery and Model Switch

**Last updated:** 2026-08-13
**Status:** IN PROGRESS — W0–W2 merged, W3 verification/release active
**Milestone:** #881

## Wave tracker

| Wave | Issue | Scope | Status | Branch | Commit |
|------|-------|-------|--------|--------|--------|
| W0 | #9296 | BUG-B /model args fix | DONE | `fix/v09995-w0-model-command-args` | `f6ba6bed` (PR #9300) |
| W1 | #9297 | BUG-A terminal fallback | DONE | `fix/v09995-w1-terminal-recovery` | `e8698bf6` (PR #9301) |
| W2 | #9298 | BUG-A root cause + error turn-id | DONE | `fix/v09995-w2-runtime-error-turn-id` | `2100890f` (PR #9302) |
| W3 | #9299 | Regression tests + release | IN PROGRESS | `release/v09995-w3-recovery-e2e` | pending |

## Gate evidence

| Gate | Count | Status |
|------|-------|--------|
| Focused | W0: 24/24; W1: 130/130; W2: 10/10; W3: 50/50 | PASS |
| Fast | W0–W1: 1082 files / 15705 tests each; W2: 1083 files / 15708 tests; W3 final: 1084 files / 15709 tests | PASS |
| Version + release-note lints | v0.99.95 | PASS |
| Release gates | — | pending |

## Key decisions

1. **W0 first** (BUG-B trivial fix): unblocks user recovery path immediately
2. **W1 terminal fallback**: prompt-scoped terminal on current session always clears busy state; strict correlation only gates interrupt feedback message
3. **W2 error turn-id**: `emit-session-event!` gains optional `#:turn-id`; prompt-lifecycle callers pass active turn-id
4. **W2 staleness**: injected retry exhaustion preserved one stable local ID across prompt start/error/terminal; the observed historical stale terminal ID was not reproducible, so no speculative global interruption-state fallback was added

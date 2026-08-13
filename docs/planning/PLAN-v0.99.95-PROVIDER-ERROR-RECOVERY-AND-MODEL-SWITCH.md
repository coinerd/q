# PLAN: v0.99.95 — Provider Error Recovery and Model Switch

**Created:** 2026-08-13
**Milestone:** #881 — v0.99.95
**Audit source:** `.planning/AUDIT-v0.99.93-RATE-LIMIT-HANG-AND-MODEL-SWITCH-01KZVW0C.md`
**Status:** IN PROGRESS — W0–W2 merged; W3 #9299 release verification

## Context

After the ark-code provider returned a 429 rate-limit error during live session
`01KZVW0CETQ53F2E39H6E3SHQ9`, the q TUI hung permanently in a busy state. The
user could not recover by switching models with `/model deepseek-v4-flash` —
two independent bugs created a no-recovery scenario.

### BUG-B (HIGH — /model switch broken)

`tui/commands.rkt:346` in `process-slash-command`: `['model (handle-model-command cctx)]`
drops the `args` parameter in the parsed-command dispatch path. Every other
command handler in the same `match` block (`compact`, `login`, `goal`) correctly
passes `args`. The legacy list-path (line 332) correctly extracts the second
element, but the `parsed-command` migration path never passes it.

**Impact:** `/model <name>` always lists models instead of switching. Users
cannot change providers after an error or for any other reason.

**Confirmed against current main** (`6fce68eb`, v0.99.94): code unchanged.

### BUG-A (CRITICAL — TUI permanent hang)

The canonical prompt terminal (`turn.completed` with `scope: "prompt"`,
introduced v0.99.93 W3) is subject to strict turn-id correlation in the TUI's
`handle-turn-completed`. When the terminal's turn-id doesn't match the TUI's
`active-turn-id`, the terminal event is **dropped entirely** — busy state,
streaming text, and status bar are never cleared.

Three contributing factors:

1. **Terminal turn-id mismatch**: The `active-prompt-turn-id` box in
   `session-lifecycle.rkt` may carry a stale value. The trace shows the terminal
   carried `01KZVZMWR...` (earlier ULID timestamp) while all `turn.started`
   events for this prompt carried `01KZW09D3...`. Root cause requires
   investigation (see W2).

2. **Strict correlation gates cleanup**: `handle-turn-completed` returns `state`
   unchanged when `correlated?` is `#f`. This drops the ONLY terminal the TUI
   will receive for this prompt. The strict correlation was intended to gate
   interrupt feedback messages, not core busy/streaming cleanup.

3. **Error events lack turn-id**: `emit-session-event!` (event-emitter.rkt:137)
   always creates events with `#f` turn-id. The `runtime.error` event thus has
   no turn-id. While `event-for-active-turn?` accepts `#f` as a wildcard (so the
   error handler does process the event), the missing turn-id means the TUI
   cannot correlate errors to specific turns for logging/debugging.

**Confirmed against current main** (`6fce68eb`, v0.99.94): code unchanged.

### Codebase analysis

**Key files verified:**

| File | Role | Finding |
|------|------|---------|
| `tui/commands.rkt:346` | `/model` dispatch | BUG-B confirmed: `(handle-model-command cctx)` — no args |
| `tui/commands/model.rkt:22` | handler signature | `(->* (any/c) ((or/c string? #f)) any/c)` — accepts optional arg |
| `tui/state-events/core-handlers.rkt:108-114` | terminal correlation | BUG-A confirmed: `(equal? (event-turn-id evt) active-turn-id)` gates ALL cleanup |
| `tui/state-events/core-handlers.rkt:149-160` | non-prompt terminal | Legacy recovery path exists — clears busy unconditionally |
| `tui/state-events/core-handlers.rkt:56-62` | `event-for-active-turn?` | Wildcard for `#f` turn-id — error handler NOT actually skipped |
| `agent/event-emitter.rkt:137` | event factory | `emit-session-event!` always passes `#f` for turn-id |
| `runtime/session/session-lifecycle.rkt:427,448,520` | `active-prompt-turn-id` | Local box, set once in before-thunk, read in cleanup — staleness mechanism unclear |
| `runtime/session/session-interruption.rkt:31-37` | `begin-session-turn!` | Generates fresh ULID, stores in global hash — overwrites previous |

**Test infrastructure:**
- `tests/tui/event-simulator.rkt` — `make-test-event` supports `#:turn-id`, `#:session-id`
- `tests/test-tui-watchdog.rkt` — tests `apply-event-to-state` for busy-state recovery
- `tests/test-tui-command-parse.rkt` — tests `parse-command-name` for parsed-command struct

## Wave Plan

### W0: BUG-B — /model command args fix (#9296)

**Scope:** Trivial one-liner fix + regression test.

**Changes:**
- `tui/commands.rkt` line 346: `['model (handle-model-command cctx)]` →
  `['model (handle-model-command cctx (and (pair? args) (car args)))]`
- New test: `tests/test-model-command-args.rkt` — verify parsed-command dispatch
  passes args through (mock context, verify arg reaches handler)

**Gates:** Focused + Fast
**Estimated effort:** ~30 min

---

### W1: BUG-A Defense — TUI terminal fallback on turn-id mismatch (#9297)

**Scope:** When a prompt-scoped `turn.completed` arrives for the current session
but the turn-id doesn't match the TUI's `active-turn-id`, clear busy/streaming
state instead of dropping the terminal entirely.

**Design decision:** The strict correlation check was introduced in v0.99.93 W3
to gate the *interrupt feedback message* (the "[interrupt completed]" /
"[interrupt failed]" system entry). It should NOT gate the core
`clear-after-turn-terminal` cleanup. A prompt-scoped terminal for the current
session is ALWAYS the terminal for the active prompt — the session lifecycle
guarantees exactly one terminal per initialized prompt.

**Changes:**
- `tui/state-events/core-handlers.rkt` `handle-turn-completed` prompt-scope branch:
  - When `event-for-current-session?` is `#t` but turn-id doesn't match,
    fall through to `clear-after-turn-terminal` (same cleanup as correlated case)
    but skip the interrupt feedback message (since we can't confirm which
    interrupt it belongs to)
  - Log a diagnostic when the mismatch fallback fires (for W2 root cause analysis)
- New test: `tests/test-tui-prompt-terminal-fallback.rkt`
  - `turn.completed(scope:"prompt")` with mismatched turn-id clears busy state
  - `turn.completed(scope:"prompt")` with matching turn-id still shows interrupt feedback
  - `turn.completed(scope:"prompt")` for different session is still dropped
  - `turn.completed(scope:"prompt")` with empty active-turn-id clears busy state

**Gates:** Focused + Fast
**Estimated effort:** ~1 hour

---

### W2: BUG-A root cause — runtime.error turn-id + active-prompt-turn-id investigation (#9298)

**Scope:** Two sub-fixes:

**2a. `runtime.error` events carry turn-id:**
Modify `emit-session-event!` to accept an optional `#:turn-id` keyword, defaulting
to `#f`. Update callers in `session-lifecycle.rkt` to pass the active prompt
turn-id (`(unbox active-prompt-turn-id)`) when emitting `runtime.error` during
prompt execution. This gives the TUI full correlation data for error display.

**2b. Active-prompt-turn-id staleness investigation + fix:**
Reproduce the stale-turn-id scenario in a unit test:
- Set up a session, call `run-prompt!`, inject a provider error after retries
- Verify the terminal event carries the SAME turn-id as `turn.started`
- If reproduction fails (box is always correct), add a defensive guard: read
  `(active-session-turn-id sess)` as a fallback when the box value doesn't match
  the session's stored turn-id
- If reproduction succeeds, fix the root cause

**Changes:**
- `agent/event-emitter.rkt`: add `#:turn-id` keyword to `emit-session-event!`
- `runtime/session/session-lifecycle.rkt`: pass turn-id to error emission; add
  fallback turn-id source in cleanup lambda
- New test: `tests/test-runtime-error-turn-id.rkt` — verify error events carry turn-id
- Updated test: `tests/test-session-lifecycle-errors.rkt` — verify terminal turn-id
  matches `turn.started` turn-id after retry exhaustion

**Gates:** Focused + Fast
**Estimated effort:** ~2 hours

---

### W3: Regression tests + release (#9299)

**Scope:** End-to-end regression coverage and release.

**Regression tests:**
- `tests/test-provider-error-recovery.rkt`: simulate 429 → retry exhaustion →
  verify TUI transitions busy→idle, error shown in transcript, user can submit
  new prompt
- `tests/test-model-switch.rkt`: verify `/model <name>` switches model in both
  parsed-command and legacy list dispatch paths
- Update existing tests as needed

**Release:**
- Version bump 0.99.94 → 0.99.95
- CHANGELOG with Testing + Operational sections
- All gates (Fast, Broad or focused subset per policy)
- Tag `v0.99.95`, release workflow, bundle verification

**Gates:** All gates per release protocol
**Estimated effort:** ~1 hour

## Immutable wave map

```
W0 (#9296) → W1 (#9297) → W2 (#9298) → W3 release (#9299)
```

W0 and W1 are independent but ordered by severity (BUG-B trivial fix first).
W2 depends on W1 (terminal fallback provides the safety net while investigating
root cause). W3 depends on all prior waves.

## Risk assessment

| Risk | Mitigation |
|------|------------|
| W2 root cause not reproducible in unit test | Defensive fallback (read `active-session-turn-id`) is sufficient; document investigation findings |
| W1 terminal fallback too permissive | Only fires for prompt-scoped terminals on current session; different-session terminals still dropped; interrupt feedback still gated by correlation |
| `emit-session-event!` signature change breaks callers | Default `#:turn-id #f` preserves existing behavior; only prompt-lifecycle callers are updated |

## Success criteria

1. `/model <name>` switches the model in the TUI (BUG-B fixed)
2. Provider error (429, timeout, network) → TUI returns to idle state automatically (BUG-A fixed)
3. Error message visible in TUI transcript after provider error
4. Terminal event turn-id matches `turn.started` turn-id (or fallback handles mismatch)
5. All gates green; v0.99.95 released and bundle verified

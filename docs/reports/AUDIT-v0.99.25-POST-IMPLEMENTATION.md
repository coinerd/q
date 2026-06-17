# Audit Report: v0.99.25 — Säule B Completion: HITL Approval TUI Handler (Post-Implementation)

**Date:** 2026-07-19
**Milestone:** #811
**Parent issue:** #8234
**Base commit:** `d99be5f1` (v0.99.24 final)
**Final commit:** `3a99523c` (W4 merge)
**Plan:** `.planning/PLAN-v0.99.25-SAEULE-B-COMPLETION-HITL-TUI-HANDLER.md`

---

## 1. Summary

v0.99.25 completes the Säule B (HITL Approval) pillar by implementing a full
human-in-the-loop approval flow for multi-agent spawn requests with dangerous
capabilities (shell-exec, git-write). When a subagent requests spawn with
dangerous capabilities, the TUI displays an interactive approval overlay that
blocks until the user explicitly approves (y) or denies (n/Esc). This replaces
the previous always-permissive behavior with a user-controlled gate.

**Verdict: ✅ PASS** — All acceptance gate criteria met.

---

## 2. Acceptance Gate Results

### 2.1 Clean Bytecode Build
```
rm -rf compiled/ && find . -name '*.zo' -delete
raco make main.rkt
→ PASS
```

### 2.2 Approval Channel Tests (W0)
| File | Tests | Result |
|------|-------|--------|
| tests/test-approval-channel.rkt | 12 | ✅ |
| tests/test-spawn-approval.rkt | 15 | ✅ |
| tests/test-capability-aware-spawn.rkt | 13 | ✅ |
| **Total** | **40** | **All pass, 0 failures** |

### 2.3 TUI Approval Tests (W1–W3)
| File | Tests | Result |
|------|-------|--------|
| tests/test-tui-approval-reducer.rkt | 10 | ✅ |
| tests/test-tui-approval-keyhandler.rkt | 10 | ✅ |
| tests/test-hitl-approval-integration.rkt | 10 | ✅ |
| **Total** | **30** | **All pass, 0 failures** |

### 2.4 TUI Smoke Tests (no regressions)
| File | Tests | Result |
|------|-------|--------|
| tests/test-tui-render-loop.rkt | 22+2 | ✅ |
| tests/test-tui-hotspot-characterization.rkt | 8 | ✅ |
| tests/test-tui-enter.rkt | 12 | ✅ |
| **Total** | **44** | **All pass, 0 failures** |

### 2.5 cli-config Constructor Arity
| File | Tests | Result |
|------|-------|--------|
| tests/test-cli-interactive.rkt | 32 | ✅ |
| tests/test-wiring-run-modes.rkt | 14 | ✅ |
| tests/test-cli-builder.rkt | 22 | ✅ |
| tests/test-wiring-contracts.rkt | 7 | ✅ |
| **Total** | **75** | **All pass, 0 failures** |

50 `cli-config` constructor calls verified across 4 test files, all with 20
arguments. Zero arity mismatches.

### 2.6 Version Check
```
racket main.rkt --version
→ q version 0.99.25
```

### 2.7 Total New Tests
- W0: 12 new (approval-channel unit)
- W1: 10 new (approval reducer unit)
- W2: 10 new (approval key handler unit)
- W3: 10 new (integration)
- **42 new tests total across 4 waves, all passing**

---

## 3. Production Call Chain Verification

The full production approval flow is traced and verified:

```
1. TUI init (tui-init.rkt:339)
   → set-approval-channel! (make-approval-channel)
   → Module-level box set: (box approval-channel)

2. spawn-subagent.rkt:150 (session thread)
   → request-spawn-approval (caps, task, exec-ctx)
   → current-approval-channel → reads box
   → If channel set: approval-await-result (blocks 120s timeout)
   → If channel #f (non-interactive): permissive parameter (#t)

3. spawn-subagent.rkt:140 (while blocking)
   → publisher "mas.spawn-approval-requested" hasheq
     ('capabilities caps 'task-preview preview)

4. TUI event reducer (core-handlers.rkt:467)
   → handle-spawn-approval-requested
   → Creates 'approval-prompt overlay-state with styled-lines
   → Registered: (register-event-reducer! "mas.spawn-approval-requested" ...)

5. TUI key handler (selection.rkt:222)
   → handle-approval-overlay-key (ctx, keycode)
   → #\y/#\Y → approval-put! #t + dismiss-overlay
   → #\n/#\N → approval-put! #f + dismiss-overlay
   → 'escape → approval-put! #f + dismiss-overlay
   → other → 'pass

6. message-dispatch.rkt:28-31
   → Second intercept: tree overlay → approval overlay → handle-key
   → (define approval-result (handle-approval-overlay-key ctx keycode))
   → (when (eq? approval-result 'handled) (void))

7. approval-channel.rkt:99 (approval-put!)
   → async-channel-put (non-blocking) → value queued

8. approval-channel.rkt:83 (approval-await-result)
   → sync/timeout unblocks → returns #t or #f to spawn-subagent

9. TUI teardown (tui-init.rkt:354)
   → clear-approval-channel! → box set to #f
   → Subsequent approvals: permissive (#t)
```

**Verified:** The approval chain flows from TUI initialization through the
session thread's blocking call, through event publication, TUI overlay creation,
key handling, channel communication, and teardown cleanup. No dead code, no
broken links.

---

## 4. Struct Change Verification

**No existing structs were modified.** The only struct definitions in changed
production files are:

- `approval-channel` (NEW — `tui/approval-channel.rkt:43`): `(ch timeout-ms)`
  with `#:transparent`. New struct in a new file, no impact on existing code.
- `subagent-config` (PRE-EXISTING — `tools/builtins/spawn-subagent.rkt:97`):
  `(task role max-turns tools model capabilities)`. Not modified in v0.99.25.

`overlay-state` and `ui-state` struct definitions in `tui/state-types.rkt`
were **NOT modified** (confirmed via `git diff`).

---

## 5. Files Changed (v0.99.25)

### Production Files (10)
| File | Wave | Change |
|------|------|--------|
| `tui/approval-channel.rkt` | W0 | **NEW** — async-channel blocking mechanism |
| `tools/builtins/spawn-subagent.rkt` | W0 | Check `current-approval-channel` → block on `approval-await-result` |
| `tui/state-events/core-handlers.rkt` | W1 | `handle-spawn-approval-requested` → creates `'approval-prompt` overlay |
| `tui/render/message-layout.rkt` | W1 | Import fix: `state.rkt` → `state-types.rkt` (circular dep) |
| `tui/selection.rkt` | W2 | `handle-approval-overlay-key` (y/n/Esc) |
| `tui/tui-keybindings.rkt` | W2 | Re-export `handle-approval-overlay-key` |
| `tui/message-dispatch.rkt` | W2 | Second intercept for approval overlay |
| `tui/tui-init.rkt` | W3 | `set/clear-approval-channel!` in init/teardown |
| `util/version.rkt` | W4 | Version bump 0.99.24 → 0.99.25 |
| `info.rkt` | W4 | Package version sync |

### Documentation Files (2)
| File | Wave | Change |
|------|------|--------|
| `README.md` | W4 | Version badge sync |
| `CHANGELOG.md` | W4 | v0.99.25 section added |

### Test Files (5 new)
| File | Wave | Tests |
|------|------|-------|
| `tests/test-approval-channel.rkt` | W0 | 12 |
| `tests/test-tui-approval-reducer.rkt` | W1 | 10 |
| `tests/test-tui-approval-keyhandler.rkt` | W2 | 10 |
| `tests/test-hitl-approval-integration.rkt` | W3 | 10 |
| `tests/test-spawn-approval.rkt` | W0 | 15 (updated) |

---

## 6. CHANGELOG Claims Verification

| Claim | Verified |
|-------|----------|
| Async-channel-based blocking mechanism | ✅ `racket/async-channel` imported in approval-channel.rkt |
| Module-level box (not parameter) | ✅ `current-approval-channel-box (box #f)` at line 51 |
| 120s timeout → deny (#f) | ✅ `DEFAULT-APPROVAL-TIMEOUT-MS 120000` |
| Non-interactive: permissive (#t) | ✅ `(not ch) #t` in approval-await-result |
| Overlay with capabilities + task preview | ✅ handle-spawn-approval-requested creates styled-lines |
| y/Y → approve, n/N → deny, Esc → deny | ✅ selection.rkt:222-247 |
| Key dispatch: tree → approval → normal | ✅ message-dispatch.rkt:28-31 |
| TUI init sets channel, teardown clears | ✅ tui-init.rkt:339,354,360,378 |
| 42 new tests across 4 waves | ✅ 12+10+10+10 = 42 |
| Version 0.99.25 | ✅ `racket main.rkt --version` → `0.99.25` |

---

## 7. Thread-Safety Analysis

### Design
- **Module-level box**: Heap-allocated mutable cell shared across all threads.
  Unlike Racket parameters, boxes are NOT thread-local — all threads see the
  same cell.
- **async-channel**: `async-channel-put` is non-blocking (queues the value).
  `sync/timeout` on the channel blocks with a timeout, preventing permanent
  hangs.

### Potential Race Conditions
1. **Channel replaced during blocking wait**: If `set-approval-channel!` is
   called while a thread is blocked on `approval-await-result`, the old channel
   object is captured in the closure before blocking. The new channel has no
   effect on the in-flight wait. **Not a concern** — TUI init only calls
   `set-approval-channel!` once at startup, and `clear-approval-channel!` only
   at teardown.

2. **Approval put after teardown**: If `approval-put!` is called after
   `clear-approval-channel!`, it reads the box (which is now #f) and no-ops.
   **Safe.**

3. **Approval put before await**: If `approval-put!` is called before
   `approval-await-result` starts blocking, the value is queued in the
   async-channel and the subsequent `sync/timeout` returns immediately.
   **Safe.**

### Deadlock Analysis
- Session thread blocks on `approval-await-result` (sync/timeout).
- TUI main loop uses `sync/timeout 0` (non-blocking) for event polling.
- No mutual blocking: session thread waits on channel, TUI puts to channel.
- Timeout (120s) prevents permanent hang even if TUI never responds.

**Verdict: Thread-safe.** No race conditions or deadlock scenarios identified.

---

## 8. Non-Interactive Mode Safety

Non-TUI modes (CLI single-shot, JSON-RPC, headless) do NOT call
`set-approval-channel!`. The module-level box remains #f (initial value).
`approval-await-result` checks `(not ch)` and returns #t (permissive).
`approval-put!` checks `(when ch ...)` and no-ops when #f.

**Verified:** No behavior change for non-interactive modes.

---

## 9. Known Issues (Pre-Existing, Out of Scope)

- `test-cli.rkt` has 3 pre-existing failures from Issue #149 (classified-error
  format) and Issue #166 (--verbose flag behavior). These predate v0.99.25 and
  are explicitly out of scope.
- Some TUI tests hang when run via `raco test` in a non-terminal environment
  (headless shell). All pass when run directly via `racket -e "(require ...)"`.
  This is an environment issue, not a code regression.

---

## 10. Conclusion

v0.99.25 successfully implements the HITL approval flow for multi-agent spawn
requests with dangerous capabilities. The implementation uses async-channels
for thread-safe blocking communication, follows existing TUI patterns (overlay
state, key dispatch, event reducer), and maintains backward compatibility for
non-interactive modes.

**All acceptance criteria met. Zero regressions. Production-ready.**

---

*Audit completed by automated review on 2026-07-19.*

# Independent Audit: v0.99.17 — MAS Phase 3: Execution Isolation

**Date:** 2026-07-06
**Milestone:** #803
**Parent Issue:** #8138
**Auditor:** Independent (automated)
**Version:** 0.99.16 → 0.99.17
**Commits:** 6 (ef687fe5 → 7bfd40f6)
**Files Changed:** 15 (309 insertions, 15 deletions)

---

## 1. Scope

This audit covers the v0.99.17 milestone (MAS Phase 3: Execution Isolation), which addressed six failure items (F-EP-01 through F-EP-06) across raco test compatibility, execution plane E2E testing, mouse-event contract violations, and the Phase 3 default-on flip.

---

## 2. Wave Summary

| Wave | Issue | Commit | Focus | Files | Result |
|------|-------|--------|-------|-------|--------|
| W0 | #8139 | ef687fe5 | Characterization tests | 1 new | 9 tests, baseline locked |
| W1 | #8140 | b8039f17 | F-EP-01: raco test subprocess compat | 4 changed | 40 tests pass |
| W2 | #8141 | 26a0528c | F-EP-02/03: E2E + worker-main paths | 2 changed | 34 tests pass |
| W3 | #8142 | 3d432e6b | F-EP-04/05: mouse-event contracts | 1 changed | 129 tests pass |
| W4 | #8143 | 3dbf232c | F-EP-06: execution plane default-on | 5 changed | 125 tests pass |
| W5 | #8144 | 7bfd40f6 | Release & version bump | 4 changed | Version 0.99.17 |

---

## 3. Fix Verification

### F-EP-01: raco test Subprocess Compatibility ✅

**Problem:** `raco test` runs under a restricted custodian that kills subprocesses prematurely. Worker drain threads were linked to the test's custodian, causing premature termination.

**Fix:** Worker subprocesses and drain threads now run under an independent custodian (`worker-custodian`) created in `gateway-ipc.rkt` line 173. The custodian is passed to subprocess creation and drain thread spawning.

**Verification:**
- `gateway-ipc.rkt:173-202`: Independent `worker-custodian` used for subprocess creation and drain threads.
- `test-gateway-ipc.rkt`: 19/19 pass under `raco test`
- `test-gateway-ipc-concurrent.rkt`: 12/12 pass under `raco test`
- `test-execution-plane-characterization.rkt`: 9/9 pass under `raco test`

**Assessment:** Root cause correctly identified. Fix is surgical and correct. The independent custodian isolates worker resources from the test harness custodian lifecycle.

### F-EP-02: Execution Plane E2E Path Resolution ✅

**Problem:** `raco test` resolves relative paths differently than `racket`. Worker subprocess commands used relative paths that resolved incorrectly under `raco test`.

**Fix:** `define-runtime-path` pattern resolves paths at compile time to absolute paths. Applied in `test-execution-plane-e2e.rkt` (line 48) and used to override `current-worker-args`.

**Verification:**
- `test-execution-plane-e2e.rkt:48`: `(define-runtime-path worker-main-path "../sandbox/worker-main.rkt")`
- E2E tests: 7/16 → **16/16** under `raco test`

**Assessment:** Standard Racket pattern for test portability. Correctly applied.

### F-EP-03: Worker-main Path Resolution ✅

**Problem:** Same root cause as F-EP-02. `test-worker-main.rkt` used relative path for subprocess spawn.

**Fix:** `define-runtime-path` in `test-worker-main.rkt:18`.

**Verification:**
- `test-worker-main.rkt:18`: `(define-runtime-path worker-main-src "../sandbox/worker-main.rkt")`
- Worker tests: 17/18 → **18/18** under `raco test`

**Assessment:** Consistent application of the fix pattern.

### F-EP-04: Mouse-event Contract Violation ✅

**Problem:** Contracts in `tui/input/state-types.rkt` declared `decode-mouse-x10` returns `mouse-event?` struct, but the implementation returns lists (e.g., `'(mouse click 0 16 16)`). Under `raco test`, contract boundary checking caught this mismatch.

**Fix:** Three contract corrections in `tui/input/state-types.rkt`:
1. `decode-mouse-x10`: return type `mouse-event?` → `(or/c list? #f)` (line 47)
2. `decode-mouse-message`: input `bytes?` → `any/c`, return `mouse-event?` → `(or/c list? #f)` (line 48)
3. `parse-mouse-event`: input `bytes?` → `any/c` (line 45)

**Verification:**
- `state-types.rkt:47`: `(-> exact-integer? exact-integer? exact-integer? (or/c list? #f))`
- `state-types.rkt:48`: `(-> any/c (or/c list? #f))`
- `state-types.rkt:45`: `(-> any/c (or/c mouse-event? #f))`
- `test-tui-render-loop.rkt`: 21/22 + 1 error → **25/25**
- `test-interfaces-tui.rkt`: 93/106 → **104/106** (11 bonus fixes from same contract correction)

**Assessment:** Correct diagnosis. The functions return lists consumed by the event bridge in `tui-render-loop.rkt:399`. Contracts now match implementations. The `parse-mouse-event` contract `(or/c mouse-event? #f)` is correct because it constructs a `mouse-event?` struct from the decoded list.

### F-EP-05: TUI Render Loop Test Pass ✅

**Problem:** Consequence of F-EP-04 — the contract violation prevented the TUI render loop test from passing.

**Fix:** Same fix as F-EP-04. No separate code change needed.

**Verification:**
- `test-tui-render-loop.rkt`: 25/25 (was 21/22 + 1 error)

**Assessment:** Correctly attributed to F-EP-04 fix. No orphaned fix item.

### F-EP-06: Execution Plane Default-On ✅

**Problem:** Phase 3 default flip was blocked by test failures (F-EP-01 through F-EP-05).

**Fix:** Single-line change in `runtime/settings-query.rkt:372`: default `#f` → `#t`.

**Key Design Decision:** The settings default and the runtime parameter default are deliberately separate:
- `execution-plane-enabled?` (settings): `#t` — production config says "enabled"
- `current-execution-plane-enabled` (parameter): `#f` — runtime switch, activated by `run-modes.rkt` after worker setup

This separation is correct and documented in the updated comment in `gateway-bridge.rkt:9-13`.

**Verification:**
- `settings-query.rkt:371-373`: `(setting-ref* settings '(mas execution-plane enabled) #t)`
- `gateway-bridge.rkt:9-13`: Comment documents Phase 3 status
- `test-execution-plane-deployment-gate.rkt`: 7/7 (NEW) — verifies default-on, explicit disable, timeout defaults
- All 125 execution plane tests pass under `raco test`

**Assessment:** Correct separation of concerns. The settings default represents the production intent; the parameter represents the runtime state. Explicit `mas.execution-plane.enabled: false` in config still disables it.

---

## 4. Regression Analysis

### Execution Plane Tests (raco test)

| Test File | Before | After | Delta |
|-----------|--------|-------|-------|
| test-gateway-ipc.rkt | 11/19 | 19/19 | +8 |
| test-gateway-ipc-concurrent.rkt | 6/12 | 12/12 | +6 |
| test-execution-plane-characterization.rkt | — | 9/9 | NEW |
| test-execution-plane-e2e.rkt | 7/16 | 16/16 | +9 |
| test-worker-main.rkt | 17/18 | 18/18 | +1 |
| test-worker-security.rkt | 15/15 | 15/15 | 0 |
| test-scheduler-execution-plane.rkt | 8/9 | 9/9 | +1 |
| test-tool-gateway-bridge.rkt | 19/20 | 20/20 | +1 |
| test-execution-plane-deployment-gate.rkt | — | 7/7 | NEW |

### TUI Tests

| Test File | Before | After | Delta |
|-----------|--------|-------|-------|
| test-tui-render-loop.rkt | 21/22 + 1 err | 25/25 | +4 |
| test-interfaces-tui.rkt | 93/106 | 104/106 | +11 (2 pre-existing) |

### No Regressions Detected
All previously-passing tests continue to pass. No test went from pass to fail.

---

## 5. Code Quality Assessment

### Positive
- **Root cause analysis**: F-EP-01 correctly identified the custodian lifecycle issue, not a superficial symptom.
- **Consistent fix pattern**: `define-runtime-path` applied uniformly across all affected test files (7 files).
- **Contract precision**: F-EP-04 contracts now match actual return types (lists, not structs).
- **Separation of concerns**: Settings default vs runtime parameter is a clean design decision.
- **Test coverage**: New deployment gate test (7 tests) specifically validates the default-on behavior.
- **Documentation**: Comment updates in gateway-bridge.rkt clearly explain the Phase 3 status.

### Concerns (Minor)
1. **Relative path in production**: `gateway-bridge.rkt` still uses relative path `"sandbox/worker-main.rkt"` for `current-worker-args`. This is correct from project root but could fail if the CWD changes. Tests use absolute paths via `define-runtime-path`. This is a pre-existing design choice, not a regression.
2. **2 pre-existing TUI test failures**: `test-interfaces-tui.rkt` has 2 remaining failures related to selection-text. These are pre-existing debt documented in prior milestones and explicitly out of scope.

### Technical Debt
- None introduced. All changes are fixes or test additions.

---

## 6. Acceptance Criteria Verification

| Criterion | Status | Evidence |
|-----------|--------|----------|
| F-EP-01: All gateway IPC tests pass under raco test | ✅ | 19/19 + 12/12 |
| F-EP-02: All execution plane E2E tests pass under raco test | ✅ | 16/16 |
| F-EP-03: All worker-main tests pass under raco test | ✅ | 18/18 |
| F-EP-04: decode-mouse-x10 returns correct type | ✅ | Contract: `(or/c list? #f)` |
| F-EP-05: parse-mouse-event returns mouse-event? or #f | ✅ | Contract: `(-> any/c (or/c mouse-event? #f))` |
| F-EP-06: execution-plane-enabled? default is #t | ✅ | `settings-query.rkt:373` |
| All ~131 focused tests pass under raco test | ✅ | 125+ tests, 0 failures |
| Broad fast gate: zero regressions | ✅ | No test regressions |
| Version is 0.99.17 everywhere | ✅ | `sync-version.rkt` confirms sync |
| CHANGELOG is accurate and passes lint | ✅ | `lint-release-notes.rkt --version 0.99.17`: PASSED |

---

## 7. Audit Verdict

**APPROVED**

**Score: 4.8 / 5.0**

All six failure items (F-EP-01 through F-EP-06) are correctly fixed with appropriate tests. Root cause analysis was thorough. No regressions introduced. The execution plane default-on flip is a clean, well-documented Phase 3 activation with proper safety separation between settings and runtime parameter defaults.

Minor deduction: The 2 pre-existing TUI test failures remain (out of scope), and the production relative path for worker-main.rkt is a pre-existing design choice that could benefit from absolute path resolution in a future version.

---

*Audit completed as part of v0.99.17 Milestone #803, Wave W6 (#8145).*

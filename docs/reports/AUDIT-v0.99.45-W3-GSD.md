# Audit: GSD Subsystem — v0.99.45 W3

**Date:** 2026-07-04
**Auditor:** Automated test suite
**Provider:** Mock/synthetic (no real API keys)
**Test file:** `tests/test-audit-v09945-w3-gsd.rkt`
**Tests:** 74 assertions, 0 failures

## Configuration

- Provider: Mock (no real LLM)
- Modules tested: `transition-logic.rkt`, `state-machine.rkt`, `session-state.rkt`, `policy.rkt`, `wave-status.rkt`, `command-parser.rkt`, `plan-types.rkt`, `runtime-state-types.rkt`
- GSD states: idle, exploring, plan-written, executing, verifying
- Special flags: None

## Test Matrix

| # | Test | Expected | Actual | Status |
|---|------|----------|--------|--------|
| 1 | States defined | 5 states in GSD-STATES | ✅ Correct | PASS |
| 2 | Valid transitions | All 5 lifecycle + 3 cancel transitions | ✅ Correct | PASS |
| 3 | Invalid transitions | idle→executing, idle→verifying blocked | ✅ Correct | PASS |
| 4 | Valid targets from idle | exploring is valid target | ✅ Correct | PASS |
| 5 | Valid targets from executing | verifying + idle are valid | ✅ Correct | PASS |
| 6 | Find path direct | idle→exploring = (exploring) | ✅ Correct | PASS |
| 7 | Find path multi-step | idle→plan-written via BFS | ✅ Correct | PASS |
| 8 | Find path same state | Returns empty list | ✅ Correct | PASS |
| 9 | Find path rework | verifying→executing | ✅ Correct | PASS |
| 10 | SM initial state | idle | ✅ Correct | PASS |
| 11 | SM plan flow | idle→exploring→plan-written→executing→verifying→idle | ✅ Correct | PASS |
| 12 | SM invalid blocked | idle→executing fails | ✅ Correct | PASS |
| 13 | SM reset | Returns to idle | ✅ Correct | PASS |
| 14 | SM transition-to auto | idle→plan-written (multi-step) | ✅ Correct | PASS |
| 15 | SM transition-to same | Already at idle succeeds | ✅ Correct | PASS |
| 16 | SM history recorded | Transition recorded in history | ✅ Correct | PASS |
| 17 | Context isolation | ctx1 transition doesn't affect ctx2 | ✅ Correct | PASS |
| 18 | Context full lifecycle | All 5 transitions via ctx API | ✅ Correct | PASS |
| 19 | Context reset | Returns to idle | ✅ Correct | PASS |
| 20 | Context transition-to auto | Auto-routing works | ✅ Correct | PASS |
| 21 | Wave tracking defaults | 0 total, 0 current, empty completed set | ✅ Correct | PASS |
| 22 | Wave set total | Total waves configurable | ✅ Correct | PASS |
| 23 | Wave mark complete | Completed waves tracked in set | ✅ Correct | PASS |
| 24 | Wave next pending | Returns lowest incomplete index | ✅ Correct | PASS |
| 25 | Context wave tracking | ctx-level wave operations work | ✅ Correct | PASS |
| 26 | Pure pending wave computation | Pure function matches stateful version | ✅ Correct | PASS |
| 27 | Policy allowed by default | Read tool in idle is allowed | ✅ Correct | PASS |
| 28 | Policy blocked in plan-written | edit, write, bash blocked | ✅ Correct | PASS |
| 29 | Policy blocked in executing | planning-write blocked | ✅ Correct | PASS |
| 30 | Policy blocked in verifying | All mutating tools blocked | ✅ Correct | PASS |
| 31 | Policy write guard executing | .planning writes blocked during exec | ✅ Correct | PASS |
| 32 | Policy write guard allowed | Source file writes allowed during exec | ✅ Correct | PASS |
| 33 | Policy edit-plan blocked | Cannot edit plan during executing | ✅ Correct | PASS |
| 34 | Policy edit-plan allowed | Can edit plan in exploring mode | ✅ Correct | PASS |
| 35 | Policy blocked tools list | Correct tool lists per mode | ✅ Correct | PASS |
| 36 | Rework limit default | Max 3 rework iterations | ✅ Correct | PASS |
| 37 | Rework limit blocked | Blocked when count reaches limit | ✅ Correct | PASS |
| 38 | Rework limit not reached | Fresh context not blocked | ✅ Correct | PASS |
| 39 | Invariants valid initial | Initial state satisfies invariants | ✅ Correct | PASS |
| 40 | Invariants invalid mode | Non-state symbol fails | ✅ Correct | PASS |
| 41 | Invariants wave overflow | current > total fails | ✅ Correct | PASS |
| 42 | Invariants need executor | Executing without executor fails | ✅ Correct | PASS |
| 43 | CMD parse plan | Returns gsd-cmd-plan AST | ✅ Correct | PASS |
| 44 | CMD parse go | Returns gsd-cmd-go AST | ✅ Correct | PASS |
| 45 | CMD parse go alias | /i → gsd-cmd-go | ✅ Correct | PASS |
| 46 | CMD parse skip | Returns gsd-cmd-skip AST | ✅ Correct | PASS |
| 47 | CMD parse reset | Returns gsd-cmd-reset AST | ✅ Correct | PASS |
| 48 | CMD parse wave-done | Returns gsd-cmd-wave-done AST | ✅ Correct | PASS |
| 49 | CMD parse wave-done alias | /wd → gsd-cmd-wave-done | ✅ Correct | PASS |
| 50 | CMD parse done | Returns gsd-cmd-done AST | ✅ Correct | PASS |
| 51 | CMD parse done force | --force flag detected | ✅ Correct | PASS |
| 52 | CMD parse replan | Returns gsd-cmd-replan AST | ✅ Correct | PASS |
| 53 | CMD parse status | /gsd → gsd-cmd-status | ✅ Correct | PASS |
| 54 | CMD parse unknown | Returns #f | ✅ Correct | PASS |
| 55 | CMD parse state artifact | /state → STATE artifact | ✅ Correct | PASS |
| 56 | CMD parse handoff artifact | /handoff → HANDOFF artifact | ✅ Correct | PASS |
| 57 | Plan create wave | Struct created with defaults | ✅ Correct | PASS |
| 58 | Plan validation valid | Valid plan passes | ✅ Correct | PASS |
| 59 | Plan validation no waves | Empty plan fails | ✅ Correct | PASS |
| 60 | Plan validation no files | All-file-less plan fails | ✅ Correct | PASS |
| 61 | Plan normalize | Produces normalized IR | ✅ Correct | PASS |
| 62 | Plan normalize duplicate titles | Returns error string | ✅ Correct | PASS |
| 63 | Plan wave set status | Status setter returns new struct | ✅ Correct | PASS |
| 64 | Plan pending waves | Correct filtering | ✅ Correct | PASS |
| 65 | Status conversion | Symbol ↔ string mapping | ✅ Correct | PASS |
| 66 | Wave slug | No spaces, lowercased | ✅ Correct | PASS |
| 67 | Wave status constants | All 5 constants correct | ✅ Correct | PASS |
| 68 | Wave status predicate | String validation | ✅ Correct | PASS |
| 69 | Wave status terminal | DONE/DEFERRED terminal, case-insensitive | ✅ Correct | PASS |
| 70 | Wave status normalize | Case-variant → canonical | ✅ Correct | PASS |
| 71 | Wave status active | Inbox/In-Progress are active | ✅ Correct | PASS |
| 72 | Wave gate defaults | Interval=5, counter=0 | ✅ Correct | PASS |
| 73 | Wave gate blocked | Blocked at interval limit | ✅ Correct | PASS |
| 74 | Wave gate increment | Counter increments by 1 | ✅ Correct | PASS |

## Findings

### FINDING-001 (low): find-transition-path returns empty list for same-state, not #f

**Severity:** Low (info)
**Category:** API consistency
**Description:** The `find-transition-path` function uses BFS to find a path from `from` to `to`. When `from == to`, the BFS immediately finds the target node with an empty path `'()`, rather than returning `#f` (which would indicate "no path needed"). This is documented in the function's contract as "Returns list of states to visit (excluding 'from, including 'to) or #f" — but the same-state case returns `'()` which is truthy.

**Impact:** Low — callers in `gsm-transition-to!` and `gsm-ctx-transition-to!` check `(eq? current target)` before calling `find-transition-path`, so the empty-list case is never reached in practice. However, if a new caller calls `find-transition-path` directly for same-state, they'd get `'()` instead of `#f`.

**Recommendation:** Document that same-state returns `'()` (empty path) or add an explicit same-state guard at the top of the function.

### FINDING-002 (info): State machine has comprehensive tool-blocklist per mode

**Severity:** Info (positive finding)
**Category:** Security
**Description:** The policy engine enforces a per-mode tool blocklist:
- **plan-written:** edit, write, bash blocked (planning phase is read-only)
- **executing:** planning-write blocked (cannot modify plan during execution)
- **verifying:** edit, write, bash, planning-write blocked (verification is read-only)
- **idle/exploring:** No restrictions

Additionally, the write guard blocks ALL writes to `.planning/` directory during execution mode, preventing plan tampering mid-execution. Path normalization prevents `..` traversal attacks.

**Impact:** Positive — defense-in-depth security posture.

### FINDING-003 (info): Rework-loop protection prevents infinite verification cycles

**Severity:** Info (positive finding)
**Category:** Safety
**Description:** The state machine tracks consecutive `verifying→executing` (rework) transitions and blocks further rework after `gsd-max-rework-iterations` (default 3). The counter is reset on fresh `plan-written→executing` transitions, allowing new execution cycles. This prevents the agent from getting stuck in an infinite verify→rework→verify loop.

**Impact:** Positive — prevents resource waste and stuck states.

## Remediation Items

None required. GSD subsystem is functioning correctly with robust state management.

## Architecture Summary

The GSD subsystem is organized in three layers:

1. **Pure logic layer** (`transition-logic.rkt`):
   - Static transition table with event-named triggers
   - BFS path finder for multi-step transitions
   - Pure `compute-next-gsm-state` — no side effects
   - Structural invariant checker

2. **Stateful layer** (`state-machine.rkt`, `session-state.rkt`):
   - Per-session contexts with semaphore-protected state
   - Event bus emission on transitions
   - Wave gate budget enforcement
   - Rework-loop protection

3. **Interface layer** (`core.rkt`, `command-parser.rkt`, `policy.rkt`, `archive.rkt`):
   - Command dispatch (/plan, /go, /replan, /skip, /reset, /wave-done, /done, /gsd)
   - Pure AST command parser with alias support
   - Unified policy engine for tool and write guards
   - Plan archival with wave completion validation

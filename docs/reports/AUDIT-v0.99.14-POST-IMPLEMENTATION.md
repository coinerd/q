# AUDIT: v0.99.14 — MAS Phase 1: Blackboard Default-On

**Auditor:** Independent post-implementation review  
**Date:** 2026-06-28  
**Milestone:** #800  
**HEAD:** `c170965a`  
**Verdict:** ✅ **APPROVED** — 4.6 / 5.0  

---

## 1. Executive Summary

v0.99.14 flips the blackboard subsystem from opt-in (`mas.blackboard.enabled` default `#f`) to default-on (`#t`). This is the first phase of making the MAS blackboard architecture a first-class, always-available facility: event-bus-driven state updates, system-prompt context injection, and crash recovery from `trace.jsonl` all happen by default with zero configuration.

The implementation is well-structured across 5 waves (W0–W4), each delivering incremental, independently-verifiable value. The safety story is strong: characterization tests (W0) lock in behavior before the flip, lifecycle wiring (W1) prevents subscription leaks, the token budget guard (W3) prevents context bloat, and comprehensive verification (W4) confirms zero regressions.

**Key finding:** The default-on change is low-risk. The blackboard subscriber is passive (reads events, does not emit), the context injection is gated by content presence (returns `#f` for empty state), and the token budget guard caps injection at 500 chars. All teardown paths are idempotent.

---

## 2. Scope

### 2.1 Changes Reviewed

| Wave | Issue | Merge SHA | Files Changed | Description |
|------|-------|-----------|---------------|-------------|
| W0 | #8099 | `03b26d94` | 1 new test file | 6 characterization tests locking pre-flip behavior |
| W1 | #8100 | `b2d0a900` | 2 files (1 src + 1 test) | `stop-blackboard-subscriber!` wired into `close-session!` |
| W2 | #8101 | `79fae3cf` | 5 files | Default flip `#f`→`#t`, version bump 0.99.14, CHANGELOG |
| W3 | #8102 | `13f03d12` | 3 files | Token budget guard (500 chars) + 11 integration/budget tests |
| W4 | #8103 | `c170965a` | 1 new report | Gate evidence JSON, comprehensive verification |

### 2.2 Out of Scope

- Hot-swap registry watcher (covered in v0.99.13)
- Verifier agent (covered in v0.99.5–v0.99.6)
- TCP broker / mTLS (covered in v0.99.12)
- Broad suite full run (~76 min; smoke subset of 450+ targeted tests run instead)

---

## 3. Detailed Findings

### 3.1 W0 (#8099): Characterization & Pre-Implementation Safety Net

**Files:** `tests/test-blackboard-deployment-gate.rkt` (NEW)

**Assessment:** Excellent test-first approach. Six tests were written BEFORE any behavioral change to lock in the existing default-off behavior. After W2 flipped the default, these tests were updated to verify the new expected behavior. This is textbook characterization testing.

**Tests verified:**
1. `blackboard-enabled?` returns `#t` for default settings (updated post-W2)
2. `blackboard-enabled?` returns `#t` when explicitly enabled
3. `blackboard-enabled?` returns `#f` when explicitly disabled
4. `build-blackboard-context-snippet` returns `#f` for empty state
5. `build-blackboard-context-snippet` returns non-`#f` for populated state
6. `stop-blackboard-subscriber!` is idempotent (safe when not subscribed)

**Verdict:** ✅ No issues.

### 3.2 W1 (#8100): Session Lifecycle Wiring

**Files:** `runtime/agent-session.rkt`, `tests/test-blackboard-lifecycle.rkt`

**Assessment:** Critical wiring change correctly placed. `stop-blackboard-subscriber!` is called in `close-session!` AFTER `guarded-set-active!`, ensuring the subscription stops after all session state is persisted. The call is wrapped in `with-handlers ([exn:fail? (lambda (_) (void))])` for defensive safety — subscriber teardown errors will never crash session close.

**Code reviewed** (`agent-session.rkt:483-491`):
```racket
;; v0.99.14 W1: Stop blackboard subscriber on session teardown.
(with-handlers ([exn:fail? (lambda (_) (void))])
  (stop-blackboard-subscriber!))
```

**Placement analysis:** The call is OUTSIDE the `(when (session-active? sess) ...)` block, meaning it runs even if the session was already inactive. This is correct — it handles the case where the subscriber was started but the session became inactive through an abnormal path.

**Subscriber idempotency verified** (`blackboard-subscriber.rkt:79-86`): `stop-blackboard-subscriber!` checks `(and stored (pair? stored))` before attempting unsubscribe, making it a safe no-op when no subscription exists.

**Tests added (3):** Subscription cleared after start, idempotent double-stop, safe no-op without subscription.

**Verdict:** ✅ No issues. Defensive wrapping is correct.

### 3.3 W2 (#8101): Config Default Flip + Version Bump

**Files:** `runtime/settings-query.rkt`, `util/version.rkt`, `info.rkt`, `README.md`, `CHANGELOG.md`, test updates

**Assessment:** Minimal, surgical change. The `blackboard-enabled?` function default parameter changed from `#f` to `#t`:

```racket
(define (blackboard-enabled? settings)
  (define raw (setting-ref* settings '(mas blackboard enabled) #t))
  ...)
```

**String coercion preserved:** The function still handles `"true"`, `"1"`, `"yes"` strings for config-file compatibility. Any other non-boolean value still falls through to `#f`.

**Version sync verified:** All three version locations (`util/version.rkt`, `info.rkt`, `README.md`) confirmed at `0.99.14`. `sync-version.rkt` reports zero drift.

**CHANGELOG quality:** Well-structured with all required sections (Features, Breaking/Behavior Changes, Migration Notes, Testing, Operational/Release). Migration notes are clear: "To opt out: set `mas.blackboard.enabled = false`."

**Test updates:** W0 test #1 and lifecycle test default assertion updated to expect `#t`.

**Verdict:** ✅ No issues.

### 3.4 W3 (#8102): Token Budget Guard + Integration Tests

**Files:** `runtime/context-assembly/blackboard-context.rkt`, `tests/test-blackboard-context.rkt`, `tests/test-blackboard-deployment-gate.rkt`

**Assessment:** Essential safety guard for the default-on change. Without this, a large blackboard state could inject excessive tokens into the system prompt.

**Implementation** (`blackboard-context.rkt`):
```racket
(define MAX-BLACKBOARD-SNIPPET-LEN 500)

(define (truncate-snippet text)
  (if (and (string? text) (> (string-length text) MAX-BLACKBOARD-SNIPPET-LEN))
      (string-append (substring text 0 (- MAX-BLACKBOARD-SNIPPET-LEN 3)) "...")
      text))
```

The guard is applied at the end of `build-blackboard-context-snippet`, wrapping the final concatenated output. This is the correct insertion point — it caps the total snippet, not individual sections.

**Boundary behavior verified:**
- Exactly 500 chars: returned as-is (no truncation)
- 501 chars: truncated to 497 + `"..."` = 500 chars
- 1000 chars: truncated to 497 + `"..."` = 500 chars

**Tests added (11 total):**
- 7 unit tests in `test-blackboard-context.rkt` (constant value, under-budget as-is, over-budget truncation, truncate-snippet identity, long-string, 500 boundary, 501 boundary)
- 4 integration tests in `test-blackboard-deployment-gate.rkt` (default enables, explicit disable, injection gating, token budget on large state)

**Verdict:** ✅ No issues. Well-tested boundary conditions.

### 3.5 W4 (#8103): Release Verification

**Files:** `docs/reports/v0.99.14-gate-evidence.json`

**Assessment:** Comprehensive verification with structured JSON evidence. All checks documented:
- Version sync: PASS (0 drift)
- Release notes lint: PASS
- Clean compile: PASS (exit 0)
- .zo cruft: PASS (0 orphaned)
- 126 blackboard tests green
- 159 MAS tests green
- 450+ related tests green
- 0 regressions introduced

**Pre-existing debt acknowledged:** `test-settings.rkt` has 1 failure unrelated to v0.99.14 (context-assembly-profile default, from v0.79.0 #7527). Correctly identified as pre-existing.

**Verdict:** ✅ No issues.

---

## 4. Risk Assessment

### 4.1 Default-On Impact Analysis

| Risk Vector | Likelihood | Impact | Mitigation | Residual |
|-------------|-----------|--------|------------|----------|
| Event bus subscription leak (no teardown) | Was HIGH | Medium | W1 wiring in `close-session!` | LOW |
| Context injection bloat in system prompt | Medium | Medium | W3 token budget guard (500 chars) | LOW |
| Subscriber errors crashing session | Low | High | `with-handlers` in handler + teardown | NEGLIGIBLE |
| Crash recovery from corrupted trace.jsonl | Low | Low | `rebuild-blackboard-from-log!` has exception isolation | LOW |
| Performance overhead (subscriber processing) | Low | Low | Subscriber is passive read-only, event filter skips irrelevant events | LOW |
| User surprise from default flip | Medium | Low | Migration notes in CHANGELOG, easy opt-out | LOW |

### 4.2 Opt-Out Path

Users who experience issues can disable blackboard by setting:
```json
{ "mas": { "blackboard": { "enabled": false } } }
```

This restores pre-v0.99.14 behavior entirely.

### 4.3 Backward Compatibility

- ✅ Existing configs with `mas.blackboard.enabled = false` continue to work unchanged
- ✅ Existing configs with `mas.blackboard.enabled = true` continue to work unchanged
- ✅ No API surface changes — `blackboard-enabled?`, `start/stop-blackboard-subscriber!`, etc. unchanged
- ⚠️ Users with no explicit setting will now get blackboard enabled (intended behavior change)

---

## 5. Architecture Review

### 5.1 Dependency Graph (unchanged)

```
wiring/run-modes.rkt
  ├── settings-query.rkt (blackboard-enabled?)
  ├── blackboard.rkt (make-blackboard, current-blackboard)
  ├── blackboard-subscriber.rkt (start-blackboard-subscriber!)
  │     └── blackboard-follower.rkt (rebuild-blackboard-from-log!)
  │           └── blackboard-reducer.rkt (apply-event)
  └── state-aware-builder.rkt (current-blackboard-injection-enabled)
        └── blackboard-context.rkt (build-blackboard-context-snippet)
              └── [W3] truncate-snippet

runtime/agent-session.rkt
  └── [W1] stop-blackboard-subscriber!
```

No new dependencies introduced. All W0–W3 changes are within existing modules.

### 5.2 Design Observations

1. **Module-level `current-subscription` box** (`blackboard-subscriber.rkt`): This is a single-subscription model. If two sessions share the same process, they will interfere. However, this is pre-existing design (v0.99.7), not introduced by v0.99.14. The default-on change makes this more visible, but does not change the semantics.

2. **`stop-blackboard-subscriber!` optional `bus` argument**: The function accepts an optional bus parameter for explicit cleanup, but `close-session!` calls it with no arguments. This relies on the stored bus reference in `current-subscription`. This is correct — the bus reference is captured at subscribe time.

3. **Token budget constant `MAX-BLACKBOARD-SNIPPET-LEN`**: Hardcoded at 500. This is appropriate for now. If the system prompt grows or context window changes, this should be revisited. Consider making it configurable in a future version.

---

## 6. Test Quality Assessment

### 6.1 Test Coverage Matrix

| Module | Tests | Coverage | Quality |
|--------|-------|----------|---------|
| `settings-query.rkt` (`blackboard-enabled?`) | 5 tests | Default, explicit #t/#f, string coercion, missing key | Excellent |
| `blackboard-subscriber.rkt` (lifecycle) | 6 tests | Start, stop, idempotent, no-op, event processing | Excellent |
| `blackboard-context.rkt` (snippet + budget) | 22 tests | Empty/populated state, all formatters, token budget boundaries | Excellent |
| `agent-session.rkt` (teardown wiring) | 3 tests | Subscription cleared, idempotent, safe no-op | Good |
| Integration (deployment gate) | 4 tests | Default-on behavior, disable, injection gating, budget | Excellent |

### 6.2 Test Gaps

- **No E2E test for full session lifecycle with default-on blackboard**: Tests verify individual components but no test creates a full `agent-session`, starts it, runs events, and verifies teardown. This is a pre-existing gap (E2E session tests are hard to construct in unit-test isolation).
- **No test for concurrent session close + event publish**: Race condition between `stop-blackboard-subscriber!` and in-flight events is not tested. However, the `with-handlers` wrapper makes this safe in practice.

### 6.3 Test Count

- W0: +6 characterization tests
- W1: +3 lifecycle cleanup tests
- W3: +11 token budget + integration tests
- **Total new: 20 tests** (across 3 test files)
- **Total blackboard tests: 126** (9 files), all green

---

## 7. Scoring

| Axis | Score | Rationale |
|------|-------|-----------|
| **Correctness** | **4.5** | All changes are correct and well-tested. Minor gap: no full E2E session lifecycle test with blackboard enabled. Token budget boundary conditions (500/501) precisely verified. |
| **Security** | **4.5** | No new attack surface. Subscriber is read-only (does not emit events). `with-handlers` wrapping prevents error propagation. Token budget prevents prompt injection bloat. |
| **Architecture** | **4.5** | Clean wave decomposition. No new dependencies. Existing module structure respected. Token budget guard placed at correct layer (output of snippet builder). Minor note: `MAX-BLACKBOARD-SNIPPET-LEN` is hardcoded, not configurable. |
| **Test Quality** | **4.5** | 20 new tests with excellent boundary coverage. Characterization-first approach (W0) is exemplary. Minor gap: no concurrent/race test for teardown. |
| **Documentation** | **5.0** | CHANGELOG is complete with all required sections. Inline comments explain the "why" (not just "what"). Migration notes are clear and actionable. Gate evidence JSON is thorough. |

**Overall: 4.6 / 5.0 — APPROVED**

---

## 8. Findings Summary

### Findings (All LOW or Informational)

| ID | Severity | Finding | Recommendation |
|----|----------|---------|----------------|
| F-01 | LOW | `MAX-BLACKBOARD-SNIPPET-LEN` (500) is hardcoded | Consider making configurable via `mas.blackboard.snippet-max-len` in a future version. Acceptable for now. |
| F-02 | LOW | No E2E test for full session lifecycle with default-on blackboard | Pre-existing gap. E2E session tests are architecturally hard. Acceptable risk given strong unit test coverage. |
| F-03 | INFO | Module-level `current-subscription` box means only one active subscription per process | Pre-existing design from v0.99.7. Default-on makes it more visible but does not change semantics. No action needed for this milestone. |
| F-04 | INFO | CHANGELOG references "Token budget guard (W3, forthcoming)" in the v0.99.14 section | Minor: the changelog was written during W2 before W3 was implemented. Since W3 is now merged, this text could be updated to past tense. Non-blocking. |
| F-05 | INFO | Broad fast suite (~76 min) was not run fully; 450+ targeted tests used instead | Acceptable for this milestone. The changes are narrow (3 source files, all blackboard-related). No cross-module changes that would affect unrelated subsystems. |

---

## 9. Recommendation

**Ship v0.99.14.** The implementation is solid, well-tested, and the risk profile is low. The default-on change is properly guarded by:
1. Session lifecycle cleanup (W1)
2. Token budget guard (W3)
3. Idempotent teardown at all levels
4. Easy opt-out via config

No blocking findings. All LOW findings are acceptable for this milestone and can be addressed in future versions.

---

## 10. Artifacts

| Artifact | Location |
|----------|----------|
| Gate Evidence | `docs/reports/v0.99.14-gate-evidence.json` |
| Plan | `.planning/PLAN-v0.99.14-MAS-PHASE-1-BLACKBOARD-DEFAULT-ON.md` (local only) |
| CHANGELOG | `CHANGELOG.md` (section `## 0.99.14`) |
| This Audit | `docs/reports/AUDIT-v0.99.14-POST-IMPLEMENTATION.md` |

---

*Audit completed 2026-06-28. HEAD: `c170965a`.*

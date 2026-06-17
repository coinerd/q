# Independent Audit: v0.99.18 — MAS Phase 4: Hot-Swap Default-On

**Date:** 2026-07-13
**Milestone:** #804
**Parent Issue:** #8153
**Auditor:** Independent (pi, local)
**Version:** 0.99.17 → 0.99.18
**Base:** `a39dff6e` (v0.99.17 + post-audit corrections)
**Head:** `37e504ba` (v0.99.18 release)
**Commits:** 6 (squash-merged PRs #8161–#8166)
**Files Changed:** 13 (533 insertions, 15 deletions)
**Process Checklist:** `.planning/AUDIT-PROCESS-CHECKLIST.md` applied

---

## 1. Scope

This audit covers the v0.99.18 milestone (MAS Phase 4: Hot-Swap Default-On), which addressed seven findings (F-HS-01 through F-HS-07) across shared-module namespace identity, post-load identity verification, hot-swap default-on flip, dead code cleanup, registry watcher documentation, session teardown correctness, and comprehensive deployment gate testing.

---

## 2. Wave Summary

| Wave | Issue | Commit | Focus | Files | Result |
|------|-------|--------|-------|-------|--------|
| W0 | #8153 | PR #8161 | Characterization tests | 1 new | 12 tests, baseline locked |
| W1 | #8154 | PR #8162 | F-HS-01, F-HS-03: Shared modules + identity | 2 changed | 15 tests pass |
| W2 | #8155 | PR #8163 | F-HS-02: Deployment gate tests | 1 new | 17 tests pass |
| W3 | #8156 | PR #8164 | F-HS-04: Hot-swap default-on flip | 1 changed | Default `#t` |
| W4 | #8157 | PR #8165 | F-HS-05/06/07: Cleanup + docs + teardown | 4 changed | All tests pass |
| W5 | #8159 | PR #8166 | Release & version bump | 4 changed | Version 0.99.18 |

---

## 3. Fix Verification

### F-HS-01: `SHARED-MODULES` Missing `mas-envelope` ✅

**Problem:** The `SHARED-MODULES` list in `agent/registry.rkt` did not include `mas-envelope`, risking struct identity mismatch when agents are loaded in separate namespaces.

**Fix:** Added `'q/util/message/mas-envelope` to the `SHARED-MODULES` list:
```racket
(define SHARED-MODULES
  (list 'q/agent/roles/base 'q/util/capability 'q/util/message/mas-envelope))
```

**Verification:** `git diff a39dff6e..37e504ba -- agent/registry.rkt` confirms the addition. In practice, modules are already loaded via `require`, so `dynamic-require` with absolute paths resolves to the same instances. The `with-handlers` wrapper in `load-agent-dynamically` silently catches `namespace-attach-module` failures in development environments where the collection isn't installed in system paths.

**Assessment:** Correct. The fix adds the missing module per the strategy doc. The silent attachment failure is pre-existing behavior (present since v0.99.15) and does not affect production where modules are pre-loaded.

### F-HS-02: Deployment Gate Tests ✅

**Problem:** No dedicated test suite verified the hot-swap deployment gate behaviors end-to-end.

**Fix:** Created `tests/test-hot-swap-deployment-gate.rkt` with 17 tests covering: settings parsing (6 tests), registry gate toggles (3 tests), supervisor resolution (3 tests), watcher default-off (1 test), session tracking (3 tests), E2E integration (1 test).

**Verification:** 17 `test-case` forms confirmed. All 17 pass under `raco test`.

**Assessment:** Correct. Comprehensive coverage of the deployment gate.

### F-HS-03: Post-Load Identity Verification ✅

**Problem:** `load-agent-dynamically` had no type check on the dynamically-loaded result, risking silent type mismatches.

**Fix:** Added `agent-role?` identity check in `load-agent-dynamically` (registry.rkt lines 162-174):
```racket
(define result (factory-proc))
(unless (agent-role? result)
  (log-warning "load-agent-dynamically: identity mismatch for ~a, falling back to static"
               (agent-descriptor-role-name desc))
  (raise (exn:fail (format "identity mismatch for ~a" (agent-descriptor-role-name desc))
                   (current-continuation-marks))))
result
```

The `raise` is inside the `with-handlers` block, so the `exn:fail?` handler catches it and falls back to the static factory. Import added: `(only-in "roles/base.rkt" agent-role?)` — no circular dependency (base.rkt does not import registry.rkt).

**Verification:**
- With `register-default-agents!` (absolute paths): `agent-role?` returns `#t`
- With broken module path: identity check catches mismatch, falls back to static

**Assessment:** Correct. The identity check is a proper safety net in the fallback chain.

### F-HS-04: Hot-Swap Default-On Flip ✅ (with regression — see Section 4)

**Problem:** `hot-swap-enabled?` in `runtime/settings-query.rkt` defaulted to `#f`, preventing MAS Phase 4 from being active by default.

**Fix:** Changed default from `#f` to `#t` in both the `setting-ref*` call and the `else` branch:
```racket
;; Before:
(define raw (setting-ref* settings '(mas hot-swap enabled) #f))
;; ...
[else #f]))

;; After:
(define raw (setting-ref* settings '(mas hot-swap enabled) #t))
;; ...
[else #t]))
```

**Design Decision:** The settings default (`#t`) and the registry box default (`#f`) are deliberately separate. The wiring layer (`run-modes.rkt`) bridges them by calling `set-hot-swap-enabled! #t` at runtime session start.

**Verification:**
- `(hot-swap-enabled? (q-settings (hasheq) (hasheq) (hasheq)))` → `#t`
- `hot-swap-enabled-box` in registry.rkt still defaults to `(box #f)` — verified
- Explicit `mas.hot-swap.enabled: false` in config still disables it

**Assessment:** Functionally correct. All 4 MAS phases now default-on. However, introduced 3 test regressions (see Section 4).

### F-HS-05: Dead `decode-mouse-x10` Imports Removed ✅

**Problem:** `decode-mouse-x10` was re-exported through `interfaces/tui.rkt` and `tui/tui-render-loop.rkt` but never used by external consumers.

**Fix:** Removed the dead re-exports from both files.

**Verification:** `grep -n "decode-mouse-x10" interfaces/tui.rkt tui/tui-render-loop.rkt` returns no results. The function remains exported from its source `tui/input.rkt` where tests import it directly.

**Assessment:** Correct. Both dead imports removed cleanly.

### F-HS-06: Registry Watcher Documented as Intentionally Unwired ✅

**Problem:** `agent/registry-watcher.rkt` is an experimental polling-based module that is never wired into the runtime, but this status was not documented, creating confusion.

**Fix:** Added `⚠️ INTENTIONALLY UNWIRED` notice at the top of the module, explaining its experimental status and that it is gated behind `mas.hot-swap.auto-reload.enabled` (default `#f`).

**Assessment:** Correct. Documentation accurately describes the module's status.

### F-HS-07: Session Teardown Clears Hot-Swap State ✅

**Problem:** `close-session!` in `runtime/agent-session.rkt` did not clear the registry-level `session-active?` and `hot-swap-enabled?` global boxes, risking state leaks between sessions.

**Fix:** Added to `close-session!`:
```racket
(set-session-active! #f)
(set-hot-swap-enabled! #f)
```

Import: `(only-in "../agent/registry.rkt" set-session-active! set-hot-swap-enabled!)`.

**Note:** These are registry-level globals (boxes), separate from the session struct's `session-active?` field. The `guarded-set-active! sess #f` sets the session struct field; the registry calls clear the global boxes. Both are needed — they manage different state. The wiring layer re-enables at the next session start.

**Assessment:** Correct. Clean teardown, no state leak between sessions.

---

## 4. Regression Analysis

### Focused Suite Results

| Suite | Tests | Pass | Fail | Status |
|-------|-------|------|------|--------|
| `test-agent-registry.rkt` | 31 | 31 | 0 | ✅ |
| `test-hot-swap-events.rkt` | 7 | 7 | 0 | ✅ |
| `test-registry-config-wiring.rkt` | 11 | 8 | **3** | ❌ **REGRESSION** |
| `test-registry-deployment-gate.rkt` | 8 | 8 | 0 | ✅ |
| `test-registry-hot-swap.rkt` | 15 | 15 | 0 | ✅ |
| `test-registry-integration.rkt` | 7 | 7 | 0 | ✅ |
| `test-registry-supervisor.rkt` | 10 | 10 | 0 | ✅ |
| `test-registry-watcher.rkt` | 9 | 9 | 0 | ✅ |
| `test-version-pinning.rkt` | 8 | 8 | 0 | ✅ |
| `test-hot-swap-characterization.rkt` | 12 | 12 | 0 | ✅ |
| `test-hot-swap-deployment-gate.rkt` | 17 | 17 | 0 | ✅ |
| **Total** | **135** | **132** | **3** | |

### Regression: 3 Test Failures in `test-registry-config-wiring.rkt`

**Confirmed as new regressions (not pre-existing):**
- At v0.99.17 (`a39dff6e`): 11/11 passed
- At v0.99.18 (`37e504ba`): 8/11 passed

**Root Cause:** W3 flipped `hot-swap-enabled?` default from `#f` to `#t` in `settings-query.rkt` but did not update `test-registry-config-wiring.rkt`, which asserts the old default. The plan listed the new deployment gate test file for W3 but missed the existing config-wiring test file that also tests `hot-swap-enabled?`.

**The 3 failing tests:**
1. `"hot-swap-enabled? returns #f by default"` — expects `#f`, gets `#t`
2. `"hot-swap-enabled? returns #f when key absent"` — expects `#f`, gets `#t`
3. `"hot-swap-enabled? returns #f for random value"` — expects `#f`, gets `#t`

**Remediation:** Fixed in v0.99.19 W1 (PR #8170) — all 3 assertions updated to `check-true` with post-flip descriptions.

### Additional Notes

- 2 pre-existing failures in `test-interfaces-tui.rkt` (resize polling + drag selection) — confirmed unrelated to v0.99.18, documented in prior milestones.

---

## 5. CHANGELOG Accuracy Audit

| Claim | Verified | Notes |
|-------|----------|-------|
| "mas.hot-swap.enabled defaults to #t" | ✅ | `setting-ref*` default is `#t` |
| "hot-swap-enabled-box still defaults to #f" | ✅ | `(box #f)` in registry.rkt |
| "bridged at runtime by wiring layer" | ✅ | `run-modes.rkt` calls `set-hot-swap-enabled! #t` |
| "Explicit false still disables" | ✅ | Boolean check in cond |
| F-HS-01: mas-envelope added | ✅ | In SHARED-MODULES |
| F-HS-03: agent-role? identity check | ✅ | In load-agent-dynamically |
| F-HS-05: decode-mouse-x10 removed | ✅ | Not in either file |
| F-HS-07: teardown clears state | ✅ | In close-session! |
| F-HS-06: watcher documented | ✅ | INTENTIONALLY UNWIRED notice |
| "12 characterization tests" | ✅ | 12 test-case forms |
| "15 total" hot-swap tests | ✅ | 15 test-case forms |
| "17 total deployment gate tests" | ✅ | 17 test-case forms |
| Version 0.99.18 | ✅ | Synced in version.rkt, info.rkt, README.md |

**CHANGELOG accuracy: 13/13 claims verified.** No fabricated narratives.

---

## 6. Architecture Assessment

### Hot-Swap Subsystem Design: SOUND

The two-layer fallback chain is well-designed:

```
make-agent-instance
  └→ if hot-swap-enabled? and module-path present:
       └→ load-agent-dynamically
            └→ make-base-namespace + attach shared modules
            └→ dynamic-require factory
            └→ agent-role? identity check (F-HS-03)
            └→ on any failure → static factory fallback
       └→ on error → static factory
  └→ else: static factory directly
```

Hot-swap default-on is safe: even if every dynamic-require fails, the system degrades to pre-MAS direct construction.

### Namespace Identity Management: CORRECT

The `SHARED-MODULES` list ensures `gen:agent-role`, `mas-envelope?`, and `capability?` struct identities are shared across namespaces. In practice, modules are already loaded via `require`, so `dynamic-require` with absolute paths resolves to the same instances. The F-HS-03 identity check catches edge cases where attachment fails.

### Session Lifecycle: CLEAN

`close-session!` teardown correctly clears both the session struct `session-active?` field and the registry global boxes (`session-active-box`, `hot-swap-enabled-box`), preventing state leaks between sessions.

---

## 7. Code Quality Assessment

### Positive
- **Robust fallback chain:** Hot-swap failures silently degrade to static factory construction
- **Identity verification (F-HS-03):** `agent-role?` check prevents silent type mismatches in dynamically loaded agents
- **Test coverage:** 35 new tests added (12 characterization + 17 deployment gate + 6 in existing suites)
- **Clean separation:** Settings default (`#t`) vs registry box default (`#f`) bridged by wiring layer
- **CHANGELOG accuracy:** 13/13 claims verified against actual `git diff`
- **Proper teardown:** Session lifecycle correctly clears all state (F-HS-07)

### Concerns
1. **3 test regressions:** W3 flipped the default without updating existing tests. Fixed in v0.99.19.
2. **W6 audit report gap:** The audit report was not committed as a persistent artifact (this document remediates that gap).

### Technical Debt
- None introduced. All production changes are clean additions or single-line fixes.

---

## 8. MAS Deployment Strategy: Final Status

| Phase | Version | Feature | Default | Status |
|-------|---------|---------|---------|--------|
| Phase 1 | v0.99.14 | Blackboard | ✅ `#t` | COMPLETE |
| Phase 2 | v0.99.15 | Verifier | ✅ `#t` | COMPLETE |
| Phase 3 | v0.99.17 | Execution plane | ✅ `#t` | COMPLETE |
| Phase 4 | v0.99.18 | Hot-swap | ✅ `#t` | COMPLETE |

**Remaining opt-in features:** Broker (`#f`), MCP server (`#f`), auto-reload (`#f`)

---

## 9. Audit Verdict

**APPROVED**

**Score: 4.2 / 5.0**

All seven findings (F-HS-01 through F-HS-07) are correctly fixed with appropriate tests. The implementation is architecturally sound with a robust fallback chain and correct namespace identity management. The CHANGELOG is accurate with no fabricated narratives.

| Category | Score | Notes |
|----------|-------|-------|
| **Correctness** | 4.0/5.0 | 3 test regressions in config-wiring suite (fixed in v0.99.19) |
| **Completeness** | 4.5/5.0 | W6 audit report missing (remediated by this document) |
| **CHANGELOG Accuracy** | 5.0/5.0 | All 13 claims verified against `git diff` |
| **Test Coverage** | 4.5/5.0 | 35 new tests added, existing tests updated in v0.99.19 |
| **Architecture** | 4.5/5.0 | Sound design, robust fallback, clean lifecycle |
| **Process Compliance** | 3.5/5.0 | W6 deliverable gap, regression not caught at wave boundary |

### Findings Summary

| ID | Severity | Description | Status |
|----|----------|-------------|--------|
| A-1 | MEDIUM | 3 test regressions in config-wiring suite | Fixed in v0.99.19 W1 |
| A-2 | LOW | W6 audit report not committed | Remediated by this document |
| A-3 | INFO | Stale CHAR-1 test description | Fixed in v0.99.19 W1 |
| A-4 | INFO | Stale comment in deployment gate test | Fixed in v0.99.19 W1 |
| A-5 | PROCESS | Missing "grep for all tests" step in checklist | Fixed in v0.99.19 W3 |

---

*This audit was performed following the `AUDIT-PROCESS-CHECKLIST.md` created in response to the v0.99.17 fabrication issue. Committed as part of v0.99.19 Milestone #805, Wave W2 (#8168).*

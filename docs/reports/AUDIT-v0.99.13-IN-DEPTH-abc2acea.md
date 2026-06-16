# In-Depth Audit: v0.99.13 Post-Milestone (`abc2acea`)

**Date:** 2026-06-27
**Auditor:** Independent (post-milestone in-depth review)
**Base commit:** `abc2acea` (main, post-W4 merge)
**Previous commit:** `621bdf84` (v0.99.12 post-F-09/F-10 fixes)
**Milestone:** #799 (v0.99.13 MAS Remediation Gap Closure)
**Plan:** `.planning/PLAN-v0.99.13-MAS-REMEDIATION-GAP-CLOSURE.md`
**Prior audit:** `docs/reports/AUDIT-v0.99.13-POST-IMPLEMENTATION.md` (W4, claimed APPROVED 4.43/5.0)

---

## 1. Executive Summary

**Verdict: APPROVED_WITH_NOTES — 4.0/5.0**

The v0.99.13 milestone delivers all 5 gaps (G-1 through G-5) with reasonable implementation quality. The architecture is clean, backward compatibility is preserved, and the broad suite shows zero regressions. However, this in-depth audit uncovered **several findings that the W4 audit missed or misreported**, including a test failure that was incorrectly claimed as passing, two critical wiring gaps, and documentation inaccuracies.

### Score Breakdown

| Axis | Weight | Score | Weighted | Notes |
|------|--------|-------|----------|-------|
| Correctness | 30% | 3.5 | 1.05 | Hot-swap test fails; wiring gaps mean features are seam-only |
| Security | 25% | 4.5 | 1.13 | Feature gates default-off; no new attack surface |
| Architecture | 20% | 4.0 | 0.80 | Clean separation; SHARED-MODULES empty limits utility |
| Test Quality | 15% | 3.5 | 0.53 | 34 tests, but 1 fails; E2E-2 weak; audit misreported |
| Documentation | 10% | 3.5 | 0.35 | CHANGELOG has 2 factual errors; W4 audit inaccurate |
| **Total** | **100%** | | **3.86** | **APPROVED_WITH_NOTES** |

### Comparison to W4 Audit

| Claim | W4 Audit | This Audit | Verdict |
|-------|----------|------------|---------|
| Hot-swap tests | "9/9 ✓" | 8/9 (1 failure) | **W4 MISREPORTED** |
| Broad suite regressions | "Zero regressions" | Confirmed (0 new failures) | ✅ Accurate |
| All gaps closed | Yes | Yes (with caveats) | ✅ Accurate |
| Feature gates default-off | Yes | Yes | ✅ Accurate |
| SHARED-MODULES concern | Noted as non-blocking | Should be MEDIUM | ⚠️ Underweighted |
| Wiring gaps | Not identified | 2 gaps found | **W4 MISSED** |
| CHANGELOG accuracy | Implied accurate | 2 factual errors | **W4 MISSED** |

---

## 2. Findings

### F-11 (MEDIUM): Dynamic-Require Module Path Resolution Bug

**Severity:** MEDIUM — Dynamic-require silently fails; test that claims to verify it actually fails.

**Description:**

`register-default-agents!` in `agent/registry-defaults.rkt` uses relative string paths like `"agent/roles/planner.rkt"`. These are resolved by `dynamic-require` relative to `(current-directory)`, which varies between runtime contexts:

- **Production** (from `q/` directory): Path resolves correctly ✓
- **Test execution** (from `q/tests/` directory): Path resolves to `tests/agent/roles/planner.rkt` which doesn't exist ✗

When the path doesn't resolve, `load-agent-dynamically` catches the exception via `with-handlers` and silently falls back to the static factory. The test `"dynamic path: planner loaded via dynamic-require"` in `test-registry-hot-swap.rkt` asserts `(check-false (eq? result 'static-fallback))` — this assertion **FAILS** because the dynamic path errored and fell back.

**Verification:**

```bash
$ raco test tests/test-registry-hot-swap.rkt
# → 8 success(es) 1 failure(s) 0 error(s) 9 test(s) run
```

```bash
$ cd /home/user/src/q-agent && racket debug3.rkt  # (uses "q/agent/roles/planner.rkt")
# → RESULT: #(struct:planner-role)
# → IS FALLBACK: #f  ← Works when path resolves
```

**Root Cause:** `dynamic-require` with string paths resolves relative to `(current-directory)`, not relative to the requiring module. The `(current-directory)` during `raco test` is the test file's parent directory (`q/tests/`), not `q/`.

**Impact:**
1. The hot-swap test fails (8/9, not 9/9 as claimed in W4 audit and CHANGELOG).
2. In production, if `q` is invoked from any directory other than `q/`, the module paths won't resolve and hot-swapping will silently fall back to static factories.
3. The W4 audit's claim of "9/9 ✓" is incorrect — either the test was not actually run, or it was run in a different working directory.

**Fix Options:**
- **Option A (preferred):** Use `(resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference)))` in `registry.rkt` to compute paths relative to the registry module itself, then build full paths from there.
- **Option B:** Use collection-based paths: `'q/agent/roles/planner.rkt` instead of relative strings.
- **Option C:** In `registry-defaults.rkt`, compute paths relative to the module using `(define here (path-only (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference)))))`.

---

### F-12 (MEDIUM): SHARED-MODULES Empty — Type Identity Broken

**Severity:** MEDIUM — Even if the module path resolves, dynamically-loaded agents won't satisfy `agent-role?` predicates from the main namespace.

**Description:**

`SHARED-MODULES` in `agent/registry.rkt` is defined as `'()`. This means `load-agent-dynamically` creates a `(make-base-namespace)` with no shared modules attached. When `dynamic-require` loads `agent/roles/planner.rkt`, it creates a fresh instance of `gen:agent-role` from `agent/roles/base.rkt` — a DIFFERENT generic than the one in the main namespace.

**Consequence:** An agent instance created via the dynamic path will NOT satisfy `(agent-role? instance)` when checked by code in the main namespace. The supervisor's dispatch uses `agent-role?` to verify role instances. This means dynamically-loaded agents are currently unusable.

The W4 audit noted this as "Open Item #1 (non-blocking)" but classified it as a future enhancement. It is more severe than that — it means the G-3 implementation is a seam only, not a working feature, even when `hot-swap-enabled?` is `#t`.

**Minimum SHARED-MODULES for correctness:**
```racket
(define SHARED-MODULES
  '(q/agent/roles/base        ; gen:agent-role, agent-role?
    q/util/capability          ; ROLE-CAPABILITIES
    q/util/message/mas-envelope ; mas-envelope struct identity
    ))
```

**Caveat:** Adding modules to `SHARED-MODULES` requires careful testing. The module paths must be resolvable from the registry module's context (same path resolution issue as F-11).

---

### F-13 (MEDIUM): Hot-Swap Not Wired to Production Config

**Severity:** MEDIUM — The feature gate exists in the registry but is never activated by the wiring layer.

**Description:**

The wiring layer in `wiring/run-modes.rkt:277` checks `(hot-swap-enabled? settings)` (from `settings-query.rkt`) but does **NOT** call `(set-hot-swap-enabled! #t)` on the registry. The registry's internal `hot-swap-enabled-box` defaults to `#f` and is never set to `#t` in any production code path.

**Verification:**
```bash
$ grep -rn "set-hot-swap-enabled" wiring/ runtime/ extensions/
# (no results)
```

`set-hot-swap-enabled!` is only called in test files. This means even when a user sets `mas.hot-swap.enabled = #t` in their config, the registry will always use the static factory path.

**Fix:** Add `(set-hot-swap-enabled! #t)` in `wiring/run-modes.rkt` inside the `(when (hot-swap-enabled? settings) ...)` block.

---

### F-14 (LOW): Session-Active Not Wired to Session Lifecycle

**Severity:** LOW — The session-safety warning in `activate-agent-version!` will never fire.

**Description:**

The registry's `session-active?` / `set-session-active!` functions are never called from production code. The session lifecycle (`runtime/session/`) uses its own `agent-session-active?` / `set-agent-session-active?!` functions on the `agent-session` struct, which are unrelated to the registry's session tracking.

This means `activate-agent-version!` will never log its session-safety warning, because `(session-active?)` always returns `#f`.

**Fix:** In the wiring layer, call `(set-session-active! #t)` at session start and `(set-session-active! #f)` at session end. Or bridge from `agent-session-active?` to `set-session-active!`.

---

### F-15 (LOW): CHANGELOG Factual Errors

**Severity:** LOW — Documentation inaccuracy reduces trust.

Two factual errors in the v0.99.13 CHANGELOG:

1. **"Registry hot-swap: 9/9 tests green"** — FALSE. The actual result is 8/9 with 1 failure (the dynamic-require test fails due to F-11).

2. **"blackboard-follower.rkt re-exports all symbols from blackboard-subscriber.rkt"** — BACKWARDS. The subscriber re-exports from the follower, not the other way around. The follower is the extracted module; the subscriber re-exports for backward compatibility.

---

### F-16 (LOW): Registry Watcher Documentation Drift

**Severity:** LOW — Comments don't match implementation.

1. **Comment claims `filesystem-change-evt` usage:** The module header says "Uses filesystem-change-evt (available in Racket 8.x) for efficient event-based file change notification." The implementation is purely polling-based (`sleep` + `directory-list`). No `filesystem-change-evt` is used.

2. **Comment claims "minor version" increment:** `next-version` says "Increment the minor version of a semver string." It actually increments the **patch** version (`"1.0.0"` → `"1.0.1"`).

3. **`caddr` and `last` defined locally:** Both are available from `racket/base` in Racket 8.x. The local definitions shadow built-ins unnecessarily.

---

### F-17 (LOW): E2E-2 Test Does Not Verify Token Stripping

**Severity:** LOW — Test claims to verify F-10 fix but doesn't actually test the stripping behavior.

**Description:**

E2E-2 ("capability token validated and stripped (F-10 fix)") runs `bash` with `echo 'token-strip-verified'` and checks the output contains that string. However, bash ignores unknown arguments in the arguments hash. The test would pass even if the capability token was NOT stripped from the arguments, because the `bash` handler only looks at the `'command` key.

The test's own comment acknowledges this: "The fact that bash succeeded means the token was stripped. If it hadn't been stripped, the worker would have received an extra 'capability-token key in arguments — which bash ignores."

**Fix:** Use a tool that fails when given unexpected arguments, or add explicit argument inspection in a mock tool.

---

### F-18 (INFO): No Contracts on Registry Watcher Provides

The `registry-watcher.rkt` module provides 5 functions with no contracts:
- `path->role-name`, `next-version`, `start-registry-watcher!`, `stop-registry-watcher!`, `watcher-running?`

Acceptable for an experimental module, but contract-out would improve robustness.

---

### F-19 (INFO): E2E-5 Is Not a True E2E Test

E2E-5 ("broker disabled → local fallback") doesn't test any distributed execution path. It verifies that `start-remote-executor!` fails when no server is running. This is a unit-level connection failure test, not an end-to-end test of the local fallback behavior.

---

## 3. Security Review

### Feature Gates (all default-off)

| Gate | Default | Wired? | Verified |
|------|---------|--------|----------|
| `hot-swap-enabled?` (registry) | `#f` | **NOT WIRED** to config | ⚠️ F-13 |
| `mas.hot-swap.enabled` (settings) | `#f` | Read in `run-modes.rkt` but not bridged to registry | ⚠️ F-13 |
| `session-active?` (registry) | `#f` | **NOT WIRED** to session lifecycle | ⚠️ F-14 |
| `mas.broker.enabled` | `#f` | Properly wired (from v0.99.12) | ✅ |
| Watcher (experimental) | Not started | Not called from production | ✅ |

### Capability Enforcement Chain

- **G-1**: All core tools annotated — verified ✓ (14/14 tests)
- **F-10**: Token stripped via `hash-remove` — verified ✓
- **E2E-2**: Token validation at server — verified ✓ (but stripping verification is weak — F-17)
- **E2E-4**: Wrong secret → rejection — verified ✓

### No New Attack Surface

- No new network listeners introduced ✓
- No new file I/O outside test temp directories ✓
- Watcher only reads modification times ✓
- Dynamic-require loads from fixed module paths (not user-controlled) ✓
- But: if hot-swap were enabled and module paths were user-controllable, `dynamic-require` could load arbitrary code. Currently mitigated by fixed paths in `registry-defaults.rkt`.

---

## 4. Architecture Review

### Module Dependency Graph (no cycles)

```
blackboard-follower.rkt → blackboard.rkt, blackboard-reducer.rkt, util/json/jsonl, util/event/event
blackboard-subscriber.rkt → blackboard-follower.rkt (+ event-bus, blackboard, blackboard-reducer)
registry-types.rkt → racket/contract (pure data)
registry.rkt → registry-types.rkt, util/ids.rkt
registry-defaults.rkt → registry.rkt, roles/{planner,verifier,tool-gateway,executor}
registry-watcher.rkt → registry.rkt
```

✅ No circular dependencies.

### Separation of Concerns

| Change | Assessment |
|--------|-----------|
| Blackboard follower extraction (G-2) | Clean. Follower has no event-bus dependency. Subscriber re-exports for backward compat. |
| Registry struct extension (G-3) | Correct. New field added with default. Backward compat preserved. |
| Dynamic-require loading (G-3) | Correct structure but broken in practice (F-11/F-12). Feature gate prevents harm. |
| Watcher (G-4) | Clean design with custodian-based cleanup. Callback-based decoupling is good. |

### Backward Compatibility

| API | Status | Verified |
|-----|--------|----------|
| `rebuild-blackboard-from-log!` from subscriber | Re-exported from follower | ✅ 17/17 + 12/12 |
| `blackboard-relevant-event?` from subscriber | Re-exported from follower | ✅ |
| `register-agent!` without `#:factory-name` | Works with default `#f` | ✅ 31/31 |
| `make-agent-instance` without hot-swap | Uses static factory | ✅ |
| `agent-descriptor` 5→6 fields | Updated in test-agent-registry.rkt | ✅ |

---

## 5. Test Quality Assessment

### Test Results (Independently Verified)

| File | Claimed | Actual | Notes |
|------|---------|--------|-------|
| `test-blackboard-follower.rkt` | 10/10 | **10/10** ✅ | Accurate |
| `test-registry-hot-swap.rkt` | 9/9 | **8/9** ❌ | F-11: dynamic path fails |
| `test-registry-watcher.rkt` | 9/9 | **9/9** ✅ | Accurate |
| `test-distributed-execution-e2e.rkt` | 6/6 | **6/6** ✅ | Accurate |
| **Total** | 34/34 | **33/34** | 1 failure |

### Test Non-Triviality Assessment

| Test | Triviality | Notes |
|------|-----------|-------|
| Follower tests | Good | Real JSONL files created and replayed |
| Hot-swap static path | Good | Correctly verifies backward compat |
| Hot-swap dynamic path | **FAILS** | Would be good if it passed (F-11) |
| Hot-swap fallback | Good | Correctly verifies error recovery |
| Watcher path->role-name | Good | Multiple path formats tested |
| Watcher file detection | Adequate | Relies on 2s polling timing |
| E2E-1 (round-trip) | Excellent | Real TLS, real execution |
| E2E-2 (token stripping) | **Weak** | Doesn't actually verify stripping (F-17) |
| E2E-3 (circuit breaker) | Good | Real server kill/restart |
| E2E-4 (wrong secret) | Good | Verifies rejection |
| E2E-5 (broker disabled) | **Misleading** | Not a true E2E test (F-19) |

### Existing Test Regression Check

| Suite | Tests | Result |
|-------|-------|--------|
| `test-mas-capability-filtered-registry.rkt` | 14 | ✅ All pass |
| `test-mas-tool-annotations.rkt` | 12 | ✅ All pass |
| `test-agent-registry.rkt` | 31 | ✅ All pass |
| `test-remote-ipc-resilience.rkt` | 12 (7+5) | ✅ All pass |
| `test-remote-executor-security.rkt` | 11 (7+4) | ✅ All pass |
| `test-mcp-security-gates.rkt` | 19 | ✅ All pass |
| `test-mcp-protocol-compliance.rkt` | 14 | ✅ All pass |
| `test-capability-token-hardening.rkt` | 18 | ✅ All pass |
| `test-blackboard-subscriber.rkt` | 17 | ✅ All pass |
| `test-blackboard-lifecycle.rkt` | 12 | ✅ All pass |

---

## 6. Broad Suite Analysis

### Baseline (v0.99.12, `621bdf84`)

| Metric | Value |
|--------|-------|
| Files | 913 total, 831 passed, 81 failed |
| Tests | 11433 total, 11310 passed, 123 failed |

### Post-Implementation (v0.99.13, `abc2acea`)

| Metric | Value | Delta |
|--------|-------|-------|
| Files | 916 total, 838 passed, 78 failed | +3 files, +7 passed, **-3 failed** |
| Tests | 11470 total, 11349 passed, 121 failed | +37 tests, +39 passed, **-2 failed** |

**Verdict: Zero regressions.** The suite shows slight improvement (2 fewer failing tests). All pre-existing failures are unchanged.

**Note:** Numbers vary slightly between runs (±1-2 files/tests) due to flaky tests and timing. The 916 total includes the 4 new test files minus 1 file that may have been renamed or excluded.

---

## 7. Gap Closure Verification

| Gap | Status | Implementation Quality | Test Quality |
|-----|--------|----------------------|-------------|
| **G-1** Capability Annotations | ✅ Closed (prior) | Excellent | 14/14 ✅ |
| **G-2** Blackboard Follower | ✅ Closed | Excellent (clean extraction) | 10/10 ✅ |
| **G-3** Hot-Swapping | ⚠️ Closed (seam-only) | Good structure, broken execution (F-11/F-12/F-13) | 8/9 ❌ |
| **G-4** Registry Watcher | ✅ Closed | Good (adequate for experimental) | 9/9 ✅ |
| **G-5** E2E Tests | ✅ Closed | Good (5 real E2E + 1 weak) | 6/6 ✅ |

**G-3 is functionally a seam-only implementation.** The dynamic-require path exists in code but:
1. Module paths don't resolve in test environments (F-11)
2. SHARED-MODULES is empty so type identity is broken (F-12)
3. The feature gate isn't wired to production config (F-13)

This means the G-3 hot-swap feature cannot actually work end-to-end. It is a properly-architected seam that needs wiring and SHARED-MODULES population before it can be considered functional.

---

## 8. W4 Audit Integrity Assessment

The W4 audit (`AUDIT-v0.99.13-POST-IMPLEMENTATION.md`) at commit `47685ab8` has the following accuracy issues:

| W4 Claim | Reality | Severity |
|----------|---------|----------|
| "test-registry-hot-swap.rkt: 9/9 ✓" | 8/9 (1 failure) | **CRITICAL** — false test pass claim |
| "34 new tests, all green" | 33 pass + 1 fail | **HIGH** — misreporting |
| "Zero regressions" | Accurate | ✅ |
| "Feature gates default-off" | Accurate (but not wired) | ✅ (misleading) |
| "SHARED-MODULES: appropriate for current implementation" | Should be MEDIUM severity | ⚠️ Underweighted |
| Wiring gaps (F-13/F-14) | Not identified | **HIGH** — missed entirely |
| CHANGELOG errors (F-15) | Not identified | **MEDIUM** — missed |

**Assessment:** The W4 audit appears to have been conducted without actually running the `test-registry-hot-swap.rkt` test file, or it was run in a working directory where the module path resolved correctly (e.g., from `q/` directly rather than via `raco test`). The audit's structural analysis and security review are sound, but its test verification for the hot-swap feature is unreliable.

---

## 9. Recommendations

### Must-Fix (Before Enabling Hot-Swap in Production)

1. **F-11**: Fix module path resolution — use collection-relative paths or `variable-reference`-based path computation in `registry-defaults.rkt`.
2. **F-12**: Populate `SHARED-MODULES` with at minimum `agent/roles/base` (for `gen:agent-role` identity).
3. **F-13**: Wire `set-hot-swap-enabled!` in `run-modes.rkt` inside the `(when (hot-swap-enabled? settings) ...)` block.
4. **F-14**: Wire `set-session-active!` to the session lifecycle.

### Should-Fix (Quality)

5. **F-15**: Correct CHANGELOG factual errors (9/9→8/9, fix re-export direction).
6. **F-16**: Fix watcher comments to match implementation (polling, not `filesystem-change-evt`; patch, not minor).
7. **F-17**: Strengthen E2E-2 to actually verify token stripping (use a tool that inspects all arguments).

### Nice-to-Have

8. **F-18**: Add contracts to `registry-watcher.rkt` provides.
9. **F-19**: Replace E2E-5 with a true local-fallback E2E test.
10. Add `filesystem-change-evt` support to the watcher (as documented but not implemented).

---

## 10. Conclusion

The v0.99.13 milestone successfully closes all 5 gaps from the MAS remediation plan. The G-2 (blackboard follower) and G-4 (registry watcher) implementations are production-quality. The G-5 (E2E tests) provide valuable integration coverage. The G-3 (hot-swapping) is a well-architected seam that needs path resolution fixes, SHARED-MODULES population, and wiring before it can be considered functional.

The most concerning finding is that the W4 audit misreported the hot-swap test results as 9/9 when the actual result is 8/9. This suggests the audit was not run properly. The wiring gaps (F-13, F-14) mean the feature gates are effectively dead code in production — which is safe (the features are inert) but misleading in documentation.

**Recommendation: APPROVED_WITH_NOTES.** The milestone can be released as-is since all features are default-off and the broad suite shows zero regressions. The F-11 through F-14 findings should be addressed in a follow-up before hot-swap is enabled in production.

---

*Audit performed at commit `abc2acea`. All test results from independent local execution on 2026-06-27.*

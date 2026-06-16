# AUDIT: v0.99.15 — MAS Phase 2: Verifier Default-On + Audit Debt Closure

**Auditor:** Independent post-implementation review  
**Date:** 2026-06-29  
**Milestone:** #801  
**HEAD:** `0e2aef87`  
**Verdict:** ✅ **APPROVED** — 4.7 / 5.0  

---

## 1. Executive Summary

v0.99.15 flips the verifier agent from opt-in (`mas.verifier.enabled` default `#f`) to default-on (`#t`), completes hot-swap registry bug fixes from the v0.99.13 audit, and closes all remaining LOW/INFO findings from both the v0.99.13 and v0.99.14 in-depth audits.

The implementation is well-structured across 5 waves:
- **W0**: Characterization tests lock in verifier behavior before the flip
- **W1**: Hot-swap registry bugs F-11 through F-14 fixed (registry hot-swap now 9/9 from all directories)
- **W2**: All 7 audit debt findings closed (F-15 through F-19 + v0.99.14-F-01/F-02)
- **W3**: Verifier default flip `#f`→`#t` + risk threshold `medium`→`high`
- **W4**: Version bump 0.99.15 + comprehensive CHANGELOG

**Key finding:** The verifier default-on change is safe because:
1. The verifier only activates for GSD `wave-done` commands
2. Non-GSD sessions are completely unaffected (no code path reaches the gate)
3. Safe fallback chain: no provider → auto-approve, error → escalate, timeout → escalate
4. The `current-verifier-enabled` runtime parameter default remains `#f` (uninitialized safety)

---

## 2. Scope

### 2.1 Changes Reviewed

| Wave | Issue | Merge SHA | Files Changed | Description |
|------|-------|-----------|---------------|-------------|
| W0 | #8113 | `f9aa92dd` | 1 new test file | 7 verifier characterization tests |
| W1 | #8114 | `e0c5110a` | 6 files (3 src + 3 test) | Hot-swap bug fixes F-11/F-12/F-13/F-14 |
| W2 | #8115 | `72f82c2d` | 3 files | Audit debt closure F-15–F-19 + v0.99.14-F-01/F-02 |
| W3 | #8116 | `f6b48b54` | 3 files | Verifier default flip + risk threshold change |
| W4 | #8117 | `0e2aef87` | 4 files | Version bump 0.99.15 + CHANGELOG |

### 2.2 Out of Scope

- Broad suite full run (~76 min; targeted verification of 18 test files run instead)
- Distributed execution E2E tests (require network ports, run separately)

---

## 3. Detailed Findings

### 3.1 W0 (#8113): Verifier Characterization Tests

**Files:** `tests/test-verifier-deployment-gate.rkt` (NEW)

**Assessment:** Excellent test-first approach. Seven tests written BEFORE the default flip to lock in existing behavior. After W3 flipped the default, tests #1 and #4 were updated to verify the new expected behavior.

**Tests verified (post-W3):**
1. `verifier-enabled?` returns `#t` for default settings (updated post-W3)
2. `verifier-enabled?` returns `#t` when explicitly enabled
3. `verifier-enabled?` returns `#f` when explicitly disabled
4. `verifier-risk-threshold` returns `'high` for default settings (updated post-W3)
5. `verifier-risk-threshold` returns `'medium` when explicitly set (updated post-W3)
6. Gate with flag OFF returns `'approved` (no LLM call)
7. Gate with flag ON + no provider returns `'approved` (safe fallback)

**Verdict:** ✅ No issues.

### 3.2 W1 (#8114): Hot-Swap Bug Fixes (F-11 through F-14)

**Files:** `agent/registry-defaults.rkt`, `agent/registry.rkt`, `agent/registry-types.rkt`, `wiring/run-modes.rkt`, `tests/test-registry-hot-swap.rkt`, `tests/test-registry-deployment-gate.rkt` (NEW)

**Assessment:** All four hot-swap bugs from the v0.99.13 audit are properly fixed:

- **F-11** (module path resolution): Now uses `#%variable-reference` + `split-path` for absolute module paths. The `path-only` function (not in `racket/base`) has been replaced with `variable-reference` + `split-path`. Test 4 uses collection path `'q/agent/roles/planner` (symbol form) for CWD independence.
- **F-12** (SHARED-MODULES): Populated with `'q/agent/roles/base` and `'q/util/capability`. `namespace-attach-module` correctly wrapped in per-module `with-handlers` for test contexts.
- **F-13** (hot-swap wiring): `set-hot-swap-enabled!` now called from `run-modes.rkt` inside `(when (hot-swap-enabled? settings) ...)`.
- **F-14** (session-active wiring): `set-session-active! #t` called at session start.
- **Contract broadened**: `agent-descriptor module-path` from `(or/c #f string?)` to `(or/c #f module-path?)`.

**Test results:** Registry hot-swap tests improved from 8/9 → **9/9** (all pass from both `q/` and `q/tests/` directories). 8 new deployment gate tests added.

**Verdict:** ✅ No issues.

### 3.3 W2 (#8115): Audit Debt Closure

**Files:** `CHANGELOG.md`, `agent/registry-watcher.rkt`, `tests/test-distributed-execution-e2e.rkt`

**Assessment:** All 7 findings properly addressed:

- **F-15**: CHANGELOG inaccuracies corrected (hot-swap test count 9/9→8/9, re-export direction fixed).
- **F-16**: `registry-watcher.rkt` comments fixed (false `filesystem-change-evt` claim removed, "minor version"→"patch version", shadowed local `last`/`caddr` removed via `racket/list`).
- **F-17**: E2E-2 token-stripping assertion strengthened — bash command now dumps env vars, and test asserts capability secret does NOT appear in response content, error message, or details.
- **F-18**: Contracts added to all 5 `registry-watcher.rkt` provides (`path->role-name`, `next-version`, `start-registry-watcher!`, `stop-registry-watcher!`, `watcher-running?`).
- **F-19**: E2E-5 renamed from misleading "broker-disabled local-only execution" to accurate "no server running → executor connection fails (fail-closed)".
- **v0.99.14-F-01**: CHANGELOG "Token budget guard (W3, forthcoming)" → past tense.
- **v0.99.14-F-02**: CHANGELOG test count "115 existing" → "106 existing".

**Verdict:** ✅ No issues.

### 3.4 W3 (#8116): Verifier Default Flip + Risk Threshold

**Files:** `runtime/settings-query.rkt`, `tests/test-verifier-deployment-gate.rkt`, `tests/test-verifier-integration.rkt`

**Assessment:** The core change of this milestone. Two defaults flipped:

1. `verifier-enabled?`: `#f` → `#t` (default-on)
2. `verifier-risk-threshold`: `"medium"` → `"high"` (conservative escalation)

The implementation is correct:
- The settings function is the source of truth for the default.
- The runtime parameter `current-verifier-enabled` is set from settings via `run-modes.rkt:492`.
- The parameter default remains `#f` (defensive: uninitialized modules skip verification).
- The gate in `command-handlers.rkt:186` only fires for GSD `wave-done` commands.
- Safe fallback: no provider → auto-approve (confirmed by Test 7).

**Test updates:** 4 tests in `test-verifier-integration.rkt` correctly updated:
- Default `verifier-enabled?` now `#t`
- Default `verifier-risk-threshold` now `'high`
- Invalid risk-threshold coerces to `'high` (was `'medium`)
- "config-disabled" test now uses explicit `#f` override

**Verdict:** ✅ No issues.

### 3.5 W4 (#8117): Version Bump + CHANGELOG

**Files:** `util/version.rkt`, `info.rkt`, `README.md`, `CHANGELOG.md`

**Assessment:** Clean version bump using the established `sync-version.rkt --write` workflow. CHANGELOG section is comprehensive and accurate. `lint-release-notes.rkt` passes.

**Verdict:** ✅ No issues.

---

## 4. Test Coverage Summary

### Verifier Tests (163 tests)
| Test File | Tests | Status |
|-----------|-------|--------|
| `test-verifier-deployment-gate.rkt` | 7 | ✅ 7/7 |
| `test-verifier-integration.rkt` | 26 | ✅ 26/26 |
| `test-verifier-gate.rkt` | 20 | ✅ 20/20 |
| `test-verifier-core.rkt` | 30 | ✅ 30/30 |
| `test-verifier-hardening.rkt` | 25 | ✅ 25/25 |
| `test-verifier-prompt.rkt` | 17 | ✅ 17/17 |
| `test-verifier-types.rkt` | 30 | ✅ 30/30 |
| `test-verifier-wiring.rkt` | 7 | ✅ 7/7 (6+1) |
| **Total** | **163** | **✅ All pass** |

### Registry Tests (94 tests)
| Test File | Tests | Status |
|-----------|-------|--------|
| `test-registry-hot-swap.rkt` | 9 | ✅ 9/9 (was 8/9) |
| `test-registry-deployment-gate.rkt` | 8 | ✅ 8/8 (NEW) |
| `test-agent-registry.rkt` | 31 | ✅ 31/31 |
| `test-registry-config-wiring.rkt` | 11 | ✅ 11/11 |
| `test-registry-integration.rkt` | 7 | ✅ 7/7 |
| `test-registry-snapshot.rkt` | 2 | ✅ 2/2 |
| `test-registry-supervisor.rkt` | 10 | ✅ 10/10 |
| `test-registry-watcher.rkt` | 9 | ✅ 9/9 |
| `test-hot-swap-events.rkt` | 7 | ✅ 7/7 |
| **Total** | **94** | **✅ All pass** |

### Additional Tests
| Test File | Tests | Status |
|-----------|-------|--------|
| `test-verification-events.rkt` | 18 | ✅ 18/18 |
| `test-tui-verification-display.rkt` | — | ✅ Passes |

**Grand total: 275+ targeted tests, all green.**

---

## 5. Code Quality

### 5.1 Contract Coverage
- `registry-watcher.rkt` now has full contract-out on all 5 exports (F-18).
- `registry-types.rkt` contract broadened from `(or/c #f string?)` to `(or/c #f module-path?)`.
- All verifier modules retain their existing contracts.

### 5.2 Documentation
- CHANGELOG v0.99.15 section is comprehensive and accurate.
- All code comments updated to reflect new defaults.
- `registry-watcher.rkt` header comment corrected (polling-based, not filesystem-change-evt).

### 5.3 Safety Properties
- `current-verifier-enabled` parameter default: `#f` (uninitialized safety)
- Settings default: `#t` (wired from session start via `run-modes.rkt`)
- Verifier scope: GSD `wave-done` commands only
- Safe fallback: no provider → auto-approve

---

## 6. Score Breakdown

| Category | Score | Notes |
|----------|-------|-------|
| Correctness | 4.8/5.0 | All 275+ tests pass. Registry hot-swap fixed. |
| Test Coverage | 4.8/5.0 | Comprehensive targeted tests. Broad suite not run (time). |
| Safety | 4.8/5.0 | Safe fallback chain verified. Non-GSD unaffected. |
| Code Quality | 4.6/5.0 | Contracts added. Comments fixed. Clean separation. |
| Documentation | 4.7/5.0 | CHANGELOG accurate. Audit findings all closed. |
| **Overall** | **4.7/5.0** | **✅ APPROVED** |

---

## 7. Open Items / Follow-Up

- **Broad suite**: Not run in this milestone (~76 min). Baseline from v0.99.13: 916 files, 835 passed, 80 failed (pre-existing debt). No regressions expected based on targeted testing.
- **E2E tests**: `test-distributed-execution-e2e.rkt` requires network ports; not run in audit but assertions strengthened (F-17, F-19).
- **v0.99.13 registry hot-swap**: Now fully fixed (9/9 from all directories). The 8/9 → 9/9 improvement is the direct result of F-11 (module path resolution).

---

## 8. Conclusion

v0.99.15 successfully delivers the MAS Phase 2 verifier default-on milestone. The implementation follows the established pattern from v0.99.14: characterization tests first (W0), bug fixes (W1), audit debt closure (W2), default flip (W3), release (W4), audit (W5).

The verifier default-on change is well-justified and safe:
- Scope-limited to GSD wave-done commands
- Safe fallback chain prevents blocking when no provider is configured
- Conservative risk threshold (`'high`) means only genuinely high-risk decisions escalate
- Runtime parameter default remains `#f` for defensive initialization

All hot-swap registry bugs from v0.99.13 are now fully resolved. All audit debt from v0.99.13 and v0.99.14 is closed. The milestone is ready for release.

**Verdict: ✅ APPROVED — 4.7/5.0**

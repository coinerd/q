# AUDIT: v0.99.22 — Remediation + Säule C Adaptive Verification

**Date:** 2026-07-16
**Auditor:** Self-audit (coinerd)
**Milestone:** #808
**Base Commit:** `ed157ad5` (v0.99.21)
**Head Commit:** `3f4ea9c7` (v0.99.22)
**Plan:** `.planning/PLAN-v0.99.22-REMEDIATION-AND-SAEULE-C-ADAPTIVE-VERIFICATION.md`

---

## 1. Executive Summary

v0.99.22 delivers three tracks: v0.99.21 audit remediation (A-1, A-2, F-5), Säule A completion (batch capabilities), and Säule C foundation (§6.1 complexity heuristic, §6.2 dynamic risk threshold). All planned items were implemented. The build passes with clean bytecode. All 51 new and existing adjacent tests pass with zero regressions.

**Overall Score: 4.5 / 5.0 — APPROVED**

---

## 2. Audit Methodology

This audit follows the `.planning/AUDIT-PROCESS-CHECKLIST.md`:
- `git diff ed157ad5..3f4ea9c7` reviewed for EVERY production file
- All test suites that import changed modules were independently run
- `raco make main.rkt` with clean bytecode (FRESH compilation)
- CHANGELOG claims verified against actual diffs
- Root cause narratives verified against actual code changes (not plan hypotheses)

---

## 3. Track 1: Remediation

### A-1: Fix `test-subagent-config.rkt` Regression — ✅ PASS

**Commit:** `8db86ed6` (W0)

**Verification:**
- `git diff ed157ad5..HEAD -- tests/test-subagent-config.rkt` confirms 2 lines changed
- Both changes add `#f` as 6th arg to `subagent-config` constructor calls (lines 13, 21)
- This is an append-only fix — no semantic change, just arity correction
- `raco test tests/test-subagent-config.rkt` → 4/4 PASS (independently verified)

**Root cause (verified):** W2 of v0.99.21 added a 6th field (`capabilities`) to the `subagent-config` struct but did not update the two constructor calls in this test file. The struct arity changed from 5 to 6 args. This is a test-only fix — no production code was changed.

### A-2: Batch Spawn Capabilities Support — ✅ PASS

**Commit:** `8db86ed6` (W0)

**Verification:**
- `git diff ed157ad5..HEAD -- tools/builtins/spawn-subagent.rkt` confirms:
  - New function `parse-job-capabilities` (lines ~564-581) — extracts capabilities from job hash
  - `run-single-job` now reads capabilities from job hash (line ~589) and passes to `run-subagent`
  - Backward compatible: when `'capabilities` key absent → `#f` → all tools
- `git diff ed157ad5..HEAD -- tools/registry-table/skill-tools.rkt` confirms schema updated:
  - Job items now document `capabilities` property with proper enum
  - Description updated to mention capabilities
- 3 new test-cases in `test-capability-aware-spawn.rkt`:
  1. `parse-job-capabilities` returns `#f` when no key
  2. Parses valid capabilities
  3. Filters invalid and returns `#f` for empty
- `raco test tests/test-capability-aware-spawn.rkt` → 13/13 PASS (10 existing + 3 new)
- `raco test tests/test-spawn-subagents-batch.rkt` → 14/14 PASS (no regressions)

**Root cause (verified):** The batch path (`run-single-job`) never called `parse-subagent-config` or any capability parsing — it went directly to `run-subagent` with a fixed hash that lacked `'capabilities`. The fix extracts the capability parsing into a standalone `parse-job-capabilities` function and wires it into `run-single-job`.

### F-5: Process Checklist Update — ✅ PASS

**Verification:**
- `grep -n "clean-bytecode\|FRESH bytecode" .planning/AUDIT-PROCESS-CHECKLIST.md` confirms the step was added (lines 60-64)
- The added text matches the plan's specification
- File is outside git repo (`.planning/` is local-only) — verified by local file inspection

---

## 4. Track 3: Säule C — Adaptive Verification

### §6.1: Complexity Heuristic for Verifier Skip — ✅ PASS

**Commit:** `b84f57ee` (W1)

**Verification:**
- `git diff ed157ad5..HEAD -- agent/verification/verifier-gate.rkt` confirms:
  - `should-skip-verification?` function added (pure function of plan-context)
  - Skip clause wired into `execute-verification-gate` (after feature-flag check, before provider check)
  - `should-run-verification-gate?` updated with optional `plan-context` parameter
  - Contract updated: `(->* (gsd-session-ctx?) ((or/c hash? #f)) boolean?)`

**Deviation from plan (IMPROVEMENT):**
The plan's implementation used `(hash-ref plan-context 'capabilities-used '())` and had no presence check. The actual implementation uses `(hash-ref plan-context 'capabilities-used #f)` with an explicit `(and capabilities-used (list? capabilities-used) ...)` guard. This is MORE conservative than planned:
- Plan: empty `'()` capabilities → still checks conditions (since `'()` is truthy in Racket)
- Actual: missing `'capabilities-used` key → `#f` → always verify

This is a correctness improvement. When `capabilities-used` is absent, the function returns `#f` (verify), which is the safest behavior. The plan's version would have skipped verification for any wave with ≤2 files when capabilities weren't provided — which is too aggressive.

**Test verification:**
- `tests/test-verifier-complexity-heuristic.rkt` (NEW): 16 tests, all PASS
- Tests cover: trivial skip, file count boundaries, capability-based blocking, conservative defaults

**Adjacent test check:**
- `test-verifier-gate.rkt` → 20/20 PASS
- `test-verifier-integration.rkt` → 26/26 PASS
- `test-verifier-wiring.rkt` → 13/13 PASS
- `test-verifier-deployment-gate.rkt` → 7/7 PASS

### §6.2: Dynamic Risk Threshold — ✅ PASS

**Commit:** `095dbdcf` (W2)

**Verification:**
- `git diff ed157ad5..HEAD -- agent/verification/verifier-gate.rkt` confirms:
  - `effective-risk-threshold` function added (pure function: `hash? symbol? → symbol?`)
  - Wired into `execute-verification-gate` via `parameterize` of `current-verifier-risk-threshold`
  - This correctly causes `enforce-risk-threshold` inside `run-verification` to use the dynamic value

**Implementation matches plan exactly:**
- `shell-exec` → `'low` (strictest)
- `git-write` → `'low` (strictest)
- `file-write` → `'medium`
- read-only → `base-threshold` (defer to user config)

**Test verification:**
- `tests/test-verifier-risk-threshold.rkt` (NEW): 9 tests, all PASS
- Tests cover: all capability→threshold mappings, precedence, backward compat, §6.1+§6.2 interaction

---

## 5. Version Bump — ✅ PASS

**Commit:** `3f4ea9c7` (W3)

**Verification:**
- `util/version.rkt`: `"0.99.22"` ✅
- `info.rkt`: `"0.99.22"` ✅
- `README.md` badge: `0.99.22` ✅
- `README.md` example output: `0.99.22` ✅
- `CHANGELOG.md`: v0.99.22 entry present with correct content ✅
- `raco test tests/test-version.rkt` → 3/3 PASS

---

## 6. Build Verification

**`raco make main.rkt` with clean bytecode:**
```
find . -name "compiled" -type d -exec rm -rf {} +
raco make main.rkt
→ BUILD: PASS (no errors, no warnings)
```

---

## 7. Regression Check — All Adjacent Suites

Every test file that imports a changed module was independently run:

| Test File | Tests | Result | Imports |
|-----------|-------|--------|---------|
| test-subagent-config.rkt | 4 | ✅ PASS | spawn-subagent |
| test-capability-aware-spawn.rkt | 13 | ✅ PASS | spawn-subagent |
| test-verifier-complexity-heuristic.rkt | 16 | ✅ PASS | verifier-gate |
| test-verifier-risk-threshold.rkt | 9 | ✅ PASS | verifier-gate |
| test-verifier-gate.rkt | 20 | ✅ PASS | verifier-gate |
| test-verifier-integration.rkt | 26 | ✅ PASS | verifier-gate |
| test-verifier-wiring.rkt | 13 | ✅ PASS | verifier-gate |
| test-verifier-deployment-gate.rkt | 7 | ✅ PASS | verifier-gate |
| test-spawn-subagents-batch.rkt | 14 | ✅ PASS | spawn-subagent |
| test-wave5-capability-gaps.rkt | 21 | ✅ PASS | spawn-subagent |
| test-mas-spawn-subagent-facade.rkt | 10 | ✅ PASS | spawn-subagent |
| test-spawn-subagent-integration.rkt | 13 | ✅ PASS | spawn-subagent |
| test-permission-gate.rkt | 11 | ✅ PASS | spawn-subagent |
| test-mas-guidance.rkt | 7 | ✅ PASS | spawn-subagent |
| test-blackboard-context-injection.rkt | 6 | ✅ PASS | spawn-subagent |
| test-mas-capability-filtered-registry.rkt | 14 | ✅ PASS | spawn-subagent |
| test-version.rkt | 3 | ✅ PASS | version |
| **TOTAL** | **207** | **0 failures** | |

**Zero regressions detected.**

---

## 8. CHANGELOG Accuracy Check

| CHANGELOG Claim | Verified Against | Status |
|----------------|-----------------|--------|
| "W0: 3 new batch-capability tests" | `git diff` shows 3 new test-cases | ✅ Accurate |
| "W1: 16 new tests for complexity heuristic" | `test-verifier-complexity-heuristic.rkt` has 16 tests | ✅ Accurate |
| "W2: 9 new tests for dynamic risk threshold" | `test-verifier-risk-threshold.rkt` has 9 tests | ✅ Accurate |
| "All existing MAS tests still pass" | 207 adjacent tests pass | ✅ Accurate |
| "2 production files changed" | Actually 3 (spawn-subagent.rkt, verifier-gate.rkt, skill-tools.rkt) | ⚠️ Minor inaccuracy |
| "2 test files changed (1 fix, 1 extend)" | test-subagent-config.rkt (fix) + test-capability-aware-spawn.rkt (extend) | ✅ Accurate |
| "2 new test files" | test-verifier-complexity-heuristic.rkt + test-verifier-risk-threshold.rkt | ✅ Accurate |
| "1 schema update" | skill-tools.rkt | ✅ Accurate |

**Finding:** The CHANGELOG's "Operational / Release" section says "2 production files changed" but there are actually 3 production code files (excluding version-release files). The third is `tools/registry-table/skill-tools.rkt` (schema update). This is a minor cosmetic inaccuracy, not a functional issue. The schema update IS mentioned separately in the same section ("1 schema update").

---

## 9. Findings & Recommendations

### Findings

**F-22-1 (Minor — CHANGELOG inaccuracy):** The "2 production files changed" claim should be "3 production files changed" (spawn-subagent.rkt, verifier-gate.rkt, skill-tools.rkt). The skill-tools.rkt schema update is mentioned separately but not counted in the production files tally. **Impact:** Cosmetic. No functional consequence.

**F-22-2 (Positive — Deviation from plan):** The `should-skip-verification?` implementation is MORE conservative than the plan specified. The plan used `(hash-ref plan-context 'capabilities-used '())` (empty list default), which is truthy in Racket. The implementation uses `#f` default with an explicit guard, requiring the `'capabilities-used` key to be PRESENT before any skip logic runs. This is a correctness improvement — it means existing callers without capability info always verify. **Impact:** Positive. No action needed.

### Recommendations

1. **No action required for F-22-1.** The CHANGELOG is a point-in-time record. The audit documents the correction.
2. **F-22-2 should be noted as a design decision in future planning.** The implementation correctly handles the Racket truthiness of `'()` by requiring explicit capability presence.

---

## 10. Score Breakdown

| Axis | Score | Notes |
|------|-------|-------|
| Correctness | 5.0/5.0 | All implementations match or improve on plan. Zero regressions. |
| Testing | 5.0/5.0 | 28 new tests, 207 adjacent tests pass, comprehensive coverage |
| Documentation | 4.5/5.0 | Minor CHANGELOG inaccuracy (production file count). Root cause narratives verified against diffs. |
| Build Stability | 5.0/5.0 | Clean bytecode build passes. No warnings. |
| Process Compliance | 4.5/5.0 | F-5 actually completed this time. Process checklist followed for all waves. Minor: CHANGELOG file count inaccuracy. |
| **Overall** | **4.5/5.0** | **APPROVED** |

---

## 11. Commits in This Release

| Commit | Wave | Description |
|--------|------|-------------|
| `8db86ed6` | W0 | Remediation: A-1 + A-2 + F-5 (#8201) |
| `b84f57ee` | W1 | Complexity heuristic for verifier skip §6.1 (#8202) |
| `095dbdcf` | W2 | Dynamic risk threshold for verifier §6.2 (#8203) |
| `3f4ea9c7` | W3 | Version bump 0.99.21 → 0.99.22 (#8204) |

---

## 12. Files Changed Summary

### Production Code (3 files)
| File | Change |
|------|--------|
| `agent/verification/verifier-gate.rkt` | §6.1 + §6.2: Skip heuristic + dynamic risk threshold |
| `tools/builtins/spawn-subagent.rkt` | A-2: `parse-job-capabilities` + batch capabilities wiring |
| `tools/registry-table/skill-tools.rkt` | A-2: Schema update for capabilities in job items |

### Release Files (4 files)
| File | Change |
|------|--------|
| `util/version.rkt` | 0.99.21 → 0.99.22 |
| `info.rkt` | Sync |
| `README.md` | Badge + example sync |
| `CHANGELOG.md` | v0.99.22 entry |

### Test Files (4 files: 2 new, 2 modified)
| File | Change | Tests |
|------|--------|-------|
| `tests/test-verifier-complexity-heuristic.rkt` | NEW (W1) | 16 |
| `tests/test-verifier-risk-threshold.rkt` | NEW (W2) | 9 |
| `tests/test-capability-aware-spawn.rkt` | EXTENDED (W0) | +3 (total 13) |
| `tests/test-subagent-config.rkt` | FIXED (W0) | 4 (no count change) |

### Local Planning (1 file, outside git)
| File | Change |
|------|--------|
| `.planning/AUDIT-PROCESS-CHECKLIST.md` | F-5: Clean-bytecode build step |

---

## 13. Conclusion

v0.99.22 is a clean release. All planned items were implemented correctly. The implementation improved on the plan in one area (more conservative skip heuristic). The one minor CHANGELOG inaccuracy (production file count) is cosmetic. No regressions detected across 207 tests in 17 test suites. Build passes with clean bytecode.

**Verdict: APPROVED for release.**

---

*Audit performed 2026-07-16 following `.planning/AUDIT-PROCESS-CHECKLIST.md`.*
*All root cause narratives verified against `git diff ed157ad5..3f4ea9c7`.*

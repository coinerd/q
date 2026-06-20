# In-Depth Audit — v0.99.36

**Date:** 2026-06-22
**Milestone:** #822 (v0.99.36 — Racket Abstraction Manual Roadmap II)
**Verdict:** ✅ **APPROVED AFTER REMEDIATION #8435**

---

## 1. Milestone Summary

**v0.99.36** comprised 11 waves (W0–W10) of systematic abstraction
improvement work, continuing the program started in v0.99.35.

| Wave | Issue | PR | Scope | Status |
|------|-------|----|-------|--------|
| W0 | #8413 | #8424 | Abstraction ownership inventory (692 modules) | ✅ |
| W1 | #8414 | #8425 | Red-flag tooling in abstraction-audit.rkt | ✅ |
| W2 | #8415 | #8426 | Version surface centralization | ✅ |
| W3 | #8416 | #8427 | Struct-out inventory + 2 explicit-export conversions | ✅ |
| W4 | #8417 | #8428 | Parameter/dynamic-context ownership map | ✅ |
| W5 | #8418 | #8429 | Result-boundary pilot (status-result.rkt) | ✅ |
| W6 | #8419 | #8430 | I/O abstraction pilot (lint-version-io.rkt) | ✅ |
| W7 | #8420 | #8431 | CLI/run-mode design-it-twice audit | ✅ |
| W8 | #8421 | #8432 | Session/TUI/settings boundary audit + 2 cleanups | ✅ |
| W9 | #8422 | #8433 | Macro/DSL/parser safety audit + 15 expansion tests | ✅ |
| W10 | #8423 | (this) | Final docs, version/changelog/metrics, gates | ✅ |

---

## 2. Gate Results

### Compile Gate
```
raco make main.rkt
```
**Result:** ✅ PASS (no errors, no warnings)

### Version Lint
```
racket scripts/lint-version.rkt
```
**Result:** ✅ PASS — 0 errors
- `util/version.rkt`: 0.99.36
- `info.rkt`: 0.99.36
- `README.md`: 0.99.36
- 27 .md docs synced from 0.99.35 → 0.99.36

### Smoke Gate
```
racket scripts/run-tests.rkt --suite smoke --profile local
```
**Result:** ✅ PASS
- Files: 19 total, 19 passed, 0 failed, 0 timeouts
- Tests: 286 total, 286 passed, 0 failed
- Elapsed: 1m 7s

### Version Tests
```
raco test tests/test-version.rkt
raco test tests/test-version-surface.rkt
```
**Result:** ✅ PASS (3 + 14 = 17 tests passed)

### Broad Gate

Initial W10 did **not** run the broad gate, which made the original approval
claim incomplete. Independent post-completion audit reproduced release-gate
failures in `tests/test-ci-local.rkt` and `tests/test-metrics-readme-sync.rkt`.

Remediation issue #8435 fixed README Status drift, synchronized README metrics,
and stabilized a flaky broad-only render benchmark assertion. The final
post-remediation broad gate was run from the committed remediation state before the final report-evidence amendment:

```
timeout 2400 racket scripts/run-tests.rkt --suite broad --profile local \
  --ledger tests/test-suite-ledger.json \
  --json-out /tmp/v09936-remediation-final/broad-local-ledger.json
```

**Result:** ✅ PASS
- Files: 1053 total, 1053 passed, 0 failed, 0 timeouts
- Tests: 14087 total, 14087 passed, 0 failed
- Known failures: 0
- New failures: 0
- Unclassified failures: 0
- Release-blocking known failures: 0
- Elapsed: 7m 18.041s

---

## 3. Code Metrics

| Metric | v0.99.35 | v0.99.36 | Delta |
|--------|----------|----------|-------|
| Modules (.rkt, non-test) | ~690 | 695 | +5 |
| Test files | ~1050 | 1061 | +11 |
| Reports in docs/reports/ | ~50 | 60 | +10 |
| `make-parameter` sites | unmapped | 189 (mapped) | — |
| `struct-out` forms | unmapped | 60 (classified) | — |
| Macro definitions | unaudited | 12 (3 audited) | — |

### New Modules (v0.99.36)
1. `scripts/version-surface.rkt` — centralized version parsing (W2)
2. `scripts/status-result.rkt` — structured result types (W5)
3. `scripts/lint-version-io.rkt` — I/O abstraction pilot (W6)
4. `tests/test-macro-dsl-safety-w9.rkt` — macro expansion tests (W9)
5. `tests/test-status-result.rkt` — result-type tests (W5)
6. `tests/test-lint-version-io.rkt` — I/O mock tests (W6)
7. `tests/test-lint-version-pure.rkt` — pure check tests (W6)
8. `tests/test-version-surface.rkt` — version parsing tests (W2)

### Modified Production Code
1. `extensions/gsd/transition-logic.rkt` — struct-out → explicit exports (W3)
2. `runtime/context-assembly/rollback-actions.rkt` — struct-out → explicit exports (W3)
3. `scripts/sync-readme-status.rkt` — result-type dispatch (W5)
4. `scripts/lint-version.rkt` — parameterized I/O, guarded main (W6)
5. `runtime/settings-query.rkt` — coerce-config-boolean helper (W8)
6. `tui/state-types.rkt` — fixed struct field comment (W8)
7. `scripts/abstraction-audit.rkt` — red-flag signals (W1)
8. `util/version.rkt` — version bump
9. `info.rkt` — version bump

---

## 4. New Failures / Regressions

**New failures: 0**  
**Unclassified: 0**  
**Release-blocking: 0**

Post-remediation #8435 evidence:

```
raco make main.rkt runtime/settings-query.rkt runtime/settings.rkt interfaces/sdk-core.rkt
=> PASS

racket scripts/lint-version.rkt
=> PASS, Errors=0

racket scripts/ci-local.rkt --quick
=> PASS, 5/5 checks

racket scripts/run-tests.rkt --suite fast --profile local
=> PASS, 961/961 files, 13158/13158 tests

racket scripts/run-tests.rkt --suite broad --profile local --ledger tests/test-suite-ledger.json
=> PASS, 1053/1053 files, 14087/14087 tests
=> Known=0, New=0, Unclassified=0, Release-blocking=0
```

All 11 waves produced backward-compatible changes. No public API
surfaces were broken. Existing tests pass after the README gate drift and
benchmark flake remediation in #8435.

---

## 5. Key Findings Across All Waves

### W0: Ownership Inventory
- 692 modules scanned with automated signals
- 60 `struct-out` forms classified: 8 GREEN, 2 YELLOW, 4 RED
- 4 RED modules flagged for no-touch: `cli/args.rkt`, `wiring/run-modes.rkt`,
  `agent/event-structs.rkt`, `scripts/run-tests.rkt`

### W1: Red-Flag Tooling
- Extended abstraction-audit.rkt with 8 signal detectors
- 274 modules flagged with at least one red flag
- Top signals: export density, parameter count, I/O mixing

### W2: Version Centralization
- Created `scripts/version-surface.rkt` — single source of truth
- Eliminated version comparison logic duplication
- Backward compat via local aliases

### W3: Explicit Exports
- Complete `struct-out` inventory (60 forms across codebase)
- 2 conversions: `transition-logic.rkt`, `rollback-actions.rkt`
- Both are `#:transparent` structs — explicit exports preserve `equal?`

### W4: Parameter Ownership
- 189 `make-parameter` sites across 78 modules mapped
- 7 ownership patterns identified
- Pattern D (direct mutation) flagged as anti-pattern
- Densest clusters: context-assembly (19), memory (14), sandbox (12)

### W5: Result-Type Boundary
- 5 structured result types in `scripts/status-result.rkt`
- Replaced ad-hoc `printf + exit 1` with typed dispatch
- Pattern reusable for other CLI scripts

### W6: I/O Abstraction
- `lint-version.rkt` refactored to parameterized I/O
- 4 I/O parameters + mock file system for testing
- `(main)` guarded in `(module+ main ...)` for testability
- Pattern directly reusable for other lint/CI scripts

### W7: Design-It-Twice
- Two alternative designs for CLI/run-mode refactoring
- Design A (Builder Pipeline) recommended over Design B
- No code changes — design document only
- RED modules preserved (deferred to future milestone)

### W8: Representation Boundaries
- Audited session, TUI, and settings state representations
- `coerce-config-boolean` helper extracted (12 boilerplate → 1 function)
- `ui-state` struct field comment fixed (documentation-only bug)

### W9: Macro Safety
- 3 complex macros audited: all safe and well-designed
- `define-typed-event` (329L): 6 optional clauses, compile-time helpers
- `define-fsm-machine` (152L): transition-clause syntax-class
- `define-tool` (65L): optional properties with defaults
- 15 new expansion tests targeting edge cases and hygiene
- 4 LOW-severity risks documented (no code changes needed)

---

## 6. Risk Assessment

### LOW Risk (Documented, No Action Required)
1. `define-typed-event`: unknown field in `#:json-keys` silently ignored
2. `define-fsm-machine`: transition to undeclared state returns `#f` silently
3. `define-tool`: `provide tool-id` always emitted (design decision)
4. `define-typed-event`: internal lambda vars theoretically capturable (hygiene-protected)

### Deferred (Future Milestones)
1. `cli/args.rkt` refactoring (634L, 19-field struct, 21 flags) — RED module
2. `wiring/run-modes.rkt` refactoring (621L, 307L build function) — RED module
3. 274 modules with abstraction red flags — prioritized in W1 report
4. 189 parameter sites needing ownership assignment — mapped in W4

### No Release Blockers After #8435
The initial W10 report overstated release readiness because broad was not run
and README gate drift remained. After remediation #8435, release gates are
green under the local profile. This release is a documentation-and-analysis
release with minimal production code changes. All changes are
backward-compatible. No public APIs changed.

---

## 7. Documentation Delivered

### Reports (10 new)
1. `docs/reports/ABSTRACTION-OWNERSHIP-v0.99.36-SUMMARY.md` — W0
2. `docs/reports/ABSTRACTION-RED-FLAGS-v0.99.36.md` — W1
3. `docs/reports/ABSTRACTION-REVIEW-CHECKLIST-v0.99.36.md` — W1
4. `docs/reports/ABSTRACTION-API-INVENTORY-v0.99.36.md` — W3
5. `docs/reports/PARAMETER-OWNERSHIP-v0.99.36.md` — W4
6. `docs/reports/RESULT-BOUNDARY-PILOT-v0.99.36.md` — W5
7. `docs/reports/PORT-IO-ABSTRACTION-PILOT-v0.99.36.md` — W6
8. `docs/reports/CLI-RUNMODE-DESIGN-v0.99.36.md` — W7
9. `docs/reports/SESSION-TUI-SETTINGS-AUDIT-v0.99.36.md` — W8
10. `docs/reports/MACRO-DSL-SAFETY-v0.99.36.md` — W9

### CHANGELOG
Updated with full v0.99.36 entry covering all 11 waves.

### Version Sync
- `util/version.rkt`: 0.99.36
- `info.rkt`: 0.99.36
- `README.md`: 0.99.36 (badge + verify line)
- 27 `docs/*.md` files synced from 0.99.35 → 0.99.36

---

## 8. Verdict

**✅ APPROVED AFTER REMEDIATION #8435**

- Compile gate green, including targeted settings/SDK recompilation
- Version lint green, Errors=0
- `ci-local --quick` green, 5/5 checks
- Fast local green: 961/961 files, 13158/13158 tests
- Broad local ledger green: 1053/1053 files, 14087/14087 tests
- Known=0, New=0, Unclassified=0, Release-blocking=0
- README Status and generated metrics synchronized
- Broad-only benchmark assertion stabilized to avoid wall-clock flakes
- All 11 waves delivered successfully

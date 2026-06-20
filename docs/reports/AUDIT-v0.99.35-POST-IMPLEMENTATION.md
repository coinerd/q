# Audit Report — v0.99.35 Post-Implementation

**Date:** 2026-06-21  
**Post-closure correction:** 2026-06-20, issue #8409  
**Milestone:** #821 (v0.99.35 — Racket Abstraction Manual Roadmap)  
**Original W9 source HEAD:** `e71fd680`  
**Remediation branch:** `fix/v09935-post-closure-audit-8409`  
**Parent Issue:** #8388  
**Remediation Issue:** #8409  
**Auditor:** Automated + manual review

## Verdict

**APPROVED after post-closure remediation #8409.**

The abstraction-delivery goals were met by the original milestone. The original
W9 broad-gate claim was not gate-truth compliant, but #8409 corrected the
reporting error and fixed the release-engineering blockers. Fresh local fast and
broad gates are now green.

The original report incorrectly labeled a non-green broad run as PASS. That
was a gate-truth error. Issue #8409 remedies the release-blocking findings from
`.planning/AUDIT-v0.99.35-POST-CLOSURE-IN-DEPTH.md`:

- version surfaces are synchronized to canonical `0.99.35`;
- `lint-version`, `ci-local --quick`, `test-bump-version`, and
  `test-ci-local` pass;
- README Status/metrics are synchronized;
- `scripts/abstraction-audit.rkt` now matches its documented default and
  excludes `tests/`, `compiled/`, `.git/`, `.planning/`, and `.pi/` trees;
- `tests/test-abstraction-audit.rkt` has a regression for the default scan
  scope;
- this report no longer claims a broad runner FAIL is a PASS.

Final post-remediation broad evidence is recorded below, in the #8409
remediation summary, and in
`.planning/AUDIT-v0.99.35-POST-CLOSURE-IN-DEPTH.md`.

---

## Milestone Summary

v0.99.35 applied the abstraction patterns identified in the W0 inventory
across 7 production modules, creating 7 new pure-helper modules and 1 new
tooling script. Every extraction followed the same disciplined pattern:
identify pure functions, extract to a new module, update the original to import
from helpers, write comprehensive unit tests, and verify with focused +
unit-fast + smoke gates.

### Wave Completion Status

| Wave | Issue | Title | Status | PR |
|------|-------|-------|--------|-----|
| W0 | #8389 | Abstraction baseline inventory | Done | #8399 |
| W1 | #8390 | Abstraction fitness tooling | Done | #8400 |
| W2 | #8391 | spawn-subagent pure helper extraction | Done | #8401 |
| W3 | #8392 | subprocess result-boundary consolidation | Done | #8402 |
| W4 | #8393 | event serialization boundary extraction | Done | #8403 |
| W5 | #8394 | GSD state-machine pure transition extraction | Done | #8404 |
| W6 | #8395 | state-aware builder pure helper extraction | Done | #8405 |
| W7 | #8396 | TUI core-handlers pure helper extraction | Done | #8406 |
| W8 | #8397 | Anthropic provider narrow pure helper extraction | Done | #8407 |
| W9 | #8398 | Final gates, docs, and in-depth audit | Done, corrected by #8409 | #8408 |
| Hotfix | #8409 | Remediate post-closure audit blockers | Done | #8410 |

---

## Extraction Outcomes

### Modules Created

| Module | Lines | Functions Extracted | Wave |
|--------|-------|---------------------|------|
| `scripts/abstraction-audit.rkt` | ~400 | Advisory scanner tool | W1/#8409 |
| `tools/builtins/spawn-subagent-helpers.rkt` | 139 | 5 pure functions | W2 |
| `sandbox/subprocess-helpers.rkt` | 152 | Struct + constructors + classifiers | W3 |
| `agent/event-json-helpers.rkt` | 303 | 16 pure serializers/deserializers | W4 |
| `extensions/gsd/transition-logic.rkt` | 209 | Pure state machine logic | W5 |
| `runtime/context-assembly/state-aware-helpers.rkt` | 182 | 6 pure functions | W6 |
| `tui/state-events/handler-helpers.rkt` | 78 | 5 pure functions | W7 |
| `llm/anthropic-helpers.rkt` | 64 | 3 pure functions | W8 |

### Modules Reduced

| Module | Before | After | Delta | Wave |
|--------|--------|-------|-------|------|
| `tools/builtins/spawn-subagent.rkt` | 809 | 741 | -68 | W2 |
| `sandbox/subprocess.rkt` | 370 | 334 | -36 | W3 |
| `agent/event-json.rkt` | 359 | 121 | -238 | W4 |
| `extensions/gsd/state-machine.rkt` | 586 | 429 | -157 | W5 |
| `runtime/context-assembly/state-aware-builder.rkt` | 584 | 441 | -143 | W6 |
| `tui/state-events/core-handlers.rkt` | 549 | 511 | -38 | W7 |
| `llm/anthropic.rkt` | 662 | 639 | -23 | W8 |

### Tests Added / Updated

| Test File | Tests | Wave |
|-----------|-------|------|
| `test-abstraction-audit.rkt` | 10 | W1/#8409 |
| `test-spawn-subagent-helpers.rkt` | 24 | W2 |
| `test-subprocess-helpers.rkt` | 32 | W3 |
| `test-event-json-helpers.rkt` | 23 | W4 |
| `test-transition-logic.rkt` | 46 | W5 |
| `test-state-aware-helpers.rkt` | 31 | W6 |
| `test-handler-helpers.rkt` | 20 | W7 |
| `test-anthropic-helpers.rkt` | 19 | W8 |

**Total helper/tool tests after #8409:** 205.

---

## Corrected Gate Results

### Original W9 Broad Gate Correction

The original W9 broad run reported failed files/timeouts and therefore should
not have been called PASS. The corrected interpretation is:

```text
Original W9 broad runner verdict: FAIL / INCOMPLETE for release approval
Reason: failed files and timeouts were present; individual triage cannot turn a
runner FAIL into a PASS.
```

This correction preserves the v0.99.29–v0.99.34 gate-truth policy: known or
environment-specific failures may be explained, but they must remain visible and
must not be mislabeled as a passing gate.

### Post-Closure Audit Blockers Remediated by #8409

Focused remediation checks before final broad rerun:

```text
racket scripts/lint-version.rkt                     PASS, Errors: 0
racket scripts/ci-local.rkt --quick                 PASS, 5/5 checks
raco test tests/test-bump-version.rkt               PASS, 3/3 tests
raco test tests/test-ci-local.rkt                   PASS, 3/3 tests
raco test tests/test-abstraction-audit.rkt          PASS, 10/10 tests
racket scripts/run-tests.rkt --profile local \
  tests/test-bench-streaming-render.rkt \
  tests/test-bump-version.rkt \
  tests/test-ci-local.rkt                           PASS, 3/3 files, 9/9 tests
```

Final #8409 committed-content gates:

```text
raco make main.rkt                                      PASS
racket scripts/run-tests.rkt --suite fast --profile local
  Files: 956 total, 956 passed, 0 failed, 0 timeouts
  Tests: 13078 total, 13078 passed, 0 failed
  Verdict: PASS

timeout 7200 racket scripts/run-tests.rkt --suite broad --profile local \
  --ledger tests/test-suite-ledger.json
  Files: 1048 total, 1048 passed, 0 failed, 0 timeouts
  Tests: 14007 total, 14007 passed, 0 failed
  Known failures: 0
  New failures: 0
  Unclassified failures: 0
  Release-blocking known failures: 0
  Verdict: PASS
```

---

## API Stability

All 7 modified production modules retain their full public `provide` surface.
Extracted functions are re-exported from the original module where they were
previously available, preserving downstream imports.

---

## Bug Fixes During Extraction

- **W5 (`find-transition-path`):** Fixed path generation bug where
  `(list from '())` produced incorrect path structure. Changed to
  `(cons from '())`.
- **W1 (regex bugs):** Fixed `#rx` patterns where `\\|` created literal pipe
  instead of alternation. Changed to `#px` for Perl-style alternation.
- **#8409 (abstraction-audit scope):** Fixed scanner default so production
  module reports exclude tests and generated/hidden planning trees.
- **#8409 (version surfaces):** Synchronized canonical version surfaces and
  README Status/metrics.

---

## Deferred Debt

The following items were explicitly deferred and remain acceptable audit-only or
document-only items:

1. `cli/args.rkt` (RED) — high-risk CLI entry point.
2. `wiring/run-modes.rkt` (RED) — high-risk wiring/initialization module.
3. `scripts/run-tests.rkt` (YELLOW) — stable test runner, documented only.

These are tracked in `.planning/ABSTRACTION-INVENTORY-v0.99.35.md` for future
reference and are not release blockers.

---

## Conclusion

v0.99.35 successfully demonstrated the abstraction pattern across modules with
varying risk profiles. The post-closure audit correctly identified that the
implementation report and release metadata were not yet gate-truth compliant.
Issue #8409 remediates those findings and restores the requirement that release
gates be reported exactly as the runner reports them.

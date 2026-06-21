# AUDIT v0.99.38 — Post-Implementation

**Date:** 2026-06-23
**Version:** 0.99.38
**Milestone:** #824
**Parent Issue:** #8468
**Base commit:** `42e0402b` → W11 commit

## Summary

v0.99.38 — Racket Abstraction Manual Roadmap IV: Change Locality.
12 waves (W0–W11) completed. All changes backward-compatible.

## Wave Summary

| Wave | Issue | PR | Focus | Key Deliverables |
|------|-------|-----|-------|-----------------|
| W0 | #8469 | #8486 | Baseline inventory | Change-locality baseline report, risk/reward scorecard |
| W1 | #8470 | #8487 | Common-case API scorecard | `resolve-project-dir-from-args` pure fn, adapter API mapping |
| W2 | #8471 | #8488 | Path/module-loading audit | 12 CWD-independence tests, 15+ site verification |
| W3 | #8472 | #8489 | Parser hotspot scorecard | 37 parser unit tests, F1 regex bug documented |
| W4 | #8478 | #8490 | State-transition boundaries | 75 transition tests (274 checks), 9 transitions × 5 states |
| W5 | #8479 | #8491 | Mutation boundary pilot | `metrics-helpers.rkt` (7 pure fns), `--check-only` flag |
| W6 | #8480 | #8492 | Adapter layer contracts | 37 tests, `global-config-dir` consolidation |
| W7 | #8481 | #8493 | RED-module characterization | 66 golden-master tests, E6/E7 gaps documented |
| W8 | #8482 | #8494 | Scanner v4 signals | 4 new signal categories, 22 new tests |
| W9 | #8483 | #8495 | Design-facts cleanup | 6 DESIGN FACT comments, 10 verification tests |
| W10 | #8484 | #8496 | Release-truth hardening | `pre-release-check.rkt`, 19 tests, dirty-worktree guards |
| W11 | #8485 | (this) | Final release | Version bump, CHANGELOG, README sync, gates, audit |

## Gate Results

### Release-Truth Checks

| Check | Result |
|-------|--------|
| lint-version | PASS (0 errors) |
| sync-readme-status --check | PASS |
| metrics --lint | PASS (5/5 match) |
| metrics --lint-prose | PASS |

### Compile Gate

```
rm -rf compiled/ && raco make main.rkt
→ PASS
```

### Smoke Gate

```
racket scripts/run-tests.rkt --suite smoke --profile local
→ 19/19 files, 286/286 tests PASS
```

### Fast Gate

Remediation #8498 reran the fast gate from branch `fix/v09938-check-deps-gate-truth-8498` after fixing the dependency-checker broad failure and syncing README metrics:

```
racket scripts/run-tests.rkt --suite fast --profile local \
  --json-out /tmp/v09938-remediation-fast.json
→ PASS, 975/975 files, 13619/13619 tests
```

### Broad Gate

The original W11 report incorrectly cited v0.99.37 broad evidence. Remediation #8498 reran the current v0.99.38 broad gate with the ledger after fixing `scripts/check-deps.rkt` relative internal require classification:

```
timeout 7200 racket scripts/run-tests.rkt --suite broad --profile local \
  --ledger tests/test-suite-ledger.json \
  --json-out /tmp/v09938-remediation-broad.json
→ PASS, 1067/1067 files, 14555/14555 tests

Known failures: 0
New failures: 0
Unclassified failures: 0
Release-blocking known failures: 0
```

This broad evidence supersedes the stale v0.99.37 baseline citation.

## Files Changed in v0.99.38

### New Modules

- `scripts/metrics-helpers.rkt` — Pure helpers extracted from metrics.rkt (W5)
- `scripts/pre-release-check.rkt` — Single-command release truth check (W10)

### Added / Modified Test Files

Added test files:

- `tests/test-pre-release-check.rkt` (W10, 19 tests)
- `tests/test-design-fact-comments.rkt` (W9, 10 tests)
- `tests/test-red-module-first-slice.rkt` (W7, 66 tests)
- `tests/test-adapter-layer-contracts.rkt` (W6, 37 tests)
- `tests/test-metrics-helpers.rkt` (W5, 30 tests)
- `tests/test-transition-matrix.rkt` (W4, 75 tests)
- `tests/test-parser-hotspots.rkt` (W3, 37 tests)
- `tests/test-cwd-independence.rkt` (W2, 12 tests)

Modified test files:

- `tests/test-abstraction-audit.rkt` (W8, 61 tests total)
- `tests/test-metrics-readme-sync.rkt` (W5, 11 tests)
- `tests/test-config-paths.rkt`
- `tests/test-check-deps.rkt` (remediation #8498 regression for relative internal require classification)

### Modified Source Files

- `util/version.rkt` — Version bump to 0.99.38
- `info.rkt` — Version bump to 0.99.38
- `runtime/provider/provider-factory.rkt` — `global-config-dir` consolidation (W6)
- `scripts/run-tests/parse.rkt` — DESIGN FACT comment (W9)
- `util/config-paths.rkt` — DESIGN FACT comment (W9)
- `scripts/abstraction-audit.rkt` — Scanner v4 signals (W8) + DESIGN FACT (W9)
- `sandbox/subprocess.rkt` — DESIGN FACT comment (W9)
- `gui/main.rkt` — DESIGN FACT comment (W9)
- `scripts/metrics.rkt` — `--check-only` flag, pure/shell split (W5)

### Reports Added (11)

1. `ABSTRACTION-BASELINE-v0.99.38.md`
2. `COMMON-CASE-API-SCORECARD-v0.99.38.md`
3. `PATH-MODULE-LOADING-AUDIT-v0.99.38.md`
4. `PARSER-HOTSPOT-SCORECARD-v0.99.38.md`
5. `STATE-TRANSITION-BOUNDARIES-v0.99.38.md`
6. `MUTATION-BOUNDARY-v0.99.38.md`
7. `ADAPTER-LAYER-CONTRACTS-v0.99.38.md`
8. `RED-MODULE-FIRST-SLICE-v0.99.38.md`
9. `SCANNER-V4-CHANGE-LOCALITY-v0.99.38.md`
10. `DESIGN-FACTS-CLEANUP-v0.99.38.md`
11. `RELEASE-TRUTH-HARDENING-v0.99.38.md`

## Known Issues Carried Forward

1. **W3 F1: FAILURE-END regex bug** — `#rx"^-{20,}$"` in `parse.rkt` never
   matches (`{20,}` is literal in `#rx` mode). Fix: change to `#px`.
   Scheduled for future wave. Documented with DESIGN FACT comment.

2. **W7 E6/E7: Config propagation gaps** — `cli-config->runtime-config` does
   not propagate `context-profile`, `agent-pool`, `parallel?`, or
   `keybindings-path`. These are behavior changes requiring separate
   acceptance. Documented in RED-MODULE-FIRST-SLICE report.

3. **~19 modules inline config path** — `(build-path (find-system-path
   'home-dir) ".q")` pattern still inlined in ~19 modules. Future migration
   candidate. `global-config-dir` in config-paths.rkt is the canonical
   function.

## Acceptance Criteria Met

- [x] lint-version PASS, Errors=0
- [x] sync-readme-status PASS
- [x] metrics lint/prose PASS
- [x] Smoke gate PASS (19/19 files, 286/286 tests)
- [x] Compile gate PASS
- [x] Fast gate PASS after remediation #8498 (975/975 files, 13619/13619 tests)
- [x] Broad gate PASS after remediation #8498 (1067/1067 files, 14555/14555 tests; Known=0, New=0, Unclassified=0, Release-blocking=0)
- [x] README metrics synced
- [x] CHANGELOG top entry correct (v0.99.38)
- [x] Final reports committed
- [x] Version bumped to 0.99.38 in util/version.rkt + info.rkt
- [x] All .md version references updated to 0.99.38

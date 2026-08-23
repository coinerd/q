# STATE v1.00.12 — SSE Stall Detection Bounds

**Milestone:** v1.00.12 (GitHub milestone #885)
**Plan:** `PLAN-v1.00.12-SSE-STALL-DETECTION-BOUNDS-REVISED.md`
**Tracker:** #9427
**Status:** COMPLETE — implemented, tested, released as v1.00.12 (2026-08-22)
**Depends on:** v1.00.08 (provider networking hardening)
**Root cause ref:** `ANALYSIS-v1.00.08-deepseek-10min-sse-stall.md`

## Wave Status

| Wave | Title | Defects | Status | PR | Merge |
|------|-------|---------|--------|-----|-------|
| W0 | Red-state phase-timeout bounds tests | SS-4 | DONE | #9446 | 0c3639cc |
| W1 | Restore SSE phase bounds via shared resolver | SS-1..SS-3 | DONE | #9447 | 716d8628 |
| W2 | Timeout message suffix + docs | SS-5 | DONE | #9448 | 7c6775e8 |
| W3 | Release (bump, notes, tag, publish) | — | DONE | #9449 (+ fixes #9450, #9451) | 338ce387 / a05df8ee / 55fdcaa0 |

## Defect Status

| ID | Defect | Wave | Test | Implemented | Verified |
|----|--------|------|------|-------------|----------|
| SS-1 | Initial phase unboundedly wide (held requests hang to full request timeout) | W1 | ✅ test-sse-phase-timeout-bounds.rkt | ✅ `(min req 120)` | ✅ |
| SS-2 | Thinking window uncapped (kimi/glm preservation vs deepseek clamp) | W1 | ✅ matrix + sweep invariants | ✅ `(min req (min (or ov 120) 300))` | ✅ |
| SS-3 | Content gap widened by sse-read override | W1 | ✅ matrix (`content = 60`) | ✅ fixed `http-stream-timeout-default` | ✅ |
| SS-4 | No regression guard for the matrix | W0→W1 | ✅ 13-case matrix file | ✅ promoted live in W1 | ✅ |
| SS-5 | Timeout messages lack triage context | W2 | ✅ suffix regex tests (migrated from reproducer) | ✅ `[phase=… data-received=… chars=…]` on all three raise sites | ✅ |
| SS-6 | Adapter parity (anthropic/azure/gemini not wired) | — | n/a | DEFERRED to v1.00.13 (documented in provider-retry.md handoff note) | n/a |

## Release

- Tag: `v1.00.12` (annotated), pushed 2026-08-22.
- GitHub Release "q v1.00.12" published 2026-08-22T15:19:06Z with assets
  `q-1.00.12.tar.gz` + `release-manifest.json`; release workflow green, no
  deployment approval required.
- CHANGELOG entry with standalone `Released 2026-08-22.` marker.
- `lint-release-readiness`: 5/5 PASS on main.

## Incident Notes

- **CHANGELOG gate-restore loss (W2+W3):** the test runner's gate-restore
  restores uncommitted tracked-file edits from HEAD when gates run; both W2's
  and W3's CHANGELOG edits were silently reverted before staging because gates
  ran between edit and commit. Rule going forward: **commit tracked-file edits
  BEFORE running gates**, re-sync metrics after commit.
- **Released-marker format:** `lint-release-readiness` requires a standalone
  `Released YYYY-MM-DD.` line inside the entry block; inline blockquote form
  does not match (#9450 → #9451 fix).
- **version-expectations (BUG-0009):** hard-coded version literals in tests/
  fail lint; use prose without version tokens or derive from `q-version`.

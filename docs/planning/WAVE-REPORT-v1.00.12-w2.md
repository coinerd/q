# WAVE-REPORT v1.00.12 W2 — Timeout message suffix + docs

**Issue:** #9430 (sub-issues #9439–#9440) · **Branch:** `feature/v10012-w2` · **PR:** #9448 · **Merge:** `7c6775e8` · **Date:** 2026-08-22

## Delivered

1. `llm/stream.rkt` (SS-5): all three `exn:fail:network:timeout:stream`
   raise sites in `stream-sse-events` (max-total-duration, consecutive-empty,
   per-chunk read) now end messages with
   `[phase=<p> data-received=(yes|no) chars=<n>]` via a local `timeout-msg`
   helper; struct fields remain the machine source of truth.
2. `tests/test-sse-phase-timeout-bounds.rkt`: SS-5 message-suffix checks
   migrated from the reproducer (initial-phase hold, content-phase stall);
   13 tests green.
3. Deleted `tests/reproducers/reproduce-sse-timeout-message-suffix.rkt`.
4. `docs/provider-retry.md`: "Streaming Timeout Matrix" section — phase
   table, TTFB held-request behavior, suffix contract, and Architectural
   handoff note (v1.00.13 Request Lifecycle Policy Unification, SS-6).
5. `CHANGELOG.md`: v1.00.12 entry (unreleased — date marker lands in W3).

## Verification

| Gate | Result |
|---|---|
| Matrix test | 13 PASS |
| Focused suites (10 files) | PASS |
| Arch suite | 30 PASS |
| Fast gate | 1110 files / 16215 tests PASS |
| Version lint | PASSED after historical-guard rewording |
| CI PR #9448 | all green, squash-merged CLEAN |

## Notes

- lint-version flags bare version tokens in non-exempt .md files;
  `docs/provider-retry.md` references were rewritten to match
  `historical-line?` patterns ("since vX", " in vX", "**vX**" bold-start,
  fenced code blocks for planning filenames). No lint-code changes.
- Metrics sync produced no extra diff this wave (folded into docs commit).
- Old-message fixtures (`test-gsd-d8-provider-retry-scaling.rkt`,
  `test-streaming-text-preservation.rkt`) unaffected as predicted.

## Open items for W3 (#9431) — release wave

Version bump 1.00.08→1.00.12 (`util/version.rkt` +
`scripts/sync-version.rkt --all --write`), CHANGELOG
`Released YYYY-MM-DD.` marker, gates with recorded evidence (fast/tui/arch/
workflows/security/lint-all/lint-release-readiness), release PR → merge →
annotated tag `v1.00.12` → release workflow → approveDeployments if gated →
verify Release assets → STATE/VALIDATION docs + mirror to `q/docs/planning/`
via docs PR → close stale PR #9377 → close milestone #885.

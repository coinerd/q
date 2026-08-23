# WAVE-REPORT v1.00.12 W1 — Restore phase bounds via shared resolver

**Issue:** #9429 (sub-issues #9435–#9438) · **Branch:** `feature/v10012-w1` · **PR:** #9447 · **Merge:** `716d8628` · **Date:** 2026-08-22

## Delivered

1. `llm/stream.rkt`: pure resolver
   `(sse-phase-timeout-secs #:request-timeout req #:sse-read-override [ov #f])`
   → `(values initial thinking content)` with
   initial = min(req,120), thinking = min(req, min(or ov 120, 300)),
   content = http-stream-timeout-default (60); constant
   `max-thinking-gap-secs = 300`; both exported via contract-out.
2. `llm/openai-compatible.rkt`: three `stream-sse-events` timeouts now come
   from the resolver; raw `sse-read` feeds only the thinking window; stale
   comment block rewritten as matrix rationale citing the v1.00.08 analysis;
   stray `HARD DEBUG` comment removed (NP-8).
3. `tests/test-sse-phase-timeout-bounds.rkt`: promoted to live suite
   (`@not-test` removed) — 11 tests green.

## Verification

| Gate | Result |
|---|---|
| Matrix test | 11 PASS (W0 assertion-red → green) |
| Focused suites | stream / stream-heartbeat-metadata / openai-compatible / model-timeouts / auto-retry / provider-retry-telemetry / provider-retry-ceiling-config all PASS |
| Fast gate | 1110 files / 16215 tests PASS |
| Arch suite | 30 PASS |
| CI PR #9447 | all checks green, squash-merged CLEAN |

## Notes

- Stale-bytecode gotcha: after editing stream.rkt, dependent test modules
  (gemini.rkt, agent/stream-reducer.rkt) needed explicit `raco make` — first
  run of test-stream/test-provider-retry-ceiling-config failed with contract /
  linklet errors that vanished after rebuild. Not regressions.
- SS-6 deferral honored: anthropic/azure/gemini untouched.

## Open items for W2 (#9430)

Message suffix `[phase= data-received= chars=]` on the three raise sites,
move reproducer checks into the matrix file + delete reproducer,
docs/provider-retry.md Streaming Timeout Matrix section, CHANGELOG entry.

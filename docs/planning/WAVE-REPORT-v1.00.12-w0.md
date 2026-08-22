# WAVE-REPORT v1.00.12 W0 — Red-state regression tests (SS-4)

**Issue:** #9428 (sub-issues #9432, #9433, #9434) · **Branch:** `feature/v10012-w0` · **Date:** 2026-08-22

## Delivered

1. `tests/test-sse-phase-timeout-bounds.rkt` (new) — the SS-4 regression guard:
   full phase-timeout matrix per plan §5 W0 task 1 (deepseek 600 s override →
   `(values 120 300 60)`, no override → `(120 120 60)`, kimi/GLM 300 s ceiling
   preserved, tighter overrides honored for thinking only, small request budgets
   clamp, plus invariant sweeps: initial ≤120 / content ≤60 / thinking ≤ min(req,300),
   all positive). Header carries the `@not-test true` marker — **committed red**
   (module requires the W1 resolver exports that do not exist yet) while staying
   out of every CI suite.
2. `tests/reproducers/reproduce-sse-timeout-message-suffix.rkt` (new) — SS-5
   assertion-red reproducer: initial-phase and content-phase stalls raise
   `exn:fail:network:timeout:stream` whose message lacks the
   `[phase=… data-received=… chars=…]` suffix. Tagged `@speed slow`
   (excluded from fast; will pass once W2 lands the suffix and moves these checks
   into the main test file).
3. Red evidence recorded in `.planning/v1.00.12-w0/red-evidence.txt`:
   - matrix file: exit 1, assertion-red via guarded dynamic-require ("RED(W0): ... not exported by llm/stream.rkt yet") — compiles clean so pre-commit lint and CI pass
   - reproducer: exit 1, 2 assertion failures ("no suffix on initial/content-phase message")
   - both failure modes documented per PA-4.

## Verification

| Check | Result |
|---|---|
| Matrix file run | COMPILE-RED (exit 1) as expected |
| Message-suffix reproducer run | ASSERTION-RED (exit 1, 2 failures) as expected |
| `tests/test-model-timeouts.rkt` | green (exit 0) |
| `tests/test-openai-compatible.rkt` | green (exit 0) |
| Suite exclusion (empirical) | matrix not in `fast` (@not-test); reproducer not in `fast` (`slow-file? #t`) |
| `raco fmt -i` | applied |

## Notes / deviations

- **Two-artifact staging** instead of a single file: rackunit has no xfail, so a
  combined file would keep CI red after W1 (suffix lands in W2). Final state after
  W2 satisfies the plan's "one test file" outcome; staging is documented here.
- **Parser gotcha discovered (documented for future waves):** the test-metadata
  annotation parser matches `@tag …` anywhere in the first 30 lines and takes the
  *last* match — prose mentioning e.g. "`@speed slow)`" in comments poisons the
  parsed value (we hit symbol `|slow)|`). Keep annotation tokens out of prose.
- No production diff in this wave PR (per W0 exit criterion).

## SS-6 note

N/A this wave. Deferral is recorded in W1's report (#9429).

# Local TDD Latency Measurement — v1.00.28 (Wave W6)

Date: 2026-09-09 · Branch: `campaign/v1.00.28-w6` · Runner: v1.00.27 binary, manifest v1 (loaded)
Raw retained samples: `artifacts/test-runtime/v1.00.28-w6/{l0,l1}-samples.json` (checksummed below)

## Method

- **L0** (12 retained samples): positional single-file runs, `racket scripts/run-tests.rkt <file>`,
  on the unit files a developer names most often during red-green iterations.
- **L1** (10 retained samples): the documented impact command
  `racket scripts/run-tests.rkt --changed-base origin/main --changed-head HEAD`, executing the
  selected set. All 10 runs selected exactly 1 file (`tests/test-run-tests-script.rkt`) via the
  impact manifest (`reason-code: changed-test-file`, `mapping-source: self`,
  `escalated_broad=false`, fail-open fallback **not** triggered).
- **L2** (10 retained samples): transitive walk on a source-touching diff
  (`--changed-base 9f529830^ --changed-head HEAD --impact-dry-run --explain`), selection-only.
  Walk selected **31 tests**; 8 `config-change` files (workflow/baseline artifacts,
  `scripts/run-tests/*` helpers) triggered **fail-open escalation → fallback: fast workflows**.
- Timing = wall-clock around the full `racket` process (includes VM startup).
- Percentiles: p50 = median; p90 = nearest-rank ⌈0.9·n⌉; max.

## Results vs Class E targets

| Level | n  | p50 (s) | p90 (s) | max (s) | Class E target | Verdict |
|-------|----|---------|---------|---------|----------------|---------|
| L0    | 12 | 1.375   | **2.041** | 4.170 | p90 ≤ 5 s   | ✅ CONFIRMED (59% headroom) |
| L1    | 10 | 48.276  | **52.711** | 57.182 | p90 ≤ 30 s | ⚠️ **MISS — attributed** |
| L2    | 10 | 7.957   | **7.986** | 8.046  | p90 ≤ 120 s | ✅ CONFIRMED for selection-only walk (93% headroom) |

## L1 miss attribution (no target edit in this milestone)

The L1 p90 miss is **not** selection overhead: the same selection computed by
`--impact-dry-run` completes in ≈2 s (and the L2 graph walk in ≈8 s including manifest load).
The cost is the **runtime of the selected file itself**: `tests/test-run-tests-script.rkt`
is tagged `@speed slow` and shells out to the runner script ~12× per run (29–57 s observed,
cold/warm variance). A one-file diff that maps to this file therefore costs ≈50 s end-to-end
regardless of how fast selection is.

Attribution and implied follow-ups (recorded for a later milestone, not acted on here):

1. The runner-script test file dominates single-file diffs touching `scripts/run-tests*`.
   Splitting its suite or raising its `@speed` tier would bring typical L1 runs under 30 s.
2. Selection machinery (manifest load + walk + plan) is 2–8 s — well inside budget.
3. Fail-open escalation fired only on config/workflow artifacts, not on source changes.

## L2 execution-cost note

Class E's L2 target is met for the **selection** phase (p90 ≈ 8 s ≪ 120 s). Execution of the
31-test transitive set is deferred by the escalation policy to L3/broad windows; its cost is
bounded by suite composition, not by selection, and is not re-measured here.

## Developer workflow consequence

During normal red-green-refactor iterations, the recommended loop is L0
(position single file, p50 ≈ 1.4 s) → L1 (impact on branch diff before push). Neither step
requires a broad `fast`-suite run; see `docs/testing.md` and
`docs/TDD-TEST-STRATEGY-PLAN.md` §L0/L1/L2/L3.

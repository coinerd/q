# PR latency guard — v1.00.30 W1

- gate: pr-latency-guard/1
- decision: fail
- input schema: q.pr-latency-guard.input/1

## Quantile budget (candidate - reference <= min(10%, 60 s); equality passes)

| quantity | reference | candidate | delta | allowed |
|---|---|---|---|---|
| p50 (ms) | 2806000 | 3306000 | 500000 | 60000 |
| p95 (ms) | 2860600 | 3306000 | 445400 | 60000 |

- reference eligible first attempts: 4 (distinct heads: 4)
- candidate eligible first attempts: 1 (distinct heads: 1)
- guard's own samples excluded from latency: 0
- censored candidate runs recorded separately: 0

## Checks

- schema: PASS
- attestation: PASS
- record-shape: PASS
- provenance: PASS
- freshness: FAIL
- uniqueness: FAIL
- required-jobs: PASS
- control-strata: PASS
- quantile-budget: FAIL
- quantile-evaluated: PASS
- no-self-recursion: PASS
- final-head-eligible: PASS

## Reasons

1. freshness check failed: reference must be frozen (frozen=true, frozen-at fresh and before the earliest candidate start, every reference record completing at or before the freeze) and every candidate record must be attested, internally ordered, not in the future and within freshness-window-seconds of evaluated-at
2. candidate has 1 distinct eligible first-attempt heads; at least 3 are required
3. candidate p50 3306000 ms exceeds reference p50 2806000 ms by 500000 ms; the budget is min(10%, 60000 ms) = 60000 ms (equality passes)

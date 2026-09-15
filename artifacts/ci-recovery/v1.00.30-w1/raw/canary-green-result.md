# PR latency guard — v1.00.30 W1

- gate: pr-latency-guard/1
- decision: fail
- input schema: q.pr-latency-guard.input/1

## Quantile budget (candidate - reference <= min(10%, 60 s); equality passes)

| quantity | reference | candidate | delta | allowed |
|---|---|---|---|---|
| p50 (ms) | 2806000 | 2830000 | 24000 | 60000 |
| p95 (ms) | 2860600 | 2839000 | -21600 | 60000 |

- reference eligible first attempts: 4 (distinct heads: 4)
- candidate eligible first attempts: 3 (distinct heads: 3)
- guard's own samples excluded from latency: 0
- censored candidate runs recorded separately: 0

## Checks

- schema: PASS
- attestation: PASS
- record-shape: PASS
- provenance: PASS
- freshness: FAIL
- uniqueness: PASS
- required-jobs: PASS
- control-strata: PASS
- quantile-budget: PASS
- quantile-evaluated: PASS
- no-self-recursion: PASS
- final-head-eligible: PASS

## Reasons

1. freshness check failed: reference must be frozen (frozen=true, frozen-at before the earliest candidate start) and every record must be attested, internally ordered, not in the future and within freshness-window-seconds of evaluated-at

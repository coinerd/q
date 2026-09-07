# Cohort Report: v1.00.26

| Field | Value |
|---|---|
| Milestone | v1.00.26 |
| Schema version | 1 |
| Report version | w4-cohort-v1 |
| Cohort size | 20 |
| Expected size | 20 |
| Exclusions | 0 |
| Validation | PASS |

## Statistics (linear-interpolation percentile estimator)

| Statistic | Value |
|---|---|
| Sample count | 0 |
| p50 (seconds) | #f |
| p95 (seconds) | #f |
| min (seconds) | #f |
| max (seconds) | #f |
| mean (seconds) | #f |

## Counts

| Count | Value |
|---|---|
| Total pass | 0 |
| Total fail | 0 |
| Total timeout | 0 |
| Total skip | 0 |
| Total flakes | 0 |
| Parallel-only failures | 0 |
| Zero-test SHAs | 0 |
| Total file count | 0 |
| Total test count | 0 |

## Reliability evidence

| Metric | Value |
|---|---|
| Total attempts | 23 |
| Successes | 20 |
| Failures | 2 |
| Cancelled | 1 |
| Reruns | 0 |

## Inventory digest

Aggregate: `0ae83bc36a59a7c9ecd1b71c5ed0448fed0ed600|0d991f86a3a3ad89a249aafcb35e82b1b78feb5a|1a46ae9ebe14a9e69402618f04c435db889f616d|3825bb46c1082697767a1bc9e5d41d62e9460c16|5c4bcd6541c9346dffca0fc9c815253828d10c17|6070c9a46e2c319adea07d4d7cf92a65238ec388|693f9f5b5810c91b22154892b0ecf2197b1138a3|6d92c40c02d69f7207611abbe4bee9cda703f003|75a266c9ef1949a855c95caf61d02b07946c7135|8c6bf76850b21230ce973e7e485548f78bcc3cb0|9d1b7f740e3821a75afdaf577b71a09e3ae5c229|9d1b7f740e3821a75afdaf577b71a09e3ae5c229|a39fc1cb8027eb9da855b4ad12883ca287d35a1c|bc5d645756ddfc9dadb6b60955090c72925ee31a|bdf880400b1e0b27d08f722314f80b4c3bb07e38|d5fdc10a0fb5393c1721f39f81197beebe923c33|eaf81d6dd66535d5b789fbf0a4eab086c11da002|f1dc89b3d399c321c4a49278d18fd7194adfa5af|f28d5c8eb124786c44d929063a5047a3ea111153|fc8ae430bdf7da68ef883aade4ae728c5035a382`

## Queue telemetry

| Metric | Value |
|---|---|
| Total wait (seconds) | 0 |
| Max queue depth | 0 |

## Runner-minute cost

| Metric | Value |
|---|---|
| Total runner-minutes | 0 |

## Exclusions

(none)

## End-to-end PR elapsed cohort (C2)

Mode producing the numbers in this report: **pr-elapsed** (mergeable-PR wall time from
first check start to last required check completion, not the queue wait alone;
no shadow duplication).

End-to-end PR elapsed target on the integrated topology: p50 ≤ 588 s and p95 ≤ 735 s (roadmap v1.00.26 §7, W6).  Targets are never revised inside this wave or milestone.

| Metric | Value |
|---|---|
| p50 (seconds) | 935.0 |
| p95 (seconds) | 957.5 |
| p50 target (seconds) | ≤ 588.0 |
| p95 target (seconds) | ≤ 735.0 |
| Unique PR head SHAs | 20 of 20 |
| Verdict | **target unachieved** |

Next lever: Next lever (separate reviewed decision; a timing miss alone implies no queue rollback): move the slow required gates off the PR critical path by splitting the aggregate workflows out of the mergeable required set and pre-warming the prepared environment cache; re-run this cohort on 20 new PR head SHAs before the next promotion-adjacent decision.

## Manifest digest

```
check:ea3f8:11860
```
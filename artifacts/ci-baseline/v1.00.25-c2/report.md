# Cohort Report: v1.00.25

| Field | Value |
|---|---|
| Milestone | v1.00.25 |
| Schema version | 1 |
| Report version | w4-cohort-v1 |
| Cohort size | 20 |
| Expected size | 20 |
| Exclusions | 8 |
| Validation | PASS |

## Statistics (linear-interpolation percentile estimator)

| Statistic | Value |
|---|---|
| Sample count | 20 |
| p50 (seconds) | 253.5 |
| p95 (seconds) | 276.0 |
| min (seconds) | 214.0 |
| max (seconds) | 277.0 |
| mean (seconds) | 254.35 |

## Counts

| Count | Value |
|---|---|
| Total pass | 341854 |
| Total fail | 0 |
| Total timeout | 0 |
| Total skip | 0 |
| Total flakes | 0 |
| Parallel-only failures | 0 |
| Zero-test SHAs | 0 |
| Total file count | 28567 |
| Total test count | 341854 |

## Reliability evidence

| Metric | Value |
|---|---|
| Total attempts | 20 |
| Successes | 20 |
| Failures | 0 |
| Cancelled | 0 |
| Reruns | 0 |

## Inventory digest

Aggregate: `1804ceb43b338896d50e0b66ade760b5888fcc862866d5df570820eaa11de505|1804ceb43b338896d50e0b66ade760b5888fcc862866d5df570820eaa11de505|1804ceb43b338896d50e0b66ade760b5888fcc862866d5df570820eaa11de505|1804ceb43b338896d50e0b66ade760b5888fcc862866d5df570820eaa11de505|1804ceb43b338896d50e0b66ade760b5888fcc862866d5df570820eaa11de505|1804ceb43b338896d50e0b66ade760b5888fcc862866d5df570820eaa11de505|1804ceb43b338896d50e0b66ade760b5888fcc862866d5df570820eaa11de505|1804ceb43b338896d50e0b66ade760b5888fcc862866d5df570820eaa11de505|1804ceb43b338896d50e0b66ade760b5888fcc862866d5df570820eaa11de505|619fc0917ccd6ff4facce5fe756909dc69f5996b53e4ea8e2d52450ef75d630d|619fc0917ccd6ff4facce5fe756909dc69f5996b53e4ea8e2d52450ef75d630d|9866493d63473a27dfb67a75efe4f19471a511bab1b4ed9c2c40d55fe1e7c058|9866493d63473a27dfb67a75efe4f19471a511bab1b4ed9c2c40d55fe1e7c058|99162a4377f26ac674d089762469c27a90f0c59ed4bb6a6c06e83e1f09a45174|99162a4377f26ac674d089762469c27a90f0c59ed4bb6a6c06e83e1f09a45174|99162a4377f26ac674d089762469c27a90f0c59ed4bb6a6c06e83e1f09a45174|99162a4377f26ac674d089762469c27a90f0c59ed4bb6a6c06e83e1f09a45174|a3fb5cac05dba3c41648476e6ce78408ce95336cbc8c491aff3b21d671490c3d|da0a3a0e51495604849657aaef13fb6952bd6a7592b6b03c10a9dadaad214c98|da0a3a0e51495604849657aaef13fb6952bd6a7592b6b03c10a9dadaad214c98`

## Queue telemetry

| Metric | Value |
|---|---|
| Total wait (seconds) | 0 |
| Max queue depth | 0 |

## Runner-minute cost

| Metric | Value |
|---|---|
| Total runner-minutes | 84.76000000000002 |

## Exclusions

| SHA | Reason | Detail |
|---|---|---|
| 09c5f44efcf57aeb5ec78096582cf0b09229652f | lane-run-failed | required-lane run failure; timing artifacts never produced (run 33429419373 https://github.com/coinerd/q/actions/runs/33429419373) |
| f663f9708580e5bfe03f29b35a53ec82e1c5d359 | lane-run-failed | required-lane run failure; timing artifacts never produced (run 33796985419 https://github.com/coinerd/q/actions/runs/33796985419) |
| 06fa74ef7a7bfbf6122e605b91075b747a011f11 | lane-run-failed | required-lane run failure; timing artifacts never produced (run 33829086298 https://github.com/coinerd/q/actions/runs/33829086298) |
| 50f5b441dea42ccd25647ae1694b3b860b76ffa8 | lane-run-failed | required-lane run failure; timing artifacts never produced (run 33867844412 https://github.com/coinerd/q/actions/runs/33867844412) |
| 6402db03136f3ece727ef85ac85b3211223104c1 | lane-run-failed | required-lane run cancelled; timing artifacts never produced (run 33960281931 https://github.com/coinerd/q/actions/runs/33960281931) |
| fa6aba1fda0092aa6a6c01db98810d5884ff0a41 | lane-run-failed | required-lane run failure; timing artifacts never produced (run 33988303721 https://github.com/coinerd/q/actions/runs/33988303721) |
| d5ce5dfc9000624921042e1ed7933c93aaf04de4 | lane-run-failed | required-lane run failure; timing artifacts never produced (run 33989736572 https://github.com/coinerd/q/actions/runs/33989736572) |
| a37df4631cd6cefac85eef2c58a4bf0fd8d26072 | lane-run-failed | required-lane run failure; timing artifacts never produced (run 33996358770 https://github.com/coinerd/q/actions/runs/33996358770) |

## Post-promotion activation cohort (C2)

Mode producing the numbers in this report: **post-promotion** (promoted defaults,
required lane itself, no shadow duplication).

Out-of-sample fast execution target on promoted defaults: p50 ≤ 115 s and p95 ≤ 135 s (roadmap v1.00.25 §6, W6).  Targets are never revised inside this wave or milestone.

| Metric | Value |
|---|---|
| p50 (seconds) | 253.5 |
| p95 (seconds) | 276.0 |
| p50 target (seconds) | ≤ 115.0 |
| p95 target (seconds) | ≤ 135.0 |
| Timing samples | 20 |
| Verdict | **target unachieved** |

Next lever: Next lever (separate reviewed decision; a timing miss alone implies no queue rollback): reduce the fast-lane critical path by trimming batch shard fan-out and reusing the prepared environment cache; re-run this cohort on new SHAs before the next promotion decision.

## Manifest digest

```
check:16a107:18189
```
# Cohort Report: 892

| Field | Value |
|---|---|
| Milestone | 892 |
| Schema version | 2 |
| Report version | w4-cohort-v1 |
| Cohort size | 20 |
| Expected size | 20 |
| Exclusions | 12 |
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
| Total attempts | 21 |
| Successes | 21 |
| Failures | 0 |
| Cancelled | 0 |
| Reruns | 0 |

## Inventory digest

Aggregate: `1c553240893f5aca34db81cd807742033d16fd1f83660688d48d663d13c7585b|1c553240893f5aca34db81cd807742033d16fd1f83660688d48d663d13c7585b|619fc0917ccd6ff4facce5fe756909dc69f5996b53e4ea8e2d52450ef75d630d|619fc0917ccd6ff4facce5fe756909dc69f5996b53e4ea8e2d52450ef75d630d|78d9c06590439f723df59c779bbc7ee55a95175c229790c33654f5291a3d4527|78d9c06590439f723df59c779bbc7ee55a95175c229790c33654f5291a3d4527|78d9c06590439f723df59c779bbc7ee55a95175c229790c33654f5291a3d4527|99162a4377f26ac674d089762469c27a90f0c59ed4bb6a6c06e83e1f09a45174|99162a4377f26ac674d089762469c27a90f0c59ed4bb6a6c06e83e1f09a45174|c49ada4bb5ad44b9d3ff4f3a2d50631d609139e55bca586ea6ab3730fcec81d3|c49ada4bb5ad44b9d3ff4f3a2d50631d609139e55bca586ea6ab3730fcec81d3|c49ada4bb5ad44b9d3ff4f3a2d50631d609139e55bca586ea6ab3730fcec81d3|dcb56ed0cd6fe26c1e7a53223681cd679bcc007590dfcbe4029271cf55625313|dcb56ed0cd6fe26c1e7a53223681cd679bcc007590dfcbe4029271cf55625313|dcb56ed0cd6fe26c1e7a53223681cd679bcc007590dfcbe4029271cf55625313|dcb56ed0cd6fe26c1e7a53223681cd679bcc007590dfcbe4029271cf55625313|dcb56ed0cd6fe26c1e7a53223681cd679bcc007590dfcbe4029271cf55625313|dcb56ed0cd6fe26c1e7a53223681cd679bcc007590dfcbe4029271cf55625313|dcb56ed0cd6fe26c1e7a53223681cd679bcc007590dfcbe4029271cf55625313|dcb56ed0cd6fe26c1e7a53223681cd679bcc007590dfcbe4029271cf55625313`

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

| SHA | Reason | Detail |
|---|---|---|
| c55e18f495ee40092c2c3e2bdd152f16b4fffddf | lane-run-failed |  |
| efe6adb465563b25b635f146dfca66c9a3a83cc8 | lane-run-failed |  |
| 7219aee72a99e214e6908dd2b9f5a97e6cb0078f | lane-run-failed |  |
| 749ea54e2aa45d9b8f019e63c02e34d883b00f56 | lane-run-failed |  |
| a37df4631cd6cefac85eef2c58a4bf0fd8d26072 | lane-run-failed |  |
| d5ce5dfc9000624921042e1ed7933c93aaf04de4 | lane-run-failed |  |
| fa6aba1fda0092aa6a6c01db98810d5884ff0a41 | lane-run-failed |  |
| 6402db03136f3ece727ef85ac85b3211223104c1 | lane-run-failed |  |
| 50f5b441dea42ccd25647ae1694b3b860b76ffa8 | lane-run-failed |  |
| 06fa74ef7a7bfbf6122e605b91075b747a011f11 | lane-run-failed |  |
| f663f9708580e5bfe03f29b35a53ec82e1c5d359 | lane-run-failed |  |
| 09c5f44efcf57aeb5ec78096582cf0b09229652f | lane-run-failed |  |

## Final-claim cohort (C3, roadmap §8)

Mode producing the numbers in this report: **final-claim** (every §8 row, fixed thresholds,
coupled guard evidence; a row without its guards is "unverified", never "pass").

Final-claim verdict over every roadmap §8 row against the FIXED thresholds (fast p50 ≤ 115 s / p95 ≤ 135 s; PR CI p50 ≤ 588 s / p95 ≤ 735 s; security runner p50 ≤ 240 s; workflows runner p50 ≤ 220 s; verified prepared-env restores ≥ 95%) with the coupled guards.  A row without its guard evidence is "unverified", never "pass"; targets are never revised inside this wave or milestone.

| Row | Measure | Target | Observed | Samples | Guards | Verdict |
|---|---|---|---|---|---|---|
| fast-p50 | fast execution p50 on the required fast lane | <= 115.0 | 267.5 | 20 of 20 | provided | **target not achieved** |
| fast-p95 | fast execution p95 on the required fast lane | <= 135.0 | 267.5 | 20 of 20 | provided | **target not achieved** |
| pr-ci-p50 | end-to-end PR CI p50 (first required-check start to last required-check end) | <= 588.0 | 1043.5 | 20 of 20 | provided | **target not achieved** |
| pr-ci-p95 | end-to-end PR CI p95 (first required-check start to last required-check end) | <= 735.0 | 1043.5 | 20 of 20 | provided | **target not achieved** |
| security-runner-p50 | security suite runner p50 | <= 240.0 | 685.5 | 20 of 20 | provided | **target not achieved** |
| workflows-runner-p50 | workflows suite runner p50 | <= 220.0 | 695.5 | 20 of 20 | provided | **target not achieved** |
| prepared-env-verified-restores | prepared-environment verified-restore rate (percent) | >= 95.0 | 100.0 | 24 of 20 | provided | **pass** |

| Overall verdict | **target not achieved** |

Reliability closure: 21 attempts, 0 failures, 0 cancelled, 0 reruns recorded; non-regression versus baseline: holds.

## Manifest digest

```
check:1f9264:24850
```
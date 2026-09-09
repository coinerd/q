# C3 final-claim decision: v1.00.28-final

| Field | Value |
|---|---|
| Decision mode | final-claim (every roadmap §8 row, fixed thresholds, coupled guards) |
| Cohort status | open |
| Unique PR head SHAs | 8 of 20 |
| Overall verdict | **target not achieved** |

## Gate

Final-claim verdict over every roadmap §8 row against the FIXED thresholds (fast p50 ≤ 115 s / p95 ≤ 135 s; PR CI p50 ≤ 588 s / p95 ≤ 735 s; security runner p50 ≤ 240 s; workflows runner p50 ≤ 220 s; verified prepared-env restores ≥ 95%) with the coupled guards.  A row without its guard evidence is "unverified", never "pass"; targets are never revised inside this wave or milestone.

## Per-row verdicts

| Row | Measure | Target | Observed | Samples | Guards | Verdict |
|---|---|---|---|---|---|---|
| class-a-fast-work-mass-delta | Class A fast work-mass delta W0→W7 (census ms; negative = workload reduced) | < 0.0 | -5.37 | #f of 20 | provided | **pass** |
| fast-p50 | fast execution p50 on the required fast lane | <= 115.0 | 282.5 | 8 of 20 | provided | **target not achieved** |
| fast-p95 | fast execution p95 on the required fast lane | <= 135.0 | 294.95 | 8 of 20 | provided | **target not achieved** |
| pr-ci-p50 | end-to-end PR CI p50 (first required-check start to last required-check end) | <= 588.0 | 820.5 | 8 of 20 | provided | **target not achieved** |
| pr-ci-p95 | end-to-end PR CI p95 (first required-check start to last required-check end) | <= 735.0 | 850.3 | 8 of 20 | provided | **target not achieved** |
| security-runner-p50 | security suite runner p50 | <= 240.0 | 709.0 | 8 of 20 | provided | **target not achieved** |
| workflows-runner-p50 | workflows suite runner p50 | <= 220.0 | 715.5 | 8 of 20 | provided | **target not achieved** |
| prepared-env-verified-restores | prepared-environment verified-restore rate (percent) | >= 95.0 | 100.0 | 24 of 20 | provided | **pass** |

## Missed rows: observed numbers and named next levers

- **fast-p50** — verdict target not achieved; observed 282.5; reasons: observed 282.5 s exceeds the fixed ≤ 115.0 s target; the target is never revised
  - Next lever (separate reviewed decision; a timing miss alone implies no queue rollback): shrink the fast-lane critical path (shard fan-out and prepared-env cache reuse); re-run the final cohort on 20 new PR head SHAs.
- **fast-p95** — verdict target not achieved; observed 294.95; reasons: observed 294.95 s exceeds the fixed ≤ 135.0 s target; the target is never revised
  - Next lever (separate reviewed decision; a timing miss alone implies no queue rollback): tail-shard rebalancing and cache reuse on the slowest fast shards; re-run the final cohort on 20 new PR head SHAs.
- **pr-ci-p50** — verdict target not achieved; observed 820.5; reasons: observed 820.5 s exceeds the fixed ≤ 588.0 s target; the target is never revised
  - Next lever (separate reviewed decision; a timing miss alone implies no queue rollback): move slow required gates off the mergeable critical path per the v1.00.26 topology analysis; re-run the final cohort.
- **pr-ci-p95** — verdict target not achieved; observed 850.3; reasons: observed 850.3 s exceeds the fixed ≤ 735.0 s target; the target is never revised
  - Next lever (separate reviewed decision; a timing miss alone implies no queue rollback): tail latency of the slowest required gates; re-run the final cohort.
- **security-runner-p50** — verdict target not achieved; observed 709.0; reasons: observed 709.0 s exceeds the fixed ≤ 240.0 s target; the target is never revised
  - Next lever (separate reviewed decision; a timing miss alone implies no queue rollback): shard the security suite and reuse the prepared environment; re-run the final cohort.
- **workflows-runner-p50** — verdict target not achieved; observed 715.5; reasons: observed 715.5 s exceeds the fixed ≤ 220.0 s target; the target is never revised
  - Next lever (separate reviewed decision; a timing miss alone implies no queue rollback): shard-count rebalancing of the workflows suite; re-run the final cohort.

Reliability closure: failed, cancelled, and rerun attempts are recorded and never dropped — cohort totals: 8 attempts, 0 failures, 0 cancelled, 0 reruns; reliability non-regression versus the recorded baseline: holds.

Targets are never revised inside this wave or this milestone.  The overall verdict is "verified" only when every row passes with its coupled guard evidence.

Reviewer: coordinator (delivery) — verified against .planning/VALIDATION.

## Final campaign verdict (v1.00.28)

The decision record ends in exactly one allowed verdict; the Class A work-mass delta row is reported alongside the fixed Class B–D rows and the incompatible measure classes are never compared against each other.

PARTIAL WORKLOAD REDUCTION; FINAL TARGET NOT ACHIEVED

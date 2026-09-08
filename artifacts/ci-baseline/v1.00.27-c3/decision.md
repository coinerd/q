# Cohort C3 Final-Claim Decision — v1.00.27 W5

- cohort-id: `v1.00.27-c3`
- cohort-mode: `final-claim`
- cohort-status at decision time: `closed` — 20 unique eligible merged-PR head SHAs, measured end-to-end on the integrated topology (fast/queue/lpt, four workers, prepared-env cutover)
- decision rule: every roadmap §8 row is evaluated against the FIXED thresholds with its coupled guards; a row without its guard evidence is `unverified`, never `pass`; **targets are never revised** inside this wave or milestone (roadmap v1.00.27 §8). A timing miss alone implies no queue rollback.
- decision date: 2026-09-19
- artifacts: `cohort.json`, `report.json` (`mode: final-claim`), `report.md` — bound by `SHA256SUMS`

## Overall verdict

**`target not achieved`** — six of the seven §8 rows miss their fixed thresholds
(observed above target on every timing row, all coupled guards satisfied); the
prepared-environment row passes. The targets stand exactly as written; the
campaign does not roll back the queue topology on a timing miss (no reliability
regression is present), and the named next levers are deferred to separate
reviewed decisions.

## Gate text (verbatim, encoded in the report tooling)

"Final-claim verdict over every roadmap §8 row against the FIXED thresholds
(fast p50 ≤ 115 s / p95 ≤ 135 s; PR CI p50 ≤ 588 s / p95 ≤ 735 s; security
runner p50 ≤ 240 s; workflows runner p50 ≤ 220 s; verified prepared-env
restores ≥ 95%) with the coupled guards. A row without its guard evidence is
"unverified", never "pass"; targets are never revised inside this wave or
milestone."

## Per-row verdicts (tool-computed; verbatim from `report.json` `final-claim-gate.rows`)

| row | measure | observed | fixed target | verdict | guards |
|---|---|---|---|---|---|
| `fast-p50` | fast execution p50 (required fast lane) | **267.5 s** | ≤ 115.0 s | **target not achieved** | satisfied |
| `fast-p95` | fast execution p95 (required fast lane) | **285.95 s** | ≤ 135.0 s | **target not achieved** | satisfied |
| `pr-ci-p50` | end-to-end PR CI p50 (first required-check start → last required-check end) | **1043.5 s** | ≤ 588.0 s | **target not achieved** | satisfied |
| `pr-ci-p95` | end-to-end PR CI p95 (same span) | **1175.65 s** | ≤ 735.0 s | **target not achieved** | satisfied |
| `security-runner-p50` | security suite runner p50 | **685.5 s** | ≤ 240.0 s | **target not achieved** | satisfied |
| `workflows-runner-p50` | workflows suite runner p50 | **695.5 s** | ≤ 220.0 s | **target not achieved** | satisfied |
| `prepared-env-verified-restores` | prepared-environment verified-restore rate | **100.0 %** (24/24 verified, 0 fallback, 24 records observed) | ≥ 95.0 % | **pass** | satisfied |

Every timing row carries n = 20 unique head-SHA samples (expected 20); the
prepared-env row is evaluated over 24 ci.yml prepared-env records in the C3
window (`run_started_at > 2026-08-31T17:14:49Z`).

Tool-computed reasons (verbatim, one per missed row):

- `observed 267.5 s exceeds the fixed ≤ 115.0 s target; the target is never revised`
- `observed 285.95 s exceeds the fixed ≤ 135.0 s target; the target is never revised`
- `observed 1043.5 s exceeds the fixed ≤ 588.0 s target; the target is never revised`
- `observed 1175.65 s exceeds the fixed ≤ 735.0 s target; the target is never revised`
- `observed 685.5 s exceeds the fixed ≤ 240.0 s target; the target is never revised`
- `observed 695.5 s exceeds the fixed ≤ 220.0 s target; the target is never revised`

## Coupled guards — all provided

- `inventory-accounted`: all 20 per-SHA inventory digests validated non-empty and re-derived from metadata-inventory artifacts (`cohort.json`).
- `reliability-non-regression`: C3 attempts summary 21 attempts / 21 successes / 0 failures / 0 reruns / 0 cancels versus the recorded baseline `artifacts/ci-baseline/v1.00.25-c2/cohort.json` (0 failures, 0 reruns, 0 cancels) → satisfied.
- `semantic-gate-equivalence`: `docs/reports/gsd-wave-evidence/v1.00.27-w2.rktd` (gate semantics independent of grouped vs ungrouped invocation).
- `four-worker-isolation-proof`: `docs/reports/gsd-wave-evidence/v1.00.27-w3.rktd` (four-worker fast shards isolated; no shared writable state).
- `failure-truth`: `docs/reports/gsd-wave-evidence/v1.00.27-w4.rktd` (CI failures recorded, never silently retried into false greens).
- `shared-state-permission-isolation`: `docs/reports/gsd-wave-evidence/v1.00.27-w2.rktd` (worker permission isolation suite).
- `prepared-env-no-bypass`: `.github/workflows/ci.yml` guarded prepared-environment (auto input, `prepared-env-report` required job) + `cohort.json` prepared-env-restore stats.

A guard-missing row would have been recorded `unverified`, never `pass`; no row
is in that state.

## Named next levers (separate reviewed decisions; targets untouched)

- fast p50/p95: shrink the fast-lane critical path (shard fan-out, prepared-env cache reuse); tail-shard rebalancing on the slowest fast shards.
- PR CI p50/p95: move slow required gates off the mergeable critical path per the v1.00.26 topology analysis; tail latency of the slowest required gates.
- security runner p50: shard the security suite and reuse the prepared environment.
- workflows runner p50: shard-count rebalancing of the workflows suite.

Each lever ends with: re-run the final cohort on 20 new PR head SHAs. The
`verified` verdict remains withholdable until every row passes with its guard.

## Queue disposition

No rollback: the reliability summary shows zero failures, reruns, and cancels
across the cohort (non-regression versus C2), and a timing miss alone implies no
queue rollback. The integrated topology stays; levers above address latency.

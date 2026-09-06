# Cohort C1 Promotion Decision — v1.00.25 W1

- cohort-id: `v1.00.25-c1`
- decision-version: `w1-decision-v1`
- cohort-status at decision time: `started` (open)
- decision rule: the roadmap initial fast threshold is evaluated as written; **a missed gate records `hold`, never a revised target** (roadmap v1.00.25 §6, W1).
- decision date: 2026-09-06

## Overall verdict

**`hold`** — all three promotion lanes are held. The paired shadow evidence required
to close C1 does not exist in the delivery environment: the three queue
configurations (`fast/queue/fifo`, `fast/queue/lpt`, `security/queue/fifo`) have
**zero recorded attempts** across the 20 eligible SHAs, while the paired baseline
leg (`fast/batch/fifo`) has complete timing evidence. A lane cannot be promoted on
zero samples, and the cohort cannot be closed while its status is `started` with
incomplete legs.

## Gate text (verbatim, from the roadmap and encoded in the report tooling)

- Fast lanes (`fast-queue`, `fast-LPT`):
  "Initial fast threshold: queue fast execution p50 ≤ 130.0 s and p95 ≤ 145.0 s
  with no reliability regression versus the paired batch baseline on the same SHAs
  (roadmap v1.00.25 §6, W1). A missed gate produces hold, never a revised target."
- Security lane (`security-queue`):
  "Selected-inventory equality and no reliability regression versus the paired
  batch baseline on the same SHAs (roadmap v1.00.25 §6, W1)."

## Numbers behind the verdict

### Paired baseline — `fast/batch/fifo` (required lane)

| metric | value |
|---|---|
| samples (successful timing) | 20 |
| attempts recorded | 20 |
| failed attempts | 0 |
| cancelled attempts | 0 |
| rerun attempts | 0 |
| p50 (linear interpolation) | 228.5815 s |
| p95 (linear interpolation) | 235.9965 s |

The baseline is **real captured evidence** (20 successful timing samples over the
20 eligible SHAs) and is the reliability/timing comparison anchor for every lane
below.

### Lane verdicts

| lane | config-id | verdict | timing gate | attempts recorded | samples | p50 / p95 | inventory equal to baseline | reliability vs baseline |
|---|---|---|---|---|---|---|---|---|
| fast-queue | `fast/queue/fifo` | **hold** | applicable | 0 | 0 | n/a | not evaluable (0 samples) | not evaluable (0 samples) |
| fast-LPT | `fast/queue/lpt` | **hold** | applicable | 0 | 0 | n/a | not evaluable (0 samples) | not evaluable (0 samples) |
| security-queue | `security/queue/fifo` | **hold** | not applicable (inventory + reliability only) | 0 | 0 | n/a | not evaluable (0 samples) | not evaluable (0 samples) |

Tool-computed reasons (verbatim from `report.json` `decision` section):

- `evidence incomplete for fast/queue/fifo: 0 of 20 SHAs have exactly one successful timing sample; cohort status is started`
- `evidence incomplete for fast/queue/lpt: 0 of 20 SHAs have exactly one successful timing sample; cohort status is started`
- `stale or missing duration evidence: LPT falls back to FIFO ordering (named reason: paired fast/queue/fifo leg evidence incomplete)`
- `evidence incomplete for security/queue/fifo: 0 of 20 SHAs have exactly one successful timing sample; cohort status is started`

The LPT fallback reason is recorded even though LPT is an ordering-only change:
with no paired FIFO-queue timing samples there is no duration evidence to order
by, so LPT ordering cannot be proven against FIFO queue on this cohort and falls
back to FIFO with that named reason.

## Why the cohort stays open (not force-closed)

The tooling refuses `closed`/`cancelled` cohort status while any leg has zero
attempts (cohort-report validation errors on incomplete legs). Forcing closure
would require dropping SHAs or fabricating samples; both are prohibited by the
wave contract ("never drop an SHA to make numbers pass") and by the honesty rules
(no fabricated data, targets never revised inside this wave). C1 therefore stays
`started`, the shadow workflow obligation from W0 stands, and this decision is
re-evaluated the moment the three queue legs have real paired samples on all
20 SHAs.

## Reviewer

- Reviewer: W1 delivery verification lane (automated pre-commit checks: format,
  affected tests, secrets/metrics lints) plus the W1 reviewer role recorded in
  `docs/reports/gsd-wave-reviews/v1.00.25-w1.rktd`.
- Review outcome: numbers in this record are reproduced directly from
  `report.json`'s `decision` section (`w1-decision-v1`); no target was revised and
  no lane was promoted.

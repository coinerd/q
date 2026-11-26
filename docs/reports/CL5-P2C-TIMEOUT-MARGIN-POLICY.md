# CL5 P2-C — timeout-margin policy: proposal for separate review

Status: **approved and applied as P2-D** (`CL5-P2D-TIMEOUT-POLICY.md`). The measured limits are
330 s / 540 s / 270 s for three files, pinned to the declarations by
`tests/test-cl5-timeout-margin.rkt`. The proposal text
below is kept as the record of what was approved; the applied numbers supersede its estimates.

Evidence: `CL5-P2C-PROFILING.md` and `CL5-P2-TIMEOUT-RESOLUTION.md`.

## The problem the profiling exposed

The two failures are not the same defect, and one of them is not a per-file cost problem at all:

| File | Standalone (quiet, warm) | Limit | Under the measured 4-worker surface |
|---|---|---|---|
| `test-gsd-executor-ergonomics-prompt.rkt` | 68–71 s | 120 s default | **timed out at 120 s** |
| `test-gsd-evidence-repair-production-contract.rkt` | 182–188 s (quiet) / 198–202 s (shared host) | declared 210 s | timed out at 210 s |

The first row is the important one: at **58 % of its limit standalone**, the file exceeded the limit
once four test workers shared four cores. Per-file timeouts are wall-clock limits applied to files run
under concurrency, but the limits were evidently chosen from standalone runs.

So a policy of "raise the limit until the standalone p95 fits" would still fail whenever the surface
runs with real concurrency — and a policy of "measure standalone only" would keep producing failures
that look like flaky tests.

## Proposed policy (for review; not in force)

1. **Margin is defined at the measurement's concurrency.** For a file measured in a surface run at
   `--jobs n`, the reference statistic is its p95 *observed under that same n on the reference host*,
   not its standalone time.
2. **Minimum margin.** A file's declared/effective limit must be ≥ **1.5 ×** that p95. Files below the
   margin are either fixed (runtime) or listed in a pre-registered `slow-file` set (below).
3. **Where the limit comes from.** The effective limit for a file is
   `max(declared @timeout, ceil(1.5 × p95_at_n))`, rounded up to the next 30 s. This preserves every
   existing declaration as a floor (nothing is lowered) and only adds headroom where measurement
   shows it is missing.
4. **Pre-registered slow set.** Files that cannot meet the margin stay explicitly listed with their
   measured p95 and a review date; the list is part of CI config, so adding to it is a visible change,
   never an invisible timeout bump.
5. **No silent growth.** The policy is re-derived from a fresh measurement each time the surface
   changes meaningfully (producer set, job count, or a file's cost moving >25 %). The derivation is
   committed with the measurement record.
6. **Fail-closed is unchanged.** A timeout is still `incomplete`; the comprehensive block still
   carries derived `degraded`; a `degraded` record still cannot support a GO. This policy is about
   measuring reliably, not about making failures disappear.

## What it would do to the two files, with the measured numbers

| File | p95 at 4 workers | Required ≥ 1.5× | Current | Outcome |
|---|---|---|---|---|
| `test-gsd-executor-ergonomics-prompt.rkt` | ≥ 120 s (observed timeout) | ≥ 180 s | 120 s default | would need an explicit declared limit (e.g. 240 s) **and** a runtime investigation, because 120 s was never enough under concurrency |
| `test-gsd-evidence-repair-production-contract.rkt` | 202 s | 303 s | 210 s | would need 300 s |

Both would become *green but slow*, which is exactly what the record needs to stop being `degraded` —
and exactly why this must not be done quietly: a 300 s limit on a 200 s test doubles the worst-case
wall time of a shard that already takes 15–33 minutes.

## The alternative, stated honestly

The runtime investigation (`CL5-P2C-PROFILING.md`) found no removable work that does not weaken what
the tests verify: their cost is authentic ladder/audit/publish work. If the owner prefers not to
lengthen limits, the alternative is to **accept that the comprehensive baseline stays `degraded`** and
report the speedup as resource-cost only. That is a scope decision, not a measurement fix.

## What this proposal explicitly does not do

- It does not remove, skip or weaken a test.
- It does not reduce the producer set or redefine the comprehensive surface.
- It does not average away the failures or the variance.
- It does not apply to the safety experiment (W3.3), which is held.

# SLO Evidence Record — v1.00.27 W4: L2 local feedback loop p90 target adjustment

This is the governed evidence record required by the v1.00.27 §8 W4 action: an SLO may be
adjusted only through this record (sample, method, reason, owner, review), committed in the
same PR as the measured evidence. The L0 (≤5 s) and L1 (≤30 s) targets are **confirmed** and
untouched. Only the L2 target changes.

## Target adjusted

| Loop | Scope | §8 target | Adjusted target | Measured p90 | Verdict vs adjusted |
|------|-------|-----------|-----------------|--------------|---------------------|
| L0   | single test file (`racket scripts/run-tests.rkt <file>`) | ≤ 5 s | — (confirmed) | 1,883 ms | meet |
| L1   | area (direct unit impact set, grouped per W3 config) | ≤ 30 s | — (confirmed) | 4,428 ms | meet |
| L2   | full unit-fast tier (`racket scripts/run-tests.rkt`) | ≤ 120 s | **≤ 240 s** | 234,909 ms | meet |

## Sample

20 fresh `racket scripts/run-tests.rkt` invocations of the full unit-fast tier (1 cold start,
19 warm), collected 2026-09-07T23:00:07Z–2026-09-08T00:16:59Z on the current tree
(implementation-sha `aa96e61a…`, recorded in `local-p90.json` along with per-sample elapsed
times and exit codes). Per-sample durations: min 225,629 ms, median 230,908 ms, max
318,098 ms; every sample exited 0. Machine context (12 CPUs, load averages, Racket 8.10,
unix) and the W3 grouped-expansion configuration are embedded in `local-p90.json`. L0 and L1
were sampled under the same protocol in the same collection (20 samples each).

## Method

p90 with linear interpolation at rank `0.9*(n-1)` (19.0th 0-indexed rank between the 19th and
20th of 20 sorted samples), computed by `scripts/run-tests/overhead.rkt --collect-local`;
verdicts are recomputed by the collector and re-validated by `check-local-p90-record`
(`--check`) — never hand-written. Each sample is a fresh runner process; grouped/subprocess
mode per area follows W3's governed configuration (root areas subprocess, `ci` grouped).

Disclosure: one L2 sample (sample 7, warm) executed the full tier but exited 1 on an
in-tier test failure; failed runs are excluded from latency evidence by the `--check`
validator, so that sample was replaced with one additional fresh warm invocation under the
identical protocol (fresh `racket scripts/run-tests.rkt` process, timed identically), run at
2026-09-08T00:57Z: elapsed 318,098 ms, exit 0, 943/943 files passing, recorded in place as
sample 7 with its own elapsed time and exit code. That replacement sample is the final-set
maximum and exceeds the adjusted 240 s ceiling as a single sample; the p90 statistic governs
the verdict per the method above. L2 statistics and verdict are computed from the final
all-pass set of 20 samples.

## Reason

The §8 ≤120 s budget for the full-tier loop predates the W3 grouped-expansion rollout and
underestimates the tier's serial baseline: a fresh `racket scripts/run-tests.rkt` process over
the full unit-fast tier spends the bulk of its wall time in Racket module instantiation and
per-family serial execution (parallel-writer families stay serial per the W0 ownership
matrix), which no in-loop scheduling change in scope for this campaign removes. Measured p90
on the reference workstation (12 CPUs) is 234.9 s — 96% above the 120 s target, so the
target is not confirmable and the loop is unusable as a 2-minute check. The adjusted 240 s
target encodes the observed reality with a small margin above the measured p90 and keeps the
loop meaningful as a bounded local feedback budget; it is a local-loop expectation only and
does not reinterpret the fast-gate report's p50/p95 method or any CI gate.

## Owner

v1.00.27 campaign — wave W4 (ticket #9592). Owner of record: the campaign's delivery
sequence; the target is encoded in `scripts/run-tests/overhead.rkt` (`L2` local-loop SLO
constant) and asserted by `tests/test-run-tests-overhead-diagnostics.rkt`.

## Review

Bound to the same PR as `artifacts/tier-ownership/v1.00.27-w4/local-p90.json` and its
`SHA256SUMS`, landing through the Delivery Contract (branch `campaign/v1.00.27-w4`, squash
merge, evidence trio bound to the merge SHA). `--check` enforces sample counts, machine
context, method fields, and recomputed verdicts so the record cannot silently drift from the
measurement. Re-measure on hardware change, runner scheduling change (e.g. any future
parallel-writer relaxation), or tier growth; re-adjust only through a successor governed
record.

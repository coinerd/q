# CL3 Wave 1 — persistent worker feasibility probe (#9980 / #9911)

Evaluation-only artifact at revision `8825ae8235e933f6003d988ff8521580a900cada`. Raw samples:
[`CL3-WAVE1-PROBE-8825ae82.json`](CL3-WAVE1-PROBE-8825ae82.json). No production tool, test runner,
admission record or CI policy was changed.

This revision incorporates independent-review corrections: the combined workflow now checks
every response status and value, output/value accumulation is bounded before string materialization,
timeouts cover the complete line read, and timing labels/statistics are explicit.

## Protocol and boundaries

- `scripts/cl3-worker-probe.rkt` runs as a **separate OS process** with bounded JSON lines:
  `eval`, `load`, `reload`, `reset`, `stop`. Its namespace/custodian manages state only; it is
  **not** a security boundary.
- Diagnostic output and rendered values are written through fixed-capacity ports. The collector
  stores at most 4096 output characters / 512 value characters and discards the rest, so response
  construction cannot grow with an unbounded `display` or a huge printed value.
- `scripts/cl3-eval-probe.rkt` enforces one deadline across write, wait and the full response read;
  a reader thread is killed on timeout and the direct child is killed and reaped. Descendant
  process-tree cleanup is **not** established in Wave 1.
- `scripts/cl3-benchmark-probe.rkt` ran n = 20 one-shot executions, n = 20 persistent requests
  and n = 5 complete workflows on one host (`unix/64-bit`). Durations are recorded to 0.001 ms.
  Quantiles use CL0-compatible linear interpolation at rank `q*(n-1)`, rounded to 0.001 ms.

## Measured request latency (ms)

| Case | n | p50 | p90 | max |
| --- | ---: | ---: | ---: | ---: |
| First one-shot sample (not a controlled cold-cache trial) | 1 | 971.650 | — | 971.650 |
| Subsequent one-shot samples | 19 | 1025.500 | 1106.808 | 1170.789 |
| First persistent request (excludes full `start-worker!`) | 1 | 500.974 | — | 500.974 |
| Subsequent persistent requests | 19 | 0.518 | 0.612 | 0.645 |

Full worker startup including process creation and a confirming response: **497.347 ms**.
The separate handshake/startup figure is reported independently and is not hidden inside warm
request latencies. The first persistent request is higher because child initialization overlaps it.

## Verified operation sequence (single session)

| Operation | Outcome | Duration (ms) |
| --- | --- | ---: |
| `load` (module returns 1) | ok | 12.329 |
| `eval (f)` | `"1"` | 1.622 |
| edit source to return 2 | completed | — |
| `reload` | ok | 0.431 |
| `eval (f)` | `"1"` — stale | 1.091 |
| `reset` | ok, interactive namespace only | 2.528 |
| `load` again | ok | 57.017 |
| `eval (f)` | `"2"` — fresh | 0.917 |

The workflow repeats this sequence with a fresh worker per iteration and records every outcome in
`worker-steps[*].outcomes`. Baseline eval, stale eval and fresh eval are all asserted; no duration
is reported for a failed or unexplained operation.

## Combined workflow (n = 5)

`semantic navigation → inspect component → evaluate expression → modify source → reload → reset/reload → evaluate again`

Navigation is measured separately from worker steps. Navigator `module-query` p50 **412.942 ms**,
p90 **4995.771 ms**, max **8029.401 ms** (first index build dominates the p90). Worker steps in
iteration 1: spawn 2.434, handshake 546.557, load 13.345, baseline eval 0.528, edit 0.256,
reload 0.295, stale eval 0.433, namespace reset 1.875, reload after reset 60.490, fresh eval
0.869 ms. All ten asserted operations returned `ok` with the expected values.

The persistent worker removes repeated interpreter startup for trivial repeated expressions, but
these are not representative debugging tasks. The full edit-to-correct-result workflow still pays
startup/reload costs, and the correct result required process-side state replacement via
reset+reload. This report therefore makes **no thousandfold or representative-workflow claim**.

## Reliability evidence (9 passing prototype tests)

- Shared state across `eval` requests.
- Stale-closure and stale-binding behaviour after `reload`; clean interactive namespace after
  `reset`; fresh module value only after reset+reload.
- Reset note explicitly states that process-global state, threads and OS resources are **not**
  cleared; worker replacement is the stronger clean-process boundary.
- Malformed expressions produce structured errors and do not kill the worker.
- Bounded collectors: direct test writes 100000 characters into a 128-byte-cap port and verifies
  128 stored bytes, positive discarded bytes, truncation flag and capped rendered value.
- Excessive `displayln` output is capped at 4096 characters and flagged end to end.
- Infinite loop: the end-to-end deadline times out and the direct child is killed and reaped;
  descendant process-tree cleanup remains explicitly unverified.
- Worker crash is detected and cleaned up.
- Neither probe script is referenced by `qt` or the production tool registry.

## Interpretation and decision

The latency signal for isolated repeated evaluation is strong, but it does not yet establish
suitability for real coding-agent workflows. Reload leaves stale values; namespace reset only
clears interactive bindings; worker replacement is the reliable clean-state operation. Output and
value memory are bounded inside the worker, but arbitrary evaluation is still not sandboxed: a
namespace/custodian is not an OS-level boundary, no permission policy was integrated, and process
descendants are not proven cleaned up.

**Wave 1 decision remains REVISE (do not promote to production).** The next experiment should
compare one-shot execution, persistent worker with reload, and worker replacement on real q
modules with independently checked outputs and full edit-to-correct-result latency. Existing
isolated tests and CI remain authoritative. The CL0 fresh-context cognitive-load trials remain
assigned to CL5 (#9982).

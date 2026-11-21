# CL3 Wave 1 — persistent worker feasibility probe (#9980 / #9911)

Evaluation-only artifact at revision `8825ae8235e933f6003d988ff8521580a900cada`. Raw samples:
[`CL3-WAVE1-PROBE-8825ae82.json`](CL3-WAVE1-PROBE-8825ae82.json). No production tool, test runner,
admission record or CI policy was changed.

## Protocol

- `scripts/cl3-worker-probe.rkt` is a **separate OS process** speaking bounded JSON lines:
  `eval`, `load`, `reload`, `reset`, `stop`. Its namespace manages state only; it is **not** a
  security boundary.
- `scripts/cl3-eval-probe.rkt` is the driver: every request has a wall-clock timeout, and
  timeouts/crashes kill and reap the child before returning.
- `scripts/cl3-benchmark-probe.rkt` ran n = 20 one-shot executions, n = 20 persistent requests,
  and n = 5 combined workflows on one host (`unix/64-bit`). Cold = first sample; warm = the rest.
  Values are rounded to whole milliseconds, so sub-millisecond persistent requests appear as 0.

## Measured results (ms)

| Case | n | p50 | p90 | max |
| --- | ---: | ---: | ---: | ---: |
| Cold one-shot `racket -e` | 1 | 1074 | — | 1074 |
| Warm one-shot `racket -e` | 19 | 956 | 1043 | 1112 |
| First persistent request (includes startup) | 1 | 15 | — | 15 |
| Subsequent persistent requests | 19 | 0 | 1 | 1 |

Separated costs: worker startup/first handshake **595 ms**; module load **12 ms**; reload
reported as 0–1 ms in this run (sub-millisecond resolution); reset **3 ms**. The startup figure
is the dominant one-time cost; it is *not* included in subsequent request latencies.

### Combined workflow (n = 5)

`semantic navigation → inspect component → evaluate expression → modify source → reload → evaluate again`

| Step | p50 | p90 | max |
| --- | ---: | ---: | ---: |
| Navigator `module-query` (includes source-identity hashing) | 438 | 460 | 8062 |
| Worker request / source edit / reload / re-evaluate (sample 1) | 612 / 1 / 91 / 1 | — | — |

The first navigator query builds the tracked-source index (8062 ms); subsequent queries with a
live cache pay roughly 438–621 ms, mostly source-identity hashing. That is visible but not the
dominant cost of the combined workflow at this sample size; process startup is. The navigator's
freshness guarantee was **not** weakened to improve these numbers.

## Reliability evidence (7 passing prototype tests)

- Shared state across `eval` requests.
- `load`, `reload`, `reset` semantics: `reset` demonstrably produces a clean namespace; a closure
  captured before `reload` remains stale afterwards (observed: old closure still returns `1`
  while a fresh lookup sees the edited module). This is an explicit **state-ambiguity finding**.
- Malformed expressions produce structured errors and do not kill the worker.
- Excessive output is capped at 4096 characters and flagged `truncated?`.
- An infinite loop times out at 2 s; the child is killed and reaped (numeric exit status).
- A worker crash is detected and cleaned up.
- Neither probe script is referenced by `qt` or the production tool registry.

## Interpretation and decision

The speedup for trivial repeated evaluations is very large (roughly three orders of magnitude
after startup), but the measured task is deliberately small and does not establish usefulness for
real q debugging tasks. Reload semantics are genuinely ambiguous: reloading refreshes module
instances but cannot refresh closures captured earlier, so `reset`/process replacement remains the
only reliable clean-state operation. Cleanup and bounded-failure behaviour are sound in this
prototype, but the security posture of arbitrary evaluation is unchanged: a namespace/custodian is
not an OS-level boundary and no q permission policy was integrated.

**Wave 1 decision: REVISE (do not promote to production).** The latency signal is promising; the
state model needs an explicit policy (reload vs mandatory reset) and a permission/sandbox design
before any #9911 implementation could be considered. Existing isolated tests and CI remain
authoritative. The CL0 fresh-context cognitive-load trials remain assigned to CL5 (#9982).

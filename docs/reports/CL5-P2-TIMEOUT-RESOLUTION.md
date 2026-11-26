# CL5 P2 — resolving the two timeouts

Purpose: establish, by measurement, whether the two timeouts that made the `dad0a9b9` cost record
`degraded` are reproducible, and whether the official surface can finish under its declared limits.
No test was removed, no timeout was extended, and the comprehensive set was not redefined.

Method: each file executed **standalone** by the production adapter with its own declared limit in
force (`racket scripts/run-tests/runner.rkt <file>`, i.e. `@timeout` metadata, else the 120 s
default), three runs each, same host (4 vCPU), warm cache, no other measurement running. Wall time
includes the per-invocation preparation phase.

## Results

| File | Declared limit | Runs (wall s) | Verdicts | Utilisation of the limit |
|---|---|---|---|---|
| `tests/test-gsd-executor-ergonomics-prompt.rkt` | none → **120 s** default | 175.2 (incl. 55 s prep), 77.2, 75.3 | **TIMEOUT**, PASS, PASS | run 1 exceeded 120 s of execution; runs 2–3 use ~63 % |
| `tests/test-gsd-evidence-repair-production-contract.rkt` | **`@timeout 210`** | 200.7, 201.6, 197.8 | PASS, PASS, PASS | **95–96 % of its declared limit, every run** |

## What this establishes

1. **Neither timeout is a hung test.** Both files do work and finish on the host.
2. **The 210 s file is deterministic and at the margin.** It needs 198–202 s against a 210 s limit.
   Any co-tenant load pushes it over — which is exactly what happened inside the 4-worker shard run
   (`elapsed 210 062 ms`, timeout). Standalone it passes; in the measured surface it did not.
3. **The 120 s file is load- and cold-start-sensitive.** First run (cold closure prepare plus a
   contended host) exceeded 120 s; warm standalone runs take ~75 s. Reproducibility across runs is
   therefore **not** established for this file: it is a latency-margin failure, not a fixed cost.
4. **The surface does not currently finish successfully under its declared limits on this host** when
   executed as designed (4 workers on 4 cores). Both failures are margin failures: one at 96 % of its
   limit, one with under 40 % headroom warm and none cold.

## Why this is not "fix the measurement"

The tempting moves are all forbidden by the design, and each would corrupt the comparison:

- deleting or excluding the files — changes the surface, so C is no longer C;
- extending their timeouts — changes the workload's semantics and inflates C for reasons unrelated to
  selection;
- measuring on a quieter/faster host — this record's host is the reference host, and a different host
  is a different measurement.

## Consequence for the GO gate (adopted in P2-B rev 2 §4.5)

The C record stays **`degraded`**. Under the approved design a degraded comprehensive record is
admissible as *cost evidence* but **cannot support a cost-based GO**. Three legitimate ways out, each
requiring its own review:

1. **Reduce the runtime** of the two files (they are GSD integration tests; this is the honest
   engineering fix, and it belongs to the execution-infrastructure track, not to CL5 selection).
2. **A timeout-margin policy**: require declared limits to exceed measured p95 by a fixed factor
   (e.g. ≥ 1.5×) for a surface to count as `ok`, and re-derive the two files' limits from measurement.
   This is a repository-wide policy change: separate review, separate PR, and it must be applied to
   the whole suite rather than to the two files that happen to have failed here.
3. **Publish the speedup as resource-cost only** (Σ producer time), with no wall-time claim, if the
   owner accepts that narrower claim. This is a scope decision, not a measurement trick.

What is *not* available is a fourth option: calling the current record an accepted performance
baseline. It is not, and the owner's decision already says so.

## Reproduction

```bash
# three standalone runs each, declared limits in force
for f in tests/test-gsd-executor-ergonomics-prompt.rkt \
         tests/test-gsd-evidence-repair-production-contract.rkt; do
  for i in 1 2 3; do /usr/bin/time -f "%e %x" racket scripts/run-tests/runner.rkt "$f"; done
done
```

Observed: `test-gsd-executor-ergonomics-prompt.rkt` 175.2 s (timeout) / 77.2 s / 75.3 s;
`test-gsd-evidence-repair-production-contract.rkt` 200.7 s / 201.6 s / 197.8 s.

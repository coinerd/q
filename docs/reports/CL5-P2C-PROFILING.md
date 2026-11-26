# CL5 P2-C — profiling the two timeout-adjacent tests

Scope: investigation only. No test was changed, no timeout was extended, no production code was
touched. Host: 4 vCPU / 7 GB, warm cache, undirected runs on a quiet host unless stated.

## Method

1. **Phase timing** of each test with temporary `eprintf` markers in a scratch copy (removed
   afterwards; the committed tests are untouched).
2. **Fixture isolation**: a one-line driver that builds the shared supplemental fixture and does
   nothing else, to separate fixture construction from assertions.
3. **Sampling profile** (`raco profile --repeat 1`) on the evidence-repair test via a driver that
   instantiates its `test` submodule.
4. **Standalone repetitions** of each file through the production adapter with its own declared limit
   in force.

## Result 1 — `test-gsd-executor-ergonomics-prompt.rkt` (limit: 120 s default)

The file has **two** `test-case`s and each builds the full supplemental fixture. Fixture construction
alone is **30.0 s**:

| Fixture phase | Time |
|---|---|
| runner-closure copy (20 files) | 0.15 s |
| `managed-compile-zo` over the closure | **5.6 s** |
| real C0 ladder inside the fixture (`raco fmt`, `raco make`, two `run-tests` invocations, lint) | ~12 s |
| git history, plan/receipt/broker setup, verification registry | balance (~12 s) |

So ~60 s of the file's runtime is two fixture builds, and ~10 s is the assertions. Standalone
repetitions (quiet host, warm): **68.8, 68.8, 70.8, 71.0, 69.0, 68.0 s** — i.e. 57–59 % of the 120 s
limit. Under the earlier 4-worker shard run it exceeded 120 s and timed out.

**One new observation, recorded because it is a different failure mode:** during this investigation a
single preparation phase failed (`VERDICT: ❌ FAIL (preparation failed — no test executed)`, 59 s, no
test executed). It did not recur in five subsequent runs. The runner failed closed exactly as
designed. The most plausible cause is a stale/partial `compiled/` artifact left by an earlier
`kill -9` of a compile in progress; it is *not* established, and the measurement protocol consequence
is in §4 below (a recorded successful warm-up).

## Result 2 — `test-gsd-evidence-repair-production-contract.rkt` (declared `@timeout 210`)

Phase timing of the single `test-case`:

| Phase | Ends at | Cost |
|---|---|---|
| setup (git init/commits/pushes, fmt) | 5.4 s | 5.4 s |
| plan snapshot, campaign record, snapshot | 14.0 s | 8.6 s |
| `verify-campaign-delivery` (real 5-stage ladder) | 16.7 s | 2.7 s |
| evidence-context stubs and stale-guard | 16.7 s | ~0 s |
| **pre-review controller work** | 98.5 s | **81.8 s** |
| review callback (clone, `validate-owned-authority`, `owned-review-authority` ×2) | 119.8 s | 21.3 s |
| **post-review publish work** | 176.6 s | **56.8 s** |

So **138.6 s of the ~180 s is the real delivery/review/audit chain**, executed in-process with its
child ladders. The sampling profiler attributes ~25 % of samples to reading child-process output
(`racket/port.rkt`), i.e. blocked on subprocesses — this is time spent doing the work the test exists
to verify, not overhead.

Standalone repetitions: **181.9, 184.1, 188.3 s** on a quiet host in this session; **197.8, 200.7,
201.6 s** in the earlier session (when other measurements shared the host). Against 210 s that is
**87–96 % of the declared limit**, and the shard run hit exactly 210 s.

## What could be removed, and what that would cost

| Candidate | Saving | Verdict |
|---|---|---|
| Cache the fixture's compiled closure across fixture instances (5.6 s each) | ~11 s on the 120 s test; 0 on the 210 s test | **Rejected for now.** Measured: relocation of a compiled tree works (a copied fixture tree runs its ladder in ~2 s without a recompile), so it is implementable, but it does not move the file that actually blocks the baseline, and it touches a shared fixture used by other tests. Not worth the risk inside a measurement workstream. |
| Reduce the C0 ladder steps in the fixture | ~12 s/file | **Rejected:** each step is part of what the fixture authenticates; removing one changes the evidence, not just the speed. |
| Collapse the evidence test's two audit passes into one | ~50–80 s | **Rejected:** the test's whole point is "publishes once with fresh audited authority and leaves frozen GREEN unchanged"; one pass would stop testing the second. |
| Raise the declared timeouts | — | **Rejected here** (owner: prefer runtime first; and a policy change needs its own review). |

**Conclusion of the investigation: the two tests are not wasting time in a way that can be removed
without weakening what they verify.** Their cost is authentic ladder, audit and publish work. The
remaining lever is the timeout policy — and the profiling changes *what that policy has to say*: the
ergonomics file is at 58 % of its limit standalone yet exceeded the limit in the measured 4-worker
surface, so a policy derived from standalone timings would still fail.

## Consequence for measurement (proposed, separate review)

`docs/reports/CL5-P2C-TIMEOUT-MARGIN-POLICY.md` proposes a **concurrency-aware margin**: derive each
file's margin from its p95 *at the measurement's own job count*, not from a standalone run. It is not
applied by this change; the two tests keep their current limits and the C record stays `degraded`.

Until that (or a runtime fix) is accepted, no comprehensive re-measurement can produce a non-degraded
baseline, so the paired-run remeasurement is deliberately **not** run again here: it would spend
~2.5 h to reproduce a known `degraded` result. That is the owner's decision point.

## Reproduction

```bash
# standalone repetitions (declared limits in force)
for i in 1 2 3; do
  racket scripts/run-tests/runner.rkt tests/test-gsd-executor-ergonomics-prompt.rkt
  racket scripts/run-tests/runner.rkt tests/test-gsd-evidence-repair-production-contract.rkt
done
```

Observed: 68.0–71.0 s (ergonomics, limit 120) and 181.9–188.3 s (evidence repair, limit 210) on a
quiet host; 197.8–201.6 s for the latter when the host also ran other measurements.

# FLAKE-FORENSICS-v1.00.29 — Suite-interference and flake forensics (W3)

Campaign v1.00.29, wave W3. Scope: forensic capture and bounded reduction for
suite-context-only flaky failures, plus the incident ledger for the two seed
incidents carried over from the v1.00.28 W9 observations.

- Capture module: `scripts/run-tests/flake-forensics.rkt`
  (schema `q.flake-forensics/1`)
- Reduction module: `scripts/run-tests/flake-reduce.rkt`
- Seed bundles: `artifacts/proof-graph/v1.00.29-w3/bundles/` (+ `SHA256SUMS`)
- Bundle generator (one-off, direct invocation only):
  `scripts/run-tests/w3-seed-flake-bundles.rkt`
- Tests: `tests/test-flake-forensics.rkt`, `tests/test-flake-reduce.rkt`

## Capture contract (q.flake-forensics/1)

A forensic bundle is captured **post-failure only**: a green run never
produces a bundle. Every mandatory field is always present; a probe that
cannot answer records the string `"unknown"` instead of omitting the key.
Mandatory fields: `schema`, `incident-id`, `source-note`, `root-cause-class`,
`git` (commit/tree), `workflow` (workflow/run/job ids), `failing-test-file`,
`behavior-ids`, `predecessor-sequence`, `shard`, `worker`, `scheduler-mode`,
`seed`, `selected-manifest-digest`, `compiled-state` (paths digest + fresh/stale
mtime verdict), `prepared-env`, `environment-policy-digest`, `temp-roots`,
`filesystem-residue`, `child-process-tree`, `surviving-pids`, `open-handles`,
`worktrees`, `captured-at`, `test-durations`, `original-stdout`,
`original-stderr`, `original-result-artifact`, `rerun-ancestry`.

`incident-id` = first 16 hex chars of the SHA-256 over the canonical
(sorted-key) JSON of the bundle without the id itself. Bundles are written to
`artifacts/proof-graph/v1.00.29-w3/bundles/<incident-id>.json`.

## Incident ledger

Taxonomy assignments are deliberately `unknown`. No root cause is predeclared
for either incident. BUG-0066 (process-kill classification) and BUG-0065
(restored-workspace bytecode staleness) are **admissible hypotheses** that the
reduction protocol may later support or eliminate — they are **not**
predeclared causes, and no bundle or verdict in this wave asserts either.

| id | incident-id | file | observed (date-only) | context | standalone | retry | root-cause-class |
|----|-------------|------|----------------------|---------|------------|-------|------------------|
| A | `9993972ef02fe8ab` | `tests/test-ci-cohort-report.rkt` | 2026-09-08 | full-suite worker | green | green (single retry) | unknown |
| B | `91a81438f485ef9b` | `tests/test-milestone-gate.rkt` | 2026-09-09 | full-suite worker, rotating | green | rotating (green on some suite runs, red on others) | unknown |

Both bundles are post-hoc captures of v1.00.28 W9 live observations
(`source-note` in each bundle: "v1.00.28 W9 live observation, post-hoc
capture"). Fields that were not recorded at failure time are `"unknown"`;
they are unknown because the W9 runs predate the forensic capture module, not
because they were clean.

## Reduction protocol (flake-reduce.rkt)

`reduce-incident` runs the protocol as pure steps over an injected run
predicate (tests drive it without real processes):

1. exact predecessor-sequence reproduction;
2. standalone check of the failing test (a standalone failure is recorded as
   `standalone-reproduces` — not suite interference);
3. deterministic delta-debugging of the predecessor sequence
   (remove halves, then finer chunks, restart after every successful removal);
4. minimal single-predecessor `A->failing` pair;
5. optional `B->A->failing` expansion (each removed predecessor is tried in
   front of the minimal context; tested/reproduced are recorded);
6. cold-vs-warm compiled-state flag (two injected thunks);
7. worker-isolation flag (two injected thunks).

Hard budget: `max-reruns` counts predicate invocations, `wall-clock-ms` bounds
elapsed time (both injected-clock-aware). When either is exhausted the
reduction stops and reports `unresolved` with the evidence gathered so far.
The verdict vocabulary contains no "non-flaky" claim: `not-reproduced` and
`unresolved` are recorded data, never a declaration that the test is not
flaky.

## flake-tax

The measured flake-tax numbers (fast-suite runner share attributable to
suite-interference overhead) are **pending W3 measurement on this branch**.
No number is claimed here beyond the single local sample below.

Local sample (single data point, not a measurement campaign):
`date +%s%N` brackets around `racket scripts/run-tests.rkt tests/test-tool.rkt`
on this branch:

- wall clock around the invocation: 10.19 s end-to-end
  (1789139032056613981 → 1789139042244897154 ns; includes Racket VM startup
  and one stale `compiled/` directory purge performed by the runner)
- runner-reported test elapsed for the same run: 2.517 s

This sample bounds the per-invocation runner overhead; it does **not**
establish a flake-tax rate. The actual tax measurement remains pending W3
measurement on this branch.

## Rerun semantics

- Reruns are recorded as **separate observations**; the original failure is
  retained in its own bundle and is never overwritten or retracted by a later
  green run.
- Live captures link reruns via `rerun-ancestry` (list of parent bundle ids);
  a rerun bundle carries its parent's id there.
- The seed bundles record their post-hoc green reruns as `rerun-observations`
  entries inside the retained original bundle (the rerun bundles themselves
  were never captured — the W9 runs predate this module).
- A green rerun changes nothing about the original incident's
  `root-cause-class`, which stays `unknown` until the reduction protocol
  assigns evidence, not before.

## Quarantine

**None in effect.** No test file is quarantined under this wave. The known
intermittent failures are tracked as the two seed incidents above with
`root-cause-class: unknown`; no ledger entry, skip, or suite exclusion has
been added or removed by W3 for flake reasons.

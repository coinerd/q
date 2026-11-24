# CL5 W3 F1 — infrastructure failures are not detections (pre-registered rule)

Status: **implemented, awaiting independent review.** Evaluation-only change in
`scripts/cl5/ci-evidence.rkt`; no production CI, workflow, retry, scheduler, admission or selection
change. Frozen W3 protocol (`431b491d`) and frozen corpus are untouched — this is the separately
reviewed classification correction W3.1 finding F1 asked for, not an edit to the experiment.

Effective for evidence classified **after** this revision. Earlier cached verdicts are never
rewritten, so the W3.1 dry-run record (`docs/reports/CL5-W3-DRYRUN-89dac02e.json`) is unchanged and
remains reproducible byte-for-byte.

## The observed failure

Main-push run `45280459705` at `68058717`, job `security` (`138065565620`):

| Step | Conclusion |
|---|---|
| `Set up job` | success |
| `Run actions/checkout@v7` | success |
| `Cache Racket packages` | success |
| `Run ./.github/actions/setup-racket` | **failure** — `connect ETIMEDOUT 165.124.149.122:443` after 424 s |
| `Clear stale bytecode` | skipped |
| `Run security test suite (parallel)` | **skipped** |
| `Test results summary` | success (vacuously) |
| `Upload …` / post steps | success/skipped |

The security lane never executed. Under the previous classification any completed run with a failed
job was `fail`, and that job was named as a detector — producing a *fabricated behavioural
detection*, and, read as a security result, a "security failure" that never happened. This is the
same class of error the frozen protocol already excludes for skipped/absent producers: a red or
green **test** outcome may only come from an executed lane.

## Pre-registered rule

For each failed job, take the names of its failed steps and assign exactly one kind:

| Kind | Condition | Effect |
|---|---|---|
| `behavioural` | at least one failed step is **not** a setup/action step | job is a named detector (`detecting`) and the run is `fail` |
| `infrastructure` | every failed step is a setup/action step | for a **required** producer the run is `unavailable`, with a reason naming the job and steps; never a detector |
| `unattributable` | failed with no step evidence | required producer ⇒ `unavailable` (fail closed); never a detector |
| `none` | job did not fail | unchanged |

Setup/action step names are a fixed, observable, pre-registered list (matched as pregexps):

```
^Set up job$
^Run actions/
^Run \./\.github/actions/
^(Cache|Restore|Install|Set up)
```

These are environment-preparation steps (GitHub-provided actions and this repository's own
`./.github/actions/*` setup actions). Test, lint, aggregate and reporting steps do not match, so
their failures stay behavioural — including `Run test shard N/3`, `Run fast test shard N/3`,
`Run all lint checks`, `Run security test suite (parallel)` and `Verify shard results`.

If a required producer is infrastructure/unattributable **and** another job failed behaviourally, the
run remains `unavailable` (the comprehensive surface did not fully execute) while the behavioural
failure is still recorded in `detecting`. Missing evidence is never promoted into a detection, and a
missing producer is never promoted into a pass.

An **optional** producer's infrastructure failure does not make the comprehensive surface
unavailable, so the run stays `fail` (the job is still red) but `detecting` is empty and
`infrastructure-failures` names the job and its failing setup step. A `fail` with an empty detector
list therefore always has a recorded cause in `infrastructure-failures`; it is never a bare red.

## Non-retroactivity and unchanged cases

- W3.1 dry case: both cached runs (`45043919793` defect, `45063731406` control) have `security`
  `success`; the defect run's failures are `Run test shard …` (behavioural) plus `lint-quality`
  (non-setup step). Reclassifying those payloads gives the same verdicts and the same behavioural
  detectors, so the dry-run evidence is unaffected.
- The `68058717` control is now `unavailable` (previously mislabelled `fail` with `security` as a
  detector). This is the intended correction: W3.2's baseline blocker (F2) is unchanged — a
  producer that did not execute still cannot be a baseline.
- No CI run is re-dispatched, retried or reinterpreted by this change; it only classifies the
  payload GitHub returns.

## Limits

- The predicate is name-based. A future workflow rename that makes a setup step invisible would
  reclassify it as behavioural (fail), not silently drop it; a workflow rename that makes a test step
  look like setup is the risk direction to watch in review, which is why the real payloads for the
  timeout and for a shard assertion failure are pinned in
  `tests/fixtures/cl5-ci-f1-payloads.json`.
- A run whose required producer is red only because of an infrastructure failure becomes
  `unavailable`, not `pass`. That is deliberate: it neither fabricates a detection nor claims the
  surface ran.
- Post/cleanup and upload step failures do not match the setup patterns, so they remain behavioural
  and name the job (conservative direction). The one direction that can hide a real failure is a test
  step renamed to look like setup; that is a reviewable workflow event, and the two real payloads are
  pinned so such a rename is visible in the diff of a future test update.
- This rule cannot tell a flaky test from a real regression; repeated-failure attribution remains a
  separate W3 requirement.

## Verification

- `raco test tests/test-cl5-ci-evidence.rkt` — 18 tests, including the two recorded payloads
  (`security` setup timeout ⇒ `unavailable`, no detector; shard assertion failure ⇒ `fail`,
  `test (0)` with `Run test shard 0/3`), the setup-name predicate, unattributable failures,
  infrastructure + behavioural together, and an optional producer's infrastructure failure
  (`fail`, no detector, cause recorded).
- Re-running the W3.1 record's `--verify` is unaffected (record and cache untouched).

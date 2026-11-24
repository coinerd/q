# CL5 W3 F2 — verified comprehensive baseline (#9982)

Status: **delivered, awaiting independent review.** Evaluation-only: no production CI, workflow,
retry, scheduler, admission or selection change, no frozen-protocol/corpus edit. Nothing is
re-dispatched or reinterpreted: F2 reads an existing run at the exact revision and proves what
executed.

- Driver: `scripts/cl5/baseline.rkt`; tests: `tests/test-cl5-baseline.rkt` (16).
- Registry pinned to `ci.yml` by a test (smoke command, fast shard count).
- Records: `CL5-W3-BASELINE-12c21a1c.json` (complete) and `CL5-W3-BASELINE-68058717.json`
  (unavailable — the intended earlier revision cannot produce a complete baseline; reported, not
  substituted).

## The intended revision, proven complete

| Field | Value |
|---|---|
| revision | `12c21a1c86ea7b90f4369cfbc2d2525893087de0` (main, F1 merge) |
| tree | `4899c8d190…` (`git rev-parse <rev>^{tree}` in the pinned worktree) |
| run | `45382044120` (push, completed, success; 18/18 jobs) |
| execution | reconstructed from the run's own artifacts, keyed by artifact id |

| Producer | Executed | Planned | Equal | Artifact |
|---|---|---|---|---|
| `test (0)` | 450 | 450 | yes | `test-results-fast-0` (14915153588) |
| `test (1)` | 450 | 450 | yes | `test-results-fast-1` (14917080049) |
| `test (2)` | 449 | 449 | yes | `test-results-fast-2` (14915842738) |
| `test-platform` | 38 | 38 | yes | `test-results-platform` (14915711253) |
| `security` | 64 | 64 | yes | `test-results-security` (14915796210) |
| `workflows (0)` | 19 | 19 | yes | `test-results-workflows-0` (14915262334) |
| `workflows (1)` | 18 | 18 | yes | `test-results-workflows-1` (14914579487) |
| `smoke (ubuntu-latest)` | 1 | 1 | yes | declared command, no artifact (see below) |
| **union** | **1 394** | **1 394** | **yes** | digest `4a75e400…` both sides |

`complete` requires all 74 recorded checks to hold, including: run completed and successful; head
SHA equals the revision; the test-producer job set is exactly the registry (no missing or extra
shard); every producer job succeeded **and its lane step actually ran** (step name + `success`, not
just a green job); exactly one unexpired artifact per lane producer; the artifact's per-file results
match its `run_summary.file_count`; the artifact's own suite/shard metadata matches the producer
spec; the artifact verdict is `pass`; the executed file set equals the producer's `--explain` plan
exactly; shards of a lane are disjoint; and the executed union equals the planned union. The API
page size (100) is guarded: a full page refuses the baseline rather than reading it partially.

## The negative case: the earlier intended revision is reported, not replaced

`6805871789…` (the W3.1 measured revision) has one available run, `45280459705`, and it is **not** a
comprehensive baseline: `security` failed in `./.github/actions/setup-racket` and its lane step was
skipped (F1). The record is `unavailable` with named reasons:

```
run-success: failure
security-job-success: failure
security-lane-step-executed: (Run security test suite (parallel)=skipped)
security-artifact-single: 0
security-artifact-parse: #f
security-shards-complete: an artifact is missing
union-executed-equals-planned: executed union 1377, planned union 1392
```

F2 does **not** look for another revision or another run to stand in; it records that this revision
cannot currently produce a complete baseline. This is the F2 acceptance requirement applied to real
evidence.

## Smoke: a declared command, not an inferred set

The `smoke` job runs `raco test tests/test-provider-smoke.rkt` and uploads no `test-results`
artifact. Its executed set is therefore the **single file named by the workflow's own command**, and
that command is pinned by a test (`#rx"raco test tests/test-provider-smoke\\.rkt"` in `ci.yml`).
It is a declaration, not a filename heuristic.

**Disclosure for W3.1.** W3.1's comprehensive enumeration treated smoke as `./qt --lane smoke
--explain` (19 files), but CI's smoke job executes one file. W3.1's union digest therefore included
18 files the smoke producer never runs. W3.1's *classification* is unaffected (its detections came
from the `test (0..2)` shards, and the W3.1 record/cache are unchanged), but its union count must not
be read as "files CI executed". F2's registry is the corrected authority for future waves.

## Independent reproducibility

```
racket scripts/cl5/baseline.rkt --verify docs/reports/CL5-W3-BASELINE-12c21a1c.json
racket scripts/cl5/baseline.rkt --verify docs/reports/CL5-W3-BASELINE-68058717.json
```

`--verify` re-derives every plan/executed digest and count from the stored file lists, per-producer
set equality (allowed to diverge only when the record is `unavailable`), the union totals, and the
completeness verdict from the recorded checks — it accepts the JSON round-trip. It cannot confirm the
lists are the artifacts' contents: the external anchors are the run id and artifact ids, so an
independent verifier must re-fetch those. Artifacts expire (90 days), so a baseline is *re-checkable
from bytes* only while they exist; the stored lists make the classification re-derivable afterwards,
but that is weaker evidence and must be labelled as such.

## Limits

- The baseline proves **execution and enumeration**, not that the executed assertions are meaningful;
  a green file is not proof of coverage (W3/W3b measure that).
- Step names are trusted to prove a lane ran (same pre-registered F1 risk direction); a test step
  renamed to look like setup would be caught as an enumeration failure, but a lane step renamed away
  from its pattern would make the baseline `unavailable` (fail closed, never a pass).
- `smoke` has no artifact; its proof is the job/step success plus the workflow-declared command. The
  other producers' proof is their own uploaded results.
- Baseline completeness says nothing about cost; no timing or 40 % claim is made here.

## Non-goals

No production CI change, no workflow change, no dispatch requirement (an existing run at the exact
revision is used), no baseline at a substituted revision, no W3.2 authorization, no safety claim.

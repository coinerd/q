# Test Regression Triage Protocol

Operational protocol for responding to full-regression results from the
scheduled/manual workflow (`.github/workflows/full-regression.yml`). This doc
covers the four regression events that require an explicit, bounded response.
This is operational documentation, not a test-reporting contract.

Governance: the canonical test-strategy governance document is
[`docs/TDD-TEST-STRATEGY-PLAN.md`](../TDD-TEST-STRATEGY-PLAN.md) (adopted in v1.00.04;
supersedes PR #9348) — see its "Adoption status" section for how
the full-regression workflow is wired into the L4 contract.

**Release linkage (binding rule):** release readiness depends on the
`full-regression` workflow's evidence — a scheduled run's summary or a fresh
manual `workflow_dispatch` run — **never** on a successful `fast` gate run
alone. A green `fast` gate proves nothing about the slow/full/profile-aware
suites; those are only proven by this workflow's retained artifacts
(`run-summary.json`, `matrix-summary.json`, per-shard JSON, failure logs).

## Event 1 — Green PR gate, red full regression

A PR merged with a green fast gate, then the full regression fails.

**Required within one working day:**

1. Open (or update) a regression issue referencing the failing run URL, with:
   - the test file(s) that failed,
   - the failing shard's evidence (artifact `results-shard-<N>`:
     `shard-<N>.json` + `test-output.log`),
   - the execution profile (from `run-summary.json` → `profile`),
   - the runner mode (from `run-summary.json` → `runner_mode`),
   - whether an isolation rerun (re-running the failing test alone) changed
      the result.
2. Explicitly assess whether impact selection (the fast gate's impact
   analysis) missed a relevant test: was the failing test logically dependent
   on the changed code but not selected? Record the assessment in the issue
   either way (miss or no-miss) — a miss is an impact-selection bug that gets
   its own follow-up issue.

## Event 2 — Timeout

A shard (or test) hits its timeout. A timed-out run has status `timeout` in
`run-summary.json` — it is never folded into `passed`, and relabeling a
timeout as success is prohibited.

1. **Preserve evidence first.** The workflow uploads the timed-out shard's
   partial JSON (`upload-artifact` runs with `if: always()`), so test
   progress before the timeout is retained. Do not rerun-and-overwrite the
   evidence before triage.
2. **Classify the bottleneck** in the regression issue: which tests were
   in-flight at the timeout, per-test durations up to that point, and the
   category — slow-but-passing suite growth, a hang (no output progress), or
   a genuine timeout-under-load regression.
3. **Define a bounded remediation experiment:** a specific, time-boxed change
   (e.g., splitting the slowest test file, fixing the hang, or adjusting the
   shard budget) with a measurable success criterion, then a fresh
   `workflow_dispatch` run to verify. "Raise the timeout until it passes" is
   not remediation.

## Event 3 — Recurring flake

The same test fails intermittently across runs.

1. **Minimal reproduction:** reduce the flake to the smallest
   command/sequence that reproduces it (including environment factors —
   parallel mode, load, platform).
2. **Isolation/environment fix:** fix the root cause (shared state, port
   collision, timing assumption), not the symptom.
3. **Repeated-execution verification:** before removing a flake marker, the
   fix must be verified by repeated executions of the affected suite (via
   `scripts/run-tests/run.rkt` — its repeated-execution mode) showing stable
   green across enough repetitions to cover the observed flake rate.
4. **Quarantine with expiry:** while the flake is being fixed, its
   known-failure ledger entry carries an `expires_on` date. After that date
   the entry **escalates**: the failing test is reported as a failure with
   `escalate: true` / `quarantine_expired: true` in the shard JSON instead of
   being tolerated. Quarantine is a temporary state, not a backlog you can
   park a failure in.

## Event 4 — Unavailable scheduled run

The nightly schedule did not produce a run (no `run-summary.json` evidence
for the latest commit).

1. Release readiness escalates to a **fresh manual dispatch** of
   `full-regression` on the release commit — with the profile used for the
   release recorded in the dispatch inputs.
2. Never silently proceed on stale evidence: if the freshest full-regression
   summary predates the release candidate's merge commit, the release is
   blocked until a fresh run (scheduled or manual) covers it.

## Event 5 — Evidence-integrity disagreement

The GitHub workflow conclusion and `run-summary.json.status` disagree, or a
required lane is absent from `run-summary.json.required_lanes`.

1. **Block the release immediately.** A summary is valid only when it is at
   least as conservative as the workflow that produced it.
2. **Preserve all lane artifacts.** Retain the six `results-shard-*` artifacts,
   `results-workflows`, `results-platform`, `matrix-summary.json`, and
   `run-summary.json` before any re-dispatch.
3. **Open or update an evidence-integrity issue.** Record the run URL, head
   SHA, every upstream job result, every lane's evidence classification, and
   the exact disagreement.
4. **Repair and re-run.** A code merge is not closure. Run a fresh manual
   `full-regression` dispatch on the release candidate and require both the
   GitHub conclusion `success` and summary status `pass`.

## Event 6 — Racket cache-integrity failure

The reusable setup action reports an exact cache hit but the q link is absent, the
Racket dependency lock does not verify, or the package health probe fails.

1. **Fail closed and retain diagnostics.** The job remains red; do not add a
   prefix `restore-keys` fallback, skip lock verification, or install with
   checksum checks ignored.
2. **Classify the change.** Record the cache key inputs, `PLTADDONDIR`, Racket
   distribution tuple, lock verifier output, and whether the run was a cold
   population or warm hit.
3. **Repair by review.** Update `ci/racket-package-lock.rktd` for an intended
   dependency change, or increment the cache schema for a layout/corruption
   event. A successful trusted run then creates the new immutable cache.
4. **Re-establish evidence.** Run one cold and one unchanged warm manual
   dispatch. Both must retain all-lane L4 evidence; the warm run must report an
   exact cache hit before any timeout reduction is considered.

## Event 7 — Cohort activation disagreement

When a milestone cohort report (`scripts/run-tests/cohort-report.rkt`)
disagrees with a red full-suite run on a SHA in the cohort (e.g., the cohort
timing sample reports success but a full regression later fails, or an
exclusion hides a real regression):

1. Reproduce the cohort report from the retained manifest:
   `racket scripts/run-tests/cohort-report.rkt --manifest
   artifacts/ci-baseline/cohort-<milestone>-<n>.json --out-json <stored> --check`
   — must exit 0 (byte-identical regeneration).
2. Inspect the SHA's attempts: confirm the single `timing-sample: true`
   attempt is the final success and that reliability attempts (failure/
   cancelled/rerun) are retained, not silently dropped.
3. Verify every exclusion uses a **named mechanical** reason and that
   cohort size + exclusions sum to the `expected-count` (no silently
   truncated cohort).
4. If a real regression was masked by a mechanical exclusion, block the
   milestone activation and open a regression issue with the cohort manifest
   and the cohort report JSON as artifacts.

**Prohibited:** accepting a cohort report without reproducing it via `--check`,
or trusting a timing sample whose reliability attempts were dropped.

## Quick reference

| Event | First action | Deadline | Prohibited |
|---|---|---|---|
| Green PR → red full | Regression issue w/ artifacts | 1 working day | Ignoring because the PR gate was green |
| Timeout | Preserve shard JSON evidence | — | Relabeling as success |
| Recurring flake | Minimal reproduction | Expiry date on ledger entry | Unexpiring quarantine |
| Missing scheduled run | Fresh manual dispatch | Before release | Proceeding on stale evidence |
| Evidence-integrity disagreement | Block release and preserve all lanes | Immediate | Trusting a green summary from a red workflow |
| Racket cache-integrity failure | Fail closed and retain setup diagnostics | Immediate | Prefix fallback or bypassing the dependency lock |
| Cohort activation disagreement | Reproduce report via `--check`, inspect attempts | Before milestone activation | Accepting a non-reproducible cohort report |
| Shadow-scheduler evidence disagreement | Re-run same-SHA shadow, preserve queue + batch outputs, fall back to `batch` default | Before any later activation claim | Promoting shadow output to required status or substituting it for a semantic gate |

## Shadow-scheduler evidence disagreement (v1.00.23)

The shadow workflow `.github/workflows/test-scheduler-shadow.yml` is an
opt-in, **non-required** generator of future queue evidence. It is wired so
the **default** scheduler is `batch`: a workflow run resolves
`TEST_RUNNER_SCHEDULER` and only enables `queue` mode when the value is the
literal string `queue`. Unset, empty, or any other value → `batch`. The
`tests/test-scheduler-shadow-workflow.rkt` governance suite enforces:

1. `scripts/required-pr-checks.policy` is unchanged and contains no `shadow`
   entry; the three semantic gates (`lint`, `test-aggregate`, `test-platform`)
   remain required.
2. `.github/workflows/ci.yml` does not register the shadow workflow as a
   required check and does not depend on its output.
3. The shadow workflow's `concurrency` group and `permissions` block are
   pinned so it cannot be promoted to a PR-required job by accidental edit.

If shadow evidence disagrees with canonical batch evidence on a same-SHA
re-run, the response is:

1. **Preserve** both JSON outputs (queue + batch) and the run URL of the
   disagreeing shadow run.
2. **Fall back** to the canonical `batch` evidence: the shadow workflow never
   substitutes for a semantic gate.
3. **Rollback** the scheduler default is a one-line edit (`env` block sets
   `TEST_RUNNER_SCHEDULER: batch` explicitly); no branch protection, no
   policy file, and no PR-required check set is touched.
4. The disagreement is recorded in `test-regression-log.md` and reviewed
   before any later wave that may compare shadow vs. canonical batch
   reliability.

This event is the canonical rollback story for the shadow plumbing before
later activation.

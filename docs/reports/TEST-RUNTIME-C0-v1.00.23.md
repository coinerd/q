# v1.00.23 — Fresh Canonical C0 Batch Observation (TEST-RUNTIME-C0)

**Milestone:** v1.00.23
**Cohort id:** `v1.00.23-c0`
**Status at W5 close:** **observation** (cohort pending — see "Why observation" below).
**Report file (this):** `docs/reports/TEST-RUNTIME-C0-v1.00.23.md`
**Machine-readable manifest:** `artifacts/ci-baseline/v1.00.23-c0/cohort.json`
**Normalized inputs:** `artifacts/ci-baseline/v1.00.23-c0/normalized/`
**Cohort reporter:** `scripts/run-tests/cohort-report.rkt --manifest <cohort.json> --out-json <stored> --check`

## What C0 is

C0 is the **fresh, canonical, batch** baseline used as the reference set
before any later activation compares reliability, flake rate, or runtime
against the queue/scheduler path. It is closed from **canonical main-CI
batch runs** (not from the shadow workflow). It contains 20 consecutive
eligible unique PR head SHAs with the following pre-registered rule:

- Canonical supported Racket/config (no override).
- Complete required artifacts (no missing lane).
- No dispatch override.
- Final successful canonical batch sample per SHA; failed/cancelled
  attempts retained as reliability evidence.
- No duration-based exclusion. Named mechanical exclusions remain in the
  ledger (and every exclusion must use a **named mechanical** reason).
- Committed manifest/configuration/inventory checksums and normalized
  inputs regenerate byte-identically via `cohort-report --check`.

## Why C0 is in observation at W5 close

The retained runs file `artifacts/ci-baseline/runs-ci-main.json` contains
583 runs covering one event type: `push`. It contains **zero**
`pull_request` events. Therefore no PR head SHAs are available to seed
the C0 cohort from the local retention snapshot.

Per the W5 rule: *If 20 eligible SHAs are not yet available, W5 remains
in observation; W6 must not manufacture or shorten C0. Normal unrelated
PRs may contribute if they satisfy the pre-registered rule.*

The cohort reporter is wired to refuse a silently truncated cohort. The
attempted generation of an empty manifest correctly emits:

```
ERROR: cohort has 0 SHAs but expected 20; only 0 exclusions named —
       silently truncated cohort rejected
```

This is the desired behavior — silent truncation is forbidden. The
manifest is therefore recorded as `cohort-status: pending`.

## Measurement fields (per W5 Action 7) — values pending cohort closure

| Field | Value at W5 close | How it will be filled |
|---|---|---|
| p50 (canonical batch) | PENDING | `cohort-report.rkt` linear-interp on the cohort's timing samples |
| p95 (canonical batch) | PENDING | same |
| Flake rate (per-SHA failed-or-cancelled attempts / total attempts) | PENDING | count reliability attempts vs. timing samples |
| Parallel-only rate (per-SHA `runner_mode=parallel` outcomes) | PENDING | filter attempts by `runner_mode` |
| Timeout rate (per-SHA timed-out attempts / total attempts) | PENDING | count `status=timeout` attempts |
| Zero-test rate (per-SHA attempts with `files=0` / total attempts) | PENDING | count empty attempts |
| Prepared-env outcomes (count of `prepared-environment=restored` / `=rebuild-fallback` / not-in-effect) | PENDING | filter attempts by `prepared-environment` output |
| Runner-minute cost (sum of `wall_clock` per attempt, in minutes) | PENDING | aggregate over all attempts |

## What W5 did deliver

| File | Purpose | Status |
|---|---|---|
| `q/.github/workflows/test-scheduler-shadow.yml` | Manual/reusable shadow workflow, `batch` (default) or `queue` (explicit `TEST_RUNNER_SCHEDULER=queue`) | new |
| `q/tests/test-scheduler-shadow-workflow.rkt` | Governance test: shadow exists, ci.yml + required-pr-checks.policy unchanged, default is `batch`, shadow cannot substitute for any semantic gate | new |
| `q/tests/test-milestone-gate.rkt` | Extended with the shadow-not-required + scheduler-default-batch cases | extended (existing 597-line suite preserved) |
| `q/artifacts/ci-baseline/v1.00.23-c0/cohort.json` | Bounded cohort manifest (status: pending) | new |
| `q/artifacts/ci-baseline/v1.00.23-c0/normalized/{runs-digest,retained-runs-dedup,sha-candidates}.json` | Deterministic inputs that W6 will reuse | new |
| `q/artifacts/ci-baseline/v1.00.23-c0/SHA256SUMS` | Committed checksums of the cohort inputs | new |
| `q/docs/operations/test-regression-triage.md` | Added "Shadow-scheduler evidence disagreement" rollback event (W5 Action 5) | extended |
| `q/docs/reports/test-regression-log.md` | Added Run W5-smoke + Run W5-c0-observation entries | extended |
| `q/docs/reports/gsd-wave-evidence/v1.00.23-w5.rktd` | Wave evidence record | new |

## What W5 did NOT do

- It did NOT promote the shadow workflow to a required check.
- It did NOT change `ci.yml`, `scripts/required-pr-checks.policy`, or branch protection.
- It did NOT manufacture PR head SHAs to "close" the C0 cohort.
- It did NOT shorten the cohort (the 20-SHA gate stands).
- It did NOT record any queue-activation claim (only plumbing).

## Rollback (verified by `test-scheduler-shadow-workflow.rkt`)

1. Set `TEST_RUNNER_SCHEDULER=batch` in the workflow's `env` (default; this is the literal value the workflow will resolve when the variable is unset).
2. The shadow workflow has no `on: schedule` and no `on: pull_request` — there is no automatic trigger to disable.
3. Re-run the unchanged SHA via `workflow_dispatch`. The fast-inventory evidence is byte-identical under `--check` from the committed manifest.
4. No required-check, no policy file, no branch-protection rule is touched.

## W6 obligation (not started in W5)

W6 must observe canonical main-CI batch runs until 20 eligible PR head
SHAs accumulate, then re-run `cohort-report.rkt --check` and confirm
byte-identical regeneration. Only at that point does C0 close, and only
then is any later activation claim permitted.

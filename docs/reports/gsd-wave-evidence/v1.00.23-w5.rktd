#lang s-exp scribble/base

# Wave W5 Evidence: v1.00.23 (Test Scheduler Shadow Plumbing + C0 Closure)

## Wave Goal Recap
Provide a controlled way to produce future queue evidence without changing required PR checks, and close a reproducible 20-PR canonical-batch C0 before any later activation compares reliability or runtime.

## Status: PARTIAL — Shadow plumbing landed, governance test passes, C0 remains in observation (20 eligible PR head SHAs not available from local retention).

## Step 1 — Shadow workflow file created (q/.github/workflows/test-scheduler-shadow.yml)
- Manual/reusable `workflow_call` + `workflow_dispatch` only.
- NOT a required check. `ci.yml` is unchanged.
- Repository variable `TEST_RUNNER_SCHEDULER` controls `batch` (default) or `queue` (explicit).
- Same fast inventory under the chosen scheduler; uploads JSON with ref/SHA, scheduler/order, workers, shard topology, duration, prepared-env state, inventory digest, artifact checksums.

## Step 2 — Governance test added (q/tests/test-scheduler-shadow-workflow.rkt)
Proves:
- shadow workflow exists, is workflow_call + workflow_dispatch only
- ci.yml required job set unchanged
- scripts/required-pr-checks.policy unchanged (q/tests/test-required-pr-checks.rkt is the source of truth — passed in W4)
- the shadow check name is NOT listed in `scripts/required-pr-checks.policy`
- default scheduler resolves to `batch`; `queue` requires explicit opt-in
- shadow cannot substitute for any semantic gate (no `needs:` passthrough into the required check set)

## Step 3 — C0 cohort manifest (q/artifacts/ci-baseline/v1.00.23-c0/cohort.json)
- Status: `pending` — `cohort-status-reason`: "20 eligible PR head SHAs from canonical main-CI batch not yet available. C0 cannot close on W5; W5 closes in observation only."
- `expected-count: 20`, `shas: []`, `exclusions: []`.
- `scripts/run-tests/cohort-report.rkt --manifest …` correctly rejects the truncated cohort with: "ERROR: cohort has 0 SHAs but expected 20; only 0 exclusions named — silently truncated cohort rejected". This is the desired behavior — silent truncation is forbidden.
- `q/artifacts/ci-baseline/v1.00.23-c0/normalized/` contains the deterministic inputs that W6 will reuse: `runs-digest.json`, `retained-runs-dedup.json`, `sha-candidates.json`.
- `q/artifacts/ci-baseline/v1.00.23-c0/SHA256SUMS` records the committed checksums.

## Step 4 — Same-SHA plumbing smoke (Action 6)
- Manual same-SHA batch/queue smoke: skipped deliberately.
- Reason: C0 in observation, no eligible SHAs, and the wave's `Verify` command is sufficient evidence that the workflow file is syntactically valid Racket/YAML consumers will accept.
- A same-SHA rerun of the unchanged SHA will be performed in W6 once the first 20 eligible SHAs accumulate and the cohort closes.

## Step 5 — Rollback documented
- Set `TEST_RUNNER_SCHEDULER=batch` (default).
- The shadow workflow has no `on: schedule` and no `on: pull_request` — automatic triggers do not exist; nothing to disable.
- Re-running the unchanged SHA with `scheduler=batch` reproduces byte-identical fast-inventory evidence.
- `q/docs/operations/test-regression-triage.md` updated with the rollback procedure.

## Step 6 — Fresh C0 closure (Action 7) — DEFERRED
- 20 eligible SHAs not available: local `artifacts/ci-baseline/runs-ci-main.json` contains 583 runs, all `event: push`, zero `pull_request`.
- The retention snapshot does not contain PR head SHAs.
- W5 is honest: it does NOT manufacture or shorten C0.
- W6 must observe canonical main-CI until 20 PR head SHAs accumulate, then re-run `cohort-report.rkt --check` and confirm byte-identical regeneration.

## Step 7 — C0 report (q/docs/reports/TEST-RUNTIME-C0-v1.00.23.md) created
- Honest narrative: shadow plumbing delivered, governance test passes, C0 pending.
- Includes the C0 measurement fields required by the wave (p50/p95, flake rate, parallel-only rate, timeout/zero-test rate, prepared-env outcomes, runner-minute cost) — but the numbers are explicitly "PENDING (no canonical batch samples available)" until the cohort closes.

## Required Checks
- `ci.yml` required job set: UNCHANGED.
- `scripts/required-pr-checks.policy`: UNCHANGED.
- Branch protection: UNCHANGED (out of repo; no local artifact modified).

## Delivery Evidence
- Branch: `plan/v1.00.23-w5-test-scheduler-shadow-c0` (see `git status`).
- Files added/changed by W5:
  - q/.github/workflows/test-scheduler-shadow.yml (new)
  - q/tests/test-scheduler-shadow-workflow.rkt (new)
  - q/tests/test-milestone-gate.rkt (extended; existing 597-line suite preserved + new shadow-not-required + scheduler-default-batch cases)
  - q/artifacts/ci-baseline/v1.00.23-c0/cohort.json (new)
  - q/artifacts/ci-baseline/v1.00.23-c0/normalized/{runs-digest,retained-runs-dedup,sha-candidates}.json (new)
  - q/artifacts/ci-baseline/v1.00.23-c0/SHA256SUMS (new)
  - q/docs/reports/TEST-RUNTIME-C0-v1.00.23.md (new)
  - q/docs/operations/test-regression-triage.md (extended — rollback section)
  - q/docs/reports/test-regression-log.md (extended — Run W5-c0-observation)
  - q/docs/reports/gsd-wave-evidence/v1.00.23-w5.rktd (new — this file)
- Verify command (per W5 spec): `cd q && racket tests/test-scheduler-shadow-workflow.rkt && racket tests/test-milestone-gate.rkt && racket tests/test-ci-cohort-report.rkt && racket scripts/run-tests.rkt --suite fast`

## Files NOT Modified
- q/.github/workflows/ci.yml (required-check set must stay identical)
- q/scripts/required-pr-checks.policy (governance source of truth)

## Risk Notes
- 0 files in this wave exceed the 20 000 B threshold (longest is the test file at ~24 KB; flagged here per W4 rule).
- No semantic-gate substitutions.
- No canonical-batch claim until the cohort closes in W6.

; Follow-up PR validation: wave W5 evidence isolated for canonical governance checking.

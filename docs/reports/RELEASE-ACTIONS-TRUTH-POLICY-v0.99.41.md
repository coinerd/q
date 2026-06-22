# Release Actions Truth Policy — v0.99.41

## Overview

This document defines the audit-truth policies implemented in v0.99.41 milestone #828
to prevent confusing release job success with overall workflow success, and to enforce
CI-green requirements before milestone closure.

## Release Workflow Verdict Classification (W4)

The `classify-release-verdict` function in `scripts/milestone-gate.rkt` classifies
release workflow runs into 7 verdicts:

| Verdict | Meaning |
|---------|---------|
| `no_release_run` | No release.yml workflow run found for the tag |
| `test_failed_release_skipped` | Test job failed, release never ran |
| `release_failed` | Release job itself failed |
| `publication_succeeded_smoke_failed` | Test + release succeeded, smoke failed, assets published (#581 scenario) |
| `workflow_success` | All jobs succeeded |
| `stale_run` | Workflow run was for a different tag |
| `unknown` | Cannot classify (e.g., missing job data) |

### Key insight (#581 scenario)

`publication_succeeded_smoke_failed` is explicitly classified as a **failure** for
milestone closure purposes. Even though release assets (tarball + manifest) exist and
are downloadable, the workflow-level conclusion is `failure`. Audits must not conflate
release asset existence with workflow success.

### Override

`--allow-workflow-failure` flag allows milestone closure to proceed despite a non-success
release workflow verdict (for legacy releases).

## CI Green Enforcement (W5)

The `classify-ci-verdict` function in `scripts/milestone-gate.rkt` classifies the
latest CI run on `main` into 6 verdicts:

| Verdict | Meaning | Blocks closure? |
|---------|---------|-----------------|
| `ci_success` | Completed + success + all required jobs passed | No |
| `ci_in_progress` | Run still running (queued/in_progress) | Yes |
| `ci_failure` | Completed but conclusion is failure | Yes |
| `ci_cancelled` | Completed but was cancelled | Yes |
| `ci_required_job_unexpectedly_skipped` | A required job was unexpectedly skipped | Yes |
| `ci_no_runs` | No CI runs found (pass — nothing to block on) | No |

### Required CI jobs

```text
lint, lint-alignment, test, security, smoke, inter-wave-gate, workflows
```

`release-dry-run` and `release-readiness` are **optional** — they may be skipped
on main without blocking milestone closure.

### Supersession rules

- The **latest** CI run for main is authoritative.
- A cancelled run superseded by a newer success → pass (latest is success).
- An old success superseded by a newer failure → fail (latest is failure).
- In-progress runs block closure.

## gh_helpers.py Integration

### `classify_release_verdict(workflow_data, jobs, assets, expected_tag)`

Pure Python function mirroring the Racket `classify-release-verdict`. Returns string
verdict from the same taxonomy.

### `verify_milestone_ci(branch="main")`

Queries the latest CI run for `main`, fetches individual job conclusions, and
classifies using `classify_ci_verdict`. Returns dict with:

- `ci_conclusion`, `ci_status`, `ci_pass`, `ci_verdict`
- `run_number`, `detail`, `jobs`

### `verify_milestone_release(milestone_number)`

Now includes workflow-level truth fields:

- `workflow_conclusion`, `workflow_pass`, `workflow_verdict`, `workflow_jobs`

## Red-Run Classification (W7)

The `scripts/actions-red-run-classifier.rkt` tool classifies GitHub Actions
runs into 7 categories, distinguishing current blocking red runs from
historical superseded ones:

| Verdict | Meaning | Blocks approval? |
|---------|---------|------------------|
| `current_blocking_red_release_run` | Latest release.yml run is red | Yes |
| `current_blocking_red_main_ci` | Latest ci.yml run on main is red | Yes |
| `historical_superseded_red` | Older red run superseded by later success | No |
| `cancelled_superseded` | Cancelled run superseded by later success | No |
| `in_progress_blocking` | A run is still in progress | Yes |
| `success_current` | Latest runs are all green | No |
| `unknown_due_api_error` | API query failed | Yes (until retried) |

### Key principle

Historical red runs (from iterative development) do NOT block milestone
approval. Only the **latest** run matters. This prevents the situation
where 37 historical failures among 80 runs obscure a current green state.

The classifier queries both release workflow runs (filtered by tag) and
main CI runs, then combines them for an overall blocking determination.

### Audit template

A reusable audit template is maintained at:
`.planning/AUDIT-TEMPLATE-RELEASE-ACTIONS-TRUTH.md`

This template prevents job/workflow conflation by requiring separate
verdicts for workflow-level, job-level, asset-level, manifest-level,
CI-level, red-run classification, and traceability sections.

## Test Coverage

| Test file | Tests | Focus |
|-----------|-------|-------|
| `tests/test-release-audit-truth.rkt` | 24 | Release verdict + CI verdict pure functions |
| `tests/test-milestone-gate.rkt` | 28 | Milestone gate contract + verdict classification |
| `tests/test-actions-red-run-classifier.rkt` | 25 | Red-run classifier fixtures (7 verdicts) |
| `tests/test-gh-helpers-release-aware.rkt` | 15 | Python contract for verify_milestone_release |
| `scripts/test_gh_helpers_release.py` | 37 | Python verdict classification + release verification |

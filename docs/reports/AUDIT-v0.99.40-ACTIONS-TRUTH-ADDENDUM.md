# v0.99.40 Actions Truth Addendum

**Date:** 2026-06-24
**Addendum to:** `.planning/AUDIT-v0.99.40-RELEASE-AUTOMATION-FINAL.md`
**Milestone:** #828 (v0.99.41)
**Purpose:** Correct the historical record for v0.99.40 release workflow #581.

## What the v0.99.40 Final Audit Claimed

The final audit for v0.99.40 milestone #827 stated:

> release.yml run #581 PASSED: test ✅, release ✅

This wording is **misleading**. It conflated individual job success with
overall workflow success. The audit also stated:

> verify_milestone_release(827): pass=True ✅

## What Actually Happened

| Item | Truth |
|------|-------|
| Release #581 test job | ✅ Passed |
| Release #581 release job | ✅ Passed (assets published) |
| Release #581 smoke job | ❌ **Failed** |
| Release #581 overall workflow | ❌ **Failed** (`conclusion: failure`) |
| GitHub Release v0.99.40 | Published (assets exist and are downloadable) |
| Workflow-level conclusion | `failure` |

**Verdict**: `publication_succeeded_smoke_failed`

Assets were published because the `release` job completed successfully and
created the GitHub Release before the `smoke` job ran. The `smoke` job then
failed, causing the overall workflow run to conclude as `failure`.

## Why This Matters

1. **Audit conflation (F5)**: The v0.99.40 audit treated green individual
   jobs as equivalent to a green workflow. They are not. A workflow is green
   only when ALL jobs pass.

2. **Ambiguous release state (F1)**: A reader of the v0.99.40 audit could
   believe the release workflow succeeded entirely. In reality, the Actions
   tab shows a red ❌ for run #581.

3. **All 20 release runs failed**: Every release workflow run for v0.99.40
   iterations concluded as `failure` due to the smoke job. Release #581 was
   not an anomaly — it was the expected outcome given the smoke job was
   running the wrong test suite.

## What v0.99.41 Remediation Does

| Wave | Fix |
|------|-----|
| W1 | Defined release-smoke suite (12 files, ~150 assertions, deterministic) |
| W2 | Made manifest verification mandatory in release smoke job |
| W3 | Changed release.yml smoke step to use `--suite release-smoke` |
| W4 | Added `classify-release-verdict` to distinguish verdicts like `publication_succeeded_smoke_failed` from `workflow_success` |
| W5 | Added `classify-ci-verdict` to enforce all required CI jobs passed on main |
| W6 | Added tag/manifest/release commit traceability evidence |
| W7 | Added `actions-red-run-classifier.rkt` to classify historical vs current blocking red runs |

## Corrected Statement

The corrected statement for v0.99.40 should read:

> release.yml run #581: test ✅, release ✅, smoke ❌ → overall workflow FAILED.
> Assets were published because the release job completed before the smoke job.
> The workflow-level conclusion is `failure`.
> Classification: `publication_succeeded_smoke_failed`.

## Historical Red Runs

The 20 failed release workflow runs for v0.99.40 iterations were all caused
by the same smoke job issue. Under the v0.99.41 red-run classifier:

- All 20 runs would be classified as `historical_superseded_red` if a later
  green run supersedes them.
- Run #581 (the latest for tag v0.99.40) is `current_blocking_red_release_run`
  until a newer release workflow run for a different tag supersedes it.

## Summary

The v0.99.40 release assets are valid and usable. The v0.99.40 release
workflow did NOT pass overall. This addendum corrects the record.

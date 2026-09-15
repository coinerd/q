# PR Latency Guard — v1.00.30 W1

Blocking pre-merge anti-regression check for CI latency. Implements the measurement
contract from `.planning/PLAN-v1.00.30-PR-CI-RECOVERY.md` (Task 1/2): deterministic
evaluation over provenance-bound input JSON, fail-closed on every unknown, stale,
cancelled/incomplete, rerun-only or forged input.

## Evaluator

`scripts/ci/pr-latency-guard.rkt` — pure evaluator over a single input JSON document;
no network, no filesystem probes beyond the declared input. CLI:

```
racket scripts/ci/pr-latency-guard.rkt --input <sample.json> --verdict-json <out.json>
```

Exit code: `0` only when every check passes; `1` otherwise. Decision and per-check
detail are written to the verdict JSON (and mirrored to stdout as `pass`/`fail`).

### Checks (all must pass)

| Check | Enforces |
|---|---|
| `provenance` | Input declares exact candidate head SHA, base SHA and run attempt; any mismatch with the declared reference binding fails. Wrong head/base/attempt never yields green. |
| `freshness` | Reference is the frozen incumbent reference (`artifacts/ci-recovery/v1.00.30-w1/reference.json`); stale or superseded references fail. |
| `quantile-budget` | Candidate p50 and p95 deltas vs reference strata both within `min(10%, 60s)`. Boundary is inclusive-pass: a 500s reference tolerates +50s, a 1000s reference +60s; a 500s +51s or 1000s +61s sample fails. A p95-only regression fails even when p50 is unchanged. |
| `sample-counts` | Required per-stratum sample counts met; missing samples are a failure, never a skip. |
| `strata-compat` | Candidate strata set must be compatible with the reference strata set; declared lazy/eager/root treatment changes are permitted only when bound to evidence in the input. |
| `uniqueness` | Duplicate heads in the sample set fail. |
| `required-jobs` | Every required workflow job must be present and successful; cancelled, incomplete, skipped or absent required jobs fail. Rerun-only evidence (a single attempt superseding recorded attempts without the full set) fails. |
| `forge-guard` | Inputs whose artifacts cannot be distinguished from fabricated data (missing artifact truth fields, hash mismatch) fail. Unknown data never yields green. |

The guard cannot pass itself recursively: the `pr-latency-guard` check's own completion
is excluded from the measured latency sample set, and the guard never cites its own
verdict as sample evidence.

## Trusted post-run reporter

The GitHub Actions workflow `.github/workflows/pr-latency-guard.yml` runs the evaluator
on the sample JSON gathered from the PR's CI runs and posts the result under the
well-known check name `pr-latency-guard` via the check-reporting path
(`raw/canary-green-result.json` shape). Only the reporter holds write scope; fetched
PR scripts are never executed with write credentials. Conservative trusted-diff
allowlist: binding/docs-only files may carry `NOT_APPLICABLE` latency decisions;
unknown or executable paths always require full samples.

## Branch policy

`scripts/required-pr-checks.policy` declares `pr-latency-guard` as required. The
coordinator enables branch protection requiring the check after bootstrap canaries
(Task 3); until that remote rule is active the wave remains in its documented launch
preflight HOLD for protection — local evidence cannot substitute for the rule.

## Frozen evidence

* `artifacts/ci-recovery/v1.00.30-w1/reference.json` — frozen incumbent reference
  (3 strata × 3 samples, base SHA `b97d369c…`).
* `artifacts/ci-recovery/v1.00.30-w1/guard-canaries.json` — attested canary verdicts,
  evaluated at the strengthened-guard commit.
* `artifacts/ci-recovery/v1.00.30-w1/SHA256SUMS` — checksums over reference and raw
  canary inputs/verdicts (`sha256sum -c` clean).
* Raw API inputs under `raw/`; no secrets retained.

## Canary verification

Re-executed on the final wave content; all four verdicts byte-identical to the
committed attestation:

| Canary | Decision | Failing checks |
|---|---|---|
| `canary-green-input` | pass | — |
| `canary-red-input` (stale ref + over-budget + dup head) | fail | freshness, quantile-budget, uniqueness |
| `canary-red-missing-job-input` | fail | required-jobs |
| `canary-red-p95-boundary-input` (inclusive boundary) | pass | — |

## Tests

`tests/test-pr-latency-guard.rkt` + `tests/test-ci-cohort-report.rkt`: 179 checks
green, including 500s/+50 and 1000s/+60 inclusive boundaries, p95-only regression,
wrong head/base/attempt, duplicate heads, absent required jobs, forged-input and
rerun-only rejections, and a deliberately red plus a valid green check on test PR
heads. `scripts/run-tests/cohort-report.rkt` reports the run cohort; the guard test
suite is included in the fast suite.

## Rollback

Revert branch protection to the pre-guard required-check set (recorded in
`raw/` before/after rule snapshots); the evaluator itself is inert unless invoked,
so rollback is policy-level only.

# CI-RECOVERY-EXPERIMENT — W2 controlled A/B/C benchmark report (v1.00.32)

Status: IMPLEMENTATION DELIVERED IN WORKTREE — **not delivered until the
parent merge**. This document defines the report's input contract and
computation rules. **No number in this document is a measurement.** No live
experiment was dispatched, no trial was executed, and every fixture in the
test suite is synthetic. Actual baseline/experiment collection is a
separately authorized coordinator gate.

Owner (W2 report slice): `scripts/run-tests/ci-recovery-benchmark-report.rkt`,
`tests/test-ci-recovery-benchmark-report.rkt`, this document.
Shared W1 contract (envelope schema + strict assessor):
`docs/reports/CI-RECOVERY-EVIDENCE.md` and
`scripts/ci/prepared-env-report.rkt` (`assess-trial-envelope`, required
directly — no subprocess).
Plan: `.planning/PLAN-v1.00.32-MEASURED-PR-CI-RECOVERY.md`, wave
`W2-controlled-experiment-and-prepared-aggregate-candidate.md` (actions 4/5).

## 1. Purpose and non-goals

One deterministic, offline report over EXPLICIT trial JSON inputs that:

- re-assesses EVERY trial envelope with the W1 strict assessor — no
  precomputed verdict, ready claim or supplied duration is ever trusted
  (the report cannot even represent a verdict input; junk self-claim keys
  in the inputs never reach the output),
- computes the FULL REQUIRED CRITICAL PATH from raw Actions timestamps,
- binds run/head/attempt/trial identity and the exact configuration across
  ALL regimes (A/B/C) on one pinned tree,
- retains EVERY attempt — including failed, cancelled and refused ones —
  in the raw output with named exclusion reasons,
- publishes raw samples, n, p50, p95 with honest uncertainty (no
  fabricated confidence intervals),
- applies the C-vs-A regression budget and the C contamination block,
  and never converts non-regression into a promotion claim.

Non-goals: no CI topology change, no activation, no promotion, no live
dispatch, no performance verdict. Trial execution, coordinator smoke
validation and the ≥8-interleaved-trials-per-regime collection protocol are
outside this implementation slice.

## 2. Treatments

| treatment | meaning |
|---|---|
| A | root-off, bounded-eager path, cold aggregate (today's default) |
| B | root-off, prepared aggregate candidate |
| C | verified root-on + prepared aggregate (launcher telemetry must show `verified-root-hit`, `root-hits > 0`, `fallbacks == 0`, exit-code 0) |

## 3. Input contract

### 3.1 Trial envelopes — `ci-recovery-trial-evidence@1`

One document per trial attempt; exact schema, binding rules and refusal
catalogue in `docs/reports/CI-RECOVERY-EVIDENCE.md` (§2/§5). The W2 report
ADDITIONALLY requires, per shard (aligned with the concurrent W1 repair):

- every `record` carries `attempt`, `trial-id`, `treatment` and `identity`
  matching the trial block / `expected.identity` (report refusal reasons:
  `record-attempt-missing/-mismatch`, `record-trial-id-missing/-mismatch`,
  `record-treatment-missing/-mismatch`, `record-identity-missing/-mismatch`),
- every `launcher` carries `exit-code` and it is `0`
  (`launcher-exit-code-missing`, `launcher-exit-code-nonzero`).

### 3.2 Actions evidence — `ci-recovery-actions-evidence@1`

One document per RUN ATTEMPT (raw GitHub Actions run/jobs/status/config
metadata). Field names are exact; `"unknown"` is the only honest
placeholder for unobserved values. The workflow/adapter owner aligns to
THIS schema.

```json
{
  "schema": "ci-recovery-actions-evidence@1",
  "run": {
    "run-id": 7100,                        // number; must equal the trial's run-id
    "head-sha": "<40 lowercase hex>",      // must equal the trial's head-sha
    "run-attempt": 1,                      // positive integer; must equal the trial's attempt
    "workflow": "ci-recovery-benchmark.yml",
    "event": "workflow_dispatch",
    "status": "completed",                 // raw run.status; required, not inferred from jobs
    "conclusion": "success",               // raw run.conclusion; required independently of jobs
    "run-started-at": "2026-10-30T10:00:00Z"  // run.run_started_at (raw API field, UTC)
  },
  "required-job-names": [
    "lint", "lint-quality", "security", "release-dry-run",
    "workflows (0)", "workflows (1)", "workflows-aggregate",
    "smoke (ubuntu-latest)", "test (0)", "test (1)", "test (2)",
    "test-aggregate", "test-platform"
  ],
  // EXACT canonical scripts/required-pr-checks.policy (all 13 names).
  // The report loads this policy relative to its module, never the cwd.

  "jobs": [
    {
      "name": "lint",
      "role": "required",                  // "required" | "pilot" (nothing else)
      "status": "completed",
      "conclusion": "success",
      "started-at": "2026-10-30T10:01:00Z",
      "completed-at": "2026-10-30T10:03:00Z"
    }
    // ...one job object per job, verbatim from the Jobs API where possible
  ],
  "config": {                              // EXACT config binding (all dimensions REQUIRED)
    "runner-image": "ubuntu-24.04:20260901.1",
    "platform": "x64",
    "racket-version": "8.10",
    "racket-executable-digest": "<64hex>",
    "lock-digest": "<64hex>",
    "resolved-set-digest": "<64hex>",
    "precompile-recipe-revision": "setup-racket-full-path-r1",
    "policy-fingerprint": "addon-store-v2-no-workspace-bytecode",
    "test-inventory-digest": "<64hex>",
    "shard-plan": [0, 1],
    "harness-revision": "ci-recovery-benchmark.yml@<40hex>"
  },
  "aggregate-attestation": {               // raw aggregate job artifact, NOT adapter verdict
    "schema": "ci-recovery-aggregate-restore-attestation@1",
    "run-id": 7100,
    "run-attempt": 1,
    "head-sha": "<40 lowercase hex>",
    "prepared-requested": "auto",           // B/C; A must be "off" if attested
    "result": "success",
    "state": "restored",                    // actual nonempty, non-"unknown" setup output
    "identity-result": "verified",
    "restore-ms": "unknown",                // nonnegative finite number or "unknown"
    "identity": {                           // observed, not copied from expected
      "os": "Linux", "os-image": "ubuntu-24.04:20260901.1", "arch": "x64",
      "racket-version": "8.10", "racket-executable-digest": "<64hex>",
      "lock-digest": "<64hex>", "resolved-set-digest": "<64hex>",
      "precompile-recipe-revision": "setup-racket-full-path-r1",
      "policy-fingerprint": "addon-store-v2-no-workspace-bytecode",
      "artifact-digest": "<64hex>"
    },
    "producer-manifest-digest": "<64hex>"
  },
  "components": {                          // producer/restore/aggregate/proof costs
    "compiled-root-producer-seconds": "unknown",   // number or "unknown" — never zero-filled
    "prepared-aggregate-seconds": "unknown",
    "restore-ms": "unknown",
    "proof-seconds": "unknown"
  }
}
```

Cross-checks enforced per trial (refusal reasons):

- `actions-run-id-mismatch`, `actions-head-sha-mismatch`,
  `actions-attempt-mismatch` — the run block must bind to the trial block;
  pairing is by `(run-id, run-attempt)`, so a doc for a different run is
  treated as missing for this trial and retained as unpaired evidence,
- `actions-schema-invalid` — present but wrong schema,
- `actions-required-job-policy-invalid`, `jobs-invalid`, `job-role-invalid`,
- `required-job-set-incomplete`, `required-job-duplicate`,
  `required-job-set-extraneous` — required-role jobs must match
  the canonical policy exactly and completely; caller policy reductions,
  substitutions and canonical required jobs disguised as pilots are refused,
- `actions-run-not-completed` — raw run status must be `completed`,
  including when all 13 required jobs succeeded; absent/null/unknown status
  never implies completion,
- `actions-run-cancelled`, `actions-run-not-successful`,
  `actions-run-conclusion-unknown` — raw run conclusion must independently
  be `success`; cancelled, failed, timed-out, skipped, neutral, or other
  non-success conclusions refuse timing eligibility. Missing/null/unknown
  conclusion is unknown evidence, never assumed success. Refused attempts
  stay excluded from n with an unknown critical path; any such C attempt
  blocks readiness even alongside eight good C trials. Both raw run fields
  are retained in trial and Actions-attempt echoes (absent fields echo
  `unknown`). Offline synthetic regressions cover this; no live proof is claimed,
- `required-job-not-completed`, `required-job-failed`,
  `required-job-cancelled`, `required-job-timestamps-missing`,
  `required-job-timestamps-order-invalid`,
- `config-dimension-missing-<dim>` — every config dimension is required,
- `actions-config-missing`,
- `config-envelope-mismatch-<dim>` — the producer-bound
  `expected.identity` must agree with the run's declared config
  (`os-image↔runner-image`, `arch↔platform`, racket version/executable/
  lock/resolved-set/recipe/policy 1:1) and `expected.shard-inventory`
  must equal `config.shard-plan` (`config-envelope-mismatch-shard-plan`).

### 3.3 Independent aggregate eligibility

For B/C, the report requires the TOP-LEVEL Actions `aggregate-attestation`
raw body with the schema above. Its run/head/attempt must match BOTH the
raw Actions run and trial envelope. `prepared-requested` must be `auto`,
`result` must be `success`, `state` must be a nonempty non-`unknown` raw
value, and `identity-result` must be `verified`. `restore-ms` is required
but may remain `unknown`. All ten observed identity dimensions (including
`artifact-digest`) must match `expected.identity`, and the observed
`producer-manifest-digest` must match the expected producer digest.

Missing/malformed/mismatched bodies produce `aggregate-attestation-invalid`.
A valid W1 assessment, adapter success/ready, or
`config.prepared-aggregate-restore` summary cannot rescue them. Any such
refused C attempt blocks C eligibility/readiness even with eight other good
samples. A requires a bound `prepared-requested: off` attestation when one
is supplied; absent A attestation is honestly unmeasured, never an invented
prepared restore. The raw body is retained in `actions-echo` unchanged.

## 4. CLI

```
racket scripts/run-tests/ci-recovery-benchmark-report.rkt \
  --envelopes ENVELOPES.jsonl [--actions ACTIONS.jsonl] \
  --out REPORT.json [--out-md REPORT.md]
```

- `ENVELOPES.jsonl`: one `ci-recovery-trial-evidence@1` document per line.
- `ACTIONS.jsonl`: one `ci-recovery-actions-evidence@1` document per line.
- Exit codes: `0` = report written (refusals are DATA inside the report,
  never a crash); `2` = usage/IO error, no report written.
- Pure API for tests/adapters:
  `(assess-benchmark-experiment envelopes actions-lists)` → report hash;
  `format-report-json` / `format-report-md` are byte-deterministic;
  helpers exported: `parse-utc-seconds`, `format-utc-seconds`,
  `regression-budget-seconds`, `config-dimensions`,
  `component-dimensions`, constants `regression-fraction` (1/10),
  `regression-cap-seconds` (60.0), `min-trials-per-arm` (8),
  `max-interleave-block` (2).

## 5. Computation rules

### 5.1 Full required critical path (the ONLY timing measure)

```
critical-path-seconds = max(required job completed_at) − run.run_started_at
```

- Anchored at the RUN start ⇒ dependency and queue waits are INCLUDED.
- NEVER the sum of parallel required jobs (that double-counts).
- NEVER the max single job duration (that hides queue time).
- Independent PILOT jobs (role `"pilot"`) are excluded from the path and
  reported separately with their own timestamps.
- Queue waits (`job.started_at − run.run_started_at`) are reported
  separately per required job, never merged into execution time.
- Any supplied/precomputed duration field in the input is IGNORED — the
  path is computed from raw timestamps only.
- The path exists only when the required job set is exact+complete, every
  required job is `completed`/`success` with calendar-valid UTC timestamps
  satisfying `run-start <= job-start <= completed-at` for EVERY required
  job. Missing starts, inverted clocks and pre-run jobs are refused;
  otherwise `critical-path-unknown` and the trial is excluded
  (`"unknown"`, never zero-filled).

### 5.2 Population integrity

- At most one timing sample per actual Actions `run-id`, globally across
  treatments and trial labels — the first eligible attempt (lowest attempt
  number for the same label). Further eligible GitHub rerun attempts are
  `rerun-excluded`; a repeated identical attempt is `duplicate-excluded`.
  Relabeling a run cannot inflate independent n.
- A run associated with conflicting `(treatment, trial-id)` labels refuses
  ALL ambiguous entries with `run-label-conflict`; no favorable label wins.
- Conflicting Actions documents for one `(run-id, run-attempt)` produce
  `actions-duplicate-conflict` and NONE is selected as eligible evidence.
  Every Actions body remains in `raw.actions-attempts`, including conflicts.
- Same `(treatment, trial-id, attempt)` with DIFFERENT retained data is a
  timestamp/attempt mix ⇒ experiment refusal `trial-duplicate-conflict`.
- Refused/failed/cancelled attempts are NEVER dropped: they stay in
  `raw.trials` with their named reasons and appear in the arm's
  `exclusions` (arm `attempts-retained` counts every input attempt).

### 5.3 Regime binding (across ALL of A/B/C)

Experiment-level refusals (any ⇒ every gate verdict is `no-claim`):

- `source-mix-head-sha` — trials on different head SHAs (one pinned tree
  is mandatory),
- `config-mix-<dim>` — any config dimension differing between trials
  (runner image, racket version, executable digest, lock, resolved set,
  recipe, policy, test inventory, shard plan, harness revision),
- `trial-duplicate-conflict`, `actions-duplicate-conflict`,
- `treatment-block-not-interleaved` — with ≥2 treatments present, more
  than `max-interleave-block` (2) consecutive same-treatment trials in
  the created-at order.

Claims additionally require `n ≥ min-trials-per-arm` (8) eligible
independent trials per compared arm (`insufficient-independent-trials`).

### 5.4 Quantiles and uncertainty

- `cohort-quantile-exact` from `scripts/run-tests/cohort-report.rkt`
  (linear interpolation at `k = q·(n−1)`, millisecond-rounded) — the
  exact method is REUSED, not reimplemented.
- Every arm publishes raw samples, n, p50, p95, `attempts-retained`,
  named exclusions and an uncertainty block:
  `confidence-interval: "none"`, `tail-estimate-limited: true` for
  n < 20 — with n=8 the p95 tail estimate is limited and no confidence
  interval is fabricated.
- Arm-level producer/restore/aggregate/proof components are reported
  only when EVERY sampled trial observed the component; otherwise
  `"unknown"` (never partially zero-filled).

## 6. Gates

### 6.1 C vs A (regression budget)

```
per quantile:  C − A  ≤  min(10% of the A baseline, 60 s)
```

Both p50 AND p95 must hold (`within-budget`); a miss is
`exceeds-budget`. Evaluable only with no experiment refusals, no refused
C trial, and both arms measured (n ≥ 8, quantiles known); otherwise
`no-claim` with named reasons. Missing data yields NO claim.

### 6.2 B vs A (aggregate benefit, descriptive only)

`benefit-seconds = A_p50 − B_p50` (positive ⇒ B faster), reported
separately with verdict `descriptive-only`. NEVER a promotion gate.

### 6.3 Promotion readiness

- `blocked` — ANY refused/fallback-contaminated C trial
  (`c-arm-refused-trial`), regardless of how many other C trials are good,
- `not-evaluated` — experiment refusals, insufficient trials, or an
  exceeding budget (`c-vs-a-exceeds-budget`),
- `non-regression-only` — within budget. This is explicitly NOT a
  useful-improvement claim and NEVER permission to promote; promotion
  requires the separate human gate (v1.00.32 plan gate 3) on the full
  wave contract.

## 7. Report shape (`ci-recovery-benchmark-report@1`)

Top-level keys: `schema`, `kind`, `treatments`, `binding` (pinned head,
source/config uniformity, shared config), `arms` (A/B/C: n, samples,
p50/p95, exclusions, attempts-retained, components, uncertainty,
arm-verdict), `comparison` (`c-vs-a`, `b-vs-a`), `promotion`, `honesty`,
`refusals`, `interleaving` (ok, max-same-treatment-block, sequence),
`raw` (`trials` — every attempt with its re-assessed W1 verdict echo,
actions echo, reasons, population-exclusion, critical path, required/
pilot job reports, config snapshot, components; `unpaired-actions`;
`attempts-total`; `actions-attempts` — all restricted raw Actions echoes,
including conflicting bodies and their raw aggregate attestations).

## 8. What this wave does NOT claim

No compiled-root or prepared-aggregate activation; no measured savings,
recovery or benefit; no live experiment or canary; no promotion. Ready
describes EVIDENCE QUALITY of synthetic-or-real trial inputs — never a
performance verdict. `within-budget`/`non-regression-only` is not a
useful-improvement claim. Any real aggregate-failure canary stays
isolated and separately authorized; unit simulations are never labelled
live canary proof.

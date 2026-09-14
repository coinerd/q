# CI Exit Truth — v1.00.30 W0 (BUG-0073 closure)

**Status:** IMPLEMENTED on `campaign/v1.00.30-w0`; masked historical evidence flagged, never relabeled green.
**Bug:** BUG-0073 — default-shell/tee masking allowed failed Racket shards to yield green required checks and proof bundles (v1.00.29 era).
**Scope of truth:** GitHub Actions runner, pipeline, shard artifact, aggregate and job conclusions must agree; failed/missing/malformed/incomplete evidence can never produce a successful required gate (R0, `docs/reports/PR-CI-RECOVERY-CONTRACT-v1.00.30.md`).

## 1. Effective-shell audit (not a grep audit)

Method (recorded in `artifacts/ci-recovery/v1.00.30-w0/tee-audit.json`): every runner/tee site in every declared workflow and composite action was checked for its enclosing run block's pipefail/status propagation. Masking was reproduced with the effective shell — GitHub's default `bash -e {0}`:

```text
bash -c 'false | tee /dev/null'; echo $?                -> 0  (masked)
bash -c 'set -o pipefail; false | tee /dev/null'; echo $? -> 1  (truthful)
```

Fixes were applied **only** where effective execution proved masking; no topology changes. Results per file:

- `ci.yml` — FIXED: lint job, fast-shard map and the shard-reduce aggregate boundary use explicit `shell: bash` with `set -o pipefail`; all tee pipelines propagate test exit; the result-truth verifier runs at the aggregate boundary before proof-bundle publication.
- `full-regression.yml`, `release.yml`, `release-core.yml`, `test-scheduler-cohort-c1.yml`, `test-scheduler-shadow.yml`, `prepared-environment-pilot.yml` — COVERED by existing `set -euo pipefail`/`set -o pipefail` blocks or explicit `shell: bash`; verified per site, unchanged.
- `nightly.yml` — COVERED-INTENTIONAL: tee sites only record fallback-cause notes after a deliberately handled lookup failure; the full suite still runs, so no required failure can be masked.
- `shard-plan-telemetry.yml` — COVERED: report-only job; upstream gate failure is recorded and the plan build is skipped explicitly.
- `benchmark.yml` — NO-PIPE-SURFACE (no pipelines in run blocks).
- Composite actions `setup-racket`, `prepare-racket-environment`, `restore-racket-environment` — COVERED (explicit `shell: bash` / pipefail-safe blocks; the prepare-summary tee sits directly under `set -o pipefail`).
- Unlisted-site gap: repository-wide scan found only `project-automation.yml` (no run pipelines) and `install-racket-runtime` (no pipes) outside the declared list; recorded, not silently omitted.

## 2. Result-truth verifier at the required aggregate boundary

`scripts/ci/verify-result-truth.rkt` is the executable boundary, wired into `ci.yml`'s shard-reduce aggregate job (before proof-bundle publication), not merely a regex test:

- Refuses a non-clean aggregate verdict; green can only verify a genuinely clean aggregate.
- Enforces run-SHA binding on the aggregate AND every shard record (legacy bundles without bindings fail).
- Fails closed on: missing shard, invalid/missing totals, partial/malformed JSON, tee failure markers, mismatched run SHA, artifact failure.
- Existing bundle-producer refusal semantics retained.

## 3. Canaries (executable, in `tests/test-ci-exit-truth.rkt`)

failed-runner/successful-tee, tee failure, timeout, partial JSON, missing shard, invalid totals, mismatched run SHA, successful-runner/failing-artifact, and genuine success. The canaries are unit-level invocations of the boundary verifier: each proves the verifier refuses its named lie with a non-zero verdict, and the job/aggregate-level consequence (the aggregate step exits non-zero, so no proof bundle is minted and the job fails with evidence retained for upload) follows structurally from the single call site wired in ci.yml rather than from a dedicated failing CI job — the W0 review-fix restated this; the earlier wording "proves the aggregate AND the job fail" overstated the executable evidence. The clean canary succeeds. Actual failed-job/aggregate execution and retained-artifact evidence have not been demonstrated by these unit tests; R0 remains HOLD pending that evidence. Two test-only repairs outside the BUG-0073 contract (tests/test-lint-release-notes.rkt, tests/test-release-entry-current.rkt) are recorded here as incidental repair scope, defensible but not contract-mandated.

## 4. Prepared-environment telemetry isolation (W9 fix preserved)

`tests/test-prepared-env-report.rkt` reruns the telemetry-isolation case under polluted ambient variables (host-like `Q_*`/prepared-environment environment); the producer's report records only the prepared environment's own inputs, preserving W9's isolation fix.

## 5. Frozen claims and baseline

`required-claims.json` freezes the required-check/suite inventory and the exact primary timing fields; `baseline.json` records the v1.00.28/v1.00.29 comparison figures with their sample-size caveat and flags every masked historical artifact as MASKED (diagnostic only). Raw API inputs and checksums live under `artifacts/ci-recovery/v1.00.30-w0/raw/` with `SHA256SUMS`. Genuine incumbent-regime runs for W1/W2 reference are collected after this wave lands; no guaranteed finish date exists without genuine traffic (155 candidate heads/30d observed, 0 validated clean eligible).

### Review remediation scope

The current tier-ownership matrix now includes `tests/test-ci-exit-truth.rkt` with explicit CI/fast/integration metadata. The other added source, `scripts/ci/verify-result-truth.rkt`, is production tooling, outside the matrix generator’s `tests/` scope. The version-current eight-column matrix under `artifacts/tier-ownership/v1.00.29-w0/` is also regenerated: despite its versioned path, the milestone gate treats it as a live drift baseline. Its checksum is refreshed. Older historical snapshots remain unchanged.

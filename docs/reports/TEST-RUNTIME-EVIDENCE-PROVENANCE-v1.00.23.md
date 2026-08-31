# Test Runtime Evidence Provenance — v1.00.23 W0

**Milestone:** v1.00.23 — Measured, Reversible Test Scheduler Foundation
**Wave:** W0 — Characterize scheduler, evidence provenance, metric definitions, and CI contracts
**Audited revision:** `fe4493fe71c746a67c3f6b3567172abbbcc39bbd` (`main`, v1.00.22)
**Source audit:** `.planning/TDD-q-Test-Design-CI-Runtime-Audit.md`
**Series roadmap:** `.planning/ROADMAP-v1.00.23-v1.00.27-TEST-DESIGN-CI-RUNTIME.md`
**Status:** W0 characterization complete — no runner, CLI, workflow, timeout, selection, metadata, or test-tier behavior changed.

---

## 1. Metric definitions are frozen; the two metrics must not be conflated

Two retained metrics measure **different spans**. The series roadmap (section 3, "Metric contract and baseline reconciliation") names them explicitly, and this report freezes those definitions for every later wave. A figure may be used only against the threshold of its own metric class.

| Metric class | Existing record | Span measured | Series use |
|---|---:|---|---|
| **A — Setup-plus-execution baseline** | v1.00.11 W0 baseline-of-record **488.0 s** p50; target **244.0 s**; v1.00.16 sample **627.0 s** → verdict **MISSED** | Setup/restore **plus** slowest fast shard execution (prepared-environment era boundary) | Governance baseline; remeasured separately after warm prepared-env evidence (v1.00.25 W5) |
| **B — Audit fast execution (runner only)** | **236.4 s** p50 / **270.1 s** p95 | Slowest fast shard's runner execution only, from ten retained PR CI runs (1,141–1,156 files, 3 outer shards × 4 inner workers) | Hypothesis baseline for ≤115 s / ≤135 s execution target; **historical/external** until inputs recovered or a fresh C0 replaces them |
| **C — Audit PR workflow elapsed** | **1,176.0 s** p50 / **1,467.6 s** p95 | GitHub workflow `createdAt`→`updatedAt` elapsed, same ten-run sample (includes queueing, prerequisite jobs, aggregates, reporting) | Hypothesis baseline for ≤588 s / ≤735 s end-to-end target; same provenance class as B |

**Freeze rule:** metric A's threshold (244.0 s) is only compared against setup-plus-execution samples; metrics B and C thresholds (115/135 s and 588/735 s) are only compared against execution-only or workflow-elapsed samples respectively. v1.00.23 records both and never compares one class against the other's threshold (roadmap "Metric Reconciliation").

### 1.1 Historical figures and their honest status

- **A (488.0 s / 244.0 s / 627.0 s MISSED):** these are repository-of-record figures already committed in earlier wave documents and the v1.00.15/v1.00.16 baseline reports (`docs/reports/test-feedback-baseline-v1.00.15.md`, v1.00.16 W0/W4 evidence). They remain the governance baseline of record for the setup-plus-execution class. The 627.0 s v1.00.16 sample stands as an honest `MISSED` verdict (ratio 1.2848× against the ≤0.5× target) — it is **not** re-used as a new baseline and **not** conflated with class B.
- **B and C (236.4/270.1 s and 1,176.0/1,467.6 s):** historical **external** audit observations from a ten-run convenience sample of retained CI artifacts. Per the roadmap, they are valid audit observations but are **not regenerable baseline-of-record inputs** until their exact input manifests are recovered or a fresh reproducible C0 cohort is created.

## 2. The audit's six companion artifacts are absent — classified, not fabricated

The audit document's "Audit artifacts" section lists six companion evidence files generated from retained CI artifacts. **All six are absent from the current tree** (verified: none exists under `q/` or `.planning/`). None of the six has a committed checksum, manifest, or byte-identical regeneration path.

| # | Absent artifact | Contents | Raw inputs needed | Recoverable from tree? | Verdict |
|---|---|---|---|---|---|
| 1 | `fast-suite-timing-summary.md` | Ten-run per-file and per-shard timing summary | 30 retained `test-results-fast-*` shard JSON artifacts (10 PR runs × 3 shards) | No — artifacts were external GitHub Actions uploads; none committed | Not regenerable as-is |
| 2 | `fast-suite-scheduling-model.md` | Batch-barrier, FIFO, and LPT scheduling model | Same 30 shard JSON artifacts + model code | No | Not regenerable as-is |
| 3 | `pr-ci-workflow-latency-summary.md` | Ten-run end-to-end PR workflow latency calculation | GitHub workflow run metadata (createdAt/updatedAt) for the ten runs | No — only figures quoted in the audit doc | Not regenerable as-is |
| 4 | `fast-suite-execution-eligibility.md` | Static grouped-mode eligibility scan (102/1,156 files) | Representative inventory + classifier source at audited commit | Partially — classifier source is in-tree, but the exact inventory snapshot is not | Regenerable only as a *new* scan, not the historical one |
| 5 | `pr-critical-path-gate-model.md` | Representative security/platform/workflow scheduler model | Run `33300977936` result artifacts | No | Not regenerable as-is |
| 6 | `slow-fast-test-analysis.json` | Per-hotspot audit findings for the ten slowest fast tests | Per-file durations from the retained artifacts | No | Not regenerable as-is |

**Can raw inputs still be recovered?** The raw inputs were GitHub Actions artifacts with `retention-days: 7` (upload steps in `.github/workflows/ci.yml`). The audit window predates this W0 by the campaign's history; the artifacts are **not** retained in the repository, are **not** committed to any `artifacts/ci-baseline/` snapshot, and no exact input manifest/checksum was committed with the audit. Raw inputs for artifacts 1, 2, 3, 5, and 6 are therefore **not recoverable** from any repository-owned source; artifact 4 is only partially reconstructible as a different (new) scan.

### 2.1 Honest outcome chosen

Per the roadmap (section 3): *"No activation may claim comparison against unverifiable inputs."* Of the two permitted honest outcomes, this wave adopts:

> **Declare a fresh reproducible C0 cohort required, and prohibit activation (and any 2× claim) against the historical B/C figures.** The 236.4/270.1 s and 1,176.0/1,467.6 s audit figures remain documented historical/external observations for planning context only. They are explicitly **not** baseline-of-record inputs for any activation decision. Activation thresholds are recomputed from a fresh C0 (20 consecutive eligible unique PR head SHAs, canonical batch mode, committed manifest with lane inventory digests and artifact checksums, byte-identical regeneration inputs under `artifacts/ci-baseline/<milestone>/`).

The alternative outcome (recover exact inputs/checksums) is not available: no committed checksums or input manifests exist to verify against, and the raw artifacts are outside retention and not repository-owned.

## 3. Adopted percentile estimator (verbatim from the series roadmap)

> "Percentiles use the same linear interpolation between closest ranks declared by the adopted TDD baseline."

The adopted TDD baseline (`docs/TDD-TEST-STRATEGY-PLAN.md`, v1.00.15 measurement obligation) declares the method as:

> "linear interpolation between closest ranks over the sorted per-(suite,shard) job step wall-clock sample; no p90 is computed or implied"

This is the only estimator permitted for cohort percentiles in this series (C0, C1, C2, and any shadow comparisons). No other quantile method may be substituted without a separately reviewed decision record.

## 4. Cohort eligibility rules (verbatim from the series roadmap, section 3.1)

All activation and final-claim cohorts use this pre-registered rule:

- 20 **consecutive eligible unique PR head SHAs** after the declared cohort-start SHA and configuration snapshot.
- An eligible PR targets `main`, uses the canonical supported Racket/default workflow configuration (no dispatch version override), reaches a terminal result, and produces complete required lane/result artifacts. Draft-only, fork-permission-denied, and superseded-before-execution PRs are recorded but mechanically ineligible.
- One sample per PR head SHA **per named cohort/configuration**: C0 takes the final successful canonical main-CI batch run; C1 takes the final successful automatically triggered queue-shadow run for the same SHA. Failed/cancelled attempts are retained as reliability evidence but do not become successful timing samples.
- C0 and C1 are paired on the same 20 consecutive eligible SHAs. Later multi-lane shadow comparisons (FIFO/LPT/security) may also share that paired window when every configuration runs automatically for every SHA; activation-default C2 remains a separate post-promotion cohort.
- No cherry-picking by duration or by deciding which PR receives shadow execution. During a paired cohort, the separate non-required shadow workflow triggers for every eligible PR. An excluded run requires a named mechanical reason (GitHub outage, absent/corrupt required artifact, or workflow cancellation by supersession) and stays in the exclusion ledger.
- Percentiles use the same linear interpolation between closest ranks declared by the adopted TDD baseline (section 3 above).
- The committed cohort manifest names repository SHA, workflow run ID, PR/head SHA, timestamps, scheduler/config variables, prepared-env outcome, lane inventory digests, and artifact checksums.
- Raw or normalized inputs sufficient for byte-identical regeneration are retained under a bounded `artifacts/ci-baseline/<milestone>/` snapshot or another repository-owned deterministic evidence path.
- Repeated workflow dispatches may be used for isolation stress, but they do not substitute for the 20-unique-PR performance cohort.

## 5. Downstream JSON and job-name consumers (inventory for W1/W2 preservation)

Every consumer of retained runner JSON, required job names, and workflow result artifacts at the audited revision (v1.00.22, `fe4493fe`). W1/W2 must keep all of these readable and correctly addressed.

| Consumer | What it consumes | Job / workflow | Contract that must survive W1/W2 |
|---|---|---|---|
| `shard-plan-report` | Downloads `test-results-fast-*` artifacts; stages `test-results.json` per shard for duration-aware planning | ci.yml job `shard-plan-report` (needs `test-aggregate`; workflow tail; report-only, `if: always()`, `continue-on-error` download; never a required check) | Retained fast-shard artifacts must remain named `test-results-fast-*` and parseable; additive fields only |
| Job step summaries | `.run_summary` fields inside `test-results.json` | ci.yml `test` (fast) job | `--json-out test-results.json` and `.run_summary` emission must remain |
| `release-readiness` | Gate evidence JSON for fast/tui/arch/workflows via `--record-gate-evidence --json-out test-results-<suite>.json` | ci.yml `release-readiness` (tag pushes) | Evidence files must remain machine-checkable for release/audit truth tests |
| `baseline-report` | Per-suite/per-shard result JSON (retained artifacts + recorded gate evidence) | `scripts/run-tests/baseline-report.rkt` | Old and new schema both accepted (additive-only telemetry) |
| Full-regression aggregation | Per-suite JSON artifacts from full regression runs | `.github/workflows/full-regression.yml` | Same additive-only rule; aggregation must not fail on old artifacts |
| Release/audit truth tests | Gate evidence + required-check sets | `tests/test-release-audit-truth.rkt`, `tests/test-milestone-gate.rkt`, `tests/test-w9-ci-workflow-verification.rkt` | Required job-name set and evidence fields pinned by `scripts/required-pr-checks.policy` and CI contract test |
| `gsd-governance` | Changed wave-evidence `.rktd` records validated by `scripts/gsd-wave-gate.rkt` against `scripts/required-pr-checks.policy` | ci.yml `gsd-governance` (push only) | Exactly one changed wave-evidence record per PR; content digest excludes evidence/review/validation dirs |

**Required job names (exact, from `scripts/required-pr-checks.policy`):** `lint`, `security`, `release-dry-run`, `workflows (0)`, `workflows (1)`, `workflows-aggregate`, `smoke (ubuntu-latest)`, `test (0)`, `test (1)`, `test (2)`, `test-aggregate`, `test-platform`.

**Workflow result artifacts:** per-shard `test-results-fast-<shard>.json` uploads (retention 7 days), recorded gate-evidence JSON for the four release suites, and the shard-plan report inputs. No artifact consumer may observe a change in W0; none does.

## 6. Pins created by this wave (what W1/W2 must preserve / flip)

The two characterization tests added in W0 freeze today's behavior on v1.00.22:

- `tests/test-runner-scheduler-characterization.rkt` — pins the fixed-batch barrier (`run-all-files` with `jobs=2`: a third short file cannot begin until both files of the first batch finish, proven with temp-dir start/completion markers and bounded synchronization, not loose wall-clock sleeps); pins output order, per-file timeout, exception/result classification, and the serial/parallel ownership seams; pins that no `--scheduler` option exists yet (absent-seam pin W2 flips). W2 may change the barrier and the scheduler seam only while preserving output order, timeouts, classification, and serial/parallel ownership.
- `tests/test-ci-runtime-contract.rkt` — pins the CI DAG: `fast-env`, fast, platform, security, workflows, smoke, and release-dry-run wait for `lint`; `shard-plan-report` is report-only on the workflow tail (needs `test-aggregate`); fast = 3 outer shards × 4 inner workers; workflow PR shards = 2 inner workers; the exact required job-name set; and the JSON/artifact consumers of section 5. W1/W3 flip only the seams their wave docs name, and only via the roadmap's atomic DAG transaction.

**No runner, CLI, workflow, timeout, selection, metadata, or test-tier behavior changes in W0.** This report is characterization and classification only.

## 7. Verification

- `racket tests/test-runner-scheduler-characterization.rkt` — PASS
- `racket tests/test-ci-runtime-contract.rkt` — PASS
- `racket scripts/run-tests.rkt --suite fast` — PASS (full required fast suite)

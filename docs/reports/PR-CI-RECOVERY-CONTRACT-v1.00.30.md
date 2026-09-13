# PR-CI Recovery Contract — v1.00.30

**Status:** FROZEN at W0 (materialized verbatim from `.planning/PLAN-v1.00.30-PR-CI-RECOVERY.md`, "Frozen recovery acceptance contract").
**Source:** `.planning/q-v1.00.29-pr-ci-regression-assessment.md` (13 September 2026).
**Baseline headline:** NOT ACHIEVED — SEVERE PR-CI PERFORMANCE REGRESSION (v1.00.29). Required PR-CI p50/p95 rose from 820.5/850.3 s (v1.00.28 final cohort) to 2558.0/3023.0 s (v1.00.29 post-W4 cohort). W4 purge plus skipped eager compilation amplified isolated test compilation to 6.774x summed per-file time. Restore success, setup savings and nightly duplicate-proof removal do not compensate for PR latency.
**Provenance artifacts:** `artifacts/ci-recovery/v1.00.30-w0/baseline.json`, `required-claims.json`, `traffic-feasibility.json`, `tee-audit.json`, `SHA256SUMS`, `raw/`.

This file is the frozen materialization of the plan's recovery acceptance contract. Historical metric definitions are reconciled **before** any activation; they are never quietly rewritten afterward. Any material change requires a reviewed plan amendment, never a quiet edit.

## Frozen recovery acceptance contract

| Gate | Fixed requirement | Failure action |
|---|---|---|
| R0 truthfulness | Runner, pipeline, shard artifact, aggregate and job conclusions agree. Failed/missing/malformed/incomplete evidence cannot yield a successful required gate. | Block performance claims and topology changes. Repair exposed failures; retain their evidence. |
| Per-wave monotonicity | For both comparable PR critical-path p50 AND p95, candidate minus reference <= min(0.10 * reference, 60 s). Equality passes; missing data fails closed. | No CI-affecting merge. A compensating change is allowed only when reviewed and measured in the SAME candidate head and net result passes the same limits; no future promises/waivers. |
| R1 containment | At least 3 comparable distinct PR head SHAs, full truthful runs; p95 of each run's slowest fast-shard runner wall <=1736.8 s. | Roll back unsuccessful containment; remain in recovery, no feature work. |
| R2 baseline recovery | Closed cohort of >=20 NEW eligible unique post-activation PR head SHAs; required PR-CI p50 <=820.5 s. | Hold W6 onward; no proof-graph/selector/scheduling work. Revert unsafe/regressing root activation to eager fallback and investigate. |
| R3 prior fixed target | Closed final cohort required PR-CI p50 <=588 s and p95 <=735 s. | Only recovery work may continue; never call the performance objective achieved. |
| R4 north-star | Closed final cohort required PR-CI p50 <=360 s and p95 <=480 s; all safety gates pass. | R4 alone permits ACHIEVED. |

R1 is containment-only, not baseline recovery: the anchor is the maximum shard time from ONE pathological PR, not a population p95. With three run maxima, linear-interpolated p95 is near the sample maximum, not a reliable tail estimate; publish n and uncertainty. The pre-W4 233–292 s job band is reported alongside it (job total versus runner-only, not like-for-like). R1's 1736.8 s cap is a NEW explicit planning choice: 20% below the assessment's rounded pathological tail of 2171 s and also materially below 2558 s. It is not a historical measured target. It does not replace end-to-end PR monotonicity or R2. R2's p95 must also satisfy the per-wave guard and be reported against 850.3 s, even though R2's absolute threshold is p50-only. No averaging p50 and p95 together to hide tail regressions.

## Metric classes (frozen for the verdict)

| Metric type | Appropriate role |
|---|---|
| Bytecode correctness, proof provenance, cache identity, coverage, branch protection | Non-negotiable safety gates; any failure blocks release. |
| Setup restore rate and setup time | Mechanism telemetry; cannot proxy for job/PR performance. |
| Duplicate-proof ratio | Secondary efficiency metric; cannot compensate for a PR-latency regression. |
| PR critical-path p50/p95 | Primary outcome gates for this developer-feedback/performance milestone. A catastrophic regression forces a negative verdict. |

Safety gates dominate. A release with safe proof reuse but a much slower PR path may claim integrity infrastructure delivered; it must never be classified as a partial performance success.

## Measurement and anti-regression mechanics (frozen)

- **Primary metric:** required PR critical-path latency = required-workflow run start to completion of its last required check, accounting for required parallel workflows by the latest required completion. Exact API timestamp fields and check-name policy are frozen in `required-claims.json` (`workflow run created_at/run_started_at` start; latest required `completed_at` end; exact branch-protection check names, no normalization or prefix matching). Historical figures are reproduced from these definitions with the sample-size caveat recorded in `baseline.json`; any definition incompatibility is disclosed and comparison is HELD, never silently substituted.
- **Separately reported (cannot substitute):** trigger-to-mergeable elapsed including runner queue, setup/compile window, per-file and per-shard work, and runner-minutes.
- **Quantiles:** sorted observations, linear interpolation at (n-1)*p, milliseconds retained until display; the exact helper is pinned by `tests/test-ci-exit-truth.rkt`. The legacy rounded/midpoint helper is not reused. Boundary tests cover <= versus > and min(10%, 60 s).
- **Exit truth (R0):** `scripts/ci/verify-result-truth.rkt` runs at the aggregate boundary; a non-clean aggregate, shard/run-SHA mismatch, missing/failed shard, partial or malformed JSON, tee failure or artifact failure can never verify a green bundle. Failed/timeout canaries are executable in `tests/test-ci-exit-truth.rkt`. BUG-0073-era masked greens are recorded as MASKED diagnostics in `baseline.json` and are never reused as safety evidence.
- **Per-wave comparison:** >=3 candidate distinct head SHAs (including the exact final head) and >=3 frozen reference heads from the previous accepted execution regime. Controls match runner class, Racket executable/version, dependencies and required suite/claim inventory; new regression tests are enumerated, never silently removed. Cache-availability strata are matched or separately reported. Declared treatment may differ by wave (W2 lazy-to-eager, W4 eager-to-external-root, W6 shard assignment) and is bound to candidate/base SHA and regime fingerprint. Repeated attempts of one SHA never satisfy uniqueness. Reference data are fixed before candidate results are viewed. No manufactured PRs to fill samples; if development produces fewer observations, HOLD.
- **Cohorts:** R2 cohort begins strictly after W4 final activation merges; final cohort strictly after W6's final topology decision. >=20 distinct heads each, mutually disjoint and disjoint from v1.00.28/v1.00.29 samples. No mixing compilation regimes; later regime changes restart the relevant window. COHORT OPEN halts `/go`; deadlines never authorize a pass.
- **Traffic feasibility:** 155 execution-change-candidate heads observed per trailing 30 days; 0 validated clean eligible (historical masking). Projected 7.7 calendar days at 1 head/day for 40 heads, 31.0 days at 0.25 head/day. No guaranteed finish date without genuine traffic. COHORT OPEN checks every 7 calendar days; at 30 days without enough records, automated retries stop and a coordinator resumption/amendment request is issued.
- **Guard:** W1 installs the named blocking `pr-latency-guard` check consuming immutable coordinator-attested Actions records for the exact head SHA and current base. Post-run reporter never waits for its own check or depends on merge, and never executes untrusted PR code with a write token. Docs-only/binding-only PRs may receive `NOT_APPLICABLE — NO EXECUTION CHANGE` only via the frozen conservative allowlist; a PR cannot self-declare exemption. Lack of coordinator permission is a HOLD, not success.
- **Rollback:** coordinator sets repository Actions variable `RACKET_PREPARED_ARTIFACT=off` (`gh variable set RACKET_PREPARED_ARTIFACT --body off --repo coinerd/q`); every relevant call site must honor it. Compiled-root activation may add a dedicated default-off switch but must preserve this global emergency override. Rebalance rollback is a reviewed revert of the recorded activation commit. Do not mutate global switches from untrusted PR jobs.

## Verdict policy (frozen)

Exactly one W7 verdict: `ACHIEVED` (R0–R4 pass); `RECOVERED — PRIOR TARGET MET, NORTH-STAR NOT ACHIEVED` (R0–R3 pass, R4 fails); `RECOVERED — BASELINE ONLY, PERFORMANCE OBJECTIVE NOT ACHIEVED` (R0–R2 pass, R3 fails); `NOT ACHIEVED — RECOVERY INCOMPLETE` (R2 fails); `BLOCKED — SAFETY/INTEGRITY` (any safety failure); or `COHORT OPEN` (insufficient evidence, not a terminal success).

v1.00.30 release requires all safety gates, complete cohorts, R2, and per-wave guard passing. R3/R4 misses may ship an explicitly negative performance-objective verdict with proven baseline recovery; no PARTIAL performance-success headline. R2 failure, safety failure or an open cohort HALTS before W8. W8 may not relabel or weaken W7's result.

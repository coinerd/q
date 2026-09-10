# Performance Contract v1.00.29 — Frozen Metric Formulas and Stage Gates

Status: **FROZEN** at wave `v1.00.29-w0` (branch `campaign/v1.00.29-w0`, base `293b6a27`).
Freeze rule: W1+ execution-change waves may cite this contract as-is. Any formula change
requires a new contract version and a new wave; behavior changes must not merge against a
stale contract. Before-state numbers live in
`artifacts/proof-graph/v1.00.29-w0/` (checksummed, `SHA256SUMS`).

Scope of observation: PR CI, main CI, release, full regression, security, platform/cross-version,
workflow suites, benchmark, scheduler-shadow, shard telemetry. All formulas are computed from
retained machine-readable run evidence (GitHub Actions API payloads committed under
`artifacts/proof-graph/v1.00.29-w0/evidence/` or later wave evidence dirs).

---

## 1. Critical-path latency (per surface)

```
wall(run)        = run.updated_at − run.created_at            [integer seconds]
chain(run)       = longest start→finish dependency chain among jobs;
                   chain edge (A→B) exists when B.started_at ≥ A.completed_at
                   and a needs/gate relation is declared in the workflow file
critical_path(run) = max over jobs J of (chain length ending at J)
critical_path_latency(surface) = wall(run) ; chain(run) reported alongside
```

- Wait mass vs work mass are reported separately for every surface:
  `wait_mass = Σ (job.started_at − predecessor.completed_at) + pre-first-job wait`,
  `work_mass = Σ chain job durations`. The full-regression incident showed why
  (setup 5199s vs 91s observed suite work before cancellation).
- Cancelled runs contribute `critical_path_latency` observations with
  `conclusion: cancelled` and **no completion time is ever imputed**.

## 2. Runner-minutes

```
runner_minutes(run) = Σ over jobs J of (J.completed_at − J.started_at) / 60
```
- Matrix jobs contribute each replica's own interval (no averaging).
- Skipped jobs contribute 0. Queued-but-never-started jobs contribute 0 and are
  counted under wait mass instead.

## 3. Avoidable duplicate proof mass

```
duplicate_mass(run set) = Σ over §4-classified duplicate pairs (a,b) observed in the
                          same SHA window of min(cost(a), cost(b)) × reusability_factor(pair.class)
reusability_factor: exact_duplicate = 1.0
                   compatible_reusable = 1.0 (positively proven compatibility only)
                   distinct_environment = 0 (not reusable without positive proof)
                   distinct_semantic = 0
                   observational_only = 0
```
- Only `exact_duplicate` and positively-proven `compatible_reusable` pairs enter the mass.
- v0 observed baseline (three retained runs alone): **6065s ≈ 101 runner-minutes**
  (platform re-run incl. 86m39s setup: 5588s; release draft+public re-verification: 477s).

## 4. Duplicate-proof ratio

```
duplicate_proof_ratio = duplicate_mass / Σ runner-seconds of all proof-producing jobs in the window
```
- Observational jobs (benchmark, shadow, cohort, telemetry) are excluded from the
  denominator and can never enter the numerator (their class is `observational_only`).

## 5. Re-verification ratio

```
re_verification_ratio(claim c, window) =
    re_executions(c) − 1        (if c executed ≥ 1 time)
    ─────────────────────
    executions(c)
```
- Per claim from `claims.json`; window = SHA or tag window.
- Aggregated: mean over required-gate claims; per-claim value reported for top offenders.

## 6. Flake tax per bundle

```
flake_tax(bundle) = Σ over re-runs R triggered by non-deterministic failure in the bundle of
                    (runner_seconds(R) + blocking_seconds(R))
blocking_seconds(R) = wall time from first failing attempt completion to green, counted
                      on the critical path of the surface
```
- v0: no retained flake incident in W0 evidence → formula frozen, value **not invented**;
  backfilled from W1+ per-run telemetry.

---

## 7. Stage gates (north-stars)

| Surface | Metric | v1.00.29 before-state | W1+ gate band (warn → fail) |
|---|---|---|---|
| main CI (`push:main`) | critical-path latency | 22m41s (dominated by cross-version 1346s) | ≤ 12m warn, ≤ 8m target trajectory; fail if > 30m or regression > 25% vs 7-run p50 |
| release (`push:tags`) | critical-path latency | 74m01s (incl. 6m14s pre-job wait, 455s inter-job gaps) | ≤ 45m warn, ≤ 30m target trajectory; fail if > 90m |
| full regression | critical-path latency | 96m05s **cancelled** (setup-dominated) | setup-racket share > 20% of wall → warn; no completion time ever imputed for cancelled runs |
| any required surface | duplicate-proof ratio | v0 sample ≈ 6065s / 319.8min ≈ 31.6% (3-run window, inflated by the cancelled setup; treat as upper bound) | > 15% warn, > 25% fail (steady-state windows) |
| any required surface | re-verification ratio | platform-cross: 2 executions per SHA window observed | > 1.3 warn, > 1.6 fail per claim |
| delivery-verifier test | standalone cost | 36.1s cold / 36.6s warm / 38.7s fast-shard context (wave-branch median, 3×3) | stale-header numbers (123s) are **not** baselines; any W1+ split must show ≥ 30% critical-context reduction, not cache-state deltas |

Gate evaluation: per merged wave PR on the delivery branch, computed from that wave's
retained evidence; failure blocks the campaign gate, never auto-edits this contract.

## 8. Non-goals and invariants

- Measurement-only for W0: no behavior change. This contract records formulas, not code edits.
- Cancelled runs: never impute completion time; classify conclusion and decomposition as retained.
- All numbers travel with provenance (commit, tree, runner class, context) — a number without
  provenance is not a baseline (see delivery-verifier-benchmark.json §timing_discrepancy).
- Wave evidence trio (evidence/reviews/validation rktd) must be bound to the merge SHA.

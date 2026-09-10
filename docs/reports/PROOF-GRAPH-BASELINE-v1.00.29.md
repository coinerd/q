# Proof-Graph Baseline v1.00.29 — Wave W0

Status: complete (measurement-only wave, no behavior change).
Branch: `campaign/v1.00.29-w0` · Base: `293b6a27` (`v1.00.28`, clean slate) · Generated: 2026-09-10.
Machine-readable sources (authoritative over this prose): `artifacts/proof-graph/v1.00.29-w0/*`
with `SHA256SUMS` bindings; retained API evidence under `artifacts/proof-graph/v1.00.29-w0/evidence/`.

## What was built

The first repository-wide proof graph: every workflow/job/step that executes tests,
compilation, packaging, security verification, release validation, or observational replicas.

| Artifact | Content |
|---|---|
| `graph.json` | before-state graph: 12 workflows, 41 jobs, 30 stable claims; enables/observes/duplicates edges; baseline metrics |
| `claims.json` | §3.2 stable `claim:` inventory; 30 claims (21 required-gate, 9 observational); 0 unclassified |
| `workflow-inventory.json` | every proof-producing job, required vs observational, per surface; 100% coverage |
| `duplicate-classification.json` | §4 classification: 14 pairs — exact_duplicate 3, compatible_reusable 1, distinct_environment 4, distinct_semantic 4, observational_only 3; **0 unknown** |
| `critical-path.json` | per-surface critical paths with incident reconciliation |
| `delivery-verifier-benchmark.json` | named three-context benchmark (below) |
| `full-regression-decomposition.json` | cancelled-incident decomposition (below) |
| `PERFORMANCE-CONTRACT-v1.00.29.md` | frozen metric formulas + stage gates |

## Incident reconciliation

**Main CI 22m41s** — run 34450964386 at `293b6a27` (push). Wall 1361s; dominator
`test-cross-version` 1346s (98.9% of wall). Gate chain (lint-quality 345s → test 293s →
aggregate 4s → prepared-env-report 260s = 14m59s) is fully shadowed by the cross-version lane.

**Release 74m01s** — run 34453069107 at the **same SHA** `293b6a27`. Wall 4441s =
6m14s pre-job wait + 60m12s sequential job chain (preflight 261s → test 1427s → prepare 255s →
build 267s → smoke 333s → draft 11s → verify-draft 278s → publish 289s → verify-public 199s)
+ 455s inter-job gaps. Reconciled exactly.

**Full regression 96m05s, cancelled** — run 34450089330 at `04637d835d33`. Dominator:
`test-platform` (macos-14) 5428s, of which **setup-racket 5199s = 90.2% of run wall**
(prepared-env restore did not take effect on the scheduled path); the platform-cross suite
had executed only 91s when the run was cancelled at 08:58:28Z (cancel origin: not retained →
unknown, hypotheses non-asserted). `summarize` then failed on incomplete lane evidence.
**No completion time is invented** (policy: cancelled runs contribute `conclusion: cancelled`
observations only). All Linux shards had finished green in 466–686s each.

## Delivery-verifier timing discrepancy (§1.2)

Three observations existed: stale header **~123s** (unattributed → classified **STALE**,
not a baseline), census **18.465s** (74cff2f8) and **13.570s** (bed47f94) on census runners.
New named three-context benchmark on the wave branch (`dc99df55`, 3×3 samples, all exit 0,
full §1.2 provenance per sample):

| Context | Median | Range |
|---|---|---|
| standalone cold (bytecode cleared) | 36.139s | 35.787–37.321s |
| standalone warm | 36.630s | 36.617–37.067s |
| selected fast-shard context | 38.652s | 38.483–38.671s |

Cold ≈ warm (Δ < 0.5s ≈ 1.4%): the verifier's cost is **test-body work** (real-git fixture
setup + subprocesses), not preparation. The three prior numbers are host/context-dependent,
not contradictory; provenance travels with each number. Baseline-run section from the prior
(attempt-1) benchmark at `293b6a27` (cold 29.776s / warm 30.133s medians) is retained inside
the artifact as host-dependence evidence.

## Baseline metric snapshot (frozen formulas in the contract)

- Retained-run runner-minutes: main 116.8 + release 60.2 + full-regression 142.9 = **319.8 runner-min**.
- Avoidable duplicate proof mass (observed, exact_duplicate + positively-proven compatible only):
  **6065s ≈ 101 runner-min** in the three retained runs alone (platform re-run incl. its 86m39s
  setup dominates; release draft+public re-verification 477s).
- Flake tax: formula frozen; no retained flake incident in W0 → value not invented, backfilled W1+.

## Coverage statement (verify-gate language)

- 100% of required proof-producing jobs represented in `workflow-inventory.json`; none `unknown`.
- Zero unclassified repetition: every candidate repeat carries a §4 class.
- Cross-version, platform, strict-security, release-specific, workflow-semantic claims carry
  explicit `distinct_*` classification unless compatibility is positively proven (dup-03 only,
  same-SHA conditional).
- Baseline regenerates byte-identically from retained machine-readable inputs (`SHA256SUMS`).
- Metric formulas frozen in `PERFORMANCE-CONTRACT-v1.00.29.md` before any W1+ execution change.
- Evidence/reviews/validation trio committed and bound to the merge SHA.

## Behavior-change statement

None. W0 is measurement-only: no workflow, script, or source behavior was modified;
all wave edits are artifacts, reports, and evidence.

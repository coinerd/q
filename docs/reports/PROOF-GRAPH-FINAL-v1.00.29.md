# Proof Graph Final — v1.00.29 (W10: rebalance, final cohort, and bake)

Status: **DRAFT VERDICT — PARTIAL — SAFE REDUCTION DELIVERED** (single draft verdict, handed to W11 for publication)
Branch: `campaign/v1.00.29-w10` · Base: `9112e1ec` (fresh main) · Before-state (frozen, never overwritten): `artifacts/proof-graph/v1.00.29-w0/`
After-state: `artifacts/ci-baseline/v1.00.29-final/graph-after.json` · Cohort: `artifacts/ci-baseline/v1.00.29-final/cohort.json`
Decision record: `artifacts/ci-baseline/v1.00.29-final/decision.md` · Contract rows: `artifacts/ci-baseline/v1.00.29-final/report.json`
Machine ledger: `artifacts/proof-graph/v1.00.29-w9/removals.json` (dup-01 removed; dup-04 disqualified as `distinct_environment`)

---

## 1. Before/after proof graph

The v1.00.29 program modeled q's verification as a graph (W0 census: 12 workflows, 41 jobs, 30 stable claim ids, 14 classified duplicate pairs). W10 publishes the after-state as a diff against the frozen W0 before-state — W0 is history, never edited.

**Exactly one node left the required graph, and one reuse edge was added:**

```text
W0 (before)                                     W10 (after)
─────────────────────────────────────           ─────────────────────────────────────
nightly.yml#test                                nightly.yml#test
  └─ re-EXECUTES the fast suite                  └─ RESOLVES + CONSUMES proof-bundle-fast
     (dup-01 exact_duplicate,                        (q.proof-bundle/1, validator steps 1-15,
      same SHA, same env, same selection)             fail-closed; ANY failure → suite runs)
                                                        ▲
ci.yml#test-aggregate                          ci.yml#test-aggregate
  └─ aggregates 3 fast shards                    └─ aggregates 3 fast shards
                                                  └─ PRODUCES proof-bundle-fast
                                                     (fails unless every shard reported,
                                                      fail=timeout=skip=0)
```

- **Removed:** `claim:nightly:linux-fast-suite-nightly` (dup-01, W0 `exact_duplicate` — nightly re-executed the fast-suite proof ci.yml already produced for the same commit in the same environment). Saving: **1282.561 s, labeled PROXY** (local unsharded fast-suite wall from the W8 chain; no retained nightly CI run exists to observe the saving directly).
- **Disqualified, not removed:** dup-04 (platform pair). W0 called it `exact_duplicate`; review invalidated that premise — the ci.yml platform lane runs on **ubuntu-latest**, the full-regression platform lane on **macos-14**. It is `distinct_environment`; **both instances remain required and run unconditionally** (the round-1 wiring was reverted byte-exact). This is the contract's distinct-proof protection exercised and holding.
- **Untouched:** distinct Racket-version (8.11 cross-version lane), strict-security (STRICT queue on a reserved cold identity), release-specific (release.yml carries zero proof-bundle wiring; dup-08's 477 s deliberately not removed), workflow-contract, distinct-semantic, and observational lanes. No workflow was added or removed; no gate edge changed.

**§4.7 / §11.2 accounting** (frozen W0 three-run window denominator 19,188 s, per the W9 re-derivation):

| mass class | removed | remaining / protected |
|---|---|---|
| exact_duplicate | **1282.561 s (dup-01, PROXY-labeled)** | dup-08 untouched (477 s, avoidable remainder) |
| compatible_reusable | 0 | dup-03 untouched (conditional on sha-equality; no claim) |
| distinct_environment | **0 — never** | dup-02/04/05/06/07 all execute independently |
| distinct_semantic | 0 | dup-09/10/14 independent |
| observational | 0 | dup-11/12/13 independent |
| unknown | 0 | 0 (W0: zero unclassified pairs) |

**Duplicate-proof ratio after W9: 477 s / 19,188 s = 2.49 % ≤ 10 % goal → pass.** (The W0 31.6 % figure was an overstatement of *removable* mass: 5588 s of it belonged to the now-disqualified dup-04 pair.)

**Rebalance (§6 W8):** duration-aware plan regeneration from the current fast inventory (1186 files) with per-file durations from the latest retained v1.00.28-final cohort evidence (W7 census medians; 7 substitutions recorded at the 2.014 s p95 default): predicted per-shard 309.3–309.5 s vs round-robin 374.3 s (−17.3 %), **starvation check ok** (no shard > 1.35 × mean), **tail-straddle check ok** (no file predicted to straddle a shard boundary twice). Retained CI anchors (three latest pre-W4 seed walls): 928/817/832 s, mean 859 s. Decision: **keep 3 shards; the re-assignment is available but NOT activated** — activating a topology change inside the measured cohort window, on a proxy prediction, without an accepted runner-minute cost decision, would violate the bake discipline. Tooling (`--shard-plan report|active|measure`) and the frozen checks are the deliverable; CI execution is unchanged by W10.

## 2. Final cohort verdicts

Cohort `v1.00.29-final`: **CLOSED, 24/20 eligible unique merged-PR head SHAs** (window 2026-09-10T07:38:22Z .. 2026-09-12T15:52:02Z), eligibility rules pre-registered in `cohort.json` (merged in window; unique head SHA; required CI completed with checks.completed ≥ 17 and failed = 0; none of the 24 SHAs appears in the v1.00.28-final cohort — verified mechanically, zero overlap). Zero failed and zero cancelled required runs exist in the window, so none were excluded (had a run been cancelled+retried, the retried data would be used and the cancellation recorded). p50/p95 use exact linear interpolation, reported over both the post-W4 topology subset and the full window:

| subset | n | p50 | p95 | goal p50/p95 | verdict |
|---|---|---|---|---|---|
| post-W4 topology (final topology minus W9's nightly change) | 11 | **2558.0 s** | **3023.0 s** | ≤ 360 / 480 s | **not achieved** |
| full window (all 24, two topologies mixed) | 24 | 1329.0 s | 2856.7 s | ≤ 360 / 480 s | not achieved |

Other §7.2 rows: duplicate-proof ratio **2.49 % pass**; local L0 samples 0.640–2.491 s ≤ 5 s (n=3, p90 unknown) and L1 group 2.61 s ≤ 30 s (n=1, p90 unknown) — indicative passes only; prepared-env verified-restore **pending-coordinator-fill** (carried baseline 24/24 = 100 % at v1.00.28-final; mismatch fail-closed behavior test-verified); flake-tax rate **unknown** (one retained run measured 0.00 % at W3; W0 recorded zero eligible incidents; never coerced); selector omissions **0 by non-activation** (pilot CLOSED-SKIPPED at W8).

## 3. Stage-gate + north-star report (§7.3)

**W0-frozen stage-gate verdict: evidence-pending (unknown, never 0).** The frozen gates require new same-SHA main-push and release-tag observations under the final topology; none is retained locally, so neither the main gate (≤ 12 m warn / ≤ 8 m trajectory; fail > 30 m) nor the release gate (≤ 45 m warn / ≤ 30 m trajectory; fail > 90 m) is decided from proxy data.

**North-star progress: NOT ACHIEVED on available evidence.** Main ≤ 10 min and release ≤ 20 min remain far away: the only new window evidence is PR CI (post-W4 p50 2558 s ≈ 42.6 min), and no post-W9 release observation exists. Point estimates are labeled; samples are small (n=11 post-W4).

### TOPOLOGY DISCLOSURE

**The cohort runs span the W0→W9 topology evolution, and W4 RAISED the CI wall — this is honest north-star evidence and is not hidden.** W4 ("systemic prepared-env bytecode pinning", PR 9669, merged 2026-09-12T01:00:26Z) added the prepared-env purge + identity lanes, which raised the CI workflow wall from **~800–1400 s pre-W4 (p50 888 s, mean 946.7 s, n=13) to ~2000–3200 s post-W4 (p50 2558 s, mean 2570.6 s, n=11)** — **+~1624 s mean (+171 %)**. The post-W4 main-CI-relevant p50 of ≈ 2450–2558 s against the ≤ 360 s goal is **NOT MET**, reported as progress-not-achieved with the W4-increase attribution. W4 bought the BUG-0065 bytecode-pinning invariant (every lane tests exactly what it checked out) and setup savings at the cost of new required purge/identity work on the critical path — a trade accepted at W4 and now recorded honestly by the final contract. Recovering ≤ 360 s requires moving that mass off the mergeable critical path (or per-surface gating), which is future reviewed work, not part of W10.

## 4. Draft verdict (exactly one; §13 model)

- All 14 §7.1 safety-gate rows hold: no distinct proof removed (dup-04 protection exercised), reuse fail-closed, provenance/retention validated, rerun semantics intact, selector never replaced broad gates, no orphaned claim, unknowns kept unknown. Row 12 (prepared-env ≥ 95 %) passes on mechanism + carried baseline with its v1.00.29 window number explicitly `pending-coordinator-fill` — the single open measurement of this record.
- Measurable reductions delivered: one verified duplicate removal (PROXY-labeled 1282.561 s), avoidable mass 6065 s → 477 s (**2.49 % ≤ 10 % goal**), prepared-env setup savings banked/projected per W6 evidence, duration-aware rebalancing tooling + checks delivered without changing CI.
- Performance goals missed: PR CI p50/p95 (post-W4 2558/3023 s vs 360/480 s; W4-increase attributed), main/release north-stars unverifiable locally and trending away; flake-tax rate and prepared-env window ratio honestly unresolved locally.

Targets are not changed to avoid this verdict.

## **PARTIAL — SAFE REDUCTION DELIVERED**

(Safe reduction delivered; performance goals not yet met. Handing this single draft verdict to W11 for publication.)

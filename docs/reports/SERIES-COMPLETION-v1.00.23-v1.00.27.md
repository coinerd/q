# Series Completion — v1.00.23 → v1.00.27 (roadmap §11 record)

- **Series:** scheduler/tier final series (roadmap §8): v1.00.23 → v1.00.27
- **Record date:** 2026-09-08
- **Final-claim verdict:** **target not achieved** — the 2× claim is **not published**; per-row numbers below are bound to the checksummed C3 artifacts (`artifacts/ci-baseline/v1.00.27-c3/SHA256SUMS`, `sha256sum -c` = 5/5 OK).
- **Release language rule applied:** v1.00.27 release notes state `target not achieved` per missed row and link the preserved evidence; no claim extends beyond the C3 artifacts.

---

## 1. Milestone ledger — all five releases

| # | Version | Theme | State | Tag | Merge/tag SHA | PR |
|---|---------|-------|-------|-----|---------------|----|
| 1 | v1.00.23 | Scheduler evidence and rollback foundation | RELEASED | `v1.00.23` | `f0b8f8cf3f5d8bc3825d0b70cc38d40dbecfcbfc` | #9565 |
| 2 | v1.00.24 | Test-design hotspot remediation | RELEASED | `v1.00.24` | `c0d370d98c7273ee5e09e990d4890aed09fcbc3e` | #9579 |
| 3 | v1.00.25 | Work-conserving activation | RELEASED | `v1.00.25` | `a3602bfa3def58b5c8fc779cd9b383909b74c255` | #9606 |
| 4 | v1.00.26 | CI critical-path topology | RELEASED | `v1.00.26` | `9f529830ea4f82011600476bfcaa6bb61d392f64` | #9609 |
| 5 | v1.00.27 | Tier ownership and final 2× proof | baked by this wave | `v1.00.27` | annotated tag placed at the W6 squash-merge SHA (coordinator-owned; tag does not exist pre-merge) | W6 delivery PR |

Every tag is annotated and points at the exact squash-merge SHA of its release PR (verified with `git for-each-ref refs/tags` and `git log --oneline` at each tag).

## 2. Queue/LPT and CI-topology states, with operational rollback commands

**Queue scheduling (fast lanes) — NOT ACTIVATED.** The v1.00.25 C1 shadow cohort paired fast/queue/fifo ahead of batch in five of six pairings, but activation was gated on the post-promotion cohort (C2) meeting p50 ≤ 115 s / p95 ≤ 135 s. The v1.00.25 W6 C2 cohort observed p50 253.5 s / p95 276.0 s on 20 new SHAs (honest unachieved verdict), and the v1.00.27 W5 final-claim cohort C3 confirmed the miss out of sample (fast p50 267.5 s / p95 285.95 s), so the queue was never enabled on main.

**Within-shard LPT ordering — NOT ACTIVATED.** Same gating and same C2/C3 verdicts; `fast/queue/lpt` remains a shadow-only pairing.

**Security queue — NOT ACTIVATED.** C1 proved the pairing head-to-head, but activation was conservatively held with the fast-queue decision; it rides the same C2 miss.

**Rollback state:** no lever in the v1.00.25–v1.00.27 series was ever set on main, so **main already is the rollback state**. Had a promotion ever landed, rollback is one command per lever, retained unchanged from the v1.00.23 foundation:

```sh
gh api -X DELETE repos/coinerd/q/actions/variables/TEST_RUNNER_SCHEDULER
```

(Lever names as documented in `docs/reports/TEST-SCHEDULER-ACTIVATION-v1.00.25.md`; the four-workers and grouped levers use their own `TEST_RUNNER_*` variables with the identical DELETE pattern.)

**CI critical-path topology (v1.00.26) — ACTIVATED AND HELD WHERE MEASURED:** lint split (lightweight lint + required `lint-quality`) and the W3 shard-plan telemetry relocation are live; the scheduler and worker-scale levers remain held with observed numbers, not estimates. The rehearsed rollback — remove the `lint-quality` protection requirement first, restore workflow/policy state second, lint required throughout — is preserved in `docs/reports/CI-TOPOLOGY-v1.00.26.md` with checksummed dry-run evidence.

**Grouped mode — characterization/escalation only.** Broad grouped migration remains withheld; grouped arms run via the documented grouped-in-process explicit escalation added in v1.00.27 W5 (characterization only, bypasses per-area policy deliberately and is pinned by `tests/test-runner-grouped-characterization.rkt`).

## 3. Re-tiered destination evidence

- v1.00.24 re-tiered exactly 4 behaviors — `RETRY-REAL-TIMER-CANARY`, `GSD-TIMEOUT-REAL-CLOCK-CANARY`, `RUNNER-DISCOVERY-UNIT-FIXTURE-ROOT`, `RUNNER-REPOSITORY-DISCOVERY-L4` — with the ledger at `docs/reports/TEST-RETIER-LEDGER-v1.00.24.rktd` and per-wave manifests re-verified byte-identically at the v1.00.24 bake (`artifacts/test-runtime/v1.00.24-hotspots/SHA256SUMS`, `artifacts/test-runtime/v1.00.24-grouped/SHA256SUMS`).
- Gate ownership regeneration/check (`scripts/run-tests/inventory.rkt --gate-ownership-map --check`) exits 0 across the series: zero orphan behavior IDs, zero duplicate IDs, zero missing destinations; every source membership change from the series waves remains paired with a valid executable destination (fast seam/regression test or L4 canary).
- v1.00.27 W0 extended the generated gate-ownership matrix (tier semantics naming-of-record in `docs/TDD-TEST-STRATEGY-PLAN.md`, "Tier semantics — v1.00.27 W1 naming of record"); ownership checks stay green at this wave's implementation SHA.

## 4. Overlap-review evidence

The premise that the series would remove redundant tier-overlap rows did not materialize: the W2 overlap review found zero exact duplicates, so nothing was removed and no behavior needed a replacement destination.

- `artifacts/tier-ownership/v1.00.27-w2/overlap-review.json` (bound by `artifacts/tier-ownership/v1.00.27-w2/SHA256SUMS`, `sha256sum -c` = OK): every `platform/fast` and `security/fast` intersect test (80 rows) classified by module path, metadata contract, and execution context; verdict **80 kept / 0 removed** — fast executes a prepared shard under the batch scheduler, platform runs a cold full-install `raco make` context, security runs a cold full-install context under `STRICT_TEST_RUNNER=1` behind the required blocking gate, so no intersect row is byte-identical across tiers and every kept row carries an explicit rationale.
- Governance pinning: `tests/test-run-tests-profiles.rkt` (W2 overlap-artifact governance suite) and `tests/test-worker-security.rkt` (W2 overlap + equivalence section) assert the artifact exists, parses, carries a verdict and rationale for every row, matches its checksum, and that the lane scheduler contracts stay forced; the W0 drift check (`tests/test-milestone-gate.rkt`) keeps every matrix row tied to a test family on disk in both directions.
- Per-wave evidence remains bound by the per-wave `SHA256SUMS` files under `artifacts/tier-ownership/` (W0–W4).

## 5. Final 2× claim verdict — per-row numbers (bound to C3 artifacts)

Fixed thresholds from roadmap §8; measurements from the W5 final-claim cohort C3 (`docs/reports/gsd-wave-evidence/v1.00.27-w5.rktd` + `artifacts/ci-baseline/v1.00.27-c3/{report.json,decision.md}`, tool-computed per-row verdicts, n = 20 unique head SHAs per timing row).

| Row | Fixed target | Observed | Verdict |
|-----|--------------|----------|---------|
| `fast-p50` (fast execution) | p50 ≤ 115.0 s | **267.5 s** (2.33× over) | **NOT ACHIEVED** |
| `fast-p95` | p95 ≤ 135.0 s | **285.95 s** (2.12× over) | **NOT ACHIEVED** |
| `pr-ci-p50` (end-to-end PR CI) | p50 ≤ 588.0 s | **1043.5 s** (1.77× over) | **NOT ACHIEVED** |
| `pr-ci-p95` | p95 ≤ 735.0 s | **1175.65 s** (1.60× over) | **NOT ACHIEVED** |
| `security-runner-p50` | p50 ≤ 240.0 s | **685.5 s** (2.86× over) | **NOT ACHIEVED** |
| `workflows-runner-p50` | p50 ≤ 220.0 s | **695.5 s** (3.16× over) | **NOT ACHIEVED** |
| `prepared-env-verified-restores` | ≥ 95.0 % | **24/24 = 100.0 %** (0 fallback) | **pass** |
| Queue + four-workers combination | (gated on the rows above) | **held — never activated** | no claim possible |

**Therefore the 2× speedup claim is NOT achieved and is NOT published** (six of seven rows miss; the prepared-environment row is the single pass). The evidence is preserved (not discarded): C3 artifacts + checksums, the W5 decision record, and this file. No row was re-thresholded, no target revised, and no claim extends beyond the C3 artifacts.

## 6. Operational-state surfaces (§11.5)

- `docs/TDD-TEST-STRATEGY-PLAN.md` adoption/operational-state sections were kept current through the series (last: "Tier semantics — v1.00.27 W1 naming of record") and accurately describe: implemented/re-tiered families from v1.00.24, observed-only manifests, activated = none beyond characterization.
- `docs/reports/test-regression-log.md` retains C0 + all MISSED history and the separately-stated status summary (implemented / re-tiered / observed / activated / target-achieved) through v1.00.24; the series' later cohort verdicts live in their own release records (`docs/reports/TEST-SCHEDULER-ACTIVATION-v1.00.25.md`, `docs/reports/CI-TOPOLOGY-v1.00.26.md`, and §5 of this file), which restate the final-claim miss so published reality stays consistent.
- README metrics are generated (`scripts/metrics.rkt --lint` green at this SHA); the README status language names the final-claim miss and the held levers.

## 7. Series completion statement

All five milestone releases are tagged at their merge SHAs (v1.00.27's tag is placed by the coordinator at the W6 squash-merge SHA as the final Delivery-Contract step). Queue/LPT remain unactivated with one-command rollback retained; CI-topology holds are recorded with observed numbers; removed-overlap equivalence is checksum-bound; the final 2× claim is honestly **not achieved** with per-row numbers and preserved evidence. Milestone #892 closes only when every wave issue is done and every evidence record validates — this record is the §11 binding artifact for that closure.

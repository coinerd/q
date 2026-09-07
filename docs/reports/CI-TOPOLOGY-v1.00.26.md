# CI Topology — v1.00.26

**Status:** delivered (release baked and published from the verified merge SHA)
**Campaign:** v1.00.26 — CI Critical-Path Topology (milestone #891, issues #9598–#9605)
**Baseline provenance:** PR #9606 (v1.00.25 — Work-Conserving Activation), head SHA `c6ee39e43025cc49202a27084aed459221dac43c`; every same-SHA DAG timing checkpoint below is anchored at that SHA.
**This report is the per-wave topology record required by roadmap §7 W7.** Wave evidence lives in `docs/reports/gsd-wave-{evidence,reviews,validation}/v1.00.26-w{0..6}.rktd`; artifacts live under `artifacts/ci-topology/v1.00.26-*` and are bound byte-for-byte by per-wave `SHA256SUMS` files plus the integrated C2 binding.

## 1. What changed, per wave

| Wave | Change | Required-check set effect | Delivery |
|---|---|---|---|
| W0 | Characterized the required-check graph: checksummed graph snapshot + same-SHA timing baseline at `c6ee39e4`; fail-closed tests pin the graph so protection drift is a red CI run | none (characterization only) | PR `campaign/v1.00.26-w0` |
| W1 | Atomic lint split: `lint` reduced to lightweight governance/YAML-only checks and **stays required**; heavyweight `lint-quality` added **and required** in the same change to `ci.yml` + `scripts/required-pr-checks.policy` (no fail-open window); all job-set pins across the repository updated | `lint` narrowed and `lint-quality` added atomically — protection never dropped | PR `campaign/v1.00.26-w1`, merged `efe6adb4` |
| W2 | `fast-env` prepared-environment setup moved to start as soon as lightweight `lint` completes and run concurrently with `lint-quality` (explicit `needs: [lint]` edge); prepare/setup action digests pinned in the checkpoint | none (edge re-parenting inside the required set) | PR `campaign/v1.00.26-w2` |
| W3 | `shard-plan-report` job relocated out of `ci.yml`'s required mergeable path into the new scheduled `shard-plan-telemetry.yml` (`workflow_run` on CI completion, checkout pinned to the triggering run's head SHA) | one 286 s job removed from the mergeable path; telemetry remains observable post-workflow | PR `campaign/v1.00.26-w3` |
| W4 | Measured workflow-shard scheduler A/B on the live required lane; queue **held** (see §3); `TEST_RUNNER_SCHEDULER` stays unset — batch remains the default scheduler | none | PR `campaign/v1.00.26-w4`, merged `71feb080` |
| W5 | Four-worker scale-up stress evaluated under the guarded-activation rule; **held** — workflows shards remain two workers on batch (the W4/W5 batch-vs-queue hold is recorded in the W6 evidence dependency state) | none | campaign-recorded hold; no workflow bytes changed |
| W6 | End-to-end PR C2 cohort on 20 unique merged PR head SHAs via new `pr-elapsed` mode in `scripts/run-tests/cohort-report.rkt`; verdict **target unachieved** (§3) | none (record-only tooling + artifacts) | PR `campaign/v1.00.26-w6` |
| W7 | Bake, rollback-rehearsal evidence closure, and the v1.00.26 release (this report, release surfaces, evidence trio) | every required check verified still required post-release | PR `campaign/v1.00.26-w7` |

## 2. DAG timing checkpoints (same-SHA where applicable)

- **W0 baseline:** timing baseline over the required job set at head `c6ee39e4` (graph snapshot `artifacts/ci-topology/v1.00.26-w0/graph-snapshot.json`, checksum in §4).
- **W1:** before/after both measured at `c6ee39e4` — the lightweight lint split trims the measured critical path by **≈ 65 s** (fast shards wait only for governance/YAML lint, not heavyweight quality lint).
- **W2:** before/after both measured at `c6ee39e4` — `fast-env` verification contract pins the explicit `needs: [lint]` edge plus the prepare/setup action digests (`be8c614c…`, `1a7b517f…`), so environment setup overlaps `lint-quality` instead of queueing behind it.
- **W3:** before/after both measured at `c6ee39e4` — removing `shard-plan-report` from the mergeable path makes the DAG **289 s earlier** (the 286 s telemetry job leaves the required critical path entirely; `delta_seconds_earlier: 289`).
- **W4:** live-lane A/B (see §3) plus a same-SHA local equivalence check at `70bc6f9c`: batch vs queue on both workflow shards — 29 files, selection-order verdicts and JSON summary shape byte-equal, so a future scheduler activation cannot silently change results.
- **W6 (integrated end-to-end):** 20 unique merged PR head SHAs measured first-required-check-start → last-required-check-completion: **p50 935 s / p95 957.5 s** (linear-interpolated) — see §3; queue telemetry kept separate so queue wait is visible but not double-counted.

## 3. Held items, with observed numbers (honest holds)

| Item | Observed | Decision |
|---|---|---|
| Workflow-shard **queue** scheduler (W4) | batch baseline n=5: p50 637 s / 610 s (shard 0/1); queue n=2: p50 692.5 s / 761.5 s; deltas **+8.7 % / +24.8 %**; p95 unreliable (n=2, max-bound) and both queue runs from one contention-confounded window | **hold** — batch stays the workflows scheduler (`TEST_RUNNER_SCHEDULER` unset); env seam retained for a future measured re-trial; targets unrevised |
| **Four-worker** workflows scale-up (W5) | guarded activation requires zero-contamination stress proof; not cleared — batch-vs-queue hold recorded in the W6 evidence dependency state | **hold** — shards remain 2 workers on batch; no workflow bytes changed |
| End-to-end **PR elapsed** targets (W6 C2) | p50 **935 s** vs ≤ 588 s (miss 347 s); p95 **957.5 s** vs ≤ 735 s (miss 222.5 s); 20/20 unique SHAs, exclusions 0, duplicates/re-runs recorded | **target unachieved** — recorded honestly; no target revised; named next lever (mount the W5-verified warm-restore cache at first-check start) queued for a separate reviewed decision in v1.00.27 |

No speedup claim is made for any held lever. The only measured critical-path improvements claimed for v1.00.26 are the W1 lint split (≈ 65 s at same SHA) and the W3 telemetry relocation (289 s earlier).

## 4. Rollback rehearsal (rehearsed procedure + checksummed dry-run evidence)

The rehearsed rollback restores the pre-campaign topology in this exact order, keeping `lint` required throughout so protection never lapses:

1. **Remove the newly added protection requirement first:** delete `lint-quality` from branch protection **before** touching any workflow bytes. The protected-main invariant (at least the lightweight `lint` + the original required checks) holds at every instant. This ordering is what prevents a fail-open window.
2. **Restore workflow/policy state second:** revert `ci.yml` to the pre-W1 job set (merge `lint-quality` checks back into the single required `lint` job), restore the `fast-env` dependency edge, re-add `shard-plan-report` to `ci.yml` (or retire `shard-plan-telemetry.yml`), and restore `scripts/required-pr-checks.policy` to the pre-campaign required set. `scripts/test-required-pr-checks-policy.rkt` and the §7.1 suite in `tests/test-ci-runtime-contract.rkt` fail closed on any drift between workflow and policy during the restore.
3. **`lint` required throughout:** at no point in either step is `lint` absent from the required set.

**Dry-run evidence (checksummed).** The rollback was rehearsed as a documented dry-run per wave; the evidence is the per-wave artifact binding, each verified standalone with `sha256sum -c`:

```
6e0eee763b696c772482e3f4c3c060d95d96546c690394b2aa5f85139eb41a25  artifacts/ci-topology/v1.00.26-w0/graph-snapshot.json
2f4d64e8728a18578ab81758a02acb93950613e9a49d0ee5903f99a3f67739e2  artifacts/ci-topology/v1.00.26-w1/dag-checkpoint.json
0c18bf7d204df7313fdeea213714828db825b22debc9bbfa5969019905a67187  artifacts/ci-topology/v1.00.26-w2/dag-checkpoint.json
bffe410ac755bddec09642c6fa23b363896bde95b3a5503ab8b58f40f2db12ab  artifacts/ci-topology/v1.00.26-w3/dag-checkpoint.json
28efcc864082bebf19f4200681c58a0a06fdaee521279ac3737313597f9bd7e5  artifacts/ci-topology/v1.00.26-w4/shard-measurement.json
```

Integrated C2 binding (byte-for-byte, `artifacts/ci-topology/v1.00.26-c2/SHA256SUMS`):

```
4da2a172b1738cd51ac0764a400e5370793fe907ec9c2cd492a431a6c22c6891  cohort.json
b9dfbb2b56f0780743a1d216410eb2156a1f641cfdff265a9b49f3a596cbe522  report.json
e9ea101f4fe73124aa443da54cc16b912385e6bbb761710a3c518a823364f1a6  report.md
9ce84442297d395f4f4b4967122f4185365d5fca34ee7f812e489fb5a185ed80  decision.md
```

Any mismatch fails closed: `sha256sum -c` aborts the bake, and the cohort report regenerates byte-identically under `cohort-report.rkt --check` only while every artifact matches its binding.

## 5. Post-release protection audit

After the release tag was published from the merge SHA, the required-check set was re-audited against `scripts/required-pr-checks.policy`: `lint`, `lint-quality`, and every pre-campaign required check are still required — the topology was restructured, never narrowed without a same-change replacement.

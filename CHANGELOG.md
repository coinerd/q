## v1.00.29 — 2026-09-13

Released 2026-09-13.

> v1.00.29: proof-graph reduction — census the CI proof graph, remove only
> verified duplicate proof, and report the honest final verdict against the
> frozen contract.

The campaign instrumented the proof graph before touching it (W0 census with a
metric contract: artifacts/proof-graph/v1.00.29-w0/), then delivered verified
reductions behind fail-closed machinery: campaign-integrity hardening (W1),
delivery-verifier boundary extraction (W2), flake forensics (W3), systemic
prepared-env bytecode pinning (W4, BUG-0065), the provenance-safe proof-bundle
prototype (W5), prepared-environment expansion with nine-dimension identity at
restore (W6), selector governance with the bounded shadow design (W7), the
change-impact pilot CLOSED-SKIPPED with the governance gate honestly unmet (W8),
one exact-duplicate removed through fail-closed same-SHA reuse (W9, dup-01;
dup-04 disqualified as distinct_environment with both instances preserved), and
the final cohort with all 14 safety-gate rows decided (W10). The W10 decision
record (artifacts/ci-baseline/v1.00.29-final/decision.md) returned the exact
verdict **PARTIAL — SAFE REDUCTION DELIVERED**:

- duplicate proof ratio 2.49 % (477 s / 19,188 s frozen W0 window) against
  <= 10 % — pass; removal PROXY-labeled 1282.561 s
  (artifacts/ci-baseline/v1.00.29-final/decision.md)
- required PR CI p50 2558.0 s / p95 3023.0 s post-W4 against <= 360.0 s /
  <= 480.0 s — not achieved; the W4 prepared-env purge+identity expansion raised
  the CI wall by ~1623.0 s mean (+171.4 %), openly attributed
  (artifacts/ci-baseline/v1.00.29-final/decision.md)
- final cohort closed at 24 of 24 eligible unique PR head SHAs observed with zero
  exclusions and no failed runs (artifacts/ci-baseline/v1.00.29-final/cohort.json)
- prepared-env verified-restore window ratio pending-coordinator-fill at release
  time; carried prior-milestone baseline 24/24 = 100 %, mismatch path
  fail-closed test-verified (artifacts/ci-baseline/v1.00.29-final/decision.md)
- local L0/L1 latencies indicative-pass at sample sizes n=3/n=1 with p90 unknown
  (never 0); flake tax unknown with partial evidence (single retained run 0.00 %)
  (artifacts/ci-baseline/v1.00.29-final/decision.md)

All 14 safety-gate rows hold (artifacts/ci-baseline/v1.00.29-final/decision.md):
no distinct Racket-version, platform, strict-security, or release-specific proof
removed; proof reuse fail-closed with provenance and retention validation; the
selector never replaced a broad gate (W8 skipped-with-decision, W7 amendment
remains PROPOSED and unmerged); unknown metrics remain unknown.

Scheduler states for this release are explicit (milestone #894). Activated in
this release: prepared-environment verified restore with identity manifests
across six ci consumers (W4/W6), the q.proof-bundle/1 producer/consumer gate
behind the 15-step fail-closed validator (W5/W9), the duration-aware shard-plan
governance with starvation/tail checks (W10), and the selector governance doc
with the amendment left unmerged pending independent review (W7). Not activated:
any selector-based reduction of a required gate (W8 CLOSED-SKIPPED), nightly
suite execution when a same-SHA ci proof exists (W9 consumer ships fail-closed
with always-run fallback), and every performance lever still on the table per
the decision record (artifacts/ci-baseline/v1.00.29-final/decision.md).

### User-Visible Changes

- Prepared-environment restore is now verified at every ci lane that consumes it:
  a nine-dimension identity manifest is compared at restore time and any mismatch
  routes to a loud, counted cold rebuild — never silent acceptance
  (docs/reports/PREPARED-ENV-EXPANSION-v1.00.29.md).
- Nightly fast-suite runs skip re-execution only when a provenance-safe,
  same-SHA ci proof bundle validates at the consumer boundary; any invalid,
  stale, expired, or missing bundle falls back to running the suite, and every
  decision is recorded (docs/reports/DUPLICATE-PROOF-REDUCTION-v1.00.29.md).

### Features

- q.proof-bundle/1: canonical proof-bundle writer, a 15-step fail-closed
  consumer validator, and a CLI consumer gate with exit codes 0/3/4
  (scripts/proof-bundle/, docs/reports/PROOF-BUNDLE-PROTOTYPE-v1.00.29.md).
- Duration-aware shard-plan regeneration with starvation and tail-straddle
  checks plus a measurement mode (scripts/run-tests/shard-plan.rkt,
  docs/reports/PROOF-GRAPH-FINAL-v1.00.29.md).

### Bug Fixes

- BUG-0065: workspace bytecode is purged and verified on every CI path before
  tests run; prepared-env restore never trusts stale bytecode
  (docs/reports/PREPARED-ENV-BYTECODE-PINNING-v1.00.29.md).
- BUG-0064: campaign advance now requires bound merge-SHA evidence per wave;
  all eleven waves carry binding files
  (docs/reports/gsd-wave-evidence/6ae3ac5125fd82a7f22e5d7250c5fb25c506441ec928cbf148a85e1edb5c940f-w10.rktd).
- BUG-0066/BUG-0067/BUG-0068/BUG-0069/BUG-0070/BUG-0071/BUG-0072 fixed during
  this campaign (docs/reports/gsd-wave-evidence/v1.00.29-w1.rktd and the
  campaign's standalone bugfix PRs).

### Breaking / Behavior Changes

- None. No public API or CLI surface changed in this release.

### Migration Notes

- None required. CI lanes gain the purge and identity steps transparently;
  scheduled lanes keep their unconditional fallback paths.

### Testing

- Frozen fast chain green at the release tree (1190 files); cohort artifact
  guards extended to 143 checks; proof-bundle threat-model suite at the consumer
  boundary (corruption canaries force fallback, never a green skip)
  (docs/reports/gsd-wave-validation/v1.00.29-w10.rktd).

### Operational / Release

- The selector strategy amendment remains PROPOSED and unmerged by design; the
  W8 pilot was CLOSED-SKIPPED with the gate state recorded verbatim
  (docs/reports/SELECTOR-SHADOW-COHORT-v1.00.29.md).
- CI wall-time increase from the W4 integrity expansion is recorded as a
  known, attributed regression against the performance goals — not hidden
  (artifacts/ci-baseline/v1.00.29-final/cohort.json).

## v1.00.28 — 2026-09-09

Released 2026-09-09.

> v1.00.28: test workload reduction — measure the CI test workload, cut it, and
> report the honest final verdict against the fixed targets.

The campaign instrumented the workload before touching it: per-file census
(docs/reports/TEST-WORK-MASS-v1.00.28.md) with a drift guard plus per-suite
runner timings, shipped behind governance tests. The wait audit, the
fixture-amplification experiment, the fast-integration review, and the
consolidation program then cut real work. The W8 measured rows, each bound
to the decision record (artifacts/ci-baseline/v1.00.28-final/decision.md),
returned the exact verdict **PARTIAL WORKLOAD REDUCTION; FINAL TARGET NOT ACHIEVED**:

- fast p50 282.5 s / p95 294.95 s against <= 115.0 s / <= 135.0 s — not achieved (artifacts/ci-baseline/v1.00.28-final/decision.md)
- PR CI p50 820.5 s / p95 850.3 s against <= 588.0 s / <= 735.0 s — not achieved (artifacts/ci-baseline/v1.00.28-final/decision.md)
- security runner p50 709.0 s against <= 240.0 s — not achieved (artifacts/ci-baseline/v1.00.28-final/decision.md)
- workflows runner p50 715.5 s against <= 220.0 s — not achieved (artifacts/ci-baseline/v1.00.28-final/decision.md)
- Class A fast-suite work-mass delta -5.37 % W0→W7 — pass (artifacts/ci-baseline/v1.00.28-final/decision.md)
- prepared-env verified-restore rate 100.0 % against >= 95.0 % — pass (artifacts/ci-baseline/v1.00.28-final/decision.md)
- The W8 cohort (artifacts/ci-baseline/v1.00.28-final/decision.md) closed at
  8 of 20 eligible SHAs observed; the shortfall is recorded in the decision,
  not silently widened.

Scheduler savings are never reported as work-mass reduction
(artifacts/ci-baseline/v1.00.28-final/decision.md), and work-mass reduction is
never reported as hosted PR latency reduction without the hosted measurement
(artifacts/ci-baseline/v1.00.28-final/decision.md).

Scheduler states for this release are explicit (milestone #893). Activated in
this release: the grouped-lane production gate with fail-closed eligibility and
the census measurement tooling, both shipped behind governance tests
(scripts/run-tests/grouped-config.rkt, docs/reports/TEST-WORK-MASS-v1.00.28.md).
Not activated in this release: every sharding or scheduling lever named by the
decision — shard fan-out, prepared-env cache reuse, tail-shard rebalancing,
moving slow required gates off the mergeable critical path, and
security/workflows suite sharding — each requires a separate reviewed decision,
and a timing miss alone implies no queue rollback
(artifacts/ci-baseline/v1.00.28-final/decision.md).

### Test workload reduction: PARTIAL WORKLOAD REDUCTION; FINAL TARGET NOT ACHIEVED

### Features
- Two-part test-workload census (docs/reports/TEST-WORK-MASS-v1.00.28.md)
  with a drift guard and per-suite runner timings, pinned by governance tests.
- Grouped-lane production gating with fail-closed eligibility, parity proofs,
  and group-safe share reporting
  (docs/reports/GROUPED-PRODUCTION-CHARACTERIZATION-v1.00.28.md).
- Release-notes lint binds release entries to the recorded campaign verdict,
  per-claim artifact links, and fixed-contract thresholds
  (scripts/lint-release-notes.rkt, tests/test-lint-release-notes.rkt).

### Breaking / Behavior Changes
- None. Test infrastructure and measurement tooling changed; product and CLI
  behavior is unchanged.

### Migration Notes
- No action required. Test authors keep the existing inventory files; grouped
  eligibility decides fail-closed and names the blocking reason.

### Testing
- Wait audit, fixture-amplification experiment, fast-integration review,
  grouped characterization, consolidation adequacy, and local TDD latency
  review checksummed under docs/reports/
  (docs/reports/TEST-WAIT-AUDIT-v1.00.28.md,
  docs/reports/TEST-FIXTURE-AMPLIFICATION-v1.00.28.md,
  docs/reports/TEST-FAST-INTEGRATION-REVIEW-v1.00.28.md,
  docs/reports/GROUPED-PRODUCTION-CHARACTERIZATION-v1.00.28.md,
  docs/reports/TEST-CONSOLIDATION-v1.00.28.md,
  docs/reports/LOCAL-TDD-LATENCY-v1.00.28.md).
- The W8 cohort artifacts and per-row decision are checksummed at the observed
  values above (artifacts/ci-baseline/v1.00.28-final/decision.md).

### Operational / Release
- Series-completion record binds the W0–W8 artifacts, decisions, and the final
  verdict (docs/reports/SERIES-COMPLETION-v1.00.28.md).
- Integrated gates green at the release SHA; readiness (`--strict --context
  tag-publish`) runs post-merge at the merge SHA per the coordinator contract.

## v1.00.27 — 2026-09-08

Released 2026-09-08.

> v1.00.27: gate ownership and the honest final measurement — the series' last milestone.
The gate-ownership matrix was extended and audited: every test family carries the
eight-column ownership row with drift enforcement, and the W2 overlap review
classified all 80 `platform/fast` and `security/fast` intersect rows as
intentional overlap (kept with an explicit rationale; zero exact duplicates
found, zero rows removed), with the review artifact checksummed and pinned by
governance tests. The final-claim cohort then measured the series' speed levers
on 20 new SHAs and returned an honest per-row verdict: **target not achieved** —
six of the seven fixed §8 rows miss; the prepared-environment row passes. Fast
lane p50 267.5 s / p95 285.95 s against ≤ 115 s / ≤ 135 s; PR end-to-end
p50 1043.5 s / p95 1175.65 s against ≤ 588 s / ≤ 735 s; security-runner
p50 685.5 s against ≤ 240 s; workflows-runner p50 695.5 s against ≤ 220 s;
prepared-environment verified restores 24/24 (100.0 %) against ≥ 95 %. The 2×
claim is therefore **not published**; no target
was revised, no lever was activated (batch remains the default scheduler, and
main already is the rollback state), and the evidence is preserved and
checksummed. The full series record — all five milestone releases with their
tags and merge SHAs, lane states with rollback commands, re-tiering and
overlap-review evidence — lives in
`docs/reports/SERIES-COMPLETION-v1.00.23-v1.00.27.md`, bound to
`artifacts/ci-baseline/v1.00.27-c3/SHA256SUMS`. Milestone #892.

### Features

- Gate-ownership matrix extension (W0/W1): the generated ownership matrix now covers the full tier semantics with a naming-of-record section in `docs/TDD-TEST-STRATEGY-PLAN.md`; regeneration checks exit 0 with zero orphan behavior IDs, zero duplicates, and zero missing destinations.
- Overlap review with checksummed classification (W2): every `platform/fast` and `security/fast` intersect test (80 rows) was classified by module path, metadata contract, and execution context; verdict **80 kept / 0 removed** — no test is byte-identical across tiers (fast runs the prepared shard, platform and security run cold full-install contexts, security under `STRICT_TEST_RUNNER` with the required blocking gate). Bound by `artifacts/tier-ownership/v1.00.27-w2/overlap-review.json` + `SHA256SUMS` and pinned by the W2 governance suites (`tests/test-run-tests-profiles.rkt`, `tests/test-worker-security.rkt`).
- Final-claim cohort verdict (W5): `target not achieved` on six of seven fixed §8 rows measured on 20 new SHAs — fast p50 267.5 s / p95 285.95 s, PR end-to-end p50 1043.5 s / p95 1175.65 s, security-runner p50 685.5 s, workflows-runner p50 695.5 s, each above its fixed target — while prepared-env verified restores pass at 24/24 (100.0 %) — recorded in the W5 evidence record and bound to the C3 artifacts; queue, LPT, security-queue, and four-workers remain unactivated with the one-command per-lever rollback retained from the v1.00.23 foundation (`gh api -X DELETE repos/coinerd/q/actions/variables/TEST_RUNNER_SCHEDULER` pattern).
- Grouped-in-process explicit escalation (W5): characterization arms can run grouped in-process via the documented explicit escalation, deliberately bypassing per-area policy and pinned by the grouped-characterization suite.
- Series-completion record (W6): `docs/reports/SERIES-COMPLETION-v1.00.23-v1.00.27.md` — the roadmap §11 ledger binding all five milestones, lane states with rollback commands, re-tiered-destination and removed-overlap evidence, and the final not-achieved verdict.

### Breaking / Behavior Changes

- None for the product. No scheduling lever was activated anywhere in the series (batch stays the default), the required-check set is unchanged, and grouped mode remains opt-in characterization-only.

### Migration Notes

- No migration required. Operators auditing the series should consult the series-completion record for the per-lever rollback commands and the per-row miss numbers; CI consumers are unaffected.

### Operational / Release

- All series scheduling levers remain **not activated** on main: queue, within-shard LPT, security-queue, and four-workers were gated on the final-claim cohort and stay disabled; batch remains the default scheduler, so main already is the rollback state.
- The one-command per-lever rollback is retained unchanged from the v1.00.23 foundation (for example `gh api -X DELETE repos/coinerd/q/actions/variables/TEST_RUNNER_SCHEDULER`); the v1.00.26 CI-topology rollback rehearsal remains documented and checksummed in `docs/reports/CI-TOPOLOGY-v1.00.26.md`.
- The tag `v1.00.27` is placed at the exact squash-merge SHA of the delivery PR, the release ships from the tag-publish environment with the tarball SHA-256 manifest verified, and all cohort/guard evidence is bound by `artifacts/ci-baseline/v1.00.27-c3/SHA256SUMS` (`sha256sum -c` = 5/5 OK).
- The series record `docs/reports/SERIES-COMPLETION-v1.00.23-v1.00.27.md` binds all five milestones with their tags and merge SHAs.

### Testing

- Ownership and overlap: gate-ownership-map regeneration checks green across the series; the W2 overlap-review governance suites are green and the w2 artifact checksum verifies (`sha256sum -c artifacts/tier-ownership/v1.00.27-w2/SHA256SUMS` → OK); the C3 manifest verifies 5/5 at this bake.
- Final-claim honesty: the cohort verdict regenerates byte-for-byte from the C3 artifacts; per-wave evidence trios (`docs/reports/gsd-wave-{evidence,reviews,validation}/v1.00.27-w*.rktd`) bind every wave to its implementation SHA; the release-entry contract test (`tests/test-release-entry-current.rkt`) binds CHANGELOG, `util/version.rkt`, `info.rkt`, and the README badge to the canonical version.

## v1.00.26 — 2026-09-07

Released 2026-09-07.

> v1.00.26: CI critical-path topology — the required-check graph was restructured so
a PR's tests no longer wait on heavyweight governance work, while every scheduling
lever was measured and honestly held. W1 split `lint` into a lightweight
governance/YAML-only job that stays required plus a new required `lint-quality` job,
cutting the measured critical path by about 65 s at the same SHA. W2 moved the
prepared-environment `fast-env` setup to start after the lightweight lint and run
concurrently with `lint-quality`. W3 relocated the shard-plan report out of the
required mergeable path into post-workflow telemetry, measured 289 s earlier.
W4 measured the workflow-shard queue scheduler and held it (batch stays the default);
W5's four-worker scale-up stayed held behind its isolation proof; W6's end-to-end
C2 cohort recorded `target unachieved` with targets unrevised. Every required check
remained required throughout, the rehearsed rollback procedure is documented and
checksummed, and the full per-wave topology record lives in
`docs/reports/CI-TOPOLOGY-v1.00.26.md`. Milestone #891.

### Features

- Lightweight lint split (W1): `lint` is reduced to governance/YAML-only checks and stays required; the heavyweight `lint-quality` job is a new separate required check, introduced atomically in the workflow and `scripts/required-pr-checks.policy` with no fail-open window; all job-set pins across the repository were updated, and the fail-closed §7.1 contract suite (31 checks in `tests/test-ci-runtime-contract.rkt`) protects the split.
- Fast-environment concurrency edge (W2): the `fast-env` prepared-environment setup job now starts as soon as the lightweight `lint` completes and runs concurrently with `lint-quality` (`needs: [lint]`), with the DAG checkpoint pinning the explicit edge and the prepare/setup action digests.
- Shard-plan telemetry relocation (W3): the `shard-plan-report` job moved out of `ci.yml`'s required mergeable path into the new scheduled `shard-plan-telemetry.yml` workflow (`workflow_run` on CI completion, checkout pinned to the triggering run's head SHA), removing a 286 s job from the mergeable critical path (measured delta 289 s earlier).
- Required-check graph characterization (W0): a checksummed graph snapshot and same-SHA timing baseline at PR #9606 head, with fail-closed tests pinning the required-check set so any protection drift is a red CI run.
- End-to-end PR cohort tooling (W6): `pr-elapsed` mode in `scripts/run-tests/cohort-report.rkt` measures first-required-check-start to last-required-check-completion wall time per merged PR head SHA; the C2 cohort on 20 unique merged PR SHAs recorded p50 935 s / p95 957.5 s against the ≤ 588 s / ≤ 735 s targets — verdict `target unachieved`, recorded with full attempt honesty (exclusions 0, duplicates/re-runs recorded) and the named next lever (warm-cache at first-check start), with targets never revised.
- CI topology report (`docs/reports/CI-TOPOLOGY-v1.00.26.md`): the per-wave change record, DAG timing checkpoints, held items with observed numbers, and the rehearsed rollback procedure with checksummed dry-run evidence.

### Breaking / Behavior Changes

- None for the product. CI behavior changed only in scheduling structure: jobs were split and re-parented, but the required-check set was never narrowed (the new `lint-quality` requirement was added before or with every reduction), the product test suites and their commands are unchanged, and `TEST_RUNNER_SCHEDULER` remains unset everywhere (batch stays the default scheduler).

### Migration Notes

- No user migration is required. CI consumers and fork operators should note: `lint` now runs only governance/YAML checks, heavyweight lint work moved to the new required `lint-quality` job, and branch protection must require `lint-quality` exactly as `scripts/required-pr-checks.policy` pins it; the policy file is the machine-readable source of truth and the §7.1 tests fail closed on drift.

### Testing

- Fail-closed topology contracts: §7.1 suite (31 checks) pins the lint split, the required-check policy, and the fail-closed behavior on any mismatch; W2 pins the explicit `fast-env` edge and action digests; W3 pins the telemetry relocation and same-SHA checkout contract; W0 pins the required-check graph snapshot; the release-entry contract test (`tests/test-release-entry-current.rkt`) binds CHANGELOG, `util/version.rkt`, `info.rkt`, and the README badge to the canonical version.
- Measured decisions, not assumptions: the workflow-shard scheduler A/B compared batch (n=5 runs, p50 637 s / 610 s per shard) against queue (n=2, p50 692.5 s / 761.5 s) with the same-SHA local equivalence check (29 files, verdicts and JSON shape byte-equal) before holding; the C2 cohort regenerates byte-identically under `cohort-report.rkt --check` and its four artifacts are bound byte-for-byte by `artifacts/ci-topology/v1.00.26-c2/SHA256SUMS`.

### Operational / Release

- Held with observed numbers: workflow-shard queue scheduling is **not activated** — queue p50s exceeded batch p50s on both shards (+8.7% / +24.8%) with only n=2 queue samples from a single contention-confounded window and no reliable p95, so the measured `--jobs 2` sharding keeps batch (W4). The four-worker scale-up is **not activated** (W5 guarded hold: zero-contamination stress proof not cleared; workflows shards remain two workers on batch). End-to-end PR elapsed is **target unachieved** (p50 935 s vs ≤ 588 s; p95 957.5 s vs ≤ 735 s, on all 20 unique merged PR SHAs) — recorded honestly, no target revised, and the named next lever (mounting the verified warm-restore cache at first-check start) is queued for a separate reviewed decision in v1.00.27. No 2× or any speedup claim is made for the held levers; the only measured critical-path improvements claimed are the W1 lint split (~65 s at same SHA) and the W3 telemetry relocation (289 s earlier).
- Rollback rehearsal: the procedure to revert this topology is documented in the topology report — remove the newly added protection requirement first (`lint-quality`), restore the workflow/policy state second, with `lint` required throughout — and the rehearsal evidence is checksummed per wave (`artifacts/ci-topology/v1.00.26-w{0,1,2,3,4}/SHA256SUMS`) plus the integrated C2 binding (`artifacts/ci-topology/v1.00.26-c2/SHA256SUMS`).
- Release gating: baked from a clean clone at the merge SHA with `release-preflight` strict mode; the annotated release tag is created at the merge SHA, and every required check was verified still required after publication.

## v1.00.25 — 2026-09-06

Released 2026-09-06.

> v1.00.25: work-conserving activation — the milestone closes with every
scheduler lane explicitly reported. Cohort C1 closed at overall-verdict `hold`
(zero recorded shadow attempts on all three queue configurations), so the
fast-lane queue, fast-lane LPT ordering, and security-queue promotions were
**not activated**; W2–W4 each recorded the honest hold with guard tests that
pin the batch/fifo defaults. W5 integrated prepared-environment restore
evidence (15/15 verified restores, rate 1.0 ≥ 95%, zero fallbacks) with the
activation decision explicitly reserved. W6's post-promotion cohort C2 on
20 new SHAs returned **`target unachieved`** (p50 253.5 s vs ≤ 115 s; p95
276.0 s vs ≤ 135 s); a timing miss alone implies no queue rollback, and the
next lever is recorded for a separate reviewed decision. Milestone #890.
Per-lane activation states with every claim bound to checksummed evidence:
`docs/reports/TEST-SCHEDULER-ACTIVATION-v1.00.25.md`.

### Features

- Prepared-environment restore evidence integration (W5): the `prepared-env-report` job aggregates per-restore outcomes (verified | rebuilt | fallback with named causes), gates CI on the ≥ 95 % verified-restore rate, and records a warm observation window of 15/15 verified restores (rate 1.0, restore times 338–635 ms); fresh setup+execution remeasures (5 samples, 249.578–275.619 s) replace historical comparison figures, which are cited as history only.
- Post-promotion cohort C2 evidence (W6): 20 new eligible PR head SHAs measured on the required CI lane itself (no shadow duplication), full attempt honesty (20 attempts, 20 successes, 0 failures/cancellations/reruns, 8 named exclusions), machine-checked by the cohort-report post-promotion gate; verdict `target unachieved` is recorded with the named next lever, never a revised target.
- Cohort decision artifacts (W1): paired shadow cohort C1 closed at overall-verdict `hold` with the real fast/batch/fifo baseline (20 samples, p50 228.5815 s / p95 235.9965 s) and named tool-computed incompleteness reasons for all three queue lanes; no target was revised.
- New activation-states report (`docs/reports/TEST-SCHEDULER-ACTIVATION-v1.00.25.md`) stating, per lane, `activated (evidence link)` or `not activated (named reason)`, with every claim linked to its C1/C2/decision artifact by checksum.

### Breaking / Behavior Changes

- None. No scheduler or ordering lever changed in this milestone: `TEST_RUNNER_SCHEDULER` was never set (batch stays the required-lane default), `FAST_SHARD_ORDERING` was never added (fifo stays the default ordering), and the security lane keeps its pinned single `--suite security` run. Permission, isolation, and sandbox semantics are unchanged by construction.

### Migration Notes

- No migration is required. Nothing was activated, so there is no rollback to exercise: main already is the rollback state, and the one-command batch rollback contract (`gh api -X DELETE repos/coinerd/q/actions/TEST_RUNNER_SCHEDULER` style levers) remains documented for any future promoting decision. Prepared-environment reporting is observational and does not change any CLI surface.

### Testing

- Hold pins make any silent future activation a red CI run: W2 pins the required fast shard's strict command line verbatim and proves `parse-args` returns batch with the variable unset, set to queue, and set to batch; W3 adds eight pins covering the unset ordering default, manual `--ordering` override seams, and distinct named fallback reasons (missing/stale/malformed/wrong-inventory evidence) with byte-identical consecutive shard-plan digests; W4 pins the security lane's workflow block and scheduler-token absence across all lanes.
- Cohort tooling verifies sample counts, configuration match, inventory equality, `SHA256SUMS` binding, and post-promotion linkage (`pr → head SHA → Actions run → successful test shards`) before accepting any comparison; the C1/C2 reports regenerate byte-identically under `cohort-report.rkt --check`.
- The release-entry contract test (`tests/test-release-entry-current.rkt`) binds CHANGELOG, `util/version.rkt`, and the README badge to the same version and to the activation report's per-lane states.

### Operational / Release

- Not activated states remain explicit: work-conserving queue scheduling is **not activated** for fast lanes (W2 hold) or security lanes (W4 hold, gate unevaluable with zero paired samples), within-shard LPT ordering is **not activated** for fast lanes (W3 hold, no predicted improvement over round-robin), security-LPT and grouped-mode broad activation and tier-overlap scheduling remain not activated and withheld, prepared-environment activation is **reserved** for a later reviewed decision despite the passing ≥ 95 % evidence, and no 2× performance claim is made; shadow execution remains non-required. The C2 fast-execution target was missed out of sample (p50 253.5 s vs ≤ 115 s, p95 276.0 s vs ≤ 135 s); the named next lever — trimming batch shard fan-out and reusing the prepared-environment cache, then re-running the cohort on new SHAs — is queued for a separate reviewed decision (candidate for the reserved v1.00.26/v1.00.27 series), with no target revised inside this milestone.

## v1.00.24 — 2026-09-05

Released 2026-09-05.

### Features

- Test-design hotspot remediation integrated across waves W1–W7 and baked as one release: deterministic retry semantics behind a test-scoped sleep-scale seam with a bounded real-timer L4 canary; deterministic GSD wave timeout semantics behind an injected clock/wait seam with a bounded real-clock L4 canary; hermetic fixture-root classifier/shard discovery via the `collect-test-files` `#:root` seam with an invariant-only repository-scale L4 smoke; private copy-on-test session/Git/worktree fixture templates with concurrent-contamination stress coverage; and grouped/subprocess equivalence characterization with named machine-readable fallback telemetry.
- New W8 integrated bake report (`docs/reports/TEST-DESIGN-HOTSPOT-BAKE-v1.00.24.md`) consolidating all seven checksummed benchmark manifests with command, source, sample counts, digests, failures/timeouts, destination, and honest per-family verdicts.

### Breaking / Behavior Changes

- None. Production defaults are unchanged: `current-auto-retry-sleep-scale` remains 1.0, the GSD timeout grace remains two real seconds, batch remains the default scheduler, and no test-selection or CI lane behavior changed.

### Migration Notes

- No migration is required. All new seams are test-scoped parameters; grouped-mode and queue/LPT findings are characterization only and no activation is required or recommended by this release.

### Testing

- Ownership regeneration passes byte-identically: 19 behavior rows, 0 orphan IDs, 0 duplicate IDs, 0 missing destinations; four re-tiered behaviors each retain an executable required/L4 destination.
- Five of six hotspot benchmark manifests reproduce byte-identically under `--check`; `w3-gsd-timeout-after.json` was regenerated during the W8 integrated preflight with the current stats-precomputing collector (its `SHA256SUMS` entry updated to match) and all six `--check` runs plus `SHA256SUMS` verification pass at the delivered tree; the W0-declared before-baseline was never delivered, so every before/after comparison is honestly labeled incomparable and no performance ratio is claimed.
- Focused W8 drills pass: minimal CWD probe from a foreign CWD + real audit-script CWD canary, deterministic retry + bounded real-timer canary, deterministic GSD timeout/cancel/cleanup + real timeout canary, fixture-root classifier/shard checks + live repository discovery smoke, concurrent private session/Git/worktree contamination stress, grouped/subprocess equivalence with all named fallback cases, and old/new JSON compatibility with truthful effective-mode counters.

### Operational / Release

- Queue was exercised only as a compatibility drill and remains non-default; grouped-mode broad activation and any 2× performance claim remain withheld. The v1.00.16 fast-gate halving verdict (MISSED, 1.2848×) stands unchanged; v1.00.24 makes no new timing target claim.

## v1.00.23 — 2026-08-31

Released 2026-08-31.

### Features

- Added reproducible canonical C0 cohort evidence with 20 validated CI pull-request head SHAs and byte-identical report regeneration.
- Preserved batch as the default scheduler; queue and shadow execution remain explicit and non-required.

### Breaking / Behavior Changes

- None. Batch remains the default scheduler and queue is opt-in only.

### Migration Notes

- No migration is required. Existing invocations retain batch behavior.

### Testing

- Canonical C0 contains 20 successful CI pull-request head SHAs with complete fast-lane and metadata artifacts; cohort report regeneration passes byte-identically.
- CI, governance, release dry-run, and focused scheduler/cohort checks passed.

### Operational / Release

- Shadow scheduling remains non-required and no 2x performance claim is made. Full local fast-suite timeout remains documented as an inconclusive local gate.

## v1.00.22 — 2026-08-30

Released 2026-08-30.

> v1.00.22: GSD surface-hygiene & tooling follow-ups — canonical plan
> format enforced at `/go` (BUG-0023 residual), single authoritative
> wave-doc Status line (BUG-0050), `/reload` bytecode purge + in-process
> recovery (BUG-0047), standalone plan validator (BUG-0048), changelog
> `BUG-XXXX` resolution against the bug registry (BUG-0049), and
> release-wave completion requiring a verified GitHub Release object
> (BUG-0051). Campaign acceptance evidence:
> `docs/reports/GSD-SURFACE-TOOLING-BAKE-v1.00.22.md`.

### Features

- **BUG-0023 (residual) — `/go` rejects inline-only plans with a named
  canonical-format error.** An inline `## Wave N` plan (no index rows) is
  rejected at `/go` with a named diagnostic naming the canonical index
  grammar; the canonical plan loads silently. `test-gsd-planning.rkt` 95/95.
- **BUG-0050 — single authoritative wave-doc Status line.** Wave docs now
  carry exactly one machine `Status:` header; the body template line is
  stripped on write, a body-vs-header divergence is detected by the
  consistency checker, and a duplicate-status lint flags stray copies.
  44 affected historical wave docs were sanitized in-campaign.
- **BUG-0047 — `/reload` recovers from stale extension bytecode.** The
  reload path purges stale `compiled/` directories (wide purge-retry) and
  reloads extensions in fresh namespaces in-process, with honest per-name
  failure reporting (never "n extensions reloaded" while broken).
- **BUG-0048 — standalone plan validator.** `scripts/validate-plan.rkt`
  runs the exact `/go` plan-validation kernel from the CLI: exits 0 with
  "plan is /go-ready", names every error/warning, and shares one kernel
  with `/go` (no drift between authoring and execution checks).
- **BUG-0049 — changelog `BUG-XXXX` references resolved against the bug
  registry.** `lint-release-notes` resolves every `BUG-XXXX` in the
  CHANGELOG against `.planning/bugs/INDEX.md`: unknown IDs and
  status/severity contradictions are named errors, so a scrambled
  drill→bug mapping can no longer ship silently.
- **BUG-0051 — release-wave completion requires a verified GitHub Release
  object.** Wave completion for a release wave now gates DONE on an
  external `releases/tags/<version>` check (release-view command + adapter
  `find-release-by-tag`): a missing/draft Release fails completion with
  "release not verified: …", closing the v1.00.2x false-completion class.

### Breaking / Behavior Changes

- `/go` rejects inline-only plans (BUG-0023 residual enforcement); a plan
  with inline `## Wave N` sections and no index rows must be converted to
  the canonical index format first.

### Migration Notes

- Wave docs with a stale second `Status:` line are auto-sanitized by
  `write-wave-doc!`; the consistency checker flags any that remain.

### Testing

- Pin tests flipped in-campaign: `test-gsd-plan-format-characterization.rkt`
  (22), `test-inline-format-deprecation.rkt` (8),
  `test-single-wave-doc-status.rkt`, `test-reload-bytecode-recovery.rkt`,
  `test-standalone-plan-validator.rkt`, `test-changelog-bug-ref-lint.rkt`,
  and `test-wave-completion-release-verification.rkt` (4) all green.
- Full fast suite run per-wave; the campaign's remaining reds at mid-campaign
  were the not-yet-flipped W6/W7 pins (red-by-design) plus known cold-cache
  flakes — each re-ran green solo.

### Operational / Release

- Version stamped `1.00.22`; PR #9544 merged; tag `v1.00.22`;
  GitHub Release published (assets: `q-1.00.22.tar.gz` +
  `release-manifest.json`). Milestone #867 closed (8/8 waves).

## v1.00.21 — 2026-08-27

Released 2026-08-27.

> v1.00.21: GSD observability + release-race hardening — campaign
> cost/budget tracking (BUG-0039), notification sinks (BUG-0040),
> wave-doc lint at `/go` (BUG-0041), go-orchestrator decomposition
> (BUG-0042), TUI error surface (BUG-0043), configurable stall
> thresholds (BUG-0044), idempotent release publish (BUG-0045).
> Campaign acceptance evidence:
> `docs/reports/GSD-OBSERVABILITY-BAKE-v1.00.21.md`.

### Features

- **BUG-0044 — stall-watchdog thresholds are runtime-configurable.**
  `gsd.watchdog.soft-limit`, `hard-limit`, `window`, `abs-backstop` in
  the settings file re-tune the watchdog with no source edits; the
  startup log emits the effective values (and their source) once per
  wave; invalid values fall back to defaults (8/15/30/300), never crash.
- **BUG-0043 — stall/error text renders in the transcript error
  surface.** An injected stall kill produces exactly one `[SYS]
  [ERROR]` transcript entry via a typed system-error event; the message
  (prompt) surface stays clean; done-class outcomes are unaffected.
- **BUG-0045 — release pipeline race hardened, publish idempotent.**
  `release.yml` gains a concurrency group keyed on the tag ref; the
  publish path re-verifies assets before declaring success and no-ops
  on an already-published tag (verified no-op success instead of 422 +
  untagged draft residue).
- **BUG-0041 — wave docs linted at `/go` entry.** Missing
  Files/Verify/Done sections (and non-canonical status headers) are
  named violations; the verdict is stored as durable campaign evidence
  and carried into the executor prompt; lint is advisory and never
  blocks execution.
- **BUG-0039 — campaign cost tracking + budget ceiling.** Usage
  metadata lands in attempt/wave/campaign records (`usage-missing` when
  absent, never zeros); `gsd.campaign.max-cost` (settings key) pauses
  the campaign durably with a named reason; raising the ceiling resumes
  cleanly.
- **BUG-0040 — campaign notification sinks.** Opt-in sinks resolved
  from settings emit campaign id, wave index, kind, reason, and spend
  on wave-done/budget-pause/terminal transitions; a raising sink never
  fails a transition; a misconfigured webhook warns once and is
  skipped; silent by default outside tmux.
- **BUG-0042 — go-orchestrator decomposed.** 2566→1460 lines and
  91→23 top-level defines: stall-policy, infra-retry-policy,
  freshness, attempt-artifacts, and campaign-budgets extracted into
  their own modules; a baseline fixture + contract suite pin the file
  under target (≤ 1500 lines).

### Breaking / Behavior Changes

- Stall thresholds are now read from settings (defaults unchanged:
  8/15/30/300); editing source constants is no longer the tuning path.
- `/go` records a wave-doc lint verdict in the campaign record and the
  executor prompt.
- A duplicate release run on an already-published tag now exits as a
  verified no-op success instead of failing with 422.

### Migration Notes

- No config changes required. Optional new keys: `gsd.watchdog.*`
  thresholds, `gsd.campaign.max-cost`, notification sink opt-ins.
  Operators who tuned the watchdog by editing source should move to the
  settings keys.

### Testing

- New suites: `test-stall-threshold-config.rkt` (12 tests),
  `test-outcome-error-surface.rkt` (7), `test-release-workflow-contract.rkt`
  (silent contract runner, exit 0), `test-wave-doc-lint.rkt` (9),
  `test-campaign-cost-tracking.rkt` (4), `test-campaign-notifier.rkt`
  (10); characterization suites across the decomposition stayed green;
  fast suite fully green at the bake base (1147 files / 16615 tests at
  the W6 checkpoint).

### Operational / Release

- Tag `v1.00.21` (annotated); artifacts via `release-core.yml` (tarball
  + manifest); first release through the concurrency-grouped,
  idempotent-publish pipeline it ships. Bugs INDEX: BUG-0039…BUG-0045
  → fixed-in v1.00.21. After merge, restart q before any further `/go`
  (v1.00.19 freshness guard).

## v1.00.20 — 2026-08-26

Released 2026-08-26.

> v1.00.20: GSD workflow reliability bake — seven campaign acceptance
> gates demonstrated live (BUG-0033…BUG-0038), then shipped. The wave-7
> integration bake proved each fix end-to-end on a real campaign and
> recorded its evidence in `docs/reports/GSD-WORKFLOW-RELIABILITY-BAKE-v1.00.20.md`.

### Features

- **BUG-0033 — exploration budgets do not kill legitimate wide reads.**
  The exploration loop detector is repetition-shaped (identical
  tool-call pairs), not read-count-shaped; a wave whose executor
  legitimately reads >70 distinct files completes without watchdog
  death (drill (a) of the v1.00.20 bake report).
- **BUG-0034 — repetition loops trip detection and auto-resume.** An
  injected identical-call loop trips the steering detector AND the
  campaign auto-resumes via infra-retry once the attempt context is
  re-fed (drill (b)); `test-gsd-campaign-infra-retry.rkt` 25/25.
- **BUG-0035 — plan/wave divergence warns before work starts.** A
  doctored `PLAN.md`/wave-doc mismatch warns at `/go` and `/gsd`
  (drill (c)); `test-gsd-plan-diff.rkt` 2/2.
- **BUG-0036 — stale writers cannot revert tracked files.** A process
  running an older loaded version refuses a tracked-file write and its
  denial names the PID to exit (drill (d));
  `test-session-hygiene-characterization.rkt` 13/13.
- **BUG-0037 — killed campaigns reconcile at /reset.** An orphaned
  killed-campaign record reconciles at `/reset`; listing shows it and
  explicit prune removes it (drill (e)); campaign lifecycle/repository
  suites green.
- **BUG-0038 — divergence surface is advisory and cheap.** Deprecation
  pins in status suites flipped to the v1.00.20 surface (W6), so
  `/gsd` status stays advisory, not fatal.

### Fixed

- Release prep: `q-version` + `info.rkt` bumped to 1.00.20 via
  `scripts/sync-version.rkt --write`; version literals purged from
  tests; README metrics re-synced (`metrics --sync-all`).

### Removed

- (none)

### Deprecated

- (none)

### Breaking / Behavior Changes

- The mutation-stall watchdog is repetition-shaped (identical
  tool-call signatures), not read-count-shaped: wide read-only
  exploration is never killed by count alone (the absolute backstop
  fires only on signature-cycling windows). Watchdog kills classify
  as retryable infrastructure failures — campaigns auto-resume with
  prior-attempt context instead of halting.
- `/go` and `/gsd` surface plan/wave status divergence as named
  warnings (advisory only, never blocking) with documented
  precedence: wave-doc header wins for progress statuses, the PLAN
  row wins only for `[DEFERRED]`.
- A process whose loaded build predates the checkout refuses
  tracked-file writes (staleness guard) and its denial names the
  stale PID to exit.

### Migration Notes

- Stall watchdog limits remain constants this release
  (soft 8 / hard 15 / window 30 / backstop 300, cycling-gated);
  operator-tunable settings keys are tracked as BUG-0044.
- `/reset` now reconciles orphaned campaign records in place —
  inspect the orphan listing before pruning.

### Testing

- Fast suite 1141 files, 0 failures at the release SHA; stall
  suites (characterization 17, watchdog 24), session-hygiene 13,
  status-consistency 8, campaign infra-retry 25 — all green; live
  seven-gate bake evidence recorded in
  `docs/reports/GSD-WORKFLOW-RELIABILITY-BAKE-v1.00.20.md`.

### Operational / Release

- Tag `v1.00.20`; artifacts built by release-core.yml (tarball +
  manifest); coordinator hotfixes for BUG-0037 (PRs #9535–#9538)
  merged ahead of the campaign delivery PR #9539.

## v1.00.19 — 2026-08-26

Released 2026-08-26.

> v1.00.19: executor infrastructure hardening — the BUG-0028 core fix
> (worker allowed-roots track worktree lifecycle), plus BUG-0029/0030/0031
> remediation delivered by a live /go campaign (five waves, tmux q-go).

### Features

- **BUG-0028 — worker allowed-roots track worktree lifecycle (W1 core).**
  `ipc-request` gains a coordinator-authoritative `trusted-working-dir`
  channel; the worker extends its request-scoped allowed roots from it, so
  with worktree isolation ON each attempt's fresh worktree is editable
  without any refresh entry point. Model-supplied `working-directory`
  keeps its plain cwd semantics (bash tool feature) and can never
  authorize new roots.
- **BUG-0028 S1+S2 — settings wiring + self-diagnosing denials (W2).**
  `gsd.worktree-isolation` in project config verifiably routes executors
  (precedence: explicit `#:isolate?` > config > default OFF); tool denials
  enumerate the roots in force; executor start logs an isolation banner
  naming active worktree + resolved roots.
- **BUG-0031 — version-freshness guard at /go (W3).** Campaign start
  compares the running build against the checkout (and origin/main,
  best-effort) and refuses stale builds with a restart-required message;
  `allow-stale` overrides with a recorded flag; every campaign record now
  carries `build-version` + base head SHA. Offline-safe.
- **BUG-0030 — mid-wave checkpointing (W4).** Executors commit to the
  delivery branch after each green implementation step; infra stops capture
  dirty-state SHA/diff summary into the attempt context; the coordinator
  warns about uncommitted .rkt drift outside an active lease.
- **BUG-0029 — attempt-artifact ledger + reclaim (W5).** Every attempt's
  branch/worktree/base-SHA is recorded with terminal status; successor wave
  prompts include an inherited-artifacts block; campaign end lists
  non-delivery leftovers with operator-approved reclaim (never auto-delete).

### Bug Fixes

- Infra-retry re-entry sites carried the new attempt-id box after the W5
  orchestrator changes (caught by tests before release).

### Reports

- Bake evidence for this campaign was gathered live during execution;
  the isolation-default decision (still OFF pending a full bake under
  isolation ON) is recorded in the campaign PLAN and bugs INDEX.

### Breaking / Behavior Changes

- `/go` refuses to run when the running build predates the checkout;
  scripts that invoked /go across upgrades must pass `allow-stale` or
  restart first.
- Strict index plans referencing missing wave docs fail at load (v1.00.18
  behavior, unchanged); no additional format changes.

### Migration Notes

- Operators who want worktree isolation set `gsd.worktree-isolation=true`
  in project config — the settings key is now wired (it previously did
  nothing).
- Pre-v1.00.19 campaign records remain loadable; new fields are absent-safe.

### Testing

- Fast suite 1137 files, 0 failures at the release SHA; targeted suites:
  worker-security 38, execution-plane characterization, gateway/IPC family,
  go-orchestrator 52, campaign-state 24, checkpoint/artifact
  characterizations — all green with recorded gate evidence.

### Operational / Release

- Tag `v1.00.19`; artifacts built by release-core.yml (tarball + manifest);
  gate evidence recorded locally per tag-publish policy.

## v1.00.18 — 2026-08-25

Released 2026-08-25.

> v1.00.18: the GSD workflow remediation campaign (BUG-0023–BUG-0027) —
> plan-format diagnostics, infra-failure auto-resume, path-annotation
> normalization, git-root/scratch ergonomics — plus the BUG-0028/BUG-0032
> executor-infrastructure fixes and the W5 integration bake.

### Features

- **BUG-0024 — campaign-level infra-failure auto-resume (W3).** An
  `infra-failed` wave now auto-retries the same wave with exponential
  backoff (30s/60s/120s, bounded by `current-gsd-campaign-infra-retries`,
  default 3) without consuming delivery attempts; each retry emits a
  `gsd.campaign.infra-retry` event, and bound exhaustion stops the
  campaign with an aggregated failure message. Retried waves receive a
  PRIOR ATTEMPT CONTEXT block distilled from the dead executor session
  (steering/log lines + edited files, durable, ~2 KB cap).
- **BUG-0023 — actionable plan-format diagnostics (W2).** A plan rejected
  for having no waves gets a companion diagnostic spelling out both
  accepted formats (index + inline) with a skeleton example. Index-based
  plans are validated strictly: missing wave docs are a hard error naming
  each file and the `W<idx>-<slug>.md` convention (`load-plan-from-index`
  no longer loads silent empty content), and non-conventional targets
  fall back to title-slug paths.
- **BUG-0025 — annotated file declarations verify correctly (W1).**
  `clean-file-path` strips trailing bracket annotations from declared
  wave file paths, and delivery-verifier rejections carry per-file
  git-relative mapping lines so path-convention mistakes are diagnosable
  from the message alone.
- **BUG-0027/BUG-0026 — executor ergonomics (W4).** Single-wave prompts
  carry a git-root working-directory contract block and scratch-file
  guidance, ending the "Kein Git-Repository" mislocation pattern.

### Bug Fixes

- **BUG-0028 — worktree-isolation default OFF.** With isolation ON,
  per-attempt worktrees invalidated the tool worker's captured
  allowed-roots (cwd at worker start, never refreshed), so executors
  could not edit ANY path and fell back to raw shell mutation. The
  default is rolled back to the proven shared-checkout path;
  `#:isolate? #t` remains the explicit opt-in until worker allowed-roots
  track worktree lifecycle (#9529).
- **BUG-0032 — `/plan <text>` no longer destroys active wave docs.** The
  plan-submit handler rotates `.planning/waves/` into
  `waves-pre-plan-backup/` instead of deleting it (wiped active campaign
  wave docs twice during the live bake).

### Reports

- `docs/reports/GSD-WORKFLOW-REMEDIATION-BAKE-v1.00.18.md` — W5
  integration bake evidence for the five-wave remediation campaign.

### Breaking / Behavior Changes

- Strict index validation: index-format plans referencing missing wave
  docs now fail `/go` with a naming error instead of loading silent
  empty waves.
- Worktree isolation default flipped OFF (see BUG-0028 above); campaigns
  run in the shared checkout unless explicitly opted in via
  `#:isolate? #t`.

### Migration Notes

- No user data migration required. Campaign records under
  `.planning/campaigns/` are forward-compatible; operators who relied on
  worktree isolation must pass the explicit opt-in flag.
- Operators who set `gsd.worktree-isolation` in config should note the
  settings key is not yet wired (BUG-0028 S1); use `#:isolate?` instead.

### Testing

- Fast suite: 1133 files, 0 failures at the release SHA.
- TUI 88, arch 31, workflows 29 — all green with recorded gate evidence
  (`.gate-evidence/`, four suites).

### Operational / Release

- Tag `v1.00.18`; release artifacts built by release-core.yml with
  tarball + manifest; gate evidence recorded locally per tag-publish
  policy.

## v1.00.17 — 2026-08-25

Released 2026-08-25.

> v1.00.17: the /go executor-hardening campaign (#9512–#9516), BUG-0022
> (#9517) remediation, the W8 integration bake, and the v1.00.17 release.

### Features

- **#9512 — per-wave worktree isolation.** `/go` wave executors run in
  dedicated git worktrees (`gsd.worktree-isolation`, default **ON** since the
  W8 bake; `#:isolate? #f` disables for tests), so concurrent waves can no
  longer read or clobber each other's uncommitted trees.
- **#9515 — auto-retry with failure context.** A failed wave delivery attempt
  no longer silently ends the campaign: the executor injects the recorded
  failure reason into the retry prompt ("Previous attempt failed — adapt").
- **#9513 — mutation-stall steering.** Repeated identical tool calls without
  intervening text now trigger steering that forces a concrete implementation
  step instead of an infinite exploration loop.
- **#9514 — role re-anchor after empty response.** An empty/whitespace-only
  model response re-anchors the executor role instead of continuing with a
  decontextualized agent.
- **#9516 — shell-risk false-positive severity.** The shell-risk classifier no
  longer aborts benign multi-command lines; findings are severity-graded.
- **#9518-lesson — branch-based delivery verification (W5).** Wave DONE now
  requires evidence against the wave branch's *pushed* head SHA, never a local
  claim — making "branch merged before its final commit existed upstream"
  unrepresentable.
- **BUG-0022 (#9517) remediation (W1B/W2B).** Connection-pool stale keep-alive
  reuse now transparently retries; the health gate no longer counts same-turn
  retries against the budget (5 → silent truncation to 2 fixed).

### Bug Fixes

- **`release-dry-run.rkt` cwd fragility (W8).** The script resolved
  `util/version.rkt` relative to the caller's cwd, so wave verify commands
  that run it from the campaign base-dir (the parent of `q/`) failed with
  "Run from q/ project root". It now resolves its project root from its own
  file location (`scripts/` always sits directly under `q/`) and runs all
  checks with `cwd = q/`; exit-code semantics unchanged (6/6 checks).

### Reports

- `docs/reports/GSD-EXECUTOR-HARDENING-BAKE-v1.00.17.md` — W8 live-bake
  evidence: dogfooded worktree isolation, branch-based verification, and the
  #9515 failure-context retry on a synthetic no-op first attempt.

### Breaking / Behavior Changes

- `gsd.worktree-isolation` now defaults ON: campaign wave executors run in
  dedicated git worktrees instead of the shared checkout. Set it to `false`
  to restore shared-checkout execution.
- Delivery verification is branch-based: a wave completes only when its
  deliverables exist as a committed diff on the wave branch (pushed head SHA
  recorded in the durable campaign record); uncommitted working-tree mutations
  no longer count as delivery.
- Shell-risk severities for `$()`/backtick substitution and bare two-operand
  `mv` are downgraded one tier; critical anchors (`rm -rf`, `dd of=/dev/`,
  force-push) are unchanged.

### Migration Notes

- No API changes. Operators with automation keyed to shell-risk severity strings
  should re-check thresholds against the new tiers; use
  `networking.pool.host-idle-ttl` to pin aggressive keep-alive hosts (e.g.
  api.z.ai) without lowering the global idle TTL.
- Campaign operators: provider/network infra failures now auto-retry at the
  campaign level before stopping; `/retry` remains available as manual override.

### Testing

- New characterization + hardening tests: executor-retry characterization pins,
  shell-risk severity baseline, conn-pool stale-reuse retry, health-gate
  turn-scoped accounting, mutation-stall watchdog, wave-worktree lifecycle,
  branch-based delivery verification. Fast suite 1129 files / 16406 tests,
  tui 88, arch 31, workflows 29 — all green; local gate evidence recorded at
  the release SHA prior to tagging.

### Operational / Release

- Rollback toggles: `gsd.worktree-isolation=false` (shared-checkout executors);
  `networking.pool.enabled=false` (pooling); stall watchdog thresholds via
  settings (defaults soft 25 / hard 60 tool calls).
- Known follow-up defects observed during the bake and filed for the next
  series: BUG-0023–BUG-0027 in `.planning/bugs/` (plan-format fragility,
  campaign halt on infra failure, verifier annotation false negative, scratch-
  workflow guard friction, git-root contract gap).

## v1.00.16 — 2026-08-24

Released 2026-08-24.

> BUG-0020/BUG-0021 remediations, the v1.00.16 fast-gate
> and TDD-adoption campaign (W0–W4), and the connection-pool chunked-body
> fix that made pooling safe to re-enable everywhere.

### Features

- **Prepared-env fast-gate cutover (W3, #9518).** The reusable
  `.github/actions/setup-racket` action gained a prepared-environment path:
  exact-cache restore with package preflight, relink fallback, and guarded
  install. `ci.yml` routes lanes through it behind `RACKET_PREPARED_ARTIFACT`.
- **`FAST_SHARD_COUNT` guarded study (W3).** Shard-count override wired through
  ci.yml matrix generation with cache-policy documentation in
  `docs/reports/CI-RACKET-CACHE-POLICY.md`; decision recorded: KEEP-3.
- **v1.00.16 W1+W2 banked work (#9511).** `grouped-eligible?` runner contract;
  oauth `#:on-complete` seam with deterministic semaphore sync
  (`test-oauth-callback-nonblocking` 8.23 s → 1.40 s); shared fixture builders
  `tests/helpers/{fast,oauth-callback}-fixtures.rkt`;
  `current-auto-retry-sleep-scale` parameter; `--json-out` crash fix in the
  runner/reporting path (string result paths → `path->string`); timesink
  remediation report `docs/reports/fast-timesink-remediation-v1.00.16.md`.
- **Halving-objective baseline of record (W4, #9519).** Regenerable
  `docs/reports/test-feedback-baseline-v1.00.16.{md,json}` plus
  `fast-gate-budget-v1.00.16.{md,json}` attribution companion, generated from
  retained CI runs 32745843124/32748197712; `baseline-report.rkt --check`
  proves byte-identical regeneration. Honest result recorded: sample p50
  627 s vs target ≤ 244 s (ratio 1.2848×) — MISSED; remaining cost attributed
  to legacy setup install path (343/348 s) + max shard (276/287 s).

### Bug Fixes

- **BUG-0020 — `/go` executor-inheritance contract violation (#9509).**
  `executor-inheritance.rkt` widened to accept the full session-config struct;
  wave executors no longer die with a contract error at spawn.
- **BUG-0021 — pooled chunked-body corruption (#9510).** Pooled connections did
  not decode `Transfer-Encoding: chunked`, so raw hex chunk sizes were spliced
  into SSE `data:` lines at TCP chunk boundaries — surfacing as malformed
  tool-call JSON ("model typos"). New RFC 7230 decoder `make-chunked-input-port`
  in `llm/conn-pool.rkt`: byte-exact reassembly across mid-line splits,
  0-chunk + trailers ⇒ connection stays reusable, framing anomalies ⇒ pool
  fault. Regression-tested against a mock 7-byte-chunk server.

### Breaking / Behavior Changes

- Connection pooling is now ENABLED in local + VPS configs
  (`networking.pool.enabled=true`) after the BUG-0021 fix; pooled responses are
  chunk-decoded transparently. Disable via `networking.pool.enabled=false` to
  return to one-connection-per-request behavior.

### Migration Notes

- No API changes. Operators self-hosting with custom provider configs should
  verify their endpoints tolerate HTTP keep-alive reuse before enabling the
  pool; hosts that close idle connections aggressively may surface first-shot
  network errors until host-specific idle TTLs are tuned.

### Testing

- New regression tests: `test-executor-inheritance.rkt` (BUG-0020);
  chunked-body decoder coverage incl. mock 7-byte-chunk server (BUG-0021);
  oauth-callback nonblocking/security suites re-seamed onto deterministic
  fixtures. Long-generation live bake against GLM-5.3/GLM-5.2/DeepSeek-V4-flash
  verified clean before re-enabling the pool. Local gate evidence (fast, tui,
  arch, workflows) recorded at this version prior to tagging.

### Operational / Release

- Rollback toggles (one line each, documented in
  `docs/reports/test-regression-log.md`): `RACKET_PREPARED_ARTIFACT=off`
  (prepared-env cutover), unset `FAST_SHARD_PLAN` (shard matrix),
  `networking.pool.enabled=false` (pooling).
- Halving-objective remeasure (warm prepared-env restore observation)
  scheduled 2026-09-30.

## v1.00.15 — 2026-08-24

> BUG-0019 remediation: peer FIN/CLOSE-WAIT mid-stream is now detected in
> seconds instead of burning the whole phase timeout, plus an opt-in
> connection pool for openai-compatible providers.

### Added

- **FIN-aware SSE liveness watchdog (W1).** `stream-sse-events` slices idle
  windows into `peer-close-probe-secs` slices (default 5 s, per-model via
  `request.peer-close-probe-secs`) with zero-timeout liveness probes. An
  unclean peer close raises the new structured exception
  `exn:fail:network:peer-closed` carrying `phase`/`data-received?`/
  `content-chars`/`elapsed-ms` plus the SS-5-style message suffix — detection
  latency drops from minutes (full thinking window) to under a second,
  independent of `thinking-gap-cap`. EOF stays normal end-of-stream;
  heartbeat/data bytes keep resetting the idle clock (BUG-0018 rule).
  Auto-retry classifies peer-closed as timeout-tier; v1.00.14
  silent-overflow economics are unchanged.
- **Flag-off connection pooling (W2).** New `llm/conn-pool.rkt`: host-keyed
  `(host, port, tls?)` pool with per-entry custodians, 55 s idle TTL,
  max-per-host 4, single-use-on-fault, and deterministic-framing reuse
  (Content-Length responses check in; chunked/EOF bodies stay single-use).
  Gated by `networking.pool.{enabled,idle-ttl-secs,max-per-host}`, default
  OFF — flag-off behavior is unchanged. Pooled requests skip the
  request-scoped custodian so teardown cannot kill pooled sockets.
  Bake-verified against GLM-5.3/GLM-5.2/DeepSeek-V4-flash; default stays OFF
  until chunked-body reuse lands (SSE responses currently do not reuse).
- **Reproducer suite (W0).** `tests/reproducers/mock-fin-server.rkt` models
  unclean-close / clean-close / heartbeat-alive / true-silence peers;
  recorded platform verdict: on Racket 8.10/OpenSSL 3 an unclean FIN always
  surfaces as `exn:fail:network` (never plain EOF), and even graceful TLS
  closes reach the client as errors — clean end-of-stream relies on the SSE
  `[DONE]` marker, as providers signal it.

## v1.00.14 — 2026-08-23

> BUG-0018 remediation: GLM-5.3 long-thinking sessions no longer die at the
> 300 s thinking-idle cap, and `/model <name>` provably reaches the request
> path on every execution path with a guaranteed `model.switched` trace.

### Added

- **Configurable thinking-gap ceiling (W1).** New per-model config key
  `timeouts.models.<model>.thinking-gap-cap` widens the SSE thinking-idle
  window past the legacy 300 s bound (widen-only precedence — a cap can
  never narrow the resolved window below the legacy bound). Ops-level
  parameter `current-max-thinking-gap-secs` (default 300) preserves
  v1.00.12/v1.00.13 semantics exactly when unset.
- **Keepalive liveness documentation + tests (W1).** The phase timeouts are
  per-read windows: heartbeat comment frames and zero-delta data chunks each
  reset the idle clock; only true silence or the total budget raises
  (`tests/test-midstream-stall.rkt`).
- **Silent-thinking overflow economics (W3).** A stream timeout in the
  thinking phase with zero visible chars gets exactly ONE retry; the second
  consecutive overflow circuit-breaks with actionable guidance ("raise
  thinking-gap-cap or /model switch") instead of burning blind restarts.
  Overflow retries back off proportionally to the consumed thinking window.
- **GSD executor model inheritance (W3, R-B3).** `/go`-spawned executor
  sessions inherit the coordinator's switched provider/model via
  `runtime/session/executor-inheritance.rkt`; without an explicit override,
  startup config semantics are unchanged.

### Fixed

- **BUG-0018 B: /model switch never reached the request path (W2).**
  Root cause R-B1: `build-session-context-for-prompt` (E4) re-applied the
  path-derived model name on EVERY prompt, silently reverting any runtime
  switch before the next request. `set-model!`/`switch-model!` now record an
  explicit `'model-override` marker and E4 defers to it; `dispatch-iteration`
  reconciles session/config divergence loudly (log + `model.divergence.reconciled`
  event).
- **R-B2 observability gap.** `handle-model-command` now refuses UI-only
  switches when no live session exists (error entry instead of fake success),
  publishes `model.switched` on every real switch, and falls back to the q
  logger when the event bus is nil. Transcript entries appear only after the
  session mutation actually succeeded.

### Testing

- `test-request-network-policy.rkt`: precedence rows for
  `thinking-gap-cap-override` (widen-only, request-budget clamped,
  initial/content unaffected).
- `test-sse-phase-timeout-bounds.rkt`: glm-style cap-900 rows; default
  matrix unchanged.
- `test-model-command.rkt`: live-session switch asserts session model-name,
  override marker, guaranteed `model.switched` event payload, and
  request-path config resolution.
- `test-auto-retry.rkt`, `test-executor-inheritance.rkt`,
  `test-provider-recovery-model-switch-e2e.rkt`.

## v1.00.13 — 2026-08-22

> Released 2026-08-22. Request lifecycle policy unification: one mandatory owner
> for provider-request lifecycle policy. Raw timeout configuration resolves once
> into semantically named policy fields consumed by every adapter; response
> headers, resource cleanup, and structured failures survive the request
> boundary; connect/TTFB is bounded; held-request classification is
> heartbeat-aware.

### Features

- **Centralized request-network policy (W1 #9461).** New
  `llm/request-policy.rkt` is the single owner of provider-request lifecycle
  semantics: the `request-network-policy` value (request budget, connect/TTFB,
  initial/thinking/content idle, stream total, body-read budget), the pure
  resolver with safety caps and early validation, and the legacy `sse-read`
  compatibility mapping. The v1.00.12 resolver moved out of `llm/stream.rkt`
  (thin compatibility re-export; stream is mechanism-only again).
- **Mandatory policy consumption across all adapters (W2 #9466).** openai-
  compatible, anthropic (+ kimi eager), azure-openai, and gemini consume one
  resolved policy per request on both streaming and eager paths; adapters no
  longer read raw timeout config or author generic constants. Completes the
  v1.00.12 SS-6 adapter-parity deferral: anthropic/azure/gemini thinking
  window 60 → policy value; stream total 600 → `max(600, 2×request)`; eager
  body reads honor the legacy `sse-read` budget instead of the flat 120 s
  fallback. Cross-adapter conformance harness proves identical mechanism
  arguments for all four adapters; the architecture gate (R1–R5) forbids
  adapters from regaining timeout-policy ownership.
- **Structured failures replace string parsing (W3 #9473).** HTTP status and
  retry-relevant headers survive the request boundary in a machine-readable
  failure context; auto-retry consumes `retry-after-ms` from that context and
  no longer parses exception message text. Human messages are rendered
  alongside, unchanged.

### Added

- **Semantic timeout config keys**: `timeouts.models.<m>.thinking-idle` and
  `timeouts.models.<m>.body-read` (explicit keys win over the legacy alias;
  thinking capped at 300 s). Non-fatal deprecation warning for legacy
  `sse-read` at wiring time.
- **Cross-adapter policy conformance suite** and **architecture ownership
  gate** (durable regression prevention, AC-1..AC-5).
- **Deterministic response-port lifecycle** for non-streaming/eager-body
  requests: close-once semantics across success, status failure, read
  timeout, request timeout, and cancellation (injectable HTTP boundary for
  tests).

### Fixed

- **Retry-After from real headers (RL-5).** `Retry-After` is parsed from the
  actual response header — delta-seconds and HTTP-date (timezone-free parser,
  injectable clock) — instead of being reconstructed from exception text
  (which never worked for real responses).
- **Connect/TTFB bound (RL-4).** An established-but-silent connection fires
  the dedicated `min(request, 120)` window with structured phase
  `'connect/ttfb` — it can no longer consume the full request budget
  (previously up to 900 s).
- **Heartbeat-aware held-request classification (RL-8).** Heartbeat-only
  streams are live-but-no-content, not dead peers; total deadline and
  empty/comment flood ceiling still bound them.
- **Hard remaining-budget reads (NP-7).** Every blocking stream read is
  capped at `min(phase-idle, remaining-total)` — no more overshooting the
  total deadline by a full phase window.

### Breaking / Behavior Changes

All deltas are intentional outcomes of the unification (pinned by the
cross-adapter conformance suite):

1. anthropic/azure/gemini streaming: thinking window 60 s →
   `min(request, min(or thinking-idle 120, 300))`; stream total 600 s →
   `max(600, 2×request)` when request > 300 s.
2. All adapters (incl. openai eager): non-streaming body reads honor the
   legacy `sse-read` (or explicit `body-read`) budget instead of the flat
   120 s fallback.
3. Connect+TTFB on every path bounded at `min(request, 120)` with
   structured phase `'connect/ttfb`.
4. Heartbeat-only initial stalls are live-but-no-content: they no longer
   trip the held-request circuit breaker; total deadline still bounds them.
5. Blocking stream reads capped at `min(phase-idle, remaining-total)`.
6. Retry delays derive from the structured `Retry-After` context
   (HTTP-date + delta-seconds), never from message text.

### Migration Notes

- Existing configs need no change: `request` + legacy `sse-read` resolve to
  the same effective windows as documented (DeepSeek `request=900`,
  `sse-read=600` → thinking 300 s; Kimi `sse-read=300` → honored).
- To widen a specific window, prefer the semantic keys
  `timeouts.models.<m>.thinking-idle` / `body-read`; explicit keys win over
  the legacy alias. `docs/provider-retry.md` carries the resolved-policy
  matrix and migration table.

### Testing

- New suites: `test-request-network-policy` (resolver contract + property
  sweep), `test-provider-network-policy-conformance` (identical mechanism
  arguments across all four adapters), `test-network-failure-context`
  (structured failure context + Retry-After parsing), 
  `test-provider-response-cleanup` (close-once lifecycle matrix),
  `test-request-policy-architecture` (R1–R5 ownership gate, empty
  allowlist), `test-stream-liveness-classification` (heartbeat matrix +
  W4 deadline matrix), `test-request-policy-migration` (DeepSeek/Kimi
  legacy-config proofs).
- All suites green in CI (fast/arch/workflows/tui + sharded regression);
  gate evidence recorded per release run.

### Deprecated

- **Legacy `sse-read` config key.** Still honored (feeds only thinking-idle
  and body-read, with the documented caps and precedence); removal is
  planned after v1.00.13. Docs: `docs/provider-retry.md`.

### Operational / Release

- CI cold-runner repair (with #9488): `raco pkg show`-based package-presence
  guard fixed in `setup-racket`/`prepare-racket-environment` actions; the
  metadata-discovery fixture tree is excluded from repo-root test collection.
- Workspace bytecode wipes spare the frozen discovery fixture
  (tracked stray `.rkt` under `compiled/`); the release readiness gate now
  names dirty files when it fails.

Released 2026-08-22.

## v1.00.12 — 2026-08-22

> Released 2026-08-22. SSE stall detection bounds: containment of the v1.00.05 regression that let
> a wide `sse-read` override stretch stream stalls to the full configured
> window (observed as ~10-minute hangs on deepseek-v4-flash). Phase windows
> are now bounded by design; timeout messages carry triage diagnostics.

### Bug Fixes

- **SS-1/SS-2/SS-3 bounded phase timeouts (W1, #9429).** New pure resolver
  `sse-phase-timeout-secs` in `llm/stream.rkt` returns the three stall windows:
  initial = `min(request-timeout, 120)` (dead-peer bound, never config-widened),
  thinking = `min(request-timeout, min(or sse-read 120, 300))` (reasoning
  window capped at new constant `max-thinking-gap-secs` = 300), content =
  fixed 60 s per-chunk gap. The openai-compatible adapter now wires all three
  through the resolver; the raw `sse-read` config feeds only the thinking
  window. kimi/glm 300 s reasoning windows are preserved while deepseek's
  `sse-read=600` can no longer produce multi-minute mid-content hangs.
- **SS-5 timeout message suffix (W2, #9430).** Every
  `exn:fail:network:timeout:stream` raised from `stream-sse-events` now ends
  with `[phase=<p> data-received=<yes|no> chars=<n>]` for log/UX triage. The
  struct fields remain the machine source of truth for retry classification.

### Documentation

- New "Streaming Timeout Matrix" section in `docs/provider-retry.md`: phase
  table, circuit-breaker interaction/TTFB row, and the v1.00.13 Request
  Lifecycle Policy Unification handoff note (adapter parity deferral, SS-6).

### Testing

- `tests/test-sse-phase-timeout-bounds.rkt` locks the resolver matrix (deepseek
  clamp, no-override defaults, kimi ceiling preservation, sweep invariants)
  and — since W2 — the message-suffix checks migrated from the deleted
  reproducer `tests/reproducers/reproduce-sse-timeout-message-suffix.rkt`.

### Breaking / Behavior Changes

- Models with `sse-read` overrides above 300 s now stall-cap at 300 s in the
  thinking phase instead of running to their full configured value; initial
  and content phases ignore `sse-read` entirely (fixed 120 s / 60 s).
- Timeout exception messages gained the diagnostic suffix (string change only;
  struct fields unchanged).

### Migration Notes

- None required. Existing `timeouts.models.<model>.sse-read` values continue
  to work; values above 300 are clamped for the thinking window.

### Operational / Release

- Containment release: no schema, config-format, or storage changes; safe to
  deploy rolling. Watch for `phase=thinking` stalls now capping at 300 s —
  models that legitimately need longer silent reasoning gaps require the
  v1.00.13 Request Lifecycle Policy Unification follow-up.

Released 2026-08-22.

## v1.00.08 — 2026-08-21

> Provider networking hardening closeout: per-model cumulative retry ceiling
> (`providers.<name>.retry-ceiling-secs`) documented and config-override
> tested through the turn-orchestrator settings path (PN-7).

### Bug Fixes

- **PN-7 cumulative retry ceiling config override.** `runtime/turn-orchestrator.rkt`
  now exposes `resolve-retry-ceiling-secs`, which reads
  `providers.<name>.retry-ceiling-secs` from session-config settings and falls
  back to the module default when absent. A dedicated test
  (`tests/test-provider-retry-ceiling-config.rkt`) proves the per-model value
  overrides the default, that another model's override does not leak, and that
  absent settings/model-name fall back to `default-cumulative-ceiling-secs`.
- **Documentation drift fix.** `docs/provider-retry.md` previously stated the
  default cumulative ceiling was 300s; the default was raised to 900s in
  v1.00.05. The `retry-ceiling-secs` examples and tables now match the actual
  default (900s / 15 min).

### Testing

- New focused test `tests/test-provider-retry-ceiling-config.rkt` (4 cases)
  covering the PN-7 settings-override resolution.
- Existing provider-networking contract tests remain green: stream port
  closure (PN-1), generator finalization (PN-3), SSE heartbeat metadata
  (PN-2b), circuit breaker (PN-4), adaptive retry (PN-6), cumulative ceiling
  (PN-7).

### Breaking / Behavior Changes

- None. `resolve-retry-ceiling-secs` is a pure extraction of the existing
  inline resolution; no retry behavior changes.

### Migration Notes

- None required. Existing `retry-ceiling-secs` config continues to work.

### Operational / Release

- Version stamped `1.00.08`; provider-networking hardening plan v1.00.08
  closed out.

Released 2026-08-21.
## v1.00.07 — 2026-08-20

> macOS platform test fixes (W2): SP12 dash/bash PIPESTATUS conditional + LF3
> symlink path-allowed? case-insensitive fix on APFS; fixes #9406 #9407.
> Merged via PR #9411.

### Bug Fixes

- **SP12 dash/bash PIPESTATUS conditional (#9406).** `tests/test-subprocess-edge-cases.rkt`
  SP12 test now probes `sh-is-dash?` at load time (mirroring `setsid-available?`).
  On dash (Linux `/bin/sh`): asserts exit-2 + "Bad substitution". On bash-as-sh
  (macOS `/bin/sh`): asserts exit-0 + PIPESTATUS[0]=1. Prevents false failure
  on macOS where `/bin/sh` is bash 3.2.
- **LF3 symlink path-allowed? on APFS (#9407).** `sandbox/worker-tools.rkt`
  `path-allowed?` normalizes both resolved path and allowed root to lowercase
  on macOS (`system-type` = `macosx`) for prefix comparison. Fixes symlink
  resolution on case-insensitive APFS filesystem where casing differences
  caused legitimate symlinks within allowed roots to be rejected.

### Testing

- Both test files pass on Linux: `tests/test-subprocess-edge-cases.rkt` (13 tests)
  and `tests/test-worker-security.rkt` (32 tests).
- All existing tests unaffected: auto-retry, stream, worker-security,
  subprocess-edge-cases.

### Breaking / Behavior Changes

- None. The SP12 test is now platform-conditional but documents the expected
  behavior on both platforms. The LF3 fix only changes macOS path comparison
  to be case-insensitive (matching APFS semantics).

### Migration Notes

- None required.

### Operational / Release

- Version stamped `1.00.07`; wave PR #9411 merged to main.

Released 2026-08-20.

## [Unreleased]

(empty)

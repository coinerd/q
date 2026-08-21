# v1.00.11 — TDD CI Integrity and Feedback-Baseline Completion

**Status:** Proposed remediation milestone  
**Owner:** q maintainers  
**Author:** Manus AI  
**Repository baseline:** `main` at `94677b8d` (2026-08-21)  
**Predecessor:** v1.00.10 closed the macOS LF3 security defect and recorded clean cold/warm all-lane L4 evidence.

## 1. Decision and scope

The original TDD improvement plan is **substantially implemented**, but it is not yet fully addressed. Its core safety controls are operational: local fail-open impact selection, a reviewed `@covers` pilot manifest, duration-aware fast sharding, blocking metadata lint in CI, a scheduled/manual all-lane L4 workflow, retained evidence, and a bounded mutation pilot. The fresh L4 cold and warm runs both passed after v1.00.10.[1] [2]

Two control-integrity gaps and one measurement gap remain. First, the required Racket 8.11 cross-version PR gate is red because the shared setup action applies a lock that requires Racket 8.10 to an 8.11 job. Second, direct metadata lint on the checked-out tree reports a materially different inventory from the green CI lint log; this makes the current claim of blocking metadata enforcement unverifiable until the discovery inputs and execution environments agree. Third, the original plan's Phase 0 rolling duration/flake baseline and evidence-backed L0–L2 latency targets remain explicitly deferred.

> **Release and TDD rule:** v1.00.11 must repair these controls without weakening L3 protection, restoring a Linux-only shortcut, reintroducing the impact selector as a PR job, suppressing the 8.11 gate, or treating a metadata discrepancy as a harmless warning.

| Original-plan control | Current evidence | v1.00.11 required outcome |
|---|---|---|
| L3 required regression gates | `test-cross-version` fails on `main`: lock verifier rejects Racket 8.11 against an 8.10 lock.[3] | The 8.11 job installs, verifies a version-appropriate reviewed dependency graph, compiles, and runs the fast suite successfully. |
| Metadata schema and blocking enforcement | CI lint log records `files=1301`, `missing-required=0`; a fresh local invocation records `files=1285`, `invalid=2`, `deprecated-alias=6`, `missing-required=142`.[4] [5] | One documented discovery contract produces the same normalized inventory locally and in CI; any remaining violations are resolved before enforcement is claimed. |
| Phase 0 feedback baseline and L0–L2 targets | The canonical strategy still lists rolling baselines and L0–L2 SLO confirmation as deferred.[6] | A deterministic, retained baseline report establishes measured targets or explicitly justified revised targets. |
| L4 reliability | Cold run 32522576690 and unchanged warm run 32526868295 both have GitHub `success` and all-lane `run-summary.status: pass`.[1] [2] | Preserve this control; do not rerun or redesign it unless a v1.00.11 change touches full-regression behavior. |

## 2. Non-negotiable invariants

| ID | Invariant | Acceptance condition |
|---|---|---|
| I1 | Cross-version testing is a real required signal. | Racket 8.11 is not skipped, downgraded, or made `continue-on-error`; it compiles and executes the configured fast suite. |
| I2 | A dependency lock is version-aware and reviewable. | Every cached Racket version is verified against a lock entry for that exact runtime version and package graph. |
| I3 | Cache isolation is exact. | No cache key or restore key permits an 8.10 store to satisfy an 8.11 job, or vice versa. |
| I4 | Metadata enforcement is reproducible. | Clean local execution and CI use the same root, discovery rules, schema version, and normalized path inventory. |
| I5 | Missing mandatory metadata is never silently reclassified. | Discovery mismatch is repaired first; thereafter invalid values, aliases, and missing tags have an explicit owner and an evidence-based closure. |
| I6 | L3 and L4 safety boundaries remain intact. | Existing fast, workflow, security, platform, cross-version, nightly/manual L4, and all-lane aggregation controls remain required as currently configured. |
| I7 | Feedback targets are measured, not asserted. | Baseline calculations use retained runner JSON and documented sample selection; unknown L0 local data remains marked unknown rather than fabricated. |

## 3. Implementation waves

### W0 — Freeze the red state and establish parity fixtures

Before changing behavior, capture the current 8.11 failure log and two metadata inventories: the failing local command and the exact CI command/environment. Add focused tests for the metadata file-discovery function and for lock selection. The parity fixture must include ordinary test files, nested test directories, generated/ignored files, symlink handling if supported, and a path outside the repository root. It must prove that the same normalized relative path list is produced in both invocation modes.

The cross-version characterization test must prove the current failure: `verify-racket-package-lock.rkt` receives a Racket 8.11 runtime and rejects a lock pinned only to 8.10. Preserve that behavior as a red test until W1 changes the lock schema.

**Exit criterion:** the PR contains machine-readable before-state artifacts or focused test fixtures for both discrepancies, with no configuration-only workaround.

### W1 — Make Racket cache and lock verification runtime-specific

Replace the single-runtime assumption in `ci/racket-package-lock.rktd` and `ci/verify-racket-package-lock.rkt` with an explicit version-indexed lock schema. The schema must identify the supported Racket runtime version and the reviewed package identities/checksums for that runtime. A verifier invocation must select exactly one matching version entry, reject an absent entry, and print the selected version and lock digest.

Update `.github/actions/setup-racket/action.yml` to accept the requested runtime as an input and to derive all cache dimensions from it: Racket version, CS variant, architecture, distribution, lock schema revision, and lock digest. The action must keep an exact key only; prefix restore keys remain forbidden. On a cache hit it must verify the selected lock before relinking q and compiling `q` plus `fmt`. On a miss it must install the reviewed dependencies, verify the selected lock, and populate only the matching version store.

Update the 8.11 CI job to call the action through the same contract as 8.10. Do not bypass package verification for 8.11. If exact package identities differ between supported runtimes, retain separate reviewed entries rather than sharing a permissive lock.

| Test surface | Required proof |
|---|---|
| Lock verifier unit tests | 8.10 accepts 8.10 entry; 8.11 accepts 8.11 entry; 8.11 against only 8.10 is rejected; unlisted versions are rejected. |
| Workflow contract tests | Cross-version job receives `8.11`; cache key includes the selected runtime and lock digest; no broad restore key is introduced. |
| CI smoke evidence | The 8.11 job reaches `raco make main.rkt` and emits a fast-suite JSON summary. |

**Exit criterion:** a main-equivalent PR run has a green `test-cross-version` job and no cache-key collision between supported runtimes.

### W2 — Reconcile metadata discovery before migrating metadata

Refactor metadata discovery into one repository-owned function with a documented input root, path normalization, ignored-directory set, symlink policy, and test-file predicate. Both `--lint-metadata` and the CI metadata step must invoke this function without a CI-only discovery branch. Add a deterministic `--metadata-inventory-json` mode that emits schema version, invocation root, normalized file list digest, counts by area, and invalid/alias/missing details.

Run the inventory from a clean checkout locally and in CI. If the two inventories differ, treat the difference as a discovery defect; add a minimal regression fixture for each category before changing tags. Only after parity is proven should the milestone migrate the verified residual violations: two invalid tags, six deprecated aliases, and every remaining mandatory-tag omission in the agreed inventory. Use area-bounded commits and preserve conditional-tag semantics; the objective is not meaningless annotation.

Update `docs/TEST_CONVENTIONS.md`, the canonical strategy adoption section, and CI comments so they describe the actual enforcement policy. The wording must distinguish hard errors from warnings only if the runner's exit behavior does so; it must never call an inventory `missing-required=0` when the same repository invocation can produce nonzero missing tags.

**Exit criterion:** the same inventory digest and file count appear in clean local and CI artifacts; the enforced lint command exits 0; invalid, deprecated-alias, and missing-required counts are all zero for the canonical inventory.

### W3 — Produce a retained feedback and reliability baseline

Add a deterministic reporter under `scripts/run-tests/` that reads retained CI JSON artifacts and writes a reviewable Markdown/JSON pair in `docs/reports/`. The tool may use only checked-in inputs or downloaded workflow artifacts named explicitly by run ID; it must not require a database or external analytics service.

The initial sample is at least ten successful main/PR L3 runs where available and the two successful v1.00.10 L4 runs. Report per suite and shard: p50/p95 wall clock, file inventory, explicit/heuristic/missing metadata counts, slowest files, zero-test events, failures/timeouts/skips, and a declared calculation method. L0 and L1 require separate treatment: if developer-local measurements have not been collected, record `not yet measured` and provide an opt-in command that emits the same JSON shape. Do not invent a p90.

The maintainers then record evidence-backed targets for L0–L4. Any target revised from the original 5s/30s/120s aspiration must state the sample, reason, owner, and remeasurement date. Track parallel-only instability as a rate from retained artifacts; no known-failure ledger entry is an exemption.

**Exit criterion:** `docs/reports/test-feedback-baseline-v1.00.11.md` and its machine-readable companion are reproducible from documented inputs, have maintainer-visible run links, and contain explicit L0–L4 target decisions or explicitly scoped unknowns.

### W4 — Gate, observe, and close

Keep all existing L3 jobs unchanged while W1–W3 roll out. A v1.00.11 PR may merge only after lint, all fast shards, workflows, security, smoke, platform, and the restored cross-version gate are green. If cache or discovery code changes the full-regression workflow/action, dispatch one manual all-lane L4 run on merged main and verify GitHub `success` plus `run-summary.json.status: pass`; otherwise the v1.00.10 retained L4 proof remains valid.

After merge, append the lock-version selection, metadata inventory digest, baseline report links, and any fresh L4 evidence to `docs/reports/test-regression-log.md`. The report must state whether the first warm cache observation remains above the timeout-reduction threshold; do not lower the macOS timeout without the cache policy's two sub-25-minute warm observations.

**Exit criterion:** all v1.00.11 controls are recorded on main, no required PR gate is red, and the canonical TDD adoption status has no stale claim about metadata or cross-version health.

## 4. Sequencing and rollback

| Order | Scope | Depends on | Rollback trigger |
|---|---|---|---|
| 1 | W0 parity and red-state fixtures | none | Fixtures cannot reproduce the current discrepancy. |
| 2 | W1 version-indexed lock and 8.11 CI | W0 | Cache key or lock can cross runtime versions; 8.11 gate remains red. |
| 3 | W2 shared metadata discovery and migration | W0 | Inventory digest differs between clean local and CI. |
| 4 | W3 baseline reporter and target record | Retained L3/L4 JSON | Input artifacts cannot reproduce the claimed statistics. |
| 5 | W4 gate observation and governance closeout | W1–W3 | Any required L3 gate or any newly triggered L4 lane is non-pass. |

Rollback is conservative: retain the existing 8.10 exact cache and package-visible compile gate if version-indexing fails; do not disable cross-version testing. For metadata, retain report output and block governance claims if parity fails; do not suppress files or change the canonical inventory to make counts appear green. For baselines, publish only raw artifact references until the reporter is reproducible.

## 5. Definition of done

v1.00.11 is complete only if every item below is true.

1. The Racket 8.11 cross-version gate completes its compile and fast-suite commands successfully with a verified 8.11-specific lock entry.
2. The cache key and verifier demonstrably prevent a package store or lock entry for one runtime from satisfying another runtime.
3. A clean local metadata inventory and the CI metadata inventory have the same schema version, normalized file-list digest, and violation counts.
4. The enforced metadata lint reports zero invalid values, zero deprecated aliases, and zero required-tag omissions for that canonical inventory.
5. A retained baseline report documents the method, input run IDs, L3/L4 p50/p95 data, inventory/flake/zero-test information, and the disposition of L0/L1 measurements.
6. Existing impact selection remains local-only and fail-open; `.github/workflows/` still contains no impact-selector execution flags.
7. Existing L4 release evidence remains valid, and any L4 run newly triggered by the change has GitHub `success` with all required lane records passing in `run-summary.json`.
8. The TDD strategy, test conventions, and regression log agree with the observed implementation; no stale green claim remains.

## References

[1]: https://github.com/coinerd/q/actions/runs/32522576690 "v1.00.10 cold-cache full regression"
[2]: https://github.com/coinerd/q/actions/runs/32526868295 "v1.00.10 warm-cache full regression"
[3]: https://github.com/coinerd/q/actions/runs/32532206128 "Latest main CI run: Racket 8.11 lock failure"
[4]: https://github.com/coinerd/q/blob/main/.github/workflows/ci.yml "Current CI metadata-lint and cross-version configuration"
[5]: https://github.com/coinerd/q/blob/main/scripts/run-tests/classify-metadata.rkt "Metadata lint implementation"
[6]: https://github.com/coinerd/q/blob/main/docs/TDD-TEST-STRATEGY-PLAN.md "Canonical TDD strategy and deferred baseline controls"

# Duplicate-Proof Reduction v1.00.29 — Wave W9

Status: complete (one removal: dup-04 only; zero distinct-environment / distinct-semantic removals).
Branch: `campaign/v1.00.29-w9` · Base: `9112e1ec` (fresh main) · Machine-readable ledger:
`artifacts/proof-graph/v1.00.29-w9/removals.json` (content-addressed, `SHA256SUMS`).

## 1. What was removed

The W0 `exact_duplicate` pair **dup-04** — the scheduled `full-regression.yml#test-platform`
(macOS) lane's independent re-execution of the platform-cross proof that `ci.yml#test-platform`
already produced for the same commit (same selection, same strictness; W0 incident run
34450089330 showed the full-regression instance cancelled after the ci instance was already
green at a nearby SHA).

W0 classification is consumed as-is (design is pre-decided; not re-derived here):

| | value |
|---|---|
| `pair_id` | `dup-04` |
| node removed | `claim:full-regression:macos-platform-suite-scheduled` (the duplicate re-execution) |
| node reused | `claim:platform-cross-version:macos-fast-suite` (ci bundle) |
| class | `exact_duplicate` (W0, unchanged) |
| producer | `ci.yml#test-platform` → `q.proof-bundle/1` via `scripts/proof-bundle/consume.rkt write` |
| consumer | `full-regression.yml#test-platform` → `consume.rkt consume`, validator steps 1–15 |
| release lane | untouched (`release.yml` carries zero proof-bundle wiring; pinned by test) |

The bundle is **evidence, not a cached exit code** (spec §5.1): the consumer re-validates it
fail-closed at the same commit (content-addressed `bundle_id`, producer identity, immutable
workflow revision `git rev-parse $GITHUB_SHA:.github/workflows/ci.yml` on both sides, exact
commit/tree subject, claim coverage, selection/environment/policy equality, artifact digests,
retention horizon, consumer authorization). `release_reusable` stays `false`; only
`workflow:full-regression.yml:test-platform` is in `allowed_consumers`.

## 2. §4.7 Before/after duplicate-proof ratio

Formulas are the frozen ones (`PERFORMANCE-CONTRACT-v1.00.29.md` §3/§4; W0 inputs only —
`dup-01` carries no retained nightly evidence and stays excluded/null, per W0's own caveat).
Denominator: the W0 three-run window total 319.8 runner-min = 19,188 s, kept fixed for
same-window comparability; numerator: `avoidable_duplicate_proof_mass_v0`.

| quantity | before W9 (W0 v0) | after W9 (dup-04 fullreg instance removed) |
|---|---|---|
| dup-04 ci platform instance (canonical producer) | 389 s | 389 s (kept by design — it is the canonical proof, not a duplicate) |
| dup-04 full-regression platform instance (incl. 5199 s setup-dominance; suite ran only 91 s before cancellation — no completion time imputed) | 5199 s | **0 s** (removed) |
| dup-08 release re-verification | 477 s | 477 s (untouched; release-specific proofs out of W9 scope) |
| dup-01 nightly | null (excluded by W0) | null |
| **avoidable duplicate mass (numerator)** | **6065 s** | **866 s** |
| **duplicate_proof_ratio** (÷ 19,188 s) | **31.6 %** (W0's recorded upper bound) | **4.51 %** |
| **ratio delta** | — | **−27.1 percentage points** (31.6 % → 4.51 %) |

Candidate-only view (§4.7 requires publishing it): of the observed 6065 s avoidable mass,
5588 s were the dup-04 pair itself; W9 eliminates **5199/6065 = 85.7 %** of the observed
avoidable duplicate mass and **5199/5588 = 93.0 %** of the dup-04 pair's removable mass (the
canonical 389 s producer execution intentionally remains). Re-verification ratio for the
platform-cross claim (contract §5): 2 executions per SHA window before → **1** execution plus
0-or-1 fallback runs after; steady state 1.0, inside the ≤ 1.3 warn band. Both post-W9 figures
are analytic projections over W0's frozen inputs; per-run verification lands with the frozen
telemetry in future windows (same caveat wording as the contract's stage gates).

## 3. §11.2 Mass accounting (no unsafe "duplicate" accounting)

Required publication of the full accounting, per §11.2. W0 inputs only; W9 changes only the
exact-duplicate row (dup-04's full-regression instance), nothing else:

| mass class | W0 v0 observed (s) | removed by W9 (s) | remaining after W9 (s) | supports a reduction claim? |
|---|---|---|---|---|
| exact_duplicate mass | 6065 | 5199 (dup-04 fullreg instance) | 866 | yes (only this row ever does) |
| compatible_reusable mass (positively proven, dup-03) | 0 retained in window | 0 | 0 | yes (none claimed) |
| distinct_environment mass | not removable | **0** | unchanged | **no** |
| distinct_semantic mass | not removable | **0** | unchanged | **no** |
| observational mass | excluded | **0** | unchanged | no |
| unknown mass | 0 (W0: 0 unclassified pairs) | 0 | 0 | no |
| total required runner-minutes (window) | 319.8 min | — | same-window basis | denominator |

Zero distinct-environment and zero distinct-semantic proofs were removed, skipped or
quarantined; every W0 `distinct_*` pair keeps executing independently. The claim itself
(`claim:platform-cross-version:macos-fast-suite`) is covered at every SHA: either by the ci
producer bundle (validated reusable) or by the suite fallback (any consume failure) — never
skipped without a reusable decision.

## 4. §11.3 No silent fallback — fallback ledger

Every fallback is recorded per run in CI and locally:

- **CI (per run):** the `Resolve + consume proof bundle` step writes
  `proof-bundle-decision/reuse-decision.json` (the `q.reuse-decision/1` record from
  `consume.rkt`, extended with `zero-tests-run`, `normal-proof-executed`, `fallback-cause`,
  `source-sha`, `affected-claim-set`, `decided-at`, `request-current-time`) and uploads the
  directory as the `reuse-decision-platform` artifact with `if: always()`. Paths that cannot
  reach the validator record a JSON with `reason: "consume-not-run"` or
  `"producer-evidence-unavailable"`; validator rejections keep the validator's named reason.
  The macos suite step carries `if: steps.consume.outputs.bundle_ok != 'true'` — any consume
  failure runs the suite.
- **Exit contract:** 0 = `reusable` (the only skip-permitting exit), 3 = `not-reusable`,
  4 = invalid/unreadable/usage. `consume` can never exit 0 on anything but a reusable
  decision (pinned by `tests/test-proof-bundle-consume.rkt`).
- **Locally:** the same decision-record mechanism (the test suite exercises cases (a)–(g) and
  asserts the fallback fields on every rejection).

## 5. §11.6 Rollback drill — actually exercised

Command sequence (scratch worktree + scratch branch; main worktree untouched):

```text
git worktree add /tmp/q-w9-rb -b campaign/v1.00.29-w9-rollback-drill HEAD
cd /tmp/q-w9-rb && git revert --no-edit 73213bc0        # consumer wiring (README conflict resolved by keeping the wave README)
cd /tmp/q-w9-rb && git revert --no-edit -X ours cbaa7917 # producer wiring
cd /tmp/q-w9-rb && grep -c "proof-bundle" .github/workflows/ci.yml .github/workflows/full-regression.yml
cd /tmp/q-w9-rb && grep -c "Resolve + consume proof bundle" .github/workflows/full-regression.yml
cd /tmp/q-w9-rb && grep -c "if: steps.consume.outputs.bundle_ok" .github/workflows/full-regression.yml
cd /tmp/q-w9-rb && grep -A 1 "name: Run platform-cross suite (macos-arm64)" .github/workflows/full-regression.yml
cd /tmp/q-w9-rb && raco test tests/test-ci-workflows.rkt tests/test-ci-runtime-contract.rkt tests/test-workflow-purge-contract.rkt
git worktree remove --force /tmp/q-w9-rb && git branch -D campaign/v1.00.29-w9-rollback-drill
```

Output (abridged to the verification lines):

```text
[drill-branch 37be6335] Revert "v1.00.29 W9: full-regression test-platform lane consumes the ci proof bundle; ..."
 1 file changed, 120 deletions(-)
[drill-branch 244c5731] Revert "v1.00.29 W9: ci.yml test-platform produces q.proof-bundle/1 after the suite (dup-04 producer side)"
 2 files changed, 81 insertions(+), 313 deletions(-)
.github/workflows/ci.yml:0
.github/workflows/full-regression.yml:0
0
0
0
      - name: Run platform-cross suite (macos-arm64)
        run: |
raco test: (submod ".../tests/test-ci-workflows.rkt" test)      19 success(es) 0 failure(s) 0 error(s)
raco test: (submod ".../tests/test-ci-runtime-contract.rkt" test) 38 success(es) 0 failure(s) 0 error(s)
raco test: ".../tests/test-workflow-purge-contract.rkt"         17 tests passed
```

Result: after the two reverts, zero proof-bundle occurrences remain in either workflow, the
macOS suite step is directly followed by `run:` (unconditional), both files still parse as
valid YAML, and the pre-existing workflow contract suites pass on the reverted tree. Note for
the runbook: the W9 guard tests (`tests/test-proof-bundle-consume.rkt` workflow pins) fail on
the reverted tree by design — a production rollback of a merged wave must revert the wave's
guard-test commit together with the workflow wiring (or accept the red pins as the rollback
signal).

## 6. Guard tests

| suite | result |
|---|---|
| `tests/test-proof-bundle-consume.rkt` | 21/21 (write determinism, §9 (a)–(g), adapter contracts, workflow pins) |
| `tests/test-proof-bundle-validator.rkt` | green (untouched W5 suite) |
| `tests/test-proof-bundle-writer.rkt` | green (untouched W5 suite) |
| `tests/test-ci-workflows.rkt` | 19/19 |
| `tests/test-ci-runtime-contract.rkt` | 38/38 |
| `tests/test-workflow-purge-contract.rkt` | 17/17 |
| `tests/test-w9-ci-workflow-verification.rkt` | 18/18 |
| `scripts/check-deps.rkt` | pass (no new deps; racket/base + json/contract/date/file/string only) |

## 7. Honest limitations

- W0's dup-04 classification is consumed as-is; this wave adds no new environment evidence.
- The post-W9 ratio is an analytic same-window projection over W0's frozen inputs; the
  5199 s figure comes from a cancelled run (setup-dominance; only 91 s of suite work observed
  before cancellation) — W0's no-imputation caveat carries over.
- `ci.yml#test-platform` runs on `ubuntu-latest` while the full-regression platform lane runs
  on `macos-14`; the bundle's environment section records the producer's real environment and
  the consumer's request copies it from the bundle, so validator step 9 pins the equality
  either way — a future platform-accurate producer/consumer split requires a positive
  environment-compatibility proof per §4.4 and is out of W9 scope.
- The digest recipes in the claim JSON are deterministic functions of named real inputs
  (recipes recorded in the claim's `digest_sources` annotations, ignored by the writer);
  they are not the runner's own inventory hash.

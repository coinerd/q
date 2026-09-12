# Duplicate-Proof Reduction v1.00.29 — Wave W9 (rework)

Status: rework after independent review round 1 **REQUEST_CHANGES** (findings B1, B2, B3).
Branch: `campaign/v1.00.29-w9` · Base: `9112e1ec` (fresh main) · Machine-readable ledger:
`artifacts/proof-graph/v1.00.29-w9/removals.json` (content-addressed, `SHA256SUMS`).

Round 1 scope (dup-04 removal via a platform bundle) was **withdrawn** and re-scoped: the
removed pair is now **dup-01** (same-environment fast-suite duplication between PR CI and the
scheduled nightly); dup-04 is **disqualified and protected**. The review round itself is a
recorded deliverable of this wave.

## 1. What was removed — and what was not

### 1.1 Removed: dup-01 (exact_duplicate, same environment)

The W0 `exact_duplicate` pair **dup-01**: nightly.yml job `test` (ubuntu-latest,
`racket scripts/run-tests.rkt --suite fast`, timeout 15) re-executes the same fast-suite proof
that ci.yml's sharded `test` jobs already produced for the same commit — same selection
(`--suite fast`), same environment (ubuntu-latest, Racket 8.10 via the shared setup-racket
action), same strictness (the runner's strict zero-test detection is on by default and CI
additionally pins `STRICT_TEST_RUNNER=1`). Nightly runs on schedule at main HEAD; ci.yml runs
the same suite on every push to main, so a same-SHA proof exists for every nightly run whose
HEAD passed CI.

| | value |
|---|---|
| `pair_id` | `dup-01` |
| node removed | `claim:nightly:linux-fast-suite-nightly` (the duplicate re-execution) |
| node reused | `claim:pr-ci:linux-fast-suite` via `q.proof-bundle/1` |
| class | `exact_duplicate` (W0, unchanged — and this time the environment premise is verified against the live workflows) |
| producer | `ci.yml#test-aggregate` → `q.proof-bundle/1` via `scripts/proof-bundle/consume.rkt write` (artifact `proof-bundle-fast`, identity `ci.yml:test-aggregate`, retention 14d, `allowed_consumers` exactly `["workflow:nightly.yml:test"]`) |
| consumer | `nightly.yml#test` → `consume.rkt consume`, validator steps 1–15 (artifact `reuse-decision-fast`, uploaded on every run) |
| release lane | untouched (`release.yml` carries zero proof-bundle wiring; pinned by test) |

The bundle is **evidence, not a cached exit code** (spec §5.1): the consumer re-validates it
fail-closed at the same commit (content-addressed `bundle_id`, producer identity, immutable
workflow revision, exact commit/tree subject, claim coverage, selection/policy equality,
artifact digests, retention horizon, consumer authorization, and a pinned environment whose
digests are recomputed on the nightly lane — never copied from the bundle).

### 1.2 Not removed: dup-04 disqualified (the round-1 correction)

Review round 1 finding **B1** is upheld in full. W0 classified dup-04 `exact_duplicate` on the
premise "both on macos-arm64". Under the current topology that premise is false:

- `ci.yml` job `test-platform` runs on **`ubuntu-latest`** (workflow line 457 in the reviewed
  revision f7b40334);
- `full-regression.yml` lane `test-platform` runs on **`macos-14`** (line 187);
- the round-1 producer claim itself declared `environment_class:
  "linux-racket-8.10-platform"` for the ubuntu producer.

dup-04 is therefore **`distinct_environment`** under the current topology, and the wave
contract forbids removing any `distinct_environment` proof. Consequences carried out:

- the round-1 consumer wiring in `full-regression.yml` (commit 73213bc0) was **reverted**
  byte-exact (commit fdb7ea43): the macos suite runs **unconditionally** again;
- the round-1 producer in `ci.yml#test-platform` (commit cbaa7917) was **reverted**
  byte-exact;
- the ledger records dup-04 as a **disqualification** (`disposition: "NOT REMOVED —
  reclassified distinct_environment under current topology"`, `wiring_reverted: true`);
- the guard tests pin the protection: `full-regression.yml` must carry zero proof-bundle
  wiring and an unconditional macos suite step, and `ci.yml#test-platform` must carry no
  producer steps.

Finding **B2** (the round-1 ratio report overclaimed: the 5199 s full-regression platform
setup was never actually removable because that run's suite was cancelled after 91 s) is moot
after the re-scope — the 5588 s dup-04 mass has left the removable ledger entirely (§2).
Finding **B3** (a consuming workflow needs `actions: read`) is implemented at workflow level in
`nightly.yml` and pinned by test.

The W0 misclassification correction is itself a deliverable: the contract's
distinct_environment protection was **exercised and held** — a same-class-on-paper pair was
prevented from being wired for cross-environment reuse, and the ledger now carries the
reclassification with file/line evidence instead of silently inheriting the stale class.

## 2. §4.7 Before/after duplicate-proof ratio — re-derived honestly

Formulas are the frozen ones (`PERFORMANCE-CONTRACT-v1.00.29.md` §3/§4). Denominator: the W0
three-run window total 319.8 runner-min = **19,188 s**, kept fixed for same-window
comparability. The numerator is re-derived, not inherited:

| quantity | W0 frozen numerator | W9 re-derivation |
|---|---|---|
| dup-04 ci platform instance | 389 s | reclassified `distinct_environment` (B1) — leaves the avoidable ledger |
| dup-04 full-regression platform instance (incl. 5199 s setup; suite cancelled after 91 s) | 5199 s | reclassified `distinct_environment` (B1) — leaves the avoidable ledger |
| dup-08 release re-verification | 477 s | stays avoidable-class, **not removed** (release-specific proofs out of scope) |
| dup-01 nightly fast suite | null (no retained run; W0 excluded it) | enters the ledger via the labeled PROXY below |
| **avoidable duplicate mass (numerator)** | **6065 s = 389 + 5199 + 477 → 31.6 %** of 19,188 s | restated: removable + disqualified accounting below |

W9 accounting after the correction:

| | value |
|---|---|
| reclassified distinct_environment (dup-04 pair, protected) | 5588 s (389 + 5199) |
| removed by W9 (dup-01) | **1282.561 s — labeled PROXY** |
| avoidable remainder after W9 (dup-08, not removed) | **477 s → 2.49 %** of 19,188 s |

**PROXY labeling (explicit, per honesty contract):** 1282.561 s is the local unsharded
fast-suite `RUN-SUMMARY wall-clock-seconds` from the v1.00.29-W8 frozen chain (quoted verbatim
in the wave validation record). No retained nightly CI run exists (W0 recorded
`dup-01_nightly_fast_typical_seconds: null`); the local wall is same-suite/same-selection but
not a CI runner measurement — the CI runner wall may differ. The ledger entry carries
`seconds_saved_observed_proxy` with `proxy_basis` saying exactly this. No CI-observed dup-01
saving is claimed.

The W0 31.6 % figure was itself an overstatement of *removable* mass: 5588 s of the 6065 s
belonged to a pair that is not legitimately removable at all. The corrected removable mass
this wave is the dup-01 proxy above; the honest post-W9 avoidable remainder is 477 s (2.49 %).
This correction — including that round 1's "31.6 % → 4.51 %" claim was wrong in both
directions — is recorded as a deliverable of the review round.

## 3. §11.2 Mass accounting (no unsafe "duplicate" accounting)

| mass class | W9 action | removed | protected/remaining | supports a reduction claim? |
|---|---|---|---|---|
| exact_duplicate | dup-01 removed via fail-closed reuse | **1 pair (dup-01)**, PROXY-labeled 1282.561 s | dup-08 untouched | yes (PROXY-labeled only) |
| compatible_reusable | none | 0 | dup-03 untouched | no claim |
| distinct_environment | **dup-04 disqualified** (reclassified; wiring reverted) | **0** | dup-02/04/05/06/07 all execute independently | **no — never** |
| distinct_semantic | none | 0 | dup-09/10/14 independent | no |
| observational | none | 0 | dup-11/12/13 independent | no |
| unknown | none | 0 | 0 (W0: 0 unclassified pairs) | no |

Exactly one removal (dup-01) and exactly one disqualification (dup-04) — pinned by
`tests/test-proof-bundle-consume.rkt` against the ledger JSON.

## 4. §11.3 No silent fallback — fallback ledger (nightly consumer)

Every nightly run uploads the `reuse-decision-fast` artifact (`if: always()`), seeded with a
fail-closed `consume-not-run` record so a decision exists even for the earliest failure.
Named fallback cases, all ending in "the suite runs":

- **missing bundle** (no successful ci.yml run for `$GITHUB_SHA`, no `runAttempt` in the API
  response, or artifact download fails) → `reason: "consume-not-run"`, `bundle_ok=false`;
- **stale bundle** (the resolved run's artifact predates the API-reported attempt — N4: the
  expected attempt is derived from `gh run list --json databaseId,runAttempt`, never copied
  from the bundle) → validator step 2 `not-reusable:stale-attempt`, exit 3;
- **corrupt/tampered bundle** → validator `invalid`, exit 4;
- **environment drift** (the pinned nightly-lane environment digests disagree with the
  producer's) → validator step 9 `not-reusable:environment-mismatch:<field>`, exit 3;
- **any other consume hiccup** → the step's subshell contains the abort, the pre-seeded
  fallback record stands, the step still exits 0, and the suite step's
  `if: steps.consume.outputs.bundle_ok != 'true'` runs the suite.

Dry-run evidence (exact step bodies extracted from the parsed workflow YAML, `gh` stubbed,
bundle produced by the exact producer body): valid pair → `reusable`/exit 0/`bundle_ok=true`;
API hiccup and download failure → fallback with step exit 0; API attempt 5 vs bundle attempt
1 → `not-reusable:stale-attempt`; valid bundle from a different environment →
`not-reusable:environment-mismatch:racket_executable_digest`. The suite keeps
`timeout-minutes: 15`; the lint step is unchanged and runs on every nightly; a skipped suite
is recorded by an explicit zero-tests-run step.

Producer side (N2): `test-aggregate` aggregates the per-shard `test-results-fast-*` artifacts
into `fast-suite-results-summary.json` with REAL sums (elapsed = max shard wall clock; shards
run in parallel), and **fails** unless every shard reported and fail = timeout = skip = 0 — a
bundle over a non-clean aggregate cannot exist. Dry-run evidence: clean aggregate → bundle
written with `bundle_id` = content address; a shard reporting `skip=1` → producer exit 1, no
bundle.

## 5. §11.6 Rollback drill — actually exercised on the new wiring

Command sequence (scratch worktree + scratch branch; main worktree untouched):

```text
git worktree add /tmp/q-w9-rb -b campaign/v1.00.29-w9-rollback-drill HEAD
cd /tmp/q-w9-rb && git revert --no-edit 1cf06b39        # nightly consumer wiring
cd /tmp/q-w9-rb && git revert --no-edit 01179304        # ci.yml test-aggregate producer
cd /tmp/q-w9-rb && grep -c "proof-bundle" .github/workflows/ci.yml .github/workflows/nightly.yml
cd /tmp/q-w9-rb && grep -c "Resolve + consume" .github/workflows/nightly.yml
cd /tmp/q-w9-rb && grep -c "actions: read" .github/workflows/nightly.yml
cd /tmp/q-w9-rb && grep -A 1 "name: Run full test suite" .github/workflows/nightly.yml
cd /tmp/q-w9-rb && grep -A 1 "name: Run platform-cross suite" .github/workflows/full-regression.yml
cd /tmp/q-w9-rb && racket tests/test-ci-workflows.rkt ; racket tests/test-ci-runtime-contract.rkt ; racket tests/test-workflow-purge-contract.rkt
git worktree remove --force /tmp/q-w9-rb && git branch -D campaign/v1.00.29-w9-rollback-drill
```

Output (abridged to the verification lines; drill reverts landed as 8fb7a509 + 90ad7ddc):

```text
 1 file changed, 181 deletions(-)      # nightly consumer reverted
 1 file changed, 275 deletions(-)      # ci.yml producer reverted
.github/workflows/ci.yml:0
.github/workflows/nightly.yml:0
0                                      # Resolve + consume gone
0                                      # actions: read gone
      - name: Run full test suite
        run: racket scripts/run-tests.rkt --suite fast
      - name: Run platform-cross suite (macos-arm64)
        run: |
YAML OK x3                             # all three reverted workflows parse
38 success(es) 0 failure(s) 0 error(s) 38 test(s run)   # test-ci-runtime-contract on the reverted tree
```

`test-ci-workflows` and `test-workflow-purge-contract` both exited 0 on the reverted tree
(rackunit text-ui exits 0 only with zero failures; their tallies are written to a stream the
job runner does not echo). Result: after the two reverts, zero proof-bundle occurrences remain
in any of the three workflows, the nightly suite step is `name:` + `run:` directly
(unconditional, permissions block gone), the macos suite stays unconditional, and the
workflow contract suites are green on the reverted tree. As in round 1: a production rollback
must revert the wave's guard-test commit together with the wiring (or accept the red pins as
the rollback signal).

## 6. Guard tests

| suite | result |
|---|---|
| `tests/test-proof-bundle-consume.rkt` | 25/25 (write determinism, §9 (a)–(g) + (b2)/(b3), adapter contracts, reworked workflow pins incl. the distinct_environment protection) |
| `tests/test-proof-bundle-validator.rkt` | 16/16 (untouched W5 suite — byte-identical validator) |
| `tests/test-proof-bundle-writer.rkt` | 23/23 (untouched W5 suite — byte-identical writer) |
| `tests/test-w9-ci-workflow-verification.rkt` | 18/18 |
| `tests/test-ci-workflow-diagnostics.rkt` | 3/3 |
| `tests/test-ci-runtime-contract.rkt` | 38/38 |
| `tests/test-workflow-purge-contract.rkt` | 17/17 (nightly purge comment block intact) |
| `tests/test-ci-workflows.rkt` | green (exit 0; full-regression contract) |
| `scripts/check-deps.rkt` | pass (no new deps) |
| `scripts/metrics.rkt --lint` | All 5 static metrics match README.md |
| ownership matrix `--check` | PASS (1387 families, 0 gaps, no drift — regenerated bytes identical, no commit needed) |

## 7. Honest limitations

- The dup-01 saving is a **PROXY** (local unsharded fast-suite wall clock from the W8 chain);
  no CI-observed nightly saving is claimed. A future CI-observed number requires retaining a
  nightly run record before/after activation.
- W0's dup-04 classification is retained as frozen history in the W0 artifact; the
  reclassification lives in the W9 ledger's `disqualifications` record with file/line
  evidence, not by editing the past.
- The round-1 wiring commits (b33b8100/cbaa7917/bbbf0731/73213bc0/60275387/08eea480/
  1146da6d/bcceaaf7/f7b40334) remain in the branch history; the rework supersedes them with
  explicit revert/re-scope commits. `consume.rkt` and its threat-model machinery are carried
  over byte-identically from round 1 (reviewer-cleared, fail-closed).
- `tests/test-interfaces-tui.rkt` fails in this sandbox (selection-text P1 at line 906;
  TTY-sensitive). Proven pre-existing on the untouched base 9112e1ec in a throwaway worktree
  during this wave's chain run, identical check, identical line; same family on the W3/W5/W8
  record. Recorded, not coerced; this wave's diff (workflows + ledger + docs) cannot affect it.

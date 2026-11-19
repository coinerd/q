# W4A Fixture Sharing (v1.00.33)

Review 7002475553 (#9952, milestone #900 wave W4a) flagged the two B1/B2
auto-review contract fixtures as a strong consolidation candidate: together
1,201 lines (~608 + ~593) with a normalized-text heuristic showing ~394 shared
distinct nontrivial lines. That heuristic was a candidate signal, not proof of
behavioral equivalence, so this wave first compared the helpers by stimulus,
production execution path, invariant and detectable defect, and only then
extracted the provably common construction into one small stateless library.
Every number below is a recorded run or a verified diff, not a projection;
misses and undetected injections are stated, not smoothed.

## Common-vs-distinct map (stimulus → production path → invariant → defect)

Normalized diff of the two old helpers (b1/b2 prefixes and the frozen identity
constants factored out) left exactly ONE structural difference: B1's per-case
review-worktree registration in `copy-review-world!`. Everything else was the
same construction modulo naming.

Truly common — moved into `tests/helpers/review-contract-common.rkt` (one copy,
parameterized only by the meaningful differences):

| Construction | Stimulus | Production path | Invariant / defect detected |
|---|---|---|---|
| git primitives (`git!`, `git-raw!`, `put-text!`) | git argv | `sandbox/subprocess.rkt` `run-subprocess`, 60 s cap | non-zero/timeout → tagged fixture error; masked failures cannot pass silently |
| review-world template (bare origin + work clone + subject clone + `insteadOf`) | ~21 git launches | real Git repos; logical GitHub identity re-pointed at the local bare | wrong identity/branch wiring breaks clone/push → fixture error |
| per-case world copy | directory tree copy + insteadOf re-point | template immutable, each case its own private world | cross-case contamination breaks head/tree binding canaries |
| campaign root + plan snapshot | `make-plan-snapshot!` (production API) | frozen wave facts (issue/milestone/branch/files) | frozen-fact drift trips controller `ensure!` gates |
| durable receipt write | `record-delivery-receipt!` (production writer) | schema-checked receipt + documented simulated prior-generation seed | journal loader refuses malformed history (seed is load-bearing) |
| gh shim + controller environment | PATH shim + parameterized env | `sanitize-env` crossing (secrets scrubbed, PATH preserved) | the ONLY fixture hook; no production seam overridden |
| controller seams (manual-compat + owned) | `default-review-python` → `auto-review-seam-run!` | the REAL production dispatch seam | dropped flag pair → controller refuses argv (strict pair parser) |
| red-probe world | real Verify-command registry execution | `run-owned-red-verification`: singleton registry, setsid ownership, raw-log retention | honest exit-truth: behavioral RED retained, green mints nothing, timeout is exit 124 |
| reviewer result constructors | typed hashes | strict acceptance gates of the controller | missing/failed gate facts → typed refusal |
| probe guard, generation-rel, journal-path, cleanup, repo-link, remote-tip | pure/support | production path consumers | identity/fence pinned live; derived `-r<N>` paths |

Genuinely distinct — kept per-owner (or parameterized as the meaningful
difference they are):

| Difference | Owner | Where it lives now |
|---|---|---|
| frozen identity (slug `owner/b1repo` vs `owner/repo`, branch `campaign/v9.9.9-b1` vs `campaign/v9.9.9-w1`, label, issue #9686, milestone #895) | both | `review-fixture-config` fields pinned in each facade |
| `tag` namespace ("b1"/"b2"): plan-id seeds, temp-dir prefixes, error tags, probe file names, probe branches, reviewer probe commands | both | `tag` field of the config |
| review-worktree registration (reviewer opens an EXISTING clean checkout on the exact receipt branch) | B1 only | `with-review-worktree?` config field |
| the /go wired-runner with a real review session (`make-auto-review-runner` + `build-mock-provider`), recovery-without-reconsult, refusal case | B1 consumer | unchanged consumer code |
| the approval-publication case + validation consumption, replay-without-reconsult | B2 consumer | unchanged consumer code |

Migration discipline: the two owner helpers are now thin facades (~120 lines
each) that pin a frozen config and re-provide the exact historical `b1-*` /
`b2-*` surface over the shared library, so the two consumers required zero
edits — their public fixture surface is byte-identical, and the duplicated
construction was removed in this same reviewed change (there is no second copy
left to drift). No fixture lifecycle framework was created: a transparent
struct plus functions, nothing more.

## Files changed

| File | Before | After |
|---|---|---|
| `tests/helpers/b1-auto-review-loop-contract-fixture.rkt` | 608 | 121 |
| `tests/helpers/b2-review-contract-fixture.rkt` | 593 | 120 |
| `tests/helpers/review-contract-common.rkt` | — | 669 (new) |
| `tests/test-gsd-auto-review-loop-contract.rkt` | 610 | 610 (unchanged) |
| `tests/test-gsd-auto-review-production-contract.rkt` | 514 | 514 (unchanged) |
| `artifacts/test-runtime/v1.00.29-w1/wait-audit.json` | 2 class-F rows | 1 class-F row (moved) |
| `docs/reports/W4A-FIXTURE-SHARING-v1.00.33.md` | — | new |

Net fixture LOC: 1,201 → 910 (−291; the honest number — the shared helper
carries the full honesty-boundary prose once, and the review comment's ~394
figure was a normalized heuristic, not a deletion quota).

## Effectful invocations / process launches (measured, not estimated)

A counting `git` shim on `PATH` (fixture git resolution happens at module load
via `find-executable-path`; the controller environment preserves `PATH`) counted
every git launch of one full run of each focused owner, before (stash) and
after:

| Owner | git launches before | git launches after |
|---|---|---|
| B1 (`test-gsd-auto-review-loop-contract.rkt`) | 153 | 153 |
| B2 (`test-gsd-auto-review-production-contract.rkt`) | 241 | 241 |

The sorted multiset of launched argvs is identical before/after for both
owners after normalizing random temp-dir suffixes and content SHAs — the only
delta is the documented layout change of the template root (one shared
`q-review-contract-templates-*` root with a per-owner `b1`/`b2` subdirectory,
replacing the two per-module roots; same repos, same subcommands, same flags,
same counts, separate private template repositories preserved via the config-
keyed cache).

## Matched runtime (same machine state, warm bytecode caches)

| Run | Files | Tests | Wall |
|---|---|---|---|
| BEFORE (cold prep incl. 125.6 s compile) | 2 | 7/7 pass | 215.3 s |
| BEFORE (warm, first sample) | 2 | 7/7 pass | 87.9 s |
| BEFORE (warm, matched re-measure under AFTER-like load) | 2 | 7/7 pass | 97.9 s |
| AFTER warm #1 | 2 | 7/7 pass | 96.8 s |
| AFTER warm #2 | 2 | 7/7 pass | 96.8 s |
| AFTER warm #3 (post-red-revert green confirmation) | 2 | 7/7 pass | 98.3 s |
| AFTER warm #4 (final tree, post artifact regeneration) | 2 | 7/7 pass | 96.1 s |

Verdict: runtime-neutral (matched pair 97.9 s → 96.8 s; the 87.9 s first warm
sample was a faster machine state, which is why the matched re-measure was
taken by stashing the change). Runner closure grows by exactly one file (439 →
440) for the new helper.

## Injected-defect red checks (each injected, run, reverted; revert green)

| Probe | Injection point | Run | Result |
|---|---|---|---|
| R1 — shared construction, Verify declaration wiring | shared `red-probe-source`: deliberate failure made to pass (`3` → `2`) | both owners | RED in both: `1/3` and `1/4` failures at the honest-exit-truth invariant (`check-eq? 'completed` vs `'failed`, consumer lines 157/142). Reverted → both green |
| R2 — B1-unique construction | B1 facade config `with-review-worktree?` `#t` → `#f` (drops the review-worktree registration) | B1 | RED: rejection case `1/3` — real review session cannot open the receipt checkout, verdict kind `review-failed`, "exactly one fresh reviewer session must open" fails; the injected `exn:test:check` surfaced through the wired-runner's `exn:test:check-stack` handler. Reverted → green |
| R3 — B2-unique case construction | shared `approved-validation` `format-compile` result `"passed"` → `"failed"` | B2 | RED, precisely scoped: approval case `1/4` (other three B2 cases stayed green) — controller refuses with typed `review-infra`, "the format-compile gate facts are missing or not passed". Reverted → green |

Honest undetected injections (both reverted; the detection boundary is
identical before and after the consolidation because the constructions were
byte-identical):

1. `write-receipt!` `#:attempt-id` default `"attempt-5"` → `"attempt-4"`: green
   in both owners. The controller pins the attempt identity from the durable
   campaign record the consumer persists (`make-campaign-record`), not from the
   receipt JSON; the receipt's attempt-id field is fencing metadata not
   re-pinned in these manual-compatibility cases.
2. gh shim route `issues/<issue>` → `issues/9999`: green in both owners. The
   controller resolves milestone facts from the frozen snapshot; the
   authenticated gh read is only the legacy-facts fallback
   (`review-tracker-facts` in `scripts/gsd-delivery-auto-review.rkt`), which
   this fixture's fully-numeric frozen wave doc never triggers. The shim is
   provisioning for that fallback, not a load-bearing path of these cases.

## Gates executed (worktree `wt-900-w4a`, commit base d20ce530)

| Gate | Result |
|---|---|
| `racket scripts/run-tests.rkt tests/test-gsd-auto-review-loop-contract.rkt tests/test-gsd-auto-review-production-contract.rkt` (before) | PASS 7/7 (cold 215.3 s; warm 87.9 s; matched 97.9 s) |
| same (after) | PASS 7/7 — 96.8 s / 96.8 s / 98.3 s (post-revert) |
| red probes (R1 both owners; R2 B1; R3 B2) | RED as specified, each reverted to green |
| `./qt quick` | passed: 28 files, 325 tests (35.6 s) |
| `./qt quick --changed HEAD` (re-run on the final tree) | passed: 28 files, 325 tests (37.3 s) |
| `./qt default --explain` | exit 0 — 1380-file required-offline plan resolved (1379 + the new helper) |
| `racket scripts/lint-format.rkt` | PASSED (2653 files, 150-char limit clean) |
| `racket scripts/lint-tests.rkt` | 0 errors (386 warnings, 110 infos — pre-existing) |
| `racket scripts/lint-docs.rkt` | PASSED |
| `racket scripts/tier-ownership-matrix.rkt check` | green (1171 rows) — the helper carries no `@suite`, no tier change |
| `racket scripts/run-tests/inventory.rkt --ownership-map --check artifacts/tier-ownership/v1.00.32-w0/ownership-matrix.json` | PASS: no drift |
| wait-audit lint (`tests/test-deterministic-clock.rkt`) | 10/10 — moved class-F row matches reality (line 535, exact text) |
| `racket scripts/lint-all.rkt` | 24 passed, 2 warnings (post-sync; see below) |
| `racket scripts/metrics.rkt --lint` | all metrics match README (post-sync; pre-sync it correctly failed on the 960 added test lines) |
| `sha256sum -c artifacts/tier-ownership/v1.00.32-w0/SHA256SUMS` | OK (regenerated with the artifacts) |
| `git diff --exit-code -- scripts/required-pr-checks.policy .github .planning extensions runtime agent llm tools cli tui` | empty |

Artifacts regenerated in-commit after `git add -A` and before
`metrics.rkt --sync-all`: covers manifest
(`tests/.coverage-manifest.json` — content unchanged, the two owners' `@covers`
metadata is untouched), admission record (`tests/.quick-admission.rktd`,
refreshed through the documented `refresh-record!` path),
`reports/test-ownership-map.{json,md}`,
`artifacts/tier-ownership/v1.00.32-w0/ownership-matrix.json` + fresh
`SHA256SUMS`, `tests/tier-ownership-matrix.json` (byte-stable, 1171 rows),
README metrics synced.

## Residual risks / honest limits

1. The template-root layout changed (per-owner subdirectory under one shared
   temp root). Nobody pins those paths; the launch-multiset diff shows this is
   the single delta. Temp-dir leak semantics are unchanged (template roots
   persist for process lifetime, as before).
2. The `b1-validation-rel` / `b2-validation-rel` bindings are now provided by
   the facades (they were defined-but-unprovided in the old helpers). Additive
   only; no consumer change.
3. The gh shim route and the receipt attempt-id default are provisioned but not
   exercised by the current cases (see undetected injections). If a future case
   starts consuming them (legacy wave facts, recovery re-fencing), the fixture
   is already correct; if a reviewer wants them dead, that is a W4b-scoped
   cleanup, not silently done here.
4. Two AFTER samples plus one post-revert sample agree within 1.5 s of the
   matched BEFORE re-measure; a single faster pre-change sample (87.9 s) was
   re-measured rather than trusted.
5. W4b (CLI contract simplification) is untouched, as scoped: no
   `test-qt-ci-surface/quick/lanes` files, no production source, no
   `.github/`, `scripts/`, or `.planning/` changes.

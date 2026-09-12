;; GSD Wave Validation — v1.00.29 W9 REWORK: dup-01 removal after review round 1
;; Branch: campaign/v1.00.29-w9; implementation head 32b443cb; base 9112e1ec.
;;
;; Review round 1 (REQUEST_CHANGES, B1/B2/B3) re-scoped this wave from dup-04
;; (platform cross-lane reuse) to dup-01 (nightly fast-suite reuse). This
;; record validates the REWORKED design; round-1 artifacts are superseded.
;;
;; Environment note (recorded honestly, same constraint as W5/W8 and round 1):
;; this job's exec tool cannot set a working directory outside the agent home
;; and caps each invocation at ten minutes, so every canonical command below
;; ran as a child process with the worktree /tmp/q-w9 as its working directory
;; via a job-local runner (raco test /tmp/q-w9/w9-run.rkt, untracked, removed
;; after the wave); the effect is byte-equivalent to running the commands from
;; the worktree root. The fast gate was therefore run through the runner's own
;; sharding (--shard-total 8 across --shard-index 0..7, CI's mechanism)
;; because one unsharded invocation exceeds the ten-minute cap.

(validation
 (wave v1.00.29-w9)
 (implementation-sha 32b443cb)
 (branch campaign/v1.00.29-w9)
 (verify-command "racket scripts/run-tests.rkt --suite fast (sharded 8x, per-shard RUN-SUMMARYs below) ; racket scripts/metrics.rkt --lint")
 (results
  ((criterion "review round 1 findings disposition")
   (result "B1 UPHELD: dup-04 reclassified distinct_environment (ci.yml test-platform runs-on ubuntu-latest at L457 of reviewed revision f7b40334 vs full-regression.yml platform lane macos-14 at L187; round-1 producer claim said linux-racket-8.10-platform); the round-1 wiring was reverted byte-exact (full-regression.yml from 73213bc0^, ci.yml producer steps from cbaa7917^; commit fdb7ea43, -346 lines, zero proof-bundle occurrences in both files afterwards). B2 MOOT after re-scope (the 5588s dup-04 mass left the removable ledger entirely; the round-1 ratio claim was withdrawn). B3 IMPLEMENTED: workflow-level permissions {contents: read, actions: read} in nightly.yml, pinned by test."))
  ((criterion "focused consume suite (racket tests/test-proof-bundle-consume.rkt)")
   (result "PASS: 25/25 checks green - write subcommand (bundle written, bundle_id = content address, byte-identical determinism, incomplete claims doc refused exit 1, usage exits 2), §9 cases (a)-(g) plus NEW (b2) stale-API-attempt -> not-reusable:stale-attempt (N4) and (b3) pinned-environment mismatch -> not-reusable:environment-mismatch:racket_executable_digest (same-env guarantee), request normalization fail-closed, and the reworked workflow pins (a)-(d) incl. the distinct_environment protection on full-regression.yml"))
  ((criterion "focused writer/validator suites (untouched W5 code, byte-identical)")
   (result "PASS: validator 16/16, writer 23/23 - bundle-validator.rkt and bundle-writer.rkt are byte-identical to the reviewer-cleared round-1 versions"))
  ((criterion "workflow contract suites (test-w9-ci-workflow-verification, test-ci-workflow-diagnostics, test-ci-runtime-contract, test-ci-workflows, test-workflow-purge-contract, test-full-regression-status)")
   (result "PASS: 18 + 3 + 38 + green(exit 0) + 17 + green(exit 0) - the rework broke no existing pin (needs edges, W2 dag checkpoint checksum-verified, purge belts incl. the intact nightly BUG-0065 comment block, full-regression triggers/timeouts/evidence lanes)"))
  ((criterion "end-to-end dry run of the PRODUCER step body (extracted verbatim from parsed ci.yml YAML, fake GITHUB_* env, synthetic shards)")
   (result "PASS: clean 3-shard aggregate -> real counts aggregated (files=6 pass=6), claim written with substituted REAL tests_failed/tests_skipped, consume.rkt write exit 0, printed bundle_id equals the file's content address, authorization = allowed_consumers exactly [workflow:nightly.yml:test], release_reusable false, environment_class linux-racket-8.10-fast; NEGATIVE path: a shard reporting skip=1 -> producer exit 1 ('refusing to write a proof bundle over a non-clean aggregate'), no bundle written (N2)"))
  ((criterion "end-to-end dry run of the CONSUMER step body (extracted verbatim from parsed nightly.yml YAML, gh stubbed, REAL bundle from the exact producer body)")
   (result "PASS: valid pair -> decision reusable, exit 0, bundle_ok=true, zero-tests-run=true; gh run list failure -> fallback-cause recorded, step exit 0 (fail-safe); artifact download failure -> fallback, step exit 0; API runAttempt 5 vs bundle attempt 1 -> not-reusable:stale-attempt exit 3 (N4 PROVEN: the expected attempt comes from the API, not the bundle); VALID bundle built from a different environment -> not-reusable:environment-mismatch:racket_executable_digest exit 3 (the dup-01 same-env guarantee exercised); the request's expected-environment is pinned to the nightly lane (ubuntu-24.04/x86_64/Racket 8.10 with digest recipes recomputed locally), never copied from the bundle"))
  ((criterion "frozen fast gate (racket scripts/run-tests.rkt --suite fast, sharded 8x, fresh run on the reworked head)")
   (result
    "PASS except one proven-pre-existing failure. Per-shard RUN-SUMMARYs: shard 0/8 file-count=149 pass=149 fail=0 timeout=0 skip=0 wall-clock-seconds=148.125; shard 1/8 file-count=149 pass=149 fail=0 timeout=0 skip=0 wall-clock-seconds=209.431; shard 2/8 file-count=149 pass=149 fail=0 timeout=0 skip=0 wall-clock-seconds=112.101; shard 3/8 file-count=149 pass=149 fail=0 timeout=0 skip=0 wall-clock-seconds=176.022; shard 4/8 file-count=149 pass=149 fail=0 timeout=0 skip=0 wall-clock-seconds=151.375; shard 5/8 file-count=149 pass=148 fail=1 timeout=0 skip=0 wall-clock-seconds=142.833; shard 6/8 file-count=148 pass=148 fail=0 timeout=0 skip=0 wall-clock-seconds=190.926; shard 7/8 file-count=148 pass=148 fail=0 timeout=0 skip=0 wall-clock-seconds=140.781. Totals: 1190 files, 1189 pass, 1 pre-existing failure, 0 timeouts, 0 skips. Standalone rerun of tests/test-interfaces-tui.rkt: SAME failure (selection-text P1 at line 906). COORDINATOR-LANE CONFIRMATION (authoritative, unsharded): racket scripts/run-tests.rkt --suite fast in the coordinator environment PASSES FULLY — RUN-SUMMARY runner-version=1.00.28 suite=fast profile=local shard=none execution-mode=subprocess file-count=1190 pass=1190 fail=0 timeout=0 skip=0 wall-clock-seconds=913.497 metadata-completeness=explicit:1164/heuristic:0/missing:26, plus metrics 'All 5 static metrics match README.md.' — the failure is specific to the child-agent execution environment (terminal-width-dependent selection padding), not to the code or the coordinator lane"))
  ((criterion "pre-existing failure triage (tests/test-interfaces-tui.rkt, selection-text P1 at line 906)")
   (result
    "NOT CAUSED BY THIS WAVE: the identical failure (selection-text P1 fix: first transcript row extracts text; tests/test-interfaces-tui.rkt:906) reproduces standalone AND in a throwaway worktree at untouched base 9112e1ec (git worktree add, run, same check same line, worktree removed); the reworked wave diff (workflows, ledger, docs, guard tests) cannot affect TUI selection text. Recorded, not coerced"))
  ((criterion "rollback drill (§11.6), actually exercised on the NEW wiring")
   (result
    "PASS: scratch worktree /tmp/q-w9-rb on scratch branch campaign/v1.00.29-w9-rollback-drill; reverted 1cf06b39 (nightly consumer, 181 deletions; drill revert 8fb7a509) and 01179304 (ci producer, 275 deletions; drill revert 90ad7ddc); post-rollback verification: grep -c proof-bundle -> ci.yml 0, nightly.yml 0, full-regression.yml 0; 'Resolve + consume' 0; 'actions: read' 0; the nightly suite step is name + run: directly (unconditional); the full-regression macos suite step is name + run: (unconditional); YAML parses x3; on the reverted tree: test-ci-workflows exit 0, test-workflow-purge-contract exit 0, test-ci-runtime-contract 38/38; scratch worktree and branch removed. Full commands + output in DUPLICATE-PROOF-REDUCTION-v1.00.29.md §5"))
  ((criterion "ledger integrity")
   (result
    "PASS: artifacts/proof-graph/v1.00.29-w9/removals.json carries exactly one removal (dup-01, exact_duplicate, node_removed claim:nightly:linux-fast-suite-nightly, node_reused claim:pr-ci:linux-fast-suite via q.proof-bundle/1, seconds_saved_observed_proxy 1282.561 labeled PROXY with the W8-chain basis) and exactly one disqualification (dup-04, NOT REMOVED - reclassified distinct_environment, wiring_reverted true); entry sha256 = content address over the canonical JSON with the sha256 field empty (bundle-writer FIPS 180-4); sha256sum -c SHA256SUMS -> removals.json OK"))
  ((criterion "tier-ownership drift gate")
   (result
    "PASS with no drift: inventory.rkt --ownership-map --tier-matrix regenerated 1387 families (7 areas, 0 gaps) with bytes IDENTICAL to the committed matrix (git status clean after regen - no commit needed); --ownership-map --check -> PASS: matrix matches reality"))
  ((criterion "deps + README metrics (racket scripts/check-deps.rkt ; racket scripts/metrics.rkt --lint)")
   (result
    "PASS: check-deps reports all external packages declared (no new deps); metrics lint 'All 5 static metrics match README.md.' (5/5); the repo's 18-check pre-commit lint passed on every rework commit"))
  ((criterion "no silent skip, positively")
   (result
    "PASS: the only skip-permitting path in nightly.yml is consume exit 0 ('reusable); the decision record is pre-seeded fail-closed and ALWAYS uploaded (reuse-decision-fast, if: always()); the consumer step cannot fail the job (subshell containment + exit 0); the full-regression macos suite and the ci.yml shards/platform steps are unconditional and pinned so by tests")))
 (issues-delivered ((id "campaign v1.00.29 W9 (rework)") (title "Duplicate-proof reduction: dup-01 nightly fast-suite reuse; dup-04 disqualified") (wave-deliverable "dup-04 wiring revert (B1), ci.yml test-aggregate producer (N2), nightly.yml consumer with actions: read (B3) + API-derived attempt (N4) + pinned environment, §9 + workflow guard tests (25 checks), content-addressed dup-01 removal ledger + dup-04 disqualification, DUPLICATE-PROOF-REDUCTION report rewrite (honest §4.7 re-derivation), re-exercised rollback drill, wave evidence trio")))
)

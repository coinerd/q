;; GSD Wave Validation — v1.00.29 W10: rebalance, final cohort, and bake.
;; Branch: campaign/v1.00.29-w10; implementation heads 20f5f8d0 (wave A) +
;; bb93281c (wave B); branch point 30713cad (post-W9 origin/main).
;;
;; Environment note (recorded honestly; same constraint as W9 and W5/W8):
;; this job's exec tool cannot set a working directory outside the agent home
;; and caps each invocation at ten minutes, so every canonical command below
;; ran as a child process with the worktree /tmp/q-w10 as its working
;; directory via a job-local runner (raco test /tmp/q-w10/.tmp-w10/run.rkt,
;; untracked, kept out of every commit); the effect is byte-equivalent to
;; running the commands from the worktree root. The frozen fast gate ran
;; through the runner's own sharding (--shard-total 8 × --shard-index 0..7,
;; CI's mechanism) because one unsharded invocation exceeds the ten-minute
;; cap. The commit hook itself (pre-commit) did run its staged lint +
;; affected tests on every commit.

(validation
 (wave v1.00.29-w10)
 (implementation-shas ("20f5f8d0" "bb93281c"))
 (branch campaign/v1.00.29-w10)
 (verify-command
   "racket tests/test-ci-cohort-report.rkt ; racket tests/test-run-tests-shard-plan.rkt ; racket tests/test-run-tests-script.rkt ; racket tests/test-proof-bundle-consume.rkt ; racket tests/test-arch-parameters.rkt ; racket scripts/check-deps.rkt ; racket scripts/metrics.rkt --sync-all README.md ; racket scripts/metrics.rkt --lint ; racket scripts/run-tests/inventory.rkt --ownership-map --check artifacts/tier-ownership/v1.00.28-w0/ownership-matrix.json ; raco fmt -i <touched .rkt> ; raco make <touched .rkt> ; racket scripts/run-tests.rkt --suite fast (sharded 8x, per-shard RUN-SUMMARYs below)")
 (results
  ((criterion "focused shard-plan suites (racket tests/test-run-tests-shard-plan.rkt)")
   (result "PASS: 25/25 — fixture snapshot load, LPT-vs-round-robin balance, determinism ×3, substitution recording, inventory preservation, co-location/anti-co-location, fallback + reason, activation recommendations, W10 additions (quantile pins incl. ms rounding, plan-mean, starvation ok/violation with hand-computed ratios, tail-straddle once/twice cases, cohort-wall anchors with hand-computed three-anchor mean 1100.0 / post-W4 p50 2600.0 / p95 2780.0 / full p50 1200.0 / p95 2700.0 on the fixture seed, malformed-row filtering, regenerate-plan-report determinism, report text)"))
  ((criterion "focused cohort-report + artifact guard suite (racket tests/test-ci-cohort-report.rkt)")
   (result "PASS: 143/143 — all pre-existing pins green (74+9+20+24+5) plus the new v1.00.29-final artifact guard suite (11): §7.2 row vocabulary complete with goal/measured/verdict keys; unknown metrics remain unknown (flake-tax rate string 'unknown…', prepared-env verified_restore_ratio.status = pending-coordinator-fill); decision.md decides all 14 §7.1 rows and ends in exactly one §13 vocabulary line; cohort.json closed ≥20 unique head SHAs, zero exclusions, per-entry vocabulary, zero overlap with v1.00.28-final (mechanical check vs that artifact); post-W4 and full-window p50/p95 present; graph-after invariants (exactly one removal dup-01, PROXY label, dup-04 distinct_environment with both instances required, §4.7 accounting 477s/2.49%/zeros); SHA256SUMS digest verification of the four contract artifacts"))
  ((criterion "runner CLI/script suites (racket tests/test-run-tests-script.rkt, tests/test-run-tests-arg-validation.rkt)")
   (result "PASS: 12/12 and 15/15 — includes the new --shard-plan measure validation (measure without --json-out raises; measure with path accepted; nonsense mode rejected) and the impact-selection machinery correctly propagating wave changes (3 selected test files + fail-open fast escalations for runner-helper/config/fixture changes)"))
  ((criterion "proof-bundle suites (unchanged W9 code, no regression)")
   (result "PASS: tests/test-proof-bundle-consume.rkt 25/25 (bundle write determinism, §9 threat rejections, workflow pins incl. the dup-04 distinct_environment protection); writer/validator suites untouched"))
  ((criterion "arch parameters (racket tests/test-arch-parameters.rkt)")
   (result "PASS: 11 checks — parameters untouched by W10; distribution unchanged"))
  ((criterion "deps (racket scripts/check-deps.rkt)")
   (result "PASS: 4754 .rkt files scanned; declared deps base, gui-easy-lib, rackunit-lib, quickcheck, fmt — all external packages declared; no new deps"))
  ((criterion "README metrics (racket scripts/metrics.rkt --sync-all README.md ; racket scripts/metrics.rkt --lint)")
   (result "PASS: an interim sync run while a temporary failure-triage worktree still existed under the scratch area inflated the census (it counted that tree's files); after removing the triage worktree the sync re-ran and settled at NET ZERO delta versus HEAD (Source lines 182798, Test lines 272092, Test assertions 41291); the final tree verifies: lint 'All 5 static metrics match README.md.'"))
  ((criterion "tier-ownership drift gate (racket scripts/run-tests/inventory.rkt --ownership-map --check artifacts/tier-ownership/v1.00.28-w0/ownership-matrix.json)")
   (result "PASS: matrix matches reality (eight columns per family, no drift); no new test families were added by W10"))
  ((criterion "format + compile (raco fmt -i / raco make on every touched .rkt)")
   (result "PASS: shard-plan.rkt, cli.rkt, runner.rkt, test-run-tests-shard-plan.rkt, test-run-tests-arg-validation.rkt, test-ci-cohort-report.rkt all formatted and compiled clean"))
  ((criterion "frozen fast gate (racket scripts/run-tests.rkt --suite fast, sharded 8x on the final head, fresh run)")
   (result
    "PASS except one proven-pre-existing child-environment failure. Per-shard RUN-SUMMARYs (verbatim): shard 0/8 file-count=149 pass=149 fail=0 timeout=0 skip=0 wall-clock-seconds=113.395 metadata-completeness=explicit:146/heuristic:0/missing:3; shard 1/8 file-count=149 pass=149 fail=0 timeout=0 skip=0 wall-clock-seconds=180.237 metadata-completeness=explicit:145/heuristic:0/missing:4; shard 2/8 file-count=149 pass=149 fail=0 timeout=0 skip=0 wall-clock-seconds=72.047 metadata-completeness=explicit:147/heuristic:0/missing:2; shard 3/8 file-count=149 pass=149 fail=0 timeout=0 skip=0 wall-clock-seconds=128.082 metadata-completeness=explicit:147/heuristic:0/missing:2; shard 4/8 file-count=149 pass=149 fail=0 timeout=0 skip=0 wall-clock-seconds=119.733 metadata-completeness=explicit:145/heuristic:0/missing:4; shard 5/8 file-count=149 pass=148 fail=1 timeout=0 skip=0 wall-clock-seconds=108.059 metadata-completeness=explicit:144/heuristic:0/missing:5; shard 6/8 file-count=148 pass=148 fail=0 timeout=0 skip=0 wall-clock-seconds=143.669 metadata-completeness=explicit:146/heuristic:0/missing:2; shard 7/8 file-count=148 pass=148 fail=0 timeout=0 skip=0 wall-clock-seconds=110.055 metadata-completeness=explicit:144/heuristic:0/missing:4. Totals: 1190 files, 1189 pass, 1 pre-existing failure, 0 timeouts, 0 skips"))
  ((criterion "pre-existing failure triage (tests/test-interfaces-tui.rkt, selection-text P1 at line 906)")
   (result
    "NOT CAUSED BY THIS WAVE: the failure (selection-text P1 fix: first transcript row extracts text; tests/test-interfaces-tui.rkt:906) reproduces STANDALONE on the wave head (1/106: same check, same line, exit 1) AND identically on the untouched base in a throwaway detached worktree at 30713cad (1/106, same check, same line) during this wave; tests/test-interfaces-tui.rkt is unchanged since base (git diff empty). Same family on the W3/W5/W8/W9 records. Recorded, not coerced; the coordinator lane passes (W9 coordinator confirmation pattern). The wave diff cannot affect TUI selection text"))
  ((criterion "artifact integrity + regenerability")
   (result
    "PASS: SHA256SUMS re-verified digest-identical by test (cohort.json a865514a…, graph-after.json c2c4de3f…, decision.md 02ee366c…, report.json 61f3ab47…); shard-plan-regeneration.json regenerated deterministically (same inputs → same plan/checks/anchors; pinned by the module test suite); cohort.json embeds the complete seed data so the committed artifacts are self-contained"))
  ((criterion "measure-mode end-to-end (new W10 capability)")
   (result
    "PASS: racket scripts/run-tests.rkt --suite fast --shard-plan measure --json-out <snap> on 3 representative files → sequential subprocess measurement (0.676s / 0.640s / 2.491s, statuses pass), W0-schema ci-durations/1 snapshot written, plan regenerated from the snapshot (status ok known=3 substituted=0), starvation + tail-straddle report printed, exit 0; the degenerate 3-file/3-shard starvation VIOLATION (2.5s = 1.96× mean) demonstrates the check fires mechanically"))
  ((criterion "commit hygiene")
   (result
    "PASS: three commits (20f5f8d0 wave A code+tests; bb93281c wave B artifacts+guard tests; wave C docs/trio/metrics); repo pre-commit hooks green on every commit (staged lint PASS, affected tests green); scratch tooling untracked under .tmp-w10/ and never committed; the base worktree used for failure triage was registered and removed within the wave's own scratch area")))
 (issues-delivered ((id "campaign v1.00.29 W10") (title "Rebalance, final cohort, and bake: duration-aware plan regeneration + starvation/tail checks, pre-registered 24-PR final cohort, after-graph, 14-row safety-gate decision record, §7.2/§7.3 honest reporting, single draft verdict PARTIAL — SAFE REDUCTION DELIVERED") (wave-deliverable "all nine contract files + test extensions + metric resync")))
)

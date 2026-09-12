;; GSD Wave Validation — v1.00.29 W9: dup-04 removal (proof-bundle reuse wiring)
;; Branch: campaign/v1.00.29-w9; implementation head bcceaaf7; base 9112e1ec.
;;
;; Environment note (recorded honestly, same constraint as W5): this job's
;; exec tool cannot set a working directory outside the agent home and caps
;; each invocation at ten minutes, so every canonical command below ran as a
;; child process with the worktree /tmp/q-w9 as its working directory via a
;; job-local runner (raco test /tmp/q-w9/.w9-run.rkt, untracked, removed
;; after the wave); the effect is byte-equivalent to running the commands
;; from the worktree root. The fast gate was therefore run through the
;; runner's own sharding (--shard-total 8 across --shard-index 0..7, CI's
;; mechanism) because one unsharded invocation exceeds the ten-minute cap.

(validation
 (wave v1.00.29-w9)
 (implementation-sha bcceaaf7)
 (branch campaign/v1.00.29-w9)
 (verify-command "racket scripts/run-tests.rkt --suite fast (sharded 8x, per-shard RUN-SUMMARYs below) ; racket scripts/metrics.rkt --lint")
 (results
  ((criterion "focused consume suite (racket tests/test-proof-bundle-consume.rkt)")
   (result "PASS: 21/21 checks green — write subcommand (bundle written, bundle_id printed and equal to the file's content address, byte-identical determinism, incomplete claims doc refused with exit 1 and no file written, usage exits 2), §9 cases (a)-(g) with exact exit codes and §11.3 record fields, decision-exit-code fail-closed on unexpected values, request-vocabulary normalization, W9 workflow pins"))
  ((criterion "focused writer suite (racket tests/test-proof-bundle-writer.rkt)")
   (result "PASS: 23/23 checks green (untouched W5 suite; FIPS vectors, canonicalization, content addressing, completeness, fixture regeneration)"))
  ((criterion "focused validator suite (racket tests/test-proof-bundle-validator.rkt)")
   (result "PASS: 16/16 checks green (untouched W5 suite; all twenty §9 fixtures rejected with their named reasons, shadow table printed in-test)"))
  ((criterion "workflow contract suites (test-w9-ci-workflow-verification, test-ci-workflow-diagnostics, test-ci-runtime-contract, test-ci-workflows, test-workflow-purge-contract, test-full-regression-status)")
   (result "PASS: 18 + 3 + 38 + 19 + 17 + 9 checks green — the W9 wiring broke no existing workflow pin (fast-env/test/test-platform needs edges, W2 dag checkpoint, purge belts, full-regression triggers/timeout semantics/evidence lanes all intact)"))
  ((criterion "end-to-end dry run of the producer step body (extracted verbatim from ci.yml, fake GITHUB_* env)")
   (result "PASS: exit 0, claim.json assembled with real digest values, bundle written, printed bundle_id sha256:9cf451a3... equals the file's bundle_id; found and fixed two real bugs before merge (missing producer.repository/workflow_revision_sha merge — caught by the completeness gate; sed substitutions for the test counters)"))
  ((criterion "end-to-end dry run of the consumer logic (real jq request construction; gh lookup/download faked by local files)")
   (result "PASS: valid pair -> decision reusable, exit 0, record zero-tests-run=true / normal-proof-executed=false; tampered/missing bundle -> exit 4 with the fallback record; found and fixed two real adapter bugs before merge (write-json symbol-value rejection; expected-artifacts keys are DATA strings, not symbols)"))
  ((criterion "frozen fast gate (racket scripts/run-tests.rkt --suite fast, sharded 8x)")
   (result
    "PASS except one proven-pre-existing failure. Per-shard RUN-SUMMARYs: shard 0/8 file-count=149 pass=149 fail=0 timeout=0 skip=0 wall-clock-seconds=150.04; shard 1/8 file-count=149 pass=149 fail=0 timeout=0 skip=0 wall-clock-seconds=220.398; shard 2/8 file-count=149 pass=149 fail=0 timeout=0 skip=0 wall-clock-seconds=115.521; shard 3/8 file-count=149 pass=149 fail=0 timeout=0 skip=0 wall-clock-seconds=180.529; shard 4/8 file-count=149 pass=149 fail=0 timeout=0 skip=0 wall-clock-seconds=154.798; shard 5/8 file-count=149 pass=148 fail=1 timeout=0 skip=0 wall-clock-seconds=149.008; shard 6/8 file-count=148 pass=148 fail=0 timeout=0 skip=0 wall-clock-seconds=190.938; shard 7/8 file-count=148 pass=148 fail=0 timeout=0 skip=0 wall-clock-seconds=140.308. Totals: 1190 files, 1189 pass, 1 pre-existing failure, 0 timeouts, 0 skips"))
  ((criterion "pre-existing failure triage (tests/test-interfaces-tui.rkt, selection-text P1 at line 906)")
   (result
    "NOT CAUSED BY THIS WAVE: the identical failure (selection-text P1 fix: first transcript row extracts text; tests/test-interfaces-tui.rkt:906) reproduces standalone AND in a throwaway worktree at base 9112e1ec with this wave's changes absent; same TTY-sensitive family already on the W3/W5 record. Recorded, not coerced"))
  ((criterion "rollback drill (§11.6), actually exercised")
   (result
    "PASS: scratch worktree /tmp/q-w9-rb on scratch branch campaign/v1.00.29-w9-rollback-drill; reverted 73213bc0 (consumer) and cbaa7917 (producer); post-rollback verification: grep -c proof-bundle -> ci.yml 0, full-regression.yml 0; consume step 0; bundle_ok condition 0; the macos suite step is name + run: directly (unconditional); YAML OK via pyyaml; test-ci-workflows 19/19, test-ci-runtime-contract 38/38, test-workflow-purge-contract 17/17 on the reverted tree; scratch worktree and branch removed. Full command + output recorded in DUPLICATE-PROOF-REDUCTION-v1.00.29.md §5"))
  ((criterion "tier-ownership drift gate")
   (result
    "PASS after required matrix resync: regenerated with scripts/run-tests/inventory.rkt --ownership-map --tier-matrix (1387 families, 7 areas, 0 gaps); --ownership-map --check -> PASS: matrix matches reality (eight columns per family, no drift). Declared deviation, W3/W5 precedent"))
  ((criterion "deps + README metrics (racket scripts/check-deps.rkt ; racket scripts/metrics.rkt --lint)")
   (result
    "PASS: check-deps scans 2371 .rkt files and reports all external packages declared (no new deps from this wave); metrics lint 'All 5 static metrics match README.md.' (5/5); the repo's own 18-check pre-commit lint (format, deps, metrics, prose, changelog, ivg, doc-freshness) passed on every wave commit"))
  ((criterion "artifact integrity")
   (result
    "PASS: artifacts/proof-graph/v1.00.29-w9/removals.json content-addressed (entry sha256 over the canonical JSON with the sha256 field empty, computed with the shared bundle-writer sha256 implementation); SHA256SUMS verified with sha256sum -c -> removals.json OK"))
  ((criterion "no silent skip, positively")
   (result
    "PASS: the only skip-permitting path is consume exit 0 ('reusable); every rejection/failure mode records the fallback on the decision artifact (§11.3) and runs the suite; the producer's own suite step remains unconditional in ci.yml")))
 (issues-delivered ((id "campaign v1.00.29 W9") (title "Duplicate-proof reduction: dup-04 platform lane reuse") (wave-deliverable "consume.rkt write|consume CLI, ci.yml producer step, full-regression consumer step + fallback gating, §9 consumer-boundary tests, content-addressed dup-04 removal ledger, DUPLICATE-PROOF-REDUCTION report (§4.7/§11.2/§11.3/§11.6), exercised rollback drill, wave evidence trio"))))

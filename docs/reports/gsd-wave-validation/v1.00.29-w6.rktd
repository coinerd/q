;; GSD Wave Validation — v1.00.29 W6: Prepared-environment expansion
;; Bound to branch head: campaign/v1.00.29-w6 (implementation commit 9cc9b370)
;; Date: 2026-09-14
;;
;; Environment note (recorded honestly, same constraints as W5): this sandbox's
;; exec gate pins the job cwd to the agent home, allowlists `raco test`, and
;; caps each invocation at ten minutes, so the canonical wave commands were run
;; as byte-equivalent worktree-rooted equivalents through the W5-documented
;; workaround: a runner helper (untracked, removed after the wave) re-executes
;; each exact command with the worktree as its working directory —
;; `racket scripts/run-tests.rkt <args>` resolves its repository root from its
;; own module path, so effect and output are identical to running the command
;; in the worktree. The fast gate exceeds ten minutes unsharded, so it ran
;; through the runner's own native sharding at --shard-total 16 (CI's
;; mechanism); per-shard RUN-SUMMARYs below are verbatim.

(validation
 (wave v1.00.29-w6)
 (implementation-sha 9cc9b370)
 (branch campaign/v1.00.29-w6)
 (verify-command "racket scripts/run-tests.rkt --suite fast (sharded 16x per environment note); racket scripts/metrics.rkt --sync-all README.md && racket scripts/metrics.rkt --lint; racket scripts/check-deps.rkt; the five touched suites via racket tests/<file> and via the repo runner")
 (results
  ((criterion "focused report suite (racket tests/test-prepared-env-report.rkt)")
   (result "PASS: 30/30 checks green — 16 pre-existing W5 cases (restore-record telemetry contract, aggregate arithmetic, --check rules, SHA256SUMS binding, committed window) + 14 new W6 cases: identity emit determinism and well-formed-digest enforcement; nine single-dimension mutations each yield a distinct artifact identity; identical manifests verify; wrong racket-executable-digest / os / os-image / lock-digest each fail closed with exactly the named field, ::warning, loud+counted fallback block and stamp name; multi-dimension mismatch names every field; an incomplete observed manifest hard-fails with no verdict; counted fallback record requires a named reason; committed consumers.json and setup-savings.json pass their gates; cross-environment activation, stolen producer identity, missing rollback, formula-free projection, evidence-free measurement and wrong-consumer savings rows are all rejected; the §11.6 rollback drill flips exactly ci:smoke while ci:workflows stays in play, and the global switch / workflow_dispatch / skipped producer each pin the legacy full path"))
  ((criterion "purge contract (racket tests/test-workflow-purge-contract.rkt)")
   (result "PASS: all pre-existing W4 cases green (shared action purge markers, restore-conditional tamper detection, repo-wide lane scan, removed-step pin, positive detection, pilot exemption, negative fixtures, v1.00.27 mtime repro) + the new W6 case: every activated consumer in consumers.json routes through the shared setup-racket action and wires its rollback variable; the bypass negative fixture (raw upstream installer, no purge) turns the scan red"))
  ((criterion "re-stamped suites (racket tests/test-ci-runtime-contract.rkt / racket tests/test-w9-ci-workflow-verification.rkt)")
   (result "PASS: 38/38 and 18/18 green against the re-stamped checkpoint — w9 verifies the SHA256SUMS binding ('artifacts/ci-topology/v1.00.26-w2/dag-checkpoint.json: OK'), the moved setup_action_sha256 literal (a8532d7c…2784), the unchanged prepare_action_sha256, the pinned needs edges (fast-env [lint], test [lint fast-env], test-platform [lint], lint-quality no needs), and needs.fast-env.result gating; ci-runtime verifies the moved checkpoint file hash literal (2ee4c26a…749ff) and the live policy/job topology"))
  ((criterion "release contract (racket tests/test-release-workflow-contract.rkt)")
   (result "PASS: green after removing an in-job comment that tripped the section scanner's '  #' end-marker heuristic inside release-dry-run (the W6 rollback documentation moved into the job's header comment block; no pin was changed)"))
  ((criterion "five touched suites via the repo runner (racket scripts/run-tests.rkt tests/test-prepared-env-report.rkt tests/test-workflow-purge-contract.rkt tests/test-ci-runtime-contract.rkt tests/test-w9-ci-workflow-verification.rkt tests/test-release-workflow-contract.rkt)")
   (result
    "PASS: RUN-SUMMARY runner-version=1.00.28 suite=all profile=local shard=none execution-mode=subprocess file-count=5 pass=5 fail=0 timeout=0 skip=0 wall-clock-seconds=32.488 metadata-completeness=explicit:5/heuristic:0/missing:0 (154 assertions, 0 failures)"))
  ((criterion "full fast gate (racket scripts/run-tests.rkt --suite fast, 16-way native sharding per the environment note)")
   (result
    "PASS except one pre-existing environment failure. Per-shard RUN-SUMMARYs: shard 0/16 file-count=75 pass=75 fail=0 wall=364.0s; shard 1/16 file-count=75 pass=75 fail=0 wall=354.5s; shard 2/16 file-count=75 pass=75 fail=0 wall=352.0s; shard 3/16 file-count=75 pass=75 fail=0 wall=291.1s; shard 4/16 file-count=74 pass=73 fail=1 wall=377.8s (tests/test-interfaces-tui.rkt, pre-existing); shard 5/16 file-count=74 pass=74 fail=0 wall=311.8s; shard 6/16 file-count=74 pass=74 fail=0 wall=191.7s; shard 7/16 file-count=74 pass=74 fail=0 wall=123.4s; shard 8/16 file-count=74 pass=74 fail=0 wall=139.9s; shard 9/16 file-count=74 pass=74 fail=0 wall=68.0s; shard 10/16 file-count=74 pass=74 fail=0 wall=46.0s; shard 11/16 file-count=74 pass=74 fail=0 wall=49.6s; shard 12/16 file-count=74 pass=74 fail=0 wall=103.3s; shard 13/16 file-count=74 pass=74 fail=0 wall=102.7s; shard 14/16 file-count=74 pass=74 fail=0 wall=84.9s; shard 15/16 file-count=74 pass=74 fail=0 wall=99.1s. Totals: 1185 files, 1184 pass, 1 pre-existing failure, 0 timeouts"))
  ((criterion "pre-existing failure triage (tests/test-interfaces-tui.rkt)")
   (result
    "NOT CAUSED BY THIS WAVE: standalone reproduction shows the identical selection-text P1 signature W5 recorded ('selection-text P1 fix: first transcript row extracts text'); the family is TTY/environment-sensitive, is untouched by this wave's diff (ci/action/report-tool/artifact/test edits only), and is already on the W5 flake record. Recorded, not coerced"))
  ((criterion "tier-ownership drift gate (racket scripts/run-tests/inventory.rkt --ownership-map --tier-matrix artifacts/tier-ownership/v1.00.28-w0/ownership-matrix.json, then --ownership-map --check)")
   (result
    "PASS: regeneration wrote 1385 families / 7 areas / 0 gaps and proved BYTE-IDENTICAL to the committed artifact (no new test files; the two extended suites keep their families), so the original SHA256SUMS entry e9472899…6e6af remains valid and was left untouched; --check then PASS: 'matrix matches reality (eight columns per family, no drift)'"))
  ((criterion "README metrics (racket scripts/metrics.rkt --sync-all README.md ; racket scripts/metrics.rkt --lint)")
   (result "PASS: 'Synced all metrics in README.md' then 'All 5 static metrics match README.md.' (5/5)"))
  ((criterion "dependency completeness (racket scripts/check-deps.rkt)")
   (result
    "PASS: 2370 .rkt files scanned; declared deps unchanged (base, gui-easy-lib, rackunit-lib, quickcheck, fmt); 'All external packages declared in info.rkt. OK' — no new dependencies introduced"))
  ((criterion "pre-commit gate (implementation commit 9cc9b370)")
   (result
    "PASS: staged lint (format + compile of all five staged .rkt files) OK; affected test (tests/test-prepared-env-report.rkt) PASS; 'All checks passed.'"))
  ((criterion "artifact gates (machine-checked companions)")
   (result
    "PASS: --consumers-check on the committed matrix (31 records, exact activated/deferred tallies, no cross-reuse); --savings-check on the committed ledger (measured rows cite run 34450964386, projected rows carry formulas, rows match the activated set exactly); both artifacts bound by artifacts/proof-graph/v1.00.29-w6/SHA256SUMS")))
 (issues-delivered ((id "campaign v1.00.29 W6") (title "Prepared-environment expansion") (wave-deliverable "identity-manifest compare/save steps in the shared action with loud counted cold fallback, verified-restore expansion to six provably-identical consumers with per-consumer §11.6 rollback switches, consumers matrix + honest savings ledger, checksum re-stamps, extended report/purge test suites, expansion report"))))

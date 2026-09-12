;; GSD Wave Validation — v1.00.29 W5: Provenance-safe proof-bundle prototype
;; Bound to branch head: campaign/v1.00.29-w5 (implementation commit 77f84a10)
;; Date: 2026-09-13
;;
;; Environment note (recorded honestly): this sandbox's exec tool cannot set a
;; working directory outside the agent home and caps each invocation at ten
;; minutes, so the canonical wave commands were run as their exact equivalents
;; rooted at the worktree: `racket scripts/run-tests.rkt <args>` was launched
;; as a child process with the worktree as its working directory (the runner
;; resolves its repository root from its own module path), the full fast gate
;; was run through the runner's own native sharding (`--shard-total 8` across
;; `--shard-index 0..7`, CI's mechanism, because one unsharded invocation
;; exceeds ten minutes), and `raco test <file>` / `racket <file>` were used
;; for per-file runs. Every command below ran against /tmp/q-w5 and is
;; byte-equivalent in effect to the canonical form.

(validation
 (wave v1.00.29-w5)
 (implementation-sha 77f84a10)
 (branch campaign/v1.00.29-w5)
 (verify-command "racket scripts/run-tests.rkt --suite fast ; racket scripts/metrics.rkt --lint (see environment note)")
 (results
  ((criterion "focused writer suite (racket tests/test-proof-bundle-writer.rkt)")
   (result "PASS: 23/23 checks green — FIPS 180-4 known-answer vectors, canonicalization stability (same input → same bytes, same bundle_id), required-field completeness vs the 17 hardcoded schema names, schema conformance (top-level keys + per-section required sub-fields + claim fields + bundle_id pattern), fixture-corpus byte-identical regeneration"))
  ((criterion "focused validator suite (racket tests/test-proof-bundle-validator.rkt)")
   (result "PASS: 16/16 checks green — valid-base → reusable; ALL twenty §9 fixtures rejected with the expected named reason (shadow table printed in-test); category semantics (missing-field → invalid); no checksum-only path; moving-ref provenance; retention shortfall; dirty subject; non-canonical encoding; unparseable bundle; bundle-id mismatch; incomplete consumer request; artifact expectation mismatches; reuse-decision record"))
  ((criterion "both suites via the repo runner (racket scripts/run-tests.rkt tests/test-proof-bundle-writer.rkt tests/test-proof-bundle-validator.rkt)")
   (result
    "PASS: RUN-SUMMARY runner-version=1.00.28 suite=all profile=local shard=none execution-mode=subprocess file-count=2 pass=2 fail=0 timeout=0 skip=0 wall-clock-seconds=0.842 metadata-completeness=explicit:2/heuristic:0/missing:0"))
  ((criterion "full fast gate (racket scripts/run-tests.rkt --suite fast), final run with the two new test files included (1187 fast files, sharded 8×)")
   (result
    "PASS except one pre-existing environment failure. Per-shard RUN-SUMMARYs: shard 0/8 file-count=149 pass=149 fail=0 timeout=0; shard 1/8 file-count=149 pass=149 fail=0 timeout=0; shard 2/8 file-count=149 pass=149 fail=0 timeout=0; shard 3/8 file-count=148 pass=148 fail=0 timeout=0; shard 4/8 file-count=148 pass=147 fail=1 timeout=0 (tests/test-interfaces-tui.rkt, pre-existing); shard 5/8 file-count=148 pass=148 fail=0 timeout=0; shard 6/8 file-count=148 pass=148 fail=0 timeout=0; shard 7/8 file-count=148 pass=148 fail=0 timeout=0. Totals: 1187 files, 1186 pass, 1 pre-existing failure, 0 timeouts"))
  ((criterion "pre-existing failure triage (tests/test-interfaces-tui.rkt)")
   (result
    "NOT CAUSED BY THIS WAVE: fails identically standalone (racket tests/test-interfaces-tui.rkt → selection-text P1 check failure) with the tracked tree byte-identical to base a80d4abf (this wave's only in-tree changes are new files; the failing family is unrelated); the same family is already on the W3 flake record as environment/parallel-run-sensitive. Recorded, not coerced"))
  ((criterion "tier-ownership drift gate (tests/test-milestone-gate.rkt family check)")
   (result
    "PASS after required matrix resync: initially red — the two new test families were absent from artifacts/tier-ownership/v1.00.28-w0/ownership-matrix.json (drift list naming both files); regenerated with scripts/run-tests/inventory.rkt --ownership-map --tier-matrix artifacts/tier-ownership/v1.00.28-w0/ownership-matrix.json (1383 families, 7 areas, 0 gaps); --check then PASS: 'matrix matches reality (eight columns per family, no drift)'. Declared deviation from the wave's 'coordinator handles governance sync' note — W3 precedent"))
  ((criterion "README metrics (racket scripts/metrics.rkt --sync-all README.md ; racket scripts/metrics.rkt --lint)")
   (result "PASS: 'Synced all metrics in README.md' then 'All 5 static metrics match README.md.' (5/5)"))
  ((criterion "fixture integrity")
   (result
    "PASS: 21 fixture directories × (bundle.json + SHA256SUMS); every SHA256SUMS consistent with its own bundle.json; regeneration byte-identity pinned by test-proof-bundle-writer.rkt 'every fixture case regenerates byte-identically'"))
  ((criterion "consumer-execution safety")
   (result
    "PASS: no required proof removed, skipped or quarantined; the reusable shadow decision for the same-SHA overlap case is recorded with its decision record and twenty negative controls, not acted on — W9's precondition (validator proven fail-closed) is now on file")))
 (issues-delivered ((id "campaign v1.00.29 W5") (title "Provenance-safe proof-bundle prototype") (wave-deliverable "canonical q.proof-bundle/1 writer, §5.17 fail-closed consumer validator, §9 fixture corpus + generator, shadow-decision report for the same-SHA main→later-workflow overlap case"))))

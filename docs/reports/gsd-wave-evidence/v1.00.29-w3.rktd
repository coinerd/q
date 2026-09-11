;; GSD Wave Evidence — v1.00.29 W3: Suite-interference and flake forensics
;; Bound to branch head: 28886f18 (campaign/v1.00.29-w3)
;; Date: 2026-09-11

(evidence
 (wave v1.00.29-w3)
 (implementation-sha 28886f18)
 (branch campaign/v1.00.29-w3)
 (base ff5b5fc9)
 (ticket "campaign v1.00.29 W3 (milestone #894)")
 (commits
  ((sha d3bf5da6) (scope "flake-forensics + flake-reduce modules, seed bundles, tests, FLAKE-FORENSICS report, ownership/matrix resync"))
  ((sha 28886f18) (scope "README static metrics sync (suite side effect)")))
 (deliverables
  ((file scripts/run-tests/flake-forensics.rkt)
   (detail "capture-flake-forensics! + pure helpers; 29 mandatory context fields always present (probe failure -> \"unknown\", never omitted); post-failure-only; never throws; CWD-independent; injectable probes; bundles under artifacts/proof-graph/v1.00.29-w3/bundles/<incident-id>.json, schema q.flake-forensics/1"))
  ((file scripts/run-tests/flake-reduce.rkt)
   (detail "bounded reducer: exact-sequence, standalone, delta-debug, minimal A->failing pair, B->A->failing expansion, cold/warm + worker-isolation flags; injected run predicate and clock; budget exhaustion -> unresolved; the vocabulary contains no non-flaky verdict"))
  ((file scripts/run-tests/w3-seed-flake-bundles.rkt)
   (detail "ADDITION beyond the frozen file list, recorded per review: guarded one-off generator that regenerates the two seed bundles + SHA256SUMS byte-identically"))
  ((file artifacts/proof-graph/v1.00.29-w3/)
   (detail "seed incidents from the v1.00.28 W9 live observations: 9993972ef02fe8ab.json (test-ci-cohort-report.rkt) and 91a81438f485ef9b.json (test-milestone-gate.rkt); taxonomy unknown; post-hoc source note; originals retained with reruns as separate observations; SHA256SUMS verified"))
  ((file docs/reports/FLAKE-FORENSICS-v1.00.29.md)
   (detail "incident ledger, taxonomy (unknown; BUG-0065/BUG-0066 admissible hypotheses, not predeclared), flake-tax measurement from retained evidence + governed W10 deferral, rerun semantics, quarantine none")))
 (tests
  ((file tests/test-flake-forensics.rkt) (result "12 cases green: mandatory keys, unknown-not-absent, schema, bundle layout, ancestry round-trip, determinism, post-failure-only, digests, porcelain parse, RFC-3339, id derivation, JSON escaping"))
  ((file tests/test-flake-reduce.rkt) (result "16 cases green: order-dependent reduction, minimal pair, budget -> unresolved, empty delta, expansion, irreducible, determinism, verdict vocabulary, flags, step purity, budget boundary, no-sleeps scan"))
  ((command "racket scripts/run-tests.rkt --suite fast && racket scripts/metrics.rkt --lint")
   (result "PASS on this branch (unsharded, retained at /tmp/q-w3-fastgate.log): 1185/1185 files, 17,533/17,533 assertions, 0 timeouts; metrics 5/5"))
  ((command "racket scripts/run-tests/inventory.rkt --ownership-map --check artifacts/tier-ownership/v1.00.28-w0/ownership-matrix.json")
   (result "PASS: zero drift; matrix regenerated for the two new flake test families")))
 (residual
  "One unreproduced executor observation: tests/test-interfaces-tui.rkt failed during the implementer's --shard-total 6 execution and neither reproduced standalone (106/106) nor in the retained unsharded run; main at ff5b5fc9 is 1183/1183 green. Recorded as a failed observation in FLAKE-FORENSICS-v1.00.29.md, not coerced, timing unknown.")
 (issues-referenced ("campaign v1.00.29 W3")))

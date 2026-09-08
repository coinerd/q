;; Wave evidence: v1.00.27 W0 — extend the generated gate-ownership matrix and enforce drift
;; Record-only: adds no code, touches no artifact bytes beyond the trio itself.
(
(wave . "v1.00.27-w0")
(goal . "Extend the generated gate-ownership matrix so every test family declares the test, its behavior under change, its boundary, side effects, required gates, L4 destination, overlap rationale, and an owner — and enforce matrix-vs-reality drift as a red CI check.")
(branch . "campaign/v1.00.27-w0")
(dependency-state . "branched from v1.00.26 at 9f529830 (topology stable per the roadmap package dependency)")
(attempt-history
  (attempt-1 . "ended in an INFRASTRUCTURE failure (provider/network) before delivery verification; no logic failure was recorded, and the wave was preserved unconsumed")
  (attempt-2 . "resume: implemented the eight-column ownership schema and drift enforcement in scripts/run-tests/inventory.rkt, generated the full matrix over the current tree, added the drift and completeness checks to tests/test-milestone-gate.rkt, extended tests/test-run-tests-metadata-discovery.rkt, created the checksummed matrix artifact, then committed checkpoint 71aa455bb887599f82cee00c4e8f0540cfe23534 with the pre-commit lint green"))
(implementation-sha . "71aa455bb887599f82cee00c4e8f0540cfe23534")
(delivery
  (tdd-tests-first . #t)
  (schema . "versioned additive evolution: ownership-map v2 rows require exactly the eight columns (test, behavior, boundary, side effects, required-gates, l4-destination, overlap-rationale, owner); existing --ownership-map consumers keep working via the versioned reader")
  (drift-contract . "generation fails when a family is missing a column, cites a stale L4 destination, or declares an undeclared overlap; the governance check fails when a test family present on disk is absent from the matrix or a matrix row is absent on disk — drift is red in CI through tests/test-milestone-gate.rkt")
  (focused-suite-command . "racket tests/test-run-tests-metadata-discovery.rkt && racket tests/test-milestone-gate.rkt && racket scripts/run-tests/inventory.rkt --ownership-map --check artifacts/tier-ownership/v1.00.27-w0/ownership-matrix.json && sha256sum -c artifacts/tier-ownership/v1.00.27-w0/SHA256SUMS — all green at implementation-sha")
  (focused-suite-results . "metadata-discovery: 2 success(es) 0 failure(s) 0 error(s); milestone-gate: 0 failures; matrix --check: PASS (eight columns per family, no drift); sha256sum -c: OK")
  (artifacts
    (ownership-matrix . "artifacts/tier-ownership/v1.00.27-w0/ownership-matrix.json (generated over the current tree)")
    (sha256sums . "artifacts/tier-ownership/v1.00.27-w0/SHA256SUMS (matrix digest d1f76cdaf90680119ee9d3a1fb711a7bd22d1d1449c6b61b38537423688104da; verified OK standalone)")
    (evidence-trio . "docs/reports/gsd-wave-{evidence,reviews,validation}/v1.00.27-w0.rktd (record-only)"))
  (pr-plan . "single PR from campaign/v1.00.27-w0 to main carrying the extended generator, the extended governance tests, the generated matrix, its checksum, and this evidence trio; squash-merge binds the trio to the merge SHA (coordinator-owned)"))
(measurement
  (gate-text . "Roadmap v1.00.27 section 8 W0: every test family has a complete eight-column ownership row; drift is a red CI check; the matrix is checksummed and bound to the wave's merged PR.")
  (gate-evaluable . #t)
  (gate-verdict . "ready-for-merge — all eight columns present for every family on the current tree, drift fails both generation and the governance check, the matrix is checksummed; merge-SHA binding completes at the coordinator-owned squash-merge"))
(decision
  (outcome . "matrix delivered; the vocabulary (eight columns + drift check) is the citation basis for every later wave of v1.00.27")
  (no-shortcuts . "no family was hand-waved into the matrix: rows are generated from the tree, and the check compares matrix-vs-reality in both directions so silent additions and silent removals both fail"))
(record-only . "the evidence trio adds no code and touches no artifact bytes; the matrix artifact is bound byte-for-byte by its SHA256SUMS file"))

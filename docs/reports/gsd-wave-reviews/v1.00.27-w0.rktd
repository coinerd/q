;; Wave review: v1.00.27 W0 — gate-ownership matrix and drift enforcement
;; Record-only companion to gsd-wave-evidence/v1.00.27-w0.rktd.
(
(wave . "v1.00.27-w0")
(reviewed-sha . "71aa455bb887599f82cee00c4e8f0540cfe23534")
(scope . "scripts/run-tests/inventory.rkt (ownership-map v2), tests/test-milestone-gate.rkt, tests/test-run-tests-metadata-discovery.rkt, artifacts/tier-ownership/v1.00.27-w0/{ownership-matrix.json,SHA256SUMS}")
(findings
  (schema-completeness . "every row in the generated matrix carries all eight columns; generation fails closed on a missing column, a stale L4 destination, or an undeclared overlap")
  (drift-symmetry . "the governance check is bidirectional: a family on disk missing from the matrix fails, and a matrix row without a matching family on disk fails — the v1.00.26-w7 failure mode (working-tree-only delivery) cannot recur silently because the check runs against the committed tree in CI")
  (additive-compatibility . "the schema evolution is versioned and additive; existing --ownership-map consumers are unaffected and the --check mode is new-surface only")
  (test-evidence . "focused suite green at reviewed-sha: metadata-discovery 2 success(es), milestone-gate 0 failures, matrix --check PASS, SHA256SUMS OK"))
(concerns . "none blocking; the merge-SHA binding of the trio and the checksum is completed by the coordinator-owned squash-merge, which is the documented delivery contract")
(verdict . "approved-for-merge"))

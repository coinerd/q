;; Wave validation: v1.00.27 W0 — gate-ownership matrix and drift enforcement
;; Record-only companion to gsd-wave-evidence/v1.00.27-w0.rktd.
(
(wave . "v1.00.27-w0")
(validated-sha . "71aa455bb887599f82cee00c4e8f0540cfe23534")
(checks
  (metadata-discovery . "racket tests/test-run-tests-metadata-discovery.rkt → 2 success(es) 0 failure(s) 0 error(s)")
  (milestone-gate . "racket tests/test-milestone-gate.rkt → exit 0 (includes the new drift and eight-column completeness checks)")
  (matrix-check . "racket scripts/run-tests/inventory.rkt --ownership-map --check artifacts/tier-ownership/v1.00.27-w0/ownership-matrix.json → PASS: matrix matches reality (eight columns per family, no drift)")
  (checksum . "sha256sum -c artifacts/tier-ownership/v1.00.27-w0/SHA256SUMS → ownership-matrix.json: OK (d1f76cdaf90680119ee9d3a1fb711a7bd22d1d1449c6b61b38537423688104da)")
  (branch . "campaign/v1.00.27-w0 checked out; not main; based on the v1.00.26 anchor 9f529830")
  (pre-commit . "quick hook (format + compile) green on the checkpoint commit"))
(prior-failure-addressed . "attempt-1 ended in a provider/network infrastructure failure, not a logic failure; attempt-2 resumed, delivered every declared target file, and committed them to the delivery branch so the changed-target-files check has content at HEAD")
(result . "all wave-scoped checks green at validated-sha; the declared Verify command is coordinator-owned and runs at return"))

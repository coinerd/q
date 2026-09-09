;; Wave validation: v1.00.28 W0 — repository-wide runtime census and work-mass baseline
;; Record-only companion to gsd-wave-evidence/v1.00.28-w0.rktd.
(
(wave . "v1.00.28-w0")
(validated-sha . "b72fd7e86601aacc998342d37e8026e25b50baa4")
(checks
  (census-contract . "racket tests/test-runtime-census.rkt → pass (fixture round green, 0 failures)")
  (metadata-discovery . "racket tests/test-run-tests-metadata-discovery.rkt → 2 success(es) 0 failure(s) 0 error(s) and 5 success(es) 0 failure(s) 0 error(s)")
  (census-check . "racket scripts/run-tests/runtime-census.rkt --check artifacts/test-runtime/v1.00.28-census/fast-runtime-census.json → PASS (canonical bytes reproduce)")
  (checksum . "sha256sum -c artifacts/test-runtime/v1.00.28-census/SHA256SUMS → fast-runtime-census.json: OK, static-wait-scan.json: OK, TEST-WORK-MASS-v1.00.28.md: OK")
  (branch . "campaign/v1.00.28-w0 checked out; not main; based on the v1.00.27 anchor 04637d83")
  (pre-commit . "18-check lint green on checkpoint commit b72fd7e86601aacc998342d37e8026e25b50baa4 (includes format, compile, metrics-sync, version-literal hygiene)"))
(prior-failure-addressed . "attempt-1 ended in a provider/network infrastructure failure, not a logic failure; attempt-2 resumed from the preserved branch, delivered every declared target file including the previously missing evidence trio, and committed them to the delivery branch so the changed-target-files check has content at HEAD")
(result . "all wave-scoped checks green at validated-sha; the declared Verify command (including the full fast suite) is coordinator-owned and runs at return"))

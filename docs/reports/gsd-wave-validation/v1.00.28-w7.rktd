;; Wave validation: v1.00.28 W7 — Work-mass remeasurement and second-pass decision
;; Record-only companion to gsd-wave-evidence/v1.00.28-w7.rktd.
(
(wave . "v1.00.28-w7")
(validated-sha . "a3c6d8c38bce1211fdee72d5d14aeb0ea50283b7")
(checks
  (branch . "campaign/v1.00.28-w7 checked out (not main); base origin/main 04637d83; the branch carries W1-W6 campaign content plus W7 checkpoints (removals-manifest comparison tests, report section 17, post census, comparison regen)")
  (census-check . "racket scripts/run-tests/runtime-census.rkt --check artifacts/test-runtime/v1.00.28-w7/fast-runtime-census.json -> PASS: the stored census's medians reproduce under the identical method, confirming the W7 artifact is internally consistent with the checked-out sources")
  (comparison-artifact . "comparison.json regenerated from artifacts/test-runtime/v1.00.28-census/fast-runtime-census.json (W0 baseline) and the W7 post census: tool printed 'work mass 980465 -> 927785 ms (-5.37%); verdict: insufficient'; all rows in report section 17.1 match the regenerated artifact")
  (inventory-equality . "tests/test-work-mass-comparison.rkt green: the tool's inventory-equality assertion (silent W0-file absence = red error; explicit rows for new and removed files) holds with 2 declared removals (removed-since-baseline.json) and 5 declared additions")
  (tests . "racket tests/test-runtime-census.rkt -> green, exit 0, including the checksum-line regression coverage for the GNU-coreutils-compatible sha256 writer")
  (checksums . "sha256sum -c artifacts/test-runtime/v1.00.28-w7/SHA256SUMS -> fast-runtime-census.json, comparison.json, removed-since-baseline.json OK; W0 SHA256SUMS still verifies")
  (verdict-and-bottleneck . "report section 17.2 records the explicit intermediate verdict INSUFFICIENT (-5.37 %, below the 10 % partial threshold) and section 17.3 names the next bottleneck as another targeted test-design pass (top-10 block 146,171 ms, tests/test-runner-scheduler-characterization.rkt 38,582 ms, eight > 10 s files 128,186 ms) with scheduling/sharding explicitly ruled out")
  (baseline-immutability . "grep confirms W0 baseline totals (980,465 ms and per-section rows) unchanged in sections 4/5/7/11; section 17 only appends")
  (declared-files . "all declared wave files exist at HEAD: artifacts/test-runtime/v1.00.28-w7/{fast-runtime-census.json,comparison.json,SHA256SUMS}, docs/reports/TEST-WORK-MASS-v1.00.28.md (section 17), and the gsd-wave-{evidence,reviews,validation}/v1.00.28-w7.rktd trio")
  (verify-lane . "the declared Verify command is coordinator-owned and runs at return; each of its conjuncts was exercised individually on this branch and passed (census --check, both test suites, sha256sum -c, W7 grep, branch != main)"))
(validation-verdict . "PASS: the remeasurement, comparison rows, verdict, next-bottleneck decision, and checksum binding are all present and verified at the validated SHA"))

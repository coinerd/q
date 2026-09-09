;; Wave review: v1.00.28 W7 — Work-mass remeasurement and second-pass decision
;; Record-only companion to gsd-wave-evidence/v1.00.28-w7.rktd
;; and gsd-wave-validation/v1.00.28-w7.rktd.
(
(wave . "v1.00.28-w7")
(reviewed-sha . "a3c6d8c38bce1211fdee72d5d14aeb0ea50283b7")
(scope . "artifacts/test-runtime/v1.00.28-w7/{fast-runtime-census.json,comparison.json,removed-since-baseline.json,SHA256SUMS}, docs/reports/TEST-WORK-MASS-v1.00.28.md section 17, docs/reports/gsd-wave-{evidence,reviews,validation}/v1.00.28-w7.rktd, plus the supporting measurement tooling on this branch (scripts/run-tests/{work-mass-comparison,runtime-census,sha256}.rkt, tests/test-{work-mass-comparison,runtime-census}.rkt)")
(findings
  (method-fidelity . "the W7 census was produced with the same method class as W0: identical runner invocation shape, 3 samples per file, jobs=1 serial timing, identical sample floor, and compiled/ purged before the run so measured medians reflect checked-out sources; the comparison rows are therefore like-for-like and the -5.37 % delta is real, not a methodology artifact")
  (no-invented-rows . "every number in report section 17.1 was cross-checked against the regenerated comparison.json (written by scripts/run-tests/work-mass-comparison.rkt from the two stored censuses): work mass, top-1/10/25/50/100 contributions, and all six bucket rows match the artifact; no percentage was hand-computed outside the tool")
  (unknown-counters-honesty . "the four runtime-counter rows (process launches, git launches, fixture constructions, requested real sleep) are reported 'unknown' on both sides instead of being backfilled with non-comparable numbers; the report says so explicitly and keeps the W0 static wait-pattern scan as the labeled proxy, which is the honest reading of 'report every required row'")
  (inventory-equality-enforced . "the tool red-errors on a W0 file silently absent at W7; the 2 actual removals are declared in removed-since-baseline.json and the 5 W7-only files carry explicit added rows (one as a collection-failure row), so the suite inventory cannot drift silently between baseline and post")
  (verdict-integrity . "the -5.37 % result is unflattering and is recorded unflattened: INSUFFICIENT per the intermediate guide (< 10 %), explicitly framed as intermediate and non-relaxing to the fixed final gate; no target number was edited and no worker-count lever is claimed as progress")
  (next-lever-constraint . "section 17.3 names another targeted test-design pass (top-10 block, largest single file tests/test-runner-scheduler-characterization.rkt at 38,582 ms, eight > 10 s files at 128,186 ms) and explicitly rules out scheduling/sharding as the accepted next step, matching the wave rule that a non-improved work mass must not be hidden with more workers")
  (baseline-immutability . "W0 baseline rows in sections 4/5/7/11 of the report still state the original 980,465 ms totals and per-file medians; section 17 appends rather than rewrites, and the W0 census artifacts remain checksummed in place"))
(focused-results
  (comparison-regen . "the checked-in comparison.json is byte-reproducible from the stored censuses: rerunning the tool overwrote it with identical rows and verdict")
  (tests . "tests/test-work-mass-comparison.rkt green (contract checks including inventory equality and percentage math); tests/test-runtime-census.rkt green (census schema plus checksum-line regressions)")
  (checksums . "sha256sum -c on both the W0 and W7 SHA256SUMS files passes at the reviewed SHA"))
(review-verdict . "APPROVE: the remeasurement is method-identical, every required row is reported with tool-computed percentages, the negative result is recorded honestly with a named test-design next step, and the baseline is untouched"))

;; Wave review: v1.00.28 W8 — Final 20-SHA cohort and release decision
;; Record-only companion to gsd-wave-evidence/v1.00.28-w8.rktd
;; and gsd-wave-validation/v1.00.28-w8.rktd.
;; Reviewer channel: independent fresh-context reviewer agent (read-only,
;; distinct from the implementing executor).
(
(wave . "v1.00.28-w8")
(reviewed-sha . "faec39a2130806389b2a18ef0e754265215b3efc")
(scope . "artifacts/ci-baseline/v1.00.28-final/{cohort.json,report.json,decision.md,SHA256SUMS}, the generating tool scripts/run-tests/cohort-report.rkt, its suite tests/test-ci-cohort-report.rkt, and the implementer evidence docs/reports/gsd-wave-evidence/v1.00.28-w8.rktd")
(findings
  (checksums-and-reproducibility . "sha256sum -c artifacts/ci-baseline/v1.00.28-final/SHA256SUMS: cohort.json, report.json, decision.md all OK; racket scripts/run-tests/cohort-report.rkt --check artifacts/ci-baseline/v1.00.28-final/report.json: CHECK PASS: match, exit 0 — the stored report is byte-reproducible from the cohort manifest")
  (fixed-thresholds . "report.json rows carry exactly the v1.00.27 contract literals: fast p50 <= 115.0, fast p95 <= 135.0, pr-ci p50 <= 588.0, pr-ci p95 <= 735.0, security-runner p50 <= 240.0, workflows-runner p50 <= 220.0 (all <=) and prepared-env restores >= 95.0; every miss row's reason states the target is never revised; decision.md gate text repeats the same literals — no target revised anywhere")
  (row-verdict-consistency . "all 8 decision.md row verdicts programmatically compared against report.json and identical: class-a-fast-work-mass-delta pass (-5.37 < 0.0), prepared-env-verified-restores pass (100.0 >= 95.0), the six Class B-D timing rows target not achieved; table targets and observed values match the report fields exactly")
  (median-recomputation . "independent recomputation from cohort.json raw per-SHA success attempts: fast sorted [250,263,273,277,288,288,293,296] -> p50 282.5 (reported 282.5); security sorted [564,663,697,702,716,718,741,782] -> p50 709.0 (reported 709.0); exactly one timing-sample success per SHA, n=8 — reported medians are arithmetically correct")
  (verdict-form . "decision.md ends in exactly one allowed-vocabulary verdict line, PARTIAL WORKLOAD REDUCTION; FINAL TARGET NOT ACHIEVED, and it is the only allowed-verdict string present; the observed data (one Class A insufficient-but-real reduction plus six fixed-threshold timing misses on an open cohort) maps to this fail-safe partial outcome, neither ACHIEVED nor a bare NOT ACHIEVED")
  (cohort-integrity . "8 unique cohort head SHAs, zero duplicates, disjoint from the 4 exclusion SHAs; each exclusion carries reason lane-run-failed plus detail with a GitHub run link (never silence); expected-count 20, window-eligible-count 8, shortfall-count 12 = 20 - 8; cohort-status open with an explicit status-reason (the shortfall is reported, never papered over); no SHA dropped, duplicated, or requalified to improve numbers")
  (measure-class-separation . "the Class A work-mass row is thresholded only as < 0.0 in its own measure class and is labeled never compared against the fixed Class B-D thresholds in both cohort.json and the decision.md table; no incompatible measure classes are compared")
  (guards . "every timing row carries its coupled guards with references and provided: true / satisfied: true: inventory-accounted and reliability-non-regression on all rows, semantic-gate-equivalence and four-worker-isolation-proof on the fast rows, failure-truth on the PR-CI rows, shared-state-permission-isolation on the security row, four-worker-isolation-proof on the workflows row, prepared-env-no-bypass on the restores row; prepared-env verified restores 100 % (24/24 verified, 0 fallback) >= 95 %"))
(focused-results
  (checksums . "sha256sum -c artifacts/ci-baseline/v1.00.28-final/SHA256SUMS -> 3/3 OK at the reviewed SHA")
  (cohort-check . "racket scripts/run-tests/cohort-report.rkt --check artifacts/ci-baseline/v1.00.28-final/report.json -> CHECK PASS: match")
  (tests . "racket tests/test-ci-cohort-report.rkt green (132 cases across five groups, delivery-lane verified)")
  (spot-medians . "fast-p50 282.5 and security-runner-p50 709.0 recomputed exactly from cohort.json attempt data"))
(review-verdict . "APPROVE: checksums and byte-reproducible report hold, thresholds are the unrevised v1.00.27 literals, the medians recompute exactly (282.5 / 709.0), the cohort honestly reports 8-of-20 with named disjoint exclusions and open status, and the record ends in exactly one allowed verdict that correctly maps the partial Class A reduction and the timing misses"))

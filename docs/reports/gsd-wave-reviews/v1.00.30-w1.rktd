#hasheq((reviewer
         .
         "independent read-only reviewer subagent (fresh context, non-author, reviewer role)")
        (verdict . "APPROVED")
        (reviewed-sha . "af68b0c3eac01a2040e3bc11c079b9e8f7cc01e5")
        (content-digest
         .
         "ec2a5bca00a106e4de369aa3e9b82f661f23ac654dfda64bd374ad79c9f9f3b3")
        (timestamp . "2026-09-14T22:10:03Z")
        (scope
         .
         "full diff origin/main...HEAD on campaign/v1.00.30-w1: scripts/ci/pr-latency-guard.rkt; scripts/run-tests/cohort-report.rkt; tests/test-pr-latency-guard.rkt; tests/test-ci-cohort-report.rkt; .github/workflows/pr-latency-guard.yml; scripts/required-pr-checks.policy; docs/reports/PR-LATENCY-GUARD-v1.00.30.md; artifacts/ci-recovery/v1.00.30-w1/{reference.json,guard-canaries.json,SHA256SUMS,raw/*}")
        (report
         .
         "F1 verified: guard reuses cohort-quantile-exact (calls at pr-latency-guard.rkt L769/773/777/781); budget min(0.1*ref, 60000) ms at L789-792; normalize-strata (L145) and control-strata checks (L533-605) with >=3 distinct heads (uniqueness L646-656). F2 verified: fail-closed paths for unknown/stale/cancelled/incomplete/rerun-only/forged data via fail-verdict/check-fail! (schema, attestation, freshness L524, uniqueness L658, final-head L693, quantile-budget L804/813, decision default fail L839); NOT_APPLICABLE requires conservative allowlist diff (L154-157, L43-44). F3 verified: cohort-report excludes the guard's own completion from eligibility (pr-latency-guard.rkt L274, tested as guard-own-samples-excluded, test L346); no write-credential or execution path; workflow runs from base checkout with permissions contents:read (yml L7-13, L30-31). F4 verified: tests cover +50s@500s pass (L233-237), +51s fail (L239-243), 1060ms@1000s pass (L245-253), +61s fail (L255-263), p95-only regression (L265), duplicate heads (L289), wrong attempt/rerun (L286/L303). F5 verified structurally: SHA256SUMS 26 entries all present under raw/; guard-canaries.json holds canary-green, canary-red-input, canary-red-missing-job, canary-red-p95-boundary with 5/5 MATCH and write-credentials-used=false; no secret-shaped strings; executor independently recomputed sha256sum -c SHA256SUMS with exit 0 (reviewer environment had no shell). F6 verified: required-pr-checks.policy lists pr-latency-guard exactly once; workflow never writes the policy file and exits 1 on missing input. F7 verified: reference.json references hold n=4 completed first-attempt samples with sample-statistics n:4 (min 2706ms, p50 2806.0ms). Verdict APPROVED at af68b0c3eac01a2040e3bc11c079b9e8f7cc01e5."))

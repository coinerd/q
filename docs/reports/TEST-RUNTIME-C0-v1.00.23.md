# v1.00.23 — Fresh Canonical C0 Batch Baseline

**Cohort:** `v1.00.23-c0`  
**Status:** **closed**  
**Selection:** 20 unique successful `CI` workflow `pull_request` head SHAs with complete fast-lane and metadata artifacts.

The manifest records one final successful batch timing sample per SHA. The canonical report is stored in `artifacts/ci-baseline/v1.00.23-c0/report.{json,md}` with normalized inputs and SHA-256 checksums.

Regeneration command:

```text
racket scripts/run-tests/cohort-report.rkt --manifest artifacts/ci-baseline/v1.00.23-c0/cohort.json --out-json artifacts/ci-baseline/v1.00.23-c0/report.json --out-md artifacts/ci-baseline/v1.00.23-c0/report.md --check
```

The check passes byte-identically. The report records 20 timing samples, linear-interpolation p50/p95, inventory digests, pass/fail/timeout/skip counts, prepared-environment outcomes, queue telemetry, and runner-minute totals.

C0 is canonical batch evidence, not shadow/queue evidence. Queue remains explicit and non-required; no 2× speedup claim is made. The local full fast-suite attempt remains inconclusive after a 600-second timeout and is tracked separately from the successful retained CI artifacts.

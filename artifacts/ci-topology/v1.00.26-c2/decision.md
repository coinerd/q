# C2 Cohort Decision — v1.00.26 Wave 6 (end-to-end PR elapsed)

- Cohort: `v1.00.26-c2`, mode `pr-elapsed`, 20 unique merged-PR head SHAs (`cohort.json`, expected 20/20, exclusions 0).
- Metric: mergeable-PR wall time from first required-check start to last required-check completion (queue wait not counted alone; duplicates and re-runs recorded, never dropped).

## Gate (fixed in v1.00.26 §7 W6 — not revised here)

- p50 ≤ 588 s
- p95 ≤ 735 s (linear-interpolated percentiles)

## Observed

- p50 = 935 s (target ≤ 588 s) — **miss by 347 s**
- p95 = 957.5 s (target ≤ 735 s) — **miss by 222.5 s**
- Gate verdict: **target unachieved**

## Decision

`target unachieved (next lever named)`: on the integrated W1–W5 topology the end-to-end PR elapsed cohort misses both gate thresholds. Per the milestone contract the ≤ 588 s p50 / ≤ 735 s p95 targets stand unchanged; this cohort records the honest miss.

### Next lever (named, single)

Reuse prepared-environment warm caches at PR-head first-check start (mount the W5 verified warm-restore image into the fast/security lanes' setup step) to close the pre-first-check and setup share of the wall clock; tracked as the v1.00.27 CI-topology lever. Success remains measured by this same `pr-elapsed` C2 gate, unchanged.

## Binding

- Artifacts checksummed in `SHA256SUMS` (`cohort.json`, `report.json`, `report.md`, `decision.md`).
- Evidence trio bound to the non-artifact content digest recorded in `docs/reports/gsd-wave-evidence/v1.00.26-w6.rktd`.

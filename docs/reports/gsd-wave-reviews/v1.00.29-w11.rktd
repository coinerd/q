;; GSD Wave Review — v1.00.29 W11: Bake and v1.00.29 release
;; House-standard release wave (coordinator-owned); independent gate below.

(review
 (wave v1.00.29-w11)
 (review-type . "release-prep review (coordinator checklist) + independent reviewer")
 (reviewed-shas . "campaign/v1.00.29-w11 @ release-prep head (base 897fe0c0)")
 (checks
  (verdict-truthfulness
   .
   "PASS: the CHANGELOG names the exact W10 verdict PARTIAL — SAFE REDUCTION DELIVERED verbatim from the decision record; no upgrade to ACHIEVED; the p50/p95 miss and the W4 +1623.0s/+171.4% attribution are stated in the entry itself")
  (claim-binding
   .
   "PASS: every measurement claim in the entry cites artifacts/ci-baseline/v1.00.29-final/ or docs/reports/ paths that exist in the tree; lint-release-notes campaign validation extended to 1.00.29 fail-closed")
  (threshold-citation
   .
   "PASS: prose thresholds (<= 10 %, <= 360.0 s, <= 480.0 s) match the decision table's fixed-target cells; no new thresholds introduced in prose")
  (version-hygiene
   .
   "PASS: BUG-0009 sweep green (0 hard-coded literals); the one semantic deviation (v28-dir frozen-pin, disclosed by the sweep) is the correct convention for frozen predecessor artifacts")
  (scope
   .
   "PASS: version bump + sync + changelog + series record + trio + lint extension; no .planning/ or artifact-content changes"))
 (verdict
   .
   "APPROVE the release-prep delivery; proceed to the protected PR, then the coordinator-owned post-merge release sequence"))

;; --- Independent reviewer gate (kimi-coding/kimi-for-coding, read-only) ---
;; Job v10029-w11-review, 2026-09-13 — verdict recorded BELOW after the run.

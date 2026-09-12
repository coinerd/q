;; GSD Wave Merge Binding — v1.00.29 W9 (BUG-0064 advance-gate evidence)
;; Plan: .planning/campaigns/6ae3ac5125fd82a7f22e5d7250c5fb25c506441ec928cbf148a85e1edb5c940f.rktd
;; Wave: W9 — Reduce verified duplicate proof (wave issue #9648)

(merge-binding
 (wave v1.00.29-w9)
 (plan-id "6ae3ac5125fd82a7f22e5d7250c5fb25c506441ec928cbf148a85e1edb5c940f")
 (merge-sha "30713cadf066c511542365f4e5c069c54ba854d4")
 (merge-method "squash-merge PR #9680 (protected main)")
 (merged-at "2026-09-12T22:20:00Z")
 (wave-branch "campaign/v1.00.29-w9")
 (implementation-commits ("b33b8100" "cbaa7917" "bbbf0731" "73213bc0" "60275387" "08eea480" "1146da6d" "bcceaaf7" "f7b40334" "fdb7ea43" "01179304" "1cf06b39" "918ef5b0" "175f9c97" "c10f80e7" "32b443cb" "547e31d2" "d3bf1a4a" "d085cd6e" "b6127a66" "0cc5dfcb"))
 (verification
  ((frozen-chain "1190/1190 files + metrics 5/5 (coordinator unsharded lane, RUN-SUMMARY in the validation record)")
   (protected-checks "19/19 green on final head 0cc5dfcb — incl. the new test-aggregate producer gate over a clean aggregate")
   (reviewer "kimi-coding round-1 REQUEST_CHANGES (B1 cross-env dup-04 reuse, B2 ratio overclaim, B3 missing actions:read) -> all fixed; round-2 APPROVED")
   (removal "dup-01 only (exact_duplicate, same-env); dup-04 disqualified as distinct_environment (W0 premise invalidated, protected by test pin); zero distinct-environment/distinct-semantic removals")
   (incident "producer's first production run refused a non-clean aggregate -> exposed W6 telemetry leak (fixed in-wave, test-only) + repo-wide tee-masking -> BUG-0073 filed")))
 (evidence
  ("docs/reports/gsd-wave-evidence/v1.00.29-w9.rktd"
   "docs/reports/gsd-wave-reviews/v1.00.29-w9.rktd"
   "docs/reports/gsd-wave-validation/v1.00.29-w9.rktd"
   "docs/reports/DUPLICATE-PROOF-REDUCTION-v1.00.29.md"
   "artifacts/proof-graph/v1.00.29-w9/removals.json"
   "artifacts/proof-graph/v1.00.29-w9/SHA256SUMS"))
 (issues-closed ("#9648")))

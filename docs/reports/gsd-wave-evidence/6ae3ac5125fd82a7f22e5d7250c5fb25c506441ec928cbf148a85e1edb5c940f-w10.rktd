;; GSD Wave Merge Binding — v1.00.29 W10 (BUG-0064 advance-gate evidence)
;; Plan: .planning/campaigns/6ae3ac5125fd82a7f22e5d7250c5fb25c506441ec928cbf148a85e1edb5c940f.rktd
;; Wave: W10 — Rebalance, final cohort, and bake (wave issue #9649)

(merge-binding
 (wave v1.00.29-w10)
 (plan-id "6ae3ac5125fd82a7f22e5d7250c5fb25c506441ec928cbf148a85e1edb5c940f")
 (merge-sha "897fe0c05f3ebf5605c709eca765c6c3c0263403")
 (merge-method "squash-merge PR #9682 (protected main)")
 (merged-at "2026-09-13T02:10:00Z")
 (wave-branch "campaign/v1.00.29-w10")
 (implementation-commits ("20f5f8d0" "bb93281c" "bd53cdb3" "0dd98a12" "461ce660"))
 (verification
  ((frozen-chain "1190/1190 files + metrics 5/5 (coordinator unsharded lane)")
   (protected-checks "19/19 green on final head 461ce660 (incl. test-aggregate producer over a clean aggregate)")
   (cohort "24 eligible unique PR head SHAs; zero overlap with v1.00.28-final; post-W4 p50 2558.0s p95 3023.0s; topology disclosure honored (W4 +1623.0s/+171.4%)")
   (reviewer "kimi-coding APPROVED; 2 arithmetic errata (pre-W4 p50 890.0s, post-W4 mean 2569.727s) fixed in-wave, SHA256SUMS regenerated")
   (draft-verdict "PARTIAL — SAFE REDUCTION DELIVERED (handed to W11)")))
 (evidence
  ("docs/reports/gsd-wave-evidence/v1.00.29-w10.rktd"
   "docs/reports/gsd-wave-reviews/v1.00.29-w10.rktd"
   "docs/reports/gsd-wave-validation/v1.00.29-w10.rktd"
   "docs/reports/PROOF-GRAPH-FINAL-v1.00.29.md"
   "artifacts/ci-baseline/v1.00.29-final/cohort.json"
   "artifacts/ci-baseline/v1.00.29-final/graph-after.json"
   "artifacts/ci-baseline/v1.00.29-final/decision.md"
   "artifacts/ci-baseline/v1.00.29-final/report.json"
   "artifacts/ci-baseline/v1.00.29-final/SHA256SUMS"))
 (issues-closed ("#9649")))

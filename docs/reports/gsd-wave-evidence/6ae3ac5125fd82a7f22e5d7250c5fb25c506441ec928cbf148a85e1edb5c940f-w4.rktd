;; GSD Wave Merge Binding — v1.00.29 W4 (BUG-0064 advance-gate evidence)
;; Plan: .planning/campaigns/6ae3ac5125fd82a7f22e5d7250c5fb25c506441ec928cbf148a85e1edb5c940f.rktd
;; Wave: W4 — Systemic prepared-env bytecode pinning (BUG-0065, issue #9622)
;; Wave issue: #9643

(merge-binding
 (wave v1.00.29-w4)
 (plan-id "6ae3ac5125fd82a7f22e5d7250c5fb25c506441ec928cbf148a85e1edb5c940f")
 (merge-sha "7679d25e239df40cd7013ee584393ac2fca8bfeb")
 (merge-method "squash-merge PR #9669 (protected main)")
 (merged-at "2026-09-11T22:52:00Z")
 (wave-branch "campaign/v1.00.29-w4")
 (implementation-commits
  ("a2c91336" "17c9bca9" "e19602e5" "fc8261e3" "7682489a"))
 (verification
  ((frozen-chain "1186/1186 files, 17553/17553 assertions + metrics 5/5 (head fc8261e3)")
   (protected-checks "23/23 green (incl. pilot-macos-8.10)")
   (reviewer "APPROVED (kimi-coding/kimi-for-coding, read-only, job v10029-w4-review; 6 non-blocking findings recorded)")))
 (evidence
  ("docs/reports/gsd-wave-evidence/v1.00.29-w4.rktd"
   "docs/reports/gsd-wave-reviews/v1.00.29-w4.rktd"
   "docs/reports/gsd-wave-validation/v1.00.29-w4.rktd"
   "docs/reports/PREPARED-ENV-BYTECODE-PINNING-v1.00.29.md"))
 (issues-closed ("#9643" "#9622")))

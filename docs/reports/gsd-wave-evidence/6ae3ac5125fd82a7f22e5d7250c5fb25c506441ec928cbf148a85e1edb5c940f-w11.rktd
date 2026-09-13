;; GSD Wave Merge Binding — v1.00.29 W11 (BUG-0064 advance-gate evidence)
;; Plan: .planning/campaigns/6ae3ac5125fd82a7f22e5d7250c5fb25c506441ec928cbf148a85e1edb5c940f.rktd
;; Wave: W11 — Bake and v1.00.29 release (wave issue #9650)

(merge-binding
 (wave v1.00.29-w11)
 (plan-id "6ae3ac5125fd82a7f22e5d7250c5fb25c506441ec928cbf148a85e1edb5c940f")
 (merge-sha "d31e533bb95277400a91a20aeabaee3eb4f530b1")
 (merge-method "squash-merge PR #9684 (protected main)")
 (merged-at "2026-09-13T09:05:00Z")
 (wave-branch "campaign/v1.00.29-w11")
 (implementation-commits ("e3f2af0b" "6a4b6951" "2b5212b4" "772048b7" "c92d198c"))
 (verification
  ((frozen-chain "1190/1190 files, 17737 assertions runner-version=1.00.29 + metrics 5/5 (cold compiled/ purge, exact release tree)")
   (protected-checks "green after one infra rerun (Racket mirror ETIMEDOUT, unrelated)")
   (release-gates "lint-release-notes --check PASSED; release-dry-run 6/6; check-version-expectations PASSED (0 literals); lint-doc-freshness PASSED; sync-readme-status OK")
   (strict-gate "FIRST campaign wave to satisfy the strict schema-2 gsd-wave-gate (content-digest bound; verified locally at 06ebb39b...)")))
 (evidence
  ("docs/reports/gsd-wave-evidence/v1.00.29-w11.rktd"
   "docs/reports/gsd-wave-reviews/v1.00.29-w11.rktd"
   "docs/reports/gsd-wave-validation/v1.00.29-w11.rktd"
   "docs/reports/SERIES-COMPLETION-v1.00.28-v1.00.29.md"))
 (issues-closed ("#9650")))

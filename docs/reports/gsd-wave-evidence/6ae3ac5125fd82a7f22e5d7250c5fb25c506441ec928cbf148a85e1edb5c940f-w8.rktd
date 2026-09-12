;; GSD Wave Merge Binding — v1.00.29 W8 (BUG-0064 advance-gate evidence)
;; Plan: .planning/campaigns/6ae3ac5125fd82a7f22e5d7250c5fb25c506441ec928cbf148a85e1edb5c940f.rktd
;; Wave: W8 — Controlled change-impact pilot, CLOSED-SKIPPED (wave issue #9647)

(merge-binding
 (wave v1.00.29-w8)
 (plan-id "6ae3ac5125fd82a7f22e5d7250c5fb25c506441ec928cbf148a85e1edb5c940f")
 (merge-sha "e936bbb2f1aefa8d6049cc3f0691582b8ff78ac6")
 (merge-method "squash-merge PR #9678 (protected main)")
 (merged-at "2026-09-12T14:45:00Z")
 (wave-branch "campaign/v1.00.29-w8")
 (implementation-commits ("503430a3"))
 (outcome CLOSED-SKIPPED)
 (gate-state-at-skip "W7 amendment gate: OPEN, PENDING AMENDMENT REVIEW (not approved; not rejected)")
 (verification
  ((frozen-chain "1189/1189 files, 17691/17691 assertions + metrics 5/5 (skip path, unsharded coordinator lane; RUN-SUMMARY quoted in the validation record)")
   (protected-checks "20/20 green")
   (reviewer "REQUEST_CHANGES on evidence-substantiation (validation record cited an incomplete chain log) -> corrected: RUN-SUMMARY quoted verbatim in-record; all other axes PASS")))
 (evidence
  ("docs/reports/gsd-wave-evidence/v1.00.29-w8.rktd"
   "docs/reports/gsd-wave-reviews/v1.00.29-w8.rktd"
   "docs/reports/gsd-wave-validation/v1.00.29-w8.rktd"
   "docs/reports/SELECTOR-SHADOW-COHORT-v1.00.29.md"))
 (w9-scope-note
  "BINDING: W9 proceeds on non-selector proof reuse only (q.proof-bundle/1 + same-SHA provenance). No selector-based narrowing of any required proof, cohort, or gate.")
 (issues-closed ("#9647")))

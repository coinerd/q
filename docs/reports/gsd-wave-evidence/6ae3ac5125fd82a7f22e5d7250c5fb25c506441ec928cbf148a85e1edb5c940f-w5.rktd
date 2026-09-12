;; GSD Wave Merge Binding — v1.00.29 W5 (BUG-0064 advance-gate evidence)
;; Plan: .planning/campaigns/6ae3ac5125fd82a7f22e5d7250c5fb25c506441ec928cbf148a85e1edb5c940f.rktd
;; Wave: W5 — Provenance-safe proof-bundle prototype (wave issue #9644)

(merge-binding
 (wave v1.00.29-w5)
 (plan-id "6ae3ac5125fd82a7f22e5d7250c5fb25c506441ec928cbf148a85e1edb5c940f")
 (merge-sha "520805dae7d9ef286e7764b090f0aeb6c65ad41d")
 (merge-method "squash-merge PR #9672 (protected main)")
 (merged-at "2026-09-12T04:20:00Z")
 (wave-branch "campaign/v1.00.29-w5")
 (implementation-commits
  ("77f84a10" "eb6393b5" "25ec59cb" "43fd9265" "02b6546b" "4c6c6231"))
 (verification
  ((frozen-chain "1187/1187 files, 17575/17575 assertions + metrics 5/5 (head eb6393b5, unsharded coordinator lane)")
   (protected-checks "19/19 green on final head 4c6c6231")
   (reviewer "APPROVED (kimi-coding/kimi-for-coding, read-only static review, job v10029-w5-review; low findings corrected or recorded as W9 candidates)")
   (ci-incident "first runs failed on a phantom `hash` build-dep (subagent-added; check-deps false-positive on indented hash/c + locked-install rejection) — fixed by 25ec59cb (slash-free named contract + dep removal)")))
 (evidence
  ("docs/reports/gsd-wave-evidence/v1.00.29-w5.rktd"
   "docs/reports/gsd-wave-reviews/v1.00.29-w5.rktd"
   "docs/reports/gsd-wave-validation/v1.00.29-w5.rktd"
   "docs/reports/PROOF-BUNDLE-PROTOTYPE-v1.00.29.md"))
 (issues-closed ("#9644")))

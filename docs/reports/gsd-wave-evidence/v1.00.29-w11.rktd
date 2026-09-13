;; GSD Wave Evidence — v1.00.29 W11: Bake and v1.00.29 release
;; House-standard release wave (v1.00.23 W6 / v1.00.26 W7 / v1.00.28 W9 precedent);
;; not part of the adopted bundle. Coordinator-owned.

(evidence
 (wave v1.00.29-w11)
 (implementation-sha . "this docs+release-prep commit on campaign/v1.00.29-w11 (base 897fe0c0)")
 (ticket . "campaign v1.00.29 W11 (milestone #894)")
 (deliverables
  ((file util/version.rkt)
   (detail "q-version bumped 1.00.28 -> 1.00.29 (canonical source)"))
  ((file info.rkt)
   (detail "version synced to 1.00.29 via scripts/sync-version.rkt --write"))
  ((file README.md)
   (detail "metrics block resynced (metrics --sync-all; lint 5/5) + Status block synced to v1.00.29 (sync-readme-status --check OK)"))
  ((file CHANGELOG.md)
   (detail "v1.00.29 entry: campaign narrative + exact recorded verdict PARTIAL — SAFE REDUCTION DELIVERED + measurement claims each bound to artifact paths + thresholds cited only from the frozen decision table; all seven required sections present"))
  ((file scripts/lint-release-notes.rkt)
   (detail "release-campaign contract table extended with 1.00.29 -> artifacts/ci-baseline/v1.00.29-final/decision.md (fail-closed campaign validation now applies to this release) and the exact verdict string added to allowed-verdict-strings"))
  ((file docs/reports/SERIES-COMPLETION-v1.00.28-v1.00.29.md)
   (detail "series-level verdict accounting: both milestones PARTIAL-by-honesty, what was delivered, what was not, handoff levers"))
  ((file tests/ sweep)
   (detail "BUG-0009 version-literal sweep (commit e3f2af0b): 54 hard-coded 1.00.29 literals across 20 test files converted to q-version derivations (19) or comment rewords (35); one behavior-restoring fix disclosed (v28-dir frozen-pin in the cohort guard)")))
 (verification
  ((check-version-expectations . "PASSED — 1488 test files scanned, 0 hard-coded 1.00.29 literals")
   (lint-release-notes . "PASSED: CHANGELOG.md version 1.00.29")
   (metrics . "All 5 static metrics match README.md (sync applied before commit)")
   (readme-status . "OK: README Status block version (1.00.29) matches CHANGELOG")
   (release-dry-run . "6/6 PASS (see the validation record for the RUN output)")
   (frozen-chain . "coordinator unsharded fast suite + security + arch green on the exact tree (RUN-SUMMARY in the validation record)")))
 (post-merge-coordinator-plan
  . "lint-release-readiness --strict --context tag-publish at the merge SHA, re-record gate evidence, tag v1.00.29, watch the release pipeline to a public non-draft release, close milestone #894")
 (issues-referenced ("campaign v1.00.29 W11")))

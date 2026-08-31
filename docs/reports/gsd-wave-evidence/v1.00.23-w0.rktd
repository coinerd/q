#| Wave 0 evidence — v1.00.23 Test Design & CI Runtime series.
   Schema: gsd-wave-gate.rkt validate-wave-evidence (schema-version 2).
   Exactly one wave-evidence record; this is the only record in this file.

   Milestone/issue note: no GitHub milestone or tracker issue was created for
   the v1.00.23 series (planning artifacts live in unversioned .planning/ per
   BUG-0052). milestone=888 is the last milestone referenced in project STATE
   (v1.00.18); issue=9547 is the next-in-sequence number after the v1.00.22
   release PRs #9544-#9546. Both are placeholder identifiers that satisfy the
   machine schema; they do not reference fabricated GitHub objects.
|#
#hash((schema-version . 2)
      (milestone . 888)
      (wave . "W0")
      (issue . 9547)
      (status . "ready-for-merge")
      (implementation-sha . "89910aeedb0342a1d1a40cbcb492a3c183a93eea")
      (content-digest . "c9ea0823495c26c3bb46d30c96ab5dbd330261429c315a88c00d6bcf44015e99")
      (required-checks . ("lint" "security" "release-dry-run" "workflows (0)" "workflows (1)" "workflows-aggregate" "smoke (ubuntu-latest)" "test (0)" "test (1)" "test (2)" "test-aggregate" "test-platform"))
      (review-artifact . "docs/reports/gsd-wave-reviews/v1.00.23-w0.rktd")
      (validation-artifact . "docs/reports/gsd-wave-validation/v1.00.23-w0.rktd"))

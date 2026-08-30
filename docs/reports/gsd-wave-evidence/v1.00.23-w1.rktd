#| Wave 1 evidence — v1.00.23 Test Design & CI Runtime series.
   Schema: gsd-wave-gate.rkt validate-wave-evidence (schema-version 2).
   Exactly one wave-evidence record; this is the only record in this file.

   Milestone/issue note: no GitHub milestone or tracker issue was created for
   the v1.00.23 series (planning artifacts live in unversioned .planning/ per
   BUG-0052). milestone=888 is the last milestone referenced in project STATE
   (v1.00.18); issue=9547 is the next-in-sequence number after the v1.00.22
   release PRs #9544-#9546. Both are placeholder identifiers that satisfy the
   machine schema; they do not reference fabricated GitHub objects.

   content-digest: SHA-256 over the W1 changed-content diff (runner.rkt,
   reporting.rkt, the three JSON/telemetry test files, and the two CI files)
   between the W0 baseline 89910aee and this wave's implementation head.
|#
#hash((schema-version . 2)
      (milestone . 888)
      (wave . "W1")
      (issue . 9547)
      (status . "ready-for-merge")
      (implementation-sha . "420ae2c62803b2dec163d15bb9351a157afd95f0")
      (content-digest . "9a2950a9eb5a2067ecaeccb602f402f72e105062178d73d2d9c9a3215af3fd6f")
      (required-checks . ("lint" "security" "release-dry-run" "workflows (0)" "workflows (1)" "workflows-aggregate" "smoke (ubuntu-latest)" "test (0)" "test (1)" "test (2)" "test-aggregate" "test-platform"))
      (review-artifact . "docs/reports/gsd-wave-reviews/v1.00.23-w1.rktd")
      (validation-artifact . "docs/reports/gsd-wave-validation/v1.00.23-w1.rktd"))

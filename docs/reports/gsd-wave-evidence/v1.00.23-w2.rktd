#| Wave 2 evidence — v1.00.23 Test Design & CI Runtime series.
   Schema: gsd-wave-gate.rkt validate-wave-evidence (schema-version 2).
   Exactly one wave-evidence record; this is the only record in this file.

   Milestone/issue note: no GitHub milestone or tracker issue was created for
   the v1.00.23 series (planning artifacts live in unversioned .planning/ per
   BUG-0052). milestone=888 is the last milestone referenced in project STATE
   (v1.00.18); issue=9547 is the next-in-sequence number after the v1.00.22
   release PRs #9544-#9546. Both are placeholder identifiers that satisfy the
   machine schema; they do not reference fabricated GitHub objects.

   content-digest: SHA-256 over the W2 changed-content diff (cli.rkt,
   runner.rkt, test-runner-scheduler-characterization.rkt, the new
   test-runner-work-queue.rkt, the four parse-args arity follow-up test files,
   and docs/TEST_CONVENTIONS.md) between the W1 baseline 3b4f1975 and this
   wave's implementation head.

   Delivery notes: --scheduler batch|queue validated in cli.rkt (invalid
   values exit 2 with a named diagnostic); queue mode is a bounded,
   work-conserving pool of exactly `jobs` long-lived workers with the
   deterministic GC schedule documented in TEST_CONVENTIONS.md; batch remains
   the default and its fixed-batch barrier is pinned by
   test-runner-scheduler-characterization.rkt (rollback path executable);
   no CI workflow opts into queue in this wave.
|#
#hash((schema-version . 2)
      (milestone . 888)
      (wave . "W2")
      (issue . 9547)
      (status . "ready-for-merge")
      (implementation-sha . "ceb91e9221dc0e28a5c074024a1354d9f0b04eb7")
      (content-digest . "7f24616abf36a791510b476b84e66545f6cfd7cd45b071b34cb4531cd81348c1")
      (required-checks . ("lint" "security" "release-dry-run" "workflows (0)" "workflows (1)" "workflows-aggregate" "smoke (ubuntu-latest)" "test (0)" "test (1)" "test (2)" "test-aggregate" "test-platform"))
      (review-artifact . "docs/reports/gsd-wave-reviews/v1.00.23-w2.rktd")
      (validation-artifact . "docs/reports/gsd-wave-validation/v1.00.23-w2.rktd"))

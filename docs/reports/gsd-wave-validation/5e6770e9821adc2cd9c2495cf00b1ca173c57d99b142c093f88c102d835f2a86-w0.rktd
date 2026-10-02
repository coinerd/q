#hasheq((branch . "binding/5e6770e9821a-w0")
        (content-digest
         .
         "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
        (fast
         .
         #hasheq((command . "racket scripts/run-tests.rkt --suite fast")
                 (result . "passed")
                 (run-summary
                  .
                  "repair branch head b3812391: 1216/1216 files, 18064/18064 tests; owned re-run inside the frozen Verify at the campaign merge head 5e9560a0 approved (the same suite)")))
        (focused-tests
         .
         #hasheq((command
                  .
                  "raco test tests/test-gsd-delivery-repair-tail.rkt tests/test-gsd-delivery-journal.rkt tests/test-gsd-delivery-receipt.rkt tests/test-gsd-delivery-coordinator.rkt tests/test-gsd-branch-delivery-verification.rkt tests/test-gsd-delivery-finalize.rkt")
                 (details
                  .
                  "18 + 16 + 11 + 21 + 10 + focused delivery suites green at the repair branch heads and at the campaign merge head")
                 (result . "passed")))
        (format-compile
         .
         #hasheq((command
                  .
                  "raco fmt -i <changed files> && raco make <changed files and dependents>")
                 (result . "passed")))
        (implementation-sha . "5e9560a07408b4ceede4168f00de6e753d42fd05")
        (issue . 9766)
        (lint
         .
         #hasheq((command
                  .
                  "racket scripts/lint-all.rkt (inside the frozen Verify)")
                 (details
                  .
                  "25 checks: format, metrics-sync, metrics-lint, prose, readme-status and the remaining lint family green at the final repair tip after the metrics regeneration and quarantine rename")
                 (result . "passed")))
        (merge-sha . "550a62b4b9959b1466f76bae6dacec4092d6e9e9")
        (merge-state
         .
         "repair delivery 550a62b4b9959b1466f76bae6dacec4092d6e9e9 IS origin/main: single-parent squash publication of PR 9771 (head 5e9560a07408b4ceede4168f00de6e753d42fd05), all required checks green on the PR head")
        (merged-at . "2026-10-02T09:55:42Z")
        (milestone . 897)
        (plan-id
         .
         "5e6770e9821adc2cd9c2495cf00b1ca173c57d99b142c093f88c102d835f2a86")
        (planning-sync . "current")
        (push-state
         .
         "origin/campaign/5e6770e9/w0 = 5e9560a07408b4ceede4168f00de6e753d42fd05 (push receipt and ls-remote confirmed); worktree clean")
        (red-first
         .
         #hasheq((command
                  .
                  "rackunit red-first authoring of tests/test-gsd-delivery-repair-tail.rkt against the absent repair-tail protocol; confirmed unbound-identifier red before implementation")
                 (failure
                  .
                  "reconcile-repair-tail-receipt!: unbound identifier (red state observed before the journal transition was implemented)")))
        (remaining-items . ())
        (review-artifact
         .
         "docs/reports/gsd-wave-reviews/5e6770e9821adc2cd9c2495cf00b1ca173c57d99b142c093f88c102d835f2a86-w0.rktd")
        (schema-version . 2)
        (status . "current")
        (test-results
         .
         "fast suite 1216/1216 files, 18064/18064 tests at b3812391; owned frozen-Verify approval at the receipt head 5e9560a0; PR 9771 CI all required checks green")
        (wave . "W0"))

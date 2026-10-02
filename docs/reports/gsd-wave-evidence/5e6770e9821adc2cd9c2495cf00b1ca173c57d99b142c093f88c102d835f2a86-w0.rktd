#hasheq((base-sha . "b55e01fbc750786c081519953cc4afa876267600")
        (branch . "binding/5e6770e9821a-w0")
        (content-digest
         .
         "3a2c7b743bf604745fb439b77e524a5d072bfddafbc4b7e7c0c64869fe708d83")
        (delivery-head-sha . "5e9560a07408b4ceede4168f00de6e753d42fd05")
        (delivery-pr . 9771)
        (delivery-scope
         .
         "v1.00.33 canary W0 seven-link autonomous delivery, repair tail under attempt-5/fence-2: the wave branch campaign/5e6770e9/w0 was pushed at 5e9560a07408b4ceede4168f00de6e753d42fd05 (origin confirmed), the durable receipt was reconciled from the prior head 3d07e1ea7b6022a254128389bb660e1fad14acd9 through the atomic fenced same-attempt transition (prior receipt preserved as history[0]), the owned verification (the wave's frozen Verify: fmt + make + focused + lint-all + the full fast suite, 1216/1216 files and 18064/18064 tests at b3812391; owned re-run at 5e9560a07408b4ceede4168f00de6e753d42fd05) approved, and the delivery was integrated through PR 9771, squash-merged as 550a62b4b9959b1466f76bae6dacec4092d6e9e9 with all required checks green. The four declared W0 targets are base-identical to b55e01fbc750786c081519953cc4afa876267600: the repair changed delivery machinery only.")
        (implementation-sha . "5e9560a07408b4ceede4168f00de6e753d42fd05")
        (issue . 9766)
        (merge-authorization
         .
         #hasheq((action
                  .
                  "squash-merge the v1.00.33 W0 repair-tail delivery pull request for the reconciled receipt head")
                 (head . "5e9560a07408b4ceede4168f00de6e753d42fd05")
                 (operator . "coinerd")
                 (source
                  .
                  "Operator directive in the v1.00.33 W0 session of 2026-10-02: complete the seven-link delivery under attempt-5/fence-2 through the repair-tail receipt reconciliation protocol (audit §21), merging the exact verified repair head once verification, CI, and independent review are green")
                 (wave . "W0")))
        (merge-method . "squash")
        (merge-sha . "550a62b4b9959b1466f76bae6dacec4092d6e9e9")
        (merged-at . "2026-10-02T09:55:42Z")
        (milestone . 897)
        (notes
         .
         "Rebound by the delivery coordinator on 2026-10-02 per audit §21 (REPORT-attempt-audit-v10033-w0-attempt5-fence2.md). content-digest is the empty-diff identity because the campaign tip differs from origin/main only in excluded evidence paths. The generation-0 binding publication (binding/5e6770e9821a-w0, implementation b55e01fbc750786c081519953cc4afa876267600) remains authoritative on main per the rebind refusal; this record binds the repair delivery facts, not a republication.")
        (plan-id
         .
         "5e6770e9821adc2cd9c2495cf00b1ca173c57d99b142c093f88c102d835f2a86")
        (required-checks
         .
         ("lint"
          "lint-quality"
          "security"
          "release-dry-run"
          "workflows (0)"
          "workflows (1)"
          "workflows-aggregate"
          "smoke (ubuntu-latest)"
          "test (0)"
          "test (1)"
          "test (2)"
          "test-aggregate"
          "test-platform"))
        (required-pr-checks
         .
         ("lint"
          "lint-quality"
          "security"
          "release-dry-run"
          "workflows (0)"
          "workflows (1)"
          "workflows-aggregate"
          "smoke (ubuntu-latest)"
          "test (0)"
          "test (1)"
          "test (2)"
          "test-aggregate"
          "test-platform"))
        (review-artifact
         .
         "docs/reports/gsd-wave-reviews/5e6770e9821adc2cd9c2495cf00b1ca173c57d99b142c093f88c102d835f2a86-w0.rktd")
        (schema-version . 2)
        (status . "ready-for-merge")
        (validation-artifact
         .
         "docs/reports/gsd-wave-validation/5e6770e9821adc2cd9c2495cf00b1ca173c57d99b142c093f88c102d835f2a86-w0.rktd")
        (wave . "W0")
        (wave-branch . "campaign/5e6770e9/w0"))

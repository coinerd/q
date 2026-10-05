#hasheq((binding-generation . 4)
        (branch . "binding/5e6770e9821a-w0-r4")
        (content-digest . "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
        (delivery-head-sha . "301e6862035c665218fd95e8eca7dfbcfd8d2773")
        (delivery-pr . 9793)
        (fast . #hasheq((command . "raco make on the four declared targets && racket scripts/run-tests.rkt tests/test-gsd-gh-cli-tracker-adapter.rkt tests/test-gsd-tracker-reconciliation.rkt") (result . "passed")))
        (focused-tests . #hasheq((command . "raco fmt (read-only, empty diff) on the extension targets and raco make on all four declared targets at 301e6862") (result . "passed")))
        (format-compile . #hasheq((command . "racket scripts/lint-all.rkt") (result . "passed")))
        (implementation-sha . "1b8a6b4089bd54b07e8c4e750c48730686df23bc")
        (issue . 9766)
        (lint . #hasheq((command . "the full declared verify of the gen4 recovery at the final gen4 tip 301e6862 (gsd-recover-delivery full mode, applied 2026-10-04T22:36Z after verification approval); the identical target tree is continuously validated by protected-main CI (20/20 green on PR 9793 head 301e6862)") (result . "passed")))
        (merge-sha . "1b8a6b4089bd54b07e8c4e750c48730686df23bc")
        (merged-at . "2026-10-04T23:00:29Z")
        (milestone . 897)
        (plan-id
         .
         "5e6770e9821adc2cd9c2495cf00b1ca173c57d99b142c093f88c102d835f2a86")
        (planning-sync . "current")
        (red-first . #hasheq((command . "gsd-delivery.py binding-review on the staged gen4 draft before the controller fix") (failure . "the rebind guard refused the staged binding because origin/main carried the -r4 source trio (no merge identity) at the binding path after the implementation squash: origin/main binding is malformed; refusing rebind — fixed by exempting the pre-binding source trio (PR 9795, controller regression added), re-affirmed by the genuine binding review")))
        (remaining-items . ())
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
         "docs/reports/gsd-wave-reviews/5e6770e9821adc2cd9c2495cf00b1ca173c57d99b142c093f88c102d835f2a86-w0-r4.rktd")
        (status . "current")
        (wave . "W0")
        (wave-branch . "campaign/5e6770e9/w0"))

#hasheq((branch . "binding/fb67e0429ed1-w3")
        (content-digest . "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
        (delivery-head-sha . "07e32106dc6da9b2a3414601e0103cf89f09d8d1")
        (delivery-pr . 9741)
        (fast
         .
         #hasheq((command
                  .
                  "CI fast shards at the delivery tree (PR #9741 head 07e32106d); racket scripts/run-tests.rkt --suite fast")
                 (detail
                  .
                  "CI ran the full fast suite at the exact delivery tree in three shards with every check green (PR #9741: 20/20 checks including test 0/1/2 and test-aggregate). Locally the fast suite passed at the #9738 implementation merge 68204ce3: 1207 files, 17953 tests, 0 failures, wall-clock 1298.373s. The executed-surface delta 68204ce3..07e32106d is exactly the three digest-excluded version-named records, so CI's green shards at the delivery head cover the binding content.")
                 (result . "passed")))
        (focused-tests
         .
         #hasheq((command
                  .
                  "raco test tests/test-gsd-wave-gate.rkt tests/test-gsd-evidence-bind.rkt tests/test-gsd-delivery-controller.rkt")
                 (detail
                  .
                  "18 tests passed at the delivery head 07e32106d in the remediation worktree (strict source-trio validation, digest binding, record-commit purity, delivery controller).")
                 (result . "passed")))
        (format-compile
         .
         #hasheq((command
                  .
                  "raco fmt -i / raco make not applicable: the delivery diff contains only .rktd evidence records")
                 (detail
                  .
                  "All three binding records parse as single safe datums through the strict gate and gsd-binding-data readers; CI format and lint gates were green at the delivery head.")
                 (result . "passed")))
        (implementation-sha . "015f9b0c5a53d93950ce0fea2652f9625e8a5132")
        (issue . 9726)
        (lint
         .
         #hasheq((command
                  .
                  "racket scripts/gsd-wave-gate.rkt <staged binding evidence> --content-digest e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 --root <staging root> --policy <policy snapshot>; required-pr-checks compared to scripts/required-pr-checks.policy at origin/main 015f9b0c5")
                 (detail
                  .
                  "The strict gate on the finalized staged binding trio with the empty-diff digest: PASS (binding-review). The 13-name required-pr-checks snapshot in the staged evidence and validation records equals the policy at origin/main. The delivery-ladder purity check over the #9741 base..head range: pure.")
                 (result . "passed")))
        (merge-sha . "015f9b0c5a53d93950ce0fea2652f9625e8a5132")
        (merged-at . "2026-09-22T13:05:58Z")
        (milestone . 896)
        (plan-id
         .
         "fb67e0429ed155a4b4e3f31afe3ef3ca7748ee891c06247b11e5e2a86cfebd75")
        (planning-sync . "current")
        (red-first
         .
         #hasheq((command
                  .
                  "python3 scripts/gsd-delivery.py prepare --repo /home/user/src/q-agent/q --plan fb67e0429ed155a4b4e3f31afe3ef3ca7748ee891c06247b11e5e2a86cfebd75 --wave 3 --pr 9738 --evidence docs/reports/gsd-wave-evidence/fb67e0429ed155a4b4e3f31afe3ef3ca7748ee891c06247b11e5e2a86cfebd75-w3.rktd --campaign-root /home/user/src/q-agent --output /home/user/src/q-agent/.planning/campaigns/fb67e0429ed155a4b4e3f31afe3ef3ca7748ee891c06247b11e5e2a86cfebd75/binding-w3")
                 (failure
                  .
                  "prepare refused the merged W3 delivery fail-closed with: source evidence is not the frozen wave output for this plan/wave. The frozen W3 snapshot declares docs/reports/gsd-wave-{evidence,reviews,validation}/v1.00.31-w3.rktd, but the merged #9738 source tree carried the trio only at campaign-hash paths, so canonical binding was impossible without fabricating provenance (an earlier probe from a repo not named q also produced the typed refusal: declared output uses a q/ prefix but the resolved repo is not q). Remediation PR #9741 - evidence-only, independently reviewed in three rounds, CI 20/20 - reissued the schema-2 trio at the exact declared paths while retaining the #9738 implementation tree, after which prepare accepted the delivery and staged this binding draft.")
                 (result . "passed")))
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
         "docs/reports/gsd-wave-reviews/fb67e0429ed155a4b4e3f31afe3ef3ca7748ee891c06247b11e5e2a86cfebd75-w3.rktd")
        (status . "current")
        (wave . "W3")
        (wave-branch . "fix/v10031-w3-frozen-trio"))

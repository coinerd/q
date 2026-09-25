#hasheq((branch . "binding/96974d2cd97b-w4")
        (content-digest . "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
        (delivery-head-sha . "6fd7e3afb4df9c12dcc62fbc105063006e8c361b")
        (delivery-pr . 9752)
        (fast . #hasheq((command . "racket scripts/run-tests.rkt --suite fast")
             (result . "passed")
             (run-summary . "RUN-SUMMARY runner-version=1.00.29 suite=fast profile=local shard=none execution-mode=subprocess file-count=1208 pass=1208 fail=0 timeout=0 skip=0 wall-clock-seconds=1326.707 metadata-completeness=explicit:1180/heuristic:0/missing:28")))
        (focused-tests . #hasheq((command . "raco test tests/test-compiled-root-workflow.rkt tests/test-compiled-root.rkt tests/test-compiled-root-manifest.rkt tests/test-prepared-env-report.rkt && raco test tests/test-workflow-invocation-contract.rkt && raco test tests/test-wave-integrity-adversarial.rkt")
             (result . "passed")
             (details . "185 focused compiled-root tests, the W1 invocation-contract matrix and the W6 adversarial rehearsal (8/8) at the reviewed implementation head 1158f1b7023b08c9608f59c8cd4c0c04915d5de6; the strict gate, gsd-evidence-bind verify (digest-ok) and record-commit (pure) are green on the merged head cc25b3663362d032b9a23a9d22f9dd5edbe637d8.")))
        (format-compile . #hasheq((command . "raco fmt -i && raco make on the changed Racket sources at the reviewed implementation head (the binding publication changes no source)")
             (result . "passed")))
        (implementation-sha . "cc25b3663362d032b9a23a9d22f9dd5edbe637d8")
        (issue . 9690)
        (lint . #hasheq((command . "racket scripts/check-deps.rkt && racket scripts/metrics.rkt --lint && racket scripts/ci/verify-artifact-provenance.rkt --root .")
             (result . "passed")
             (details . "at the reviewed implementation head: dependencies OK, all 5 static metrics match README.md, provenance lint exit 0 (946 historical notes, none for the ci-recovery/v1.00.30-w4 or wave-delivery-integrity/v1.00.31-w6 dirs); the merged implementation PR #9752 finished 22/22 CI checks green with 0 failures.")))
        (merge-sha . "cc25b3663362d032b9a23a9d22f9dd5edbe637d8")
        (merged-at . "2026-09-25T02:10:15Z")
        (milestone . 895)
        (plan-id
         .
         "96974d2cd97b152f99d296d6956f13b4f3b2bc4f79428894f74a1f3df49753d8")
        (planning-sync . "current")
        (red-first . #hasheq((command . "independent read-only review rounds 1-12 against the blocked-wave repair, each round reproduced from the committed tree")
             (failure . "the blocked wave ce13d0388 was structurally undeliverable (its producer invocation was unexecutable, no consumer could reach a root, its zero-module run was refused, its evidence digest was unreproducible and its branch was never pushed); rounds 1-12 then found and fixed 12 further defects, including a pre-squash rehearsal head that became unfetchable, an invocation-contract regression, and an off-lane launcher failure that local execution could not reach. Captures are retained at artifacts/ci-recovery/v1.00.30-w4/raw/.")))
        (remaining-items
         .
         (#hasheq((classification . "deferred-noncritical")
                  (owner . "coordinator (W4 pilot activation)")
                  (rationale
                   .
                   "The wave is a fast-lane pilot: promotion beyond the declared fast lanes, the pr-latency-guard sample on the exact final head, and the regime comparison against the frozen W2 references are coordinator-owned remote gates recorded in artifacts/ci-recovery/v1.00.30-w4/activation.json (status HOLD; activation-merge-sha and regime-fingerprint PENDING until the protected merge lands)."))
          #hasheq((classification . "deferred-noncritical")
                  (owner . "W7 release record")
                  (rationale
                   .
                   "The v1.00.30 W4 outcome is recorded in the v1.00.31 W7 wave artifacts (artifacts/wave-delivery-integrity/v1.00.31-w7/w4-recovery.json) rather than inside this wave\'s own records."))))
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
         "docs/reports/gsd-wave-reviews/96974d2cd97b152f99d296d6956f13b4f3b2bc4f79428894f74a1f3df49753d8-w4.rktd")
        (status . "current")
        (wave . "W4")
        (wave-branch . "campaign/v1.00.30-w4"))

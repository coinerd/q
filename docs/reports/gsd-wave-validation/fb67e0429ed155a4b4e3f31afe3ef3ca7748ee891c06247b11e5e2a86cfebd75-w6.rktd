#hasheq(
 (branch . "binding/fb67e0429ed1-w6")
 (content-digest . "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
 (delivery-head-sha . "9adbe65d37eafefaec9999c0a7d324d13f1be5bd")
 (delivery-pr . 9750)
 (fast
  . #hasheq(
     (command . "racket scripts/run-tests.rkt --suite fast")
     (result . "passed")
     (run-summary . "RUN-SUMMARY runner-version=1.00.29 suite=fast profile=local shard=none execution-mode=subprocess file-count=1208 pass=1208 fail=0 timeout=0 skip=0 wall-clock-seconds=1674.212 metadata-completeness=explicit:1180/heuristic:0/missing:28")))
 (focused-tests
  . #hasheq(
     (command . "raco test tests/test-wave-integrity-adversarial.rkt tests/test-wave-delivery-integrity-register.rkt && raco test tests/test-milestone-gate.rkt && python3 scripts/gsd-delivery.py --repo <q> --plan <plan> --wave 6 --output <staging> --campaign-root <root> --expected-branch campaign/v1.00.31-w6 binding-review")
     (result . "passed")
     (details . "adversarial 8/8, register 10/10, milestone-gate 75/75 at the reviewed implementation head; binding-review reports the staged draft as reviewed against the merged implementation, with the strict gate green on the finalized trio.")))
 (format-compile
  . #hasheq(
     (command . "raco fmt -i && raco make on the changed Racket sources at the reviewed implementation head (the binding publication changes no source)")
     (result . "passed")))
 (implementation-sha . "e799f9d7837e003757e22dbbcfe2099c45515eba")
 (issue . 9729)
 (lint
  . #hasheq(
     (command . "racket scripts/check-deps.rkt && racket scripts/metrics.rkt --lint && racket scripts/metrics.rkt --lint-prose README.md && racket scripts/tier-ownership-matrix.rkt check && racket scripts/run-tests/inventory.rkt --ownership-map --check artifacts/tier-ownership/v1.00.29-w0/ownership-matrix.json && racket scripts/ci/verify-artifact-provenance.rkt --root . --current-wave v1.00.31-w6")
     (result . "passed")
     (details . "at the reviewed implementation head: dependencies OK, all 5 static metrics match README.md, prose lint passed, tier matrix green (1005 rows), the eight-column ownership matrix matches reality, provenance lint exit 0 with 0 notes; the W6 rehearsal artifacts regenerate consistently on main (the matrix and its report reproduce byte-identically except the rehearsal-head field, which necessarily advances with the commit carrying the artifact, and SHA256SUMS rebinds to the same content).")))
 (merge-sha . "e799f9d7837e003757e22dbbcfe2099c45515eba")
 (merged-at . "2026-09-24T09:02:32Z")
 (milestone . 896)
 (plan-id . "fb67e0429ed155a4b4e3f31afe3ef3ca7748ee891c06247b11e5e2a86cfebd75")
 (planning-sync . "current")
 (red-first
  . #hasheq(
     (command . "racket scripts/ci/inject-wave-defect.rkt and racket tests/test-wave-delivery-integrity-register.rkt against the pre-fix baseline 8db4417ea (captures under artifacts/wave-delivery-integrity/v1.00.31-w6/raw/)")
     (failure . "F1-F11 had no rehearsal harness (the register rows were prose plus guards, so a defect could be introduced and shipped without ever being demonstrated to be refused), and F12/F13 were absent from the W0 register and its guard registrations. Both captures are retained in the wave artifacts.")))
 (remaining-items
  . (#hasheq(
      (classification . "deferred-noncritical")
      (owner . "W7 follow-up (rehearsal witness hardening)")
      (rationale . "verdict-line? matches verdicts by line prefix rather than by an exact delimiter, sufficient for the current typed verdicts but not for a longer word sharing the prefix."))
     #hasheq(
      (classification . "deferred-noncritical")
      (owner . "W7 follow-up (matrix generator)")
      (rationale . "the injection-matrix generator leaves the human report unchanged when its Rehearsal head literal is absent, but the adversarial test asserts report/matrix head agreement, so a missing literal fails the suite instead of shipping a silent drift."))))
 (required-pr-checks
  . ("lint"
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
  . "docs/reports/gsd-wave-reviews/fb67e0429ed155a4b4e3f31afe3ef3ca7748ee891c06247b11e5e2a86cfebd75-w6.rktd")
 (schema-version . 2)
 (status . "current")
 (suites
  . #hasheq(
     (workflows . "RUN-SUMMARY runner-version=1.00.29 suite=workflows profile=local shard=none execution-mode=subprocess file-count=33 pass=33 fail=0 timeout=0 skip=0 wall-clock-seconds=88.305 metadata-completeness=explicit:33/heuristic:0/missing:0")))
 (wave . "W6")
 (wave-branch . "campaign/v1.00.31-w6"))
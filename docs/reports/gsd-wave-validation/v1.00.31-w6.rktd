#hasheq(
 (schema-version . 2)
 (status . "current")
 (milestone . 896)
 (wave . "W6")
 (issue . 9729)
 (branch . "campaign/v1.00.31-w6")
 (implementation-sha . "4a5930cdff84ed27471e517b971347873ce8064b")
 (content-digest . "49d238088adcd52c4888e9a3c633c73d1bc0bb9843e5fc03ded2d2a07da6ff40")
 (review-artifact . "docs/reports/gsd-wave-reviews/v1.00.31-w6.rktd")
 (planning-sync . "current")
 (red-first
  . #hasheq(
     (command . "racket scripts/ci/inject-wave-defect.rkt and racket tests/test-wave-delivery-integrity-register.rkt against the pre-fix baseline 8db4417ea (captures under artifacts/wave-delivery-integrity/v1.00.31-w6/raw/)")
     (failure . "F1-F11 had no rehearsal harness at all: the register rows were prose plus guards, so a defect could be introduced, shipped and never demonstrated to be refused (no injection matrix, no negative controls, no verdict). The two new rows were also unregistered: F12 (a finalized-looking trio carrying sentinel placeholders) and F13 (a plan-body amendment while the frozen snapshot still reports clean) were absent from the W0 register and from the register test. Both captures are retained under artifacts/wave-delivery-integrity/v1.00.31-w6/raw/ (red-first-harness.txt, red-first-register-f12-f13.txt).")))
 (focused-tests
  . #hasheq(
     (command . "raco test tests/test-wave-integrity-adversarial.rkt tests/test-wave-delivery-integrity-register.rkt && raco test tests/test-milestone-gate.rkt && racket scripts/ci/verify-artifact-provenance.rkt --root . --current-wave v1.00.31-w6 --only-current-wave")
     (result . "passed")
     (details . "adversarial 8/8; register 10/10; milestone-gate 75/75 (the resynced eight-column ownership matrix passes the red-CI drift check); provenance lint exit 0 with 0 notes; the rehearsal harness exits 0 with verdict PERMANENT (13/13 rows refused, clean synthetic wave accepted by every guard, missing-guard rehearsal refused fail-closed).")))
 (format-compile
  . #hasheq(
     (command . "raco fmt -i && raco make on the changed Racket sources at the reviewed implementation head")
     (result . "passed")))
 (lint
  . #hasheq(
     (command . "racket scripts/check-deps.rkt && racket scripts/metrics.rkt --lint && racket scripts/metrics.rkt --lint-prose README.md && racket scripts/tier-ownership-matrix.rkt check && racket scripts/run-tests/inventory.rkt --ownership-map --check artifacts/tier-ownership/v1.00.29-w0/ownership-matrix.json && racket scripts/ci/verify-artifact-provenance.rkt --root . --current-wave v1.00.31-w6")
     (result . "passed")
     (details . "dependencies OK; all 5 static metrics match README.md; prose lint passed; tier-ownership matrix check green (1005 rows); the eight-column ownership matrix matches reality (the W6 test family row now declares the cwd side-effect the harness actually uses); provenance lint exit 0 (0 notes, 0 drift); the wave artifacts regenerate byte-identically and SHA256SUMS rebinds to the same content.")))
 (fast
  . #hasheq(
     (command . "racket scripts/run-tests.rkt --suite fast")
     (result . "passed")
     (run-summary . "RUN-SUMMARY runner-version=1.00.29 suite=fast profile=local shard=none execution-mode=subprocess file-count=1208 pass=1208 fail=0 timeout=0 skip=0 wall-clock-seconds=1674.212 metadata-completeness=explicit:1180/heuristic:0/missing:28")))
 (suites
  . #hasheq(
     (workflows . "RUN-SUMMARY runner-version=1.00.29 suite=workflows profile=local shard=none execution-mode=subprocess file-count=33 pass=33 fail=0 timeout=0 skip=0 wall-clock-seconds=88.305 metadata-completeness=explicit:33/heuristic:0/missing:0")))
 (remaining-items
  . (#hasheq(
      (classification . "deferred-noncritical")
      (owner . "W7 follow-up (rehearsal witness hardening)")
      (rationale . "verdict-line? matches verdicts by line prefix rather than by an exact delimiter, which is sufficient for the current typed verdicts but would accept a longer word sharing the prefix; a stricter delimiter-exact form is a hardening nicety, not a guarantee gap."))
     #hasheq(
      (classification . "deferred-noncritical")
      (owner . "W7 follow-up (matrix generator)")
      (rationale . "the injection-matrix generator leaves the human report unchanged when its Rehearsal head literal is absent, but the adversarial test asserts report/matrix head agreement, so a missing literal fails the suite instead of shipping a silent drift.")))))
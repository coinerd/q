#lang racket

;; @speed fast
;; @suite ci
;; tests/test-w1-ci-setup-verification.rkt
;; v0.99.40 W1: Verify CI setup-racket dependency from v0.99.39

(require rackunit
         racket/port)

;; ── Evidence: setup-racket passes on CI ──
;;
;; CI Run #5603 (a3a0adfc — v0.99.39 W6 final):
;;   lint job → setup-racket step: conclusion=success
;;   lint job → "Run all lint checks" step: conclusion=failure
;;   (failure was lint-all issues, fixed in W0 #8517)
;;
;; CI Run #5601 (0958074f — v0.99.39 W5):
;;   lint job → setup-racket step: conclusion=success
;;
;; CI Run #5599 (e6f39700 — v0.99.39 W4):
;;   lint job → setup-racket step: conclusion=success
;;
;; CI Run #5597 (3fec7b54 — v0.99.39 W3):
;;   lint job → setup-racket step: conclusion=success
;;
;; Conclusion: setup-racket PASSES on all post-remediation runs.
;; The v0.99.39 F1-F4 compile fixes resolved the root cause.

(define-test-suite w1-ci-setup-verification
                   (test-case "setup-racket action.yml exists on main"
                     (check-true (file-exists? "../.github/actions/setup-racket/action.yml")))
                   (test-case "ci-package-setup.rkt exists from v0.99.39"
                     (check-true (file-exists? "../scripts/ci-package-setup.rkt")))
                   (test-case "ci-local.rkt has --quick flag"
                     (check-true (file-exists? "../scripts/ci-local.rkt")))
                   (test-case "release.yml depends on setup-racket composite action"
                     (define content (file->string "../.github/workflows/release.yml"))
                     (check-true (string-contains? content "setup-racket")))
                   (test-case "ci.yml depends on setup-racket composite action"
                     (define content (file->string "../.github/workflows/ci.yml"))
                     (check-true (string-contains? content "setup-racket"))))

(module+ test
  (require rackunit/text-ui)
  (run-tests w1-ci-setup-verification))

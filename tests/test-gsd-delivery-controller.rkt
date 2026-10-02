#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery.py
;; @timeout 960
;; The wrapper runs the FULL Python delivery-controller contract suite (133
;; real-git tests) in one subprocess. CI runners are ~2x slower than the
;; local machine and the suite has outgrown its earlier budgets: after
;; #9735/#9744/#9754 it measures 391s locally (129 tests), past the 280s
;; inner budget that once fit 121 tests, producing false timeout failures;
;; the v1.00.33 W0 repair-tail regressions add four more (133). The campaign
;; line settled on a 900s inner budget (654e96e0); this branch adopts the
;; same numbers with the measured rationale above.
(require rackunit
         racket/runtime-path
         racket/system
         "../sandbox/subprocess.rkt")
(define-runtime-path python-tests "test-gsd-delivery-controller.py")
(module+ test
  (test-case "delivery controller real-git and fake-GitHub contract regressions"
    (define python (or (find-executable-path "python3") (find-executable-path "python")))
    (check-not-false python "Python is required for protected-delivery tooling")
    (when python
      (define result
        (run-subprocess (path->string python)
                        #:args (list (path->string python-tests))
                        #:timeout 900))
      (check-false (subprocess-result-timed-out? result))
      (check-equal? (subprocess-result-exit-code result)
                    0
                    (string-append (subprocess-result-stdout result)
                                   (subprocess-result-stderr result))))))

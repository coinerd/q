#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery.py
;; @timeout 300
;; The wrapper runs the FULL Python delivery-controller contract suite (121
;; real-git tests) in one subprocess. CI runners are ~2x slower than the
;; local machine and the suite outgrew the default 120s per-file fast-suite
;; budget (test (2) timeout on PR #9720), so the runner budget is explicitly
;; widened here; the inner subprocess timeout stays below it so the wrapper
;; reports the failure itself rather than being killed.
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
                        #:timeout 280))
      (check-false (subprocess-result-timed-out? result))
      (check-equal? (subprocess-result-exit-code result)
                    0
                    (string-append (subprocess-result-stdout result)
                                   (subprocess-result-stderr result))))))

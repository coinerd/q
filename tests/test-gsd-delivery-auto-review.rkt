#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @covers scripts/gsd-delivery.py
;; @timeout 1860
;; Register slice B's Python real-Git controller regressions in Racket CI.
(require rackunit
         racket/runtime-path
         racket/system
         "../sandbox/subprocess.rkt")
(define-runtime-path python-tests "test-gsd-delivery-auto-review.py")
(module+ test
  (test-case "automatic review trusted controller real-Git regressions"
    (define python (or (find-executable-path "python3") (find-executable-path "python")))
    (check-not-false python "Python is required for the delivery controller")
    (when python
      (define result
        (run-subprocess (path->string python)
                        #:args (list (path->string python-tests))
                        #:timeout 1800))
      (check-false (subprocess-result-timed-out? result))
      (check-equal? (subprocess-result-exit-code result)
                    0
                    (string-append (subprocess-result-stdout result)
                                   (subprocess-result-stderr result))))))

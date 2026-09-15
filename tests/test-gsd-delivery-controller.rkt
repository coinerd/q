#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery.py
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
                        #:timeout 120))
      (check-false (subprocess-result-timed-out? result))
      (check-equal? (subprocess-result-exit-code result)
                    0
                    (string-append (subprocess-result-stdout result)
                                   (subprocess-result-stderr result))))))

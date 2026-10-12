#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @isolation process-tree
;; @mutates repo
;; @timeout 960
;; @covers scripts/gsd-delivery.py scripts/gsd-binding-data.rkt scripts/gsd-gates/owned-audit.rkt
(require rackunit
         racket/runtime-path
         "../sandbox/subprocess.rkt")
(define-runtime-path python-tests "test-gsd-owned-audit.py")
(define (run-python! label selection)
  (define result
    (run-subprocess "python3"
                    #:args (list (path->string python-tests) selection)
                    #:timeout 900
                    #:process-group? #t))
  (check-false (subprocess-result-timed-out? result))
  (check-equal? (subprocess-result-exit-code result)
                0
                (format "~a: ~a" label (subprocess-result-stderr result))))
(module+ test
  (test-case "pure audit laws and missing RED fails closed without consumption"
    (run-python! "unit laws" "OwnedAuditTests")
    (run-python! "no-red world" "NoRedWorldTests")))

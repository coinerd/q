#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @isolation subprocess
;; @mutates repo
;; @timeout 150
;; @covers scripts/gsd-delivery.py scripts/gsd-binding-data.rkt scripts/gsd-gates/owned-audit.rkt
(require rackunit
         racket/runtime-path
         "../sandbox/subprocess.rkt")
(define-runtime-path python-tests "test-gsd-owned-audit.py")
(module+ test
  (test-case "linked RED: approved audit publishes through the strict gate, zero duplicate Verify"
    (define result
      (run-subprocess "python3"
                      #:args (list (path->string python-tests) "LinkedRedApprovalTests")
                      #:timeout 140
                      #:process-group? #t))
    (check-false (subprocess-result-timed-out? result))
    (check-equal? (subprocess-result-exit-code result) 0 (subprocess-result-stderr result))))

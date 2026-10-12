#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @isolation subprocess
;; @mutates repo
;; @timeout 150
;; @covers scripts/gsd-delivery.py scripts/gsd-binding-data.rkt extensions/gsd/review-proof.rkt
(require rackunit
         racket/runtime-path
         "../sandbox/subprocess.rkt")
(define-runtime-path python-tests "test-gsd-owned-audit.py")
(module+ test
  (test-case "linked RED: infra stops, drift and pre-push raw recheck"
    (define result
      (run-subprocess "python3"
                      #:args (list (path->string python-tests) "RedInfraStopTests")
                      #:timeout 140
                      #:process-group? #t))
    (check-false (subprocess-result-timed-out? result))
    (check-equal? (subprocess-result-exit-code result) 0 (subprocess-result-stderr result))))

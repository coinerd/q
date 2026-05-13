#lang racket

;; BOUNDARY: integration

;; tests/test-context-assembly-budgeting.rkt — tests for context-assembly/budgeting

(require rackunit
         rackunit/text-ui
         "../runtime/context-assembly/budgeting.rkt")

(define budget-tests
  (test-suite "Context Assembly Budgeting"

    (test-case "make-context-assembly-config: defaults"
      (define cfg (make-context-assembly-config))
      (check-equal? (context-assembly-config-recent-tokens cfg) 30000)
      (check-equal? (context-assembly-config-max-catalog-entries cfg) 40)
      (check-equal? (context-assembly-config-max-catalog-tokens cfg) 2000)
      (check-equal? (context-assembly-config-summary-window cfg) 4000))

    (test-case "make-context-assembly-config: custom"
      (define cfg
        (make-context-assembly-config #:recent-tokens 50000
                                      #:max-catalog-entries 20
                                      #:max-catalog-tokens 1000
                                      #:summary-window 2000))
      (check-equal? (context-assembly-config-recent-tokens cfg) 50000)
      (check-equal? (context-assembly-config-max-catalog-entries cfg) 20))

    (test-case "make-context-assembly-config: rejects zero recent-tokens"
      (check-exn exn:fail? (lambda () (make-context-assembly-config #:recent-tokens 0))))

    (test-case "context-result construction"
      (define cr (context-result '() 0 0 0 0 #f '() #f))
      (check-true (context-result? cr))
      (check-equal? (context-result-total-tokens cr) 0)
      (check-false (context-result-over-budget? cr)))))

(module+ main
  (run-tests budget-tests))
(module+ test
  (run-tests budget-tests))

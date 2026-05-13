#lang racket

;; BOUNDARY: integration

;; tests/test-context-fit.rkt — W2-D3: Test scaffold for runtime/context-fit.rkt
;; v0.29.13: Smoke tests for message truncation within token budgets.

(require rackunit
         "../runtime/context-fit.rkt"
         "../runtime/context-policy.rkt"
         "../util/protocol-types.rkt")

(test-case "truncate-messages-to-budget returns empty for empty input"
  (check-equal? (truncate-messages-to-budget '() 1000) '()))

(test-case "truncate-messages-to-budget keeps messages within budget"
  (define msgs
    (for/list ([i (in-range 10)])
      (make-message (number->string i)
                    #f
                    'user
                    'text
                    (list (make-text-part (make-string 50 #\a)))
                    i
                    (hasheq))))
  (define budget 200)
  (define result (truncate-messages-to-budget msgs budget))
  ;; Verify actual token budget compliance, not just count
  (define total-tokens (for/sum ([m (in-list result)]) (estimate-message-tokens m)))
  (check-true (<= total-tokens budget)
              (format "total tokens ~a exceed budget ~a" total-tokens budget)))

(test-case "truncate-messages-to-budget truncates when over budget"
  (define msgs
    (for/list ([i (in-range 20)])
      (make-message (number->string i)
                    #f
                    'user
                    'text
                    (list (make-text-part (make-string 100 #\b)))
                    i
                    (hasheq))))
  (define budget 200)
  (define result (truncate-messages-to-budget msgs budget))
  ;; Should have dropped some messages (pinning may cause slight over-budget)
  (check-true (< (length result) 20)))

(test-case "fit-messages-from-recent returns empty for empty input"
  (check-equal? (fit-messages-from-recent '() 1000) '()))

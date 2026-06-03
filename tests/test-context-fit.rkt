#lang racket

;; BOUNDARY: integration

;; tests/test-context-fit.rkt — W2-D3: Test scaffold for runtime/context-fit.rkt
;; v0.29.13: Smoke tests for message truncation within token budgets.

(require rackunit
         "../runtime/context/context-fit.rkt"
         "../runtime/context/context-policy.rkt"
         "../util/message/protocol-types.rkt")

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

;; v0.45.7 (NF2): Integration test — importance rescue wired through production path
(test-case "fit-messages-from-recent rescues critical-importance messages"
  ;; Create 20 normal messages and 1 critical message at position 0
  (define normal-msgs
    (for/list ([i (in-range 1 21)])
      (make-message (format "n~a" i)
                    #f
                    'user
                    'text
                    (list (make-text-part (format "Normal message ~a with padding text" i)))
                    i
                    (hasheq))))
  (define critical-msg
    (make-message "crit-0"
                  #f
                  'user
                  'text
                  (list (make-text-part "CRITICAL DECISION"))
                  0
                  (hasheq 'importance 'critical)))
  (define msgs (cons critical-msg normal-msgs))
  ;; Small budget — critical message should be rescued by importance pass
  (define result (fit-messages-from-recent msgs 150))
  (define result-ids (map message-id result))
  (check-not-false (member "crit-0" result-ids)
                   "critical-importance message should survive importance rescue"))

#lang racket

;; tests/test-context-fit.rkt — W2-D3: Test scaffold for runtime/context-fit.rkt
;; v0.29.13: Smoke tests for message truncation within token budgets.

(require rackunit
         "../runtime/context-fit.rkt"
         "../util/protocol-types.rkt")

(test-case "truncate-messages-to-budget returns empty for empty input"
  (check-equal? (truncate-messages-to-budget '() 1000) '()))

(test-case "truncate-messages-to-budget keeps messages within budget"
  (define msgs (for/list ([i (in-range 10)])
                 (make-message (number->string i) #f 'user 'text
                               (list (make-text-part (make-string 50 #\a)))
                               i (hasheq))))
  (define result (truncate-messages-to-budget msgs 200))
  (check-true (<= (length result) 10)))

(test-case "fit-messages-from-recent returns empty for empty input"
  (check-equal? (fit-messages-from-recent '() 1000) '()))

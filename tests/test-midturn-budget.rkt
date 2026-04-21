#lang racket

;;; tests/test-midturn-budget.rkt — mid-turn token budget check (v0.14.1 Wave 5)
;;;
;;; Tests that check-mid-turn-budget! emits context.mid-turn-over-budget
;;; events when the context grows past 90% of max-context-tokens.

(require rackunit
         "../agent/event-bus.rkt"
         "../util/protocol-types.rkt"
         "../runtime/iteration.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-test-message text)
  (make-message (symbol->string (gensym 'midturn-test))
                #f
                'assistant
                'text
                (list (make-text-part text))
                0
                (hasheq)))

;; ============================================================
;; Test suite
;; ============================================================

(test-case
 "check-mid-turn-budget: no event when under budget"
 (define bus (make-event-bus))
 (define events '())
 (subscribe! bus (lambda (evt) (set! events (cons evt events))))
 ;; Small context, large budget → no event
 (define ctx (list (make-test-message "hello")))
 (define config (hash 'max-context-tokens 128000))
 (define result (check-mid-turn-budget! ctx bus "test-session" config))
 (check-true (> result 0))
 (check-equal? (length events) 0))

(test-case
 "check-mid-turn-budget: event emitted when over 90% budget"
 (define bus (make-event-bus))
 (define events '())
 (subscribe! bus (lambda (evt) (set! events (cons evt events))))
 ;; Create a large context that exceeds 90% of a tiny budget
 (define big-text (make-string 5000 #\x))
 (define ctx (for/list ([_ (in-range 10)]) (make-test-message big-text)))
 ;; Budget = 500 tokens → 90% = 450 → definitely over
 (define config (hash 'max-context-tokens 500))
 (define result (check-mid-turn-budget! ctx bus "test-session" config))
 (check-true (> result 0))
 (check-equal? (length events) 1)
 (define evt (car events))
 (check-equal? (event-ev evt) "context.mid-turn-over-budget")
 (define payload (event-payload evt))
 (check-true (hash-has-key? payload 'estimated-tokens))
 (check-true (hash-has-key? payload 'budget)))

(test-case
 "check-mid-turn-budget: estimated tokens returned correctly"
 (define bus (make-event-bus))
 (define ctx (list (make-test-message "test message")))
 (define config (hash 'max-context-tokens 128000))
 (define result (check-mid-turn-budget! ctx bus "test-session" config))
 ;; Should return positive integer
 (check-true (and (integer? result) (positive? result))))

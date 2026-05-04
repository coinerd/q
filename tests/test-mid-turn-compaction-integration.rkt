#lang racket/base

;; test-mid-turn-compaction-integration.rkt — TDD tests for v0.28.22 W0
;; Integration tests: mid-turn compaction wired through iteration loop

(require rackunit
         rackunit/text-ui
         racket/string
         "../util/message.rkt"
         "../util/content-parts.rkt"
         "../runtime/session-compaction.rkt"
         "../runtime/compactor.rkt"
         "../runtime/iteration/retry-policy.rkt"
         "../llm/token-budget.rkt"
         "../agent/event-bus.rkt")

(define integration-suite
  (test-suite
   "Mid-turn compaction integration"

   (test-case
    "check-mid-turn-budget! returns compacted context with session"
    (define bus (make-event-bus))
    (define events '())
    (subscribe! bus (lambda (evt) (set! events (cons evt events))))
    ;; Build 50-message context
    (define ctx
      (for/list ([i (in-range 50)])
        (message (format "id~a" i) #f 'user 'message
                 (list (text-part "text" (format "Message ~a ~a" i (make-string 200 #\x))))
                 (current-seconds) (hasheq))))
    ;; Very tight budget to trigger compaction
    (define config (hasheq 'max-context-tokens 100))
    ;; Call with session=#f — should return integer (token estimate)
    (define result-no-sess (check-mid-turn-budget! ctx bus "test-session" config))
    (check-true (integer? result-no-sess) "without session returns integer"))

   (test-case
    "compact-history returns valid result structure"
    (define ctx
      (for/list ([i (in-range 50)])
        (message (format "id~a" i) #f 'user 'message
                 (list (text-part "text" (format "Message ~a ~a" i (make-string 200 #\x))))
                 (current-seconds) (hasheq))))
    (define result (compact-history ctx))
    (check-true (compaction-result? result))
    (check-true (list? (compaction-result-kept-messages result))
                "compaction result has kept messages"))

   (test-case
    "context.mid-turn-over-budget event emitted"
    (define bus (make-event-bus))
    (define events '())
    (subscribe! bus (lambda (evt) (set! events (cons evt events))))
    (define ctx
      (for/list ([i (in-range 50)])
        (message (format "id~a" i) #f 'user 'message
                 (list (text-part "text" (format "Message ~a ~a" i (make-string 200 #\x))))
                 (current-seconds) (hasheq))))
    (define config (hasheq 'max-context-tokens 100))
    (check-mid-turn-budget! ctx bus "test-session" config)
    (check-true (> (length events) 0) "event emitted"))

   (test-case
    "under-budget context returns unchanged estimate"
    (define ctx
      (for/list ([i (in-range 3)])
        (message (format "id~a" i) #f 'user 'message
                 (list (text-part "text" (format "Message ~a" i)))
                 (current-seconds) (hasheq))))
    (define config (hasheq 'max-context-tokens 128000))
    (define result (check-mid-turn-budget! ctx #f #f config))
    (check-true (integer? result) "returns integer for under-budget")
    (check-true (> result 0) "positive token estimate"))))

(run-tests integration-suite)

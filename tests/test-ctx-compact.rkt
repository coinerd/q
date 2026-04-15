#lang racket

;; tests/test-ctx-compact.rkt — tests for Programmatic Compaction (#708-#709)
;;
;; Covers:
;;   - #708: ctx-compact with custom instructions, callbacks, rate limiting
;;   - #709: Parent feature

(require rackunit
         "../agent/event-bus.rkt"
         "../runtime/compactor.rkt"
         "../runtime/ctx-compact.rkt"
         "../util/protocol-types.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define msg-counter 0)
(define (next-id!)
  (set! msg-counter (add1 msg-counter))
  (format "msg-~a" msg-counter))

(define (make-user-msg text)
  (make-message (next-id!) #f 'user 'text
                (list (make-text-part text))
                (current-seconds) (hasheq)))

(define (make-assistant-msg text)
  (make-message (next-id!) #f 'assistant 'text
                (list (make-text-part text))
                (current-seconds) (hasheq)))

(define (make-test-messages count)
  (for/list ([i (in-range count)])
    (if (even? i)
        (make-user-msg (format "question ~a" i))
        (make-assistant-msg (format "answer ~a" i)))))

;; ============================================================
;; Rate limiting
;; ============================================================

(test-case "rate-limit-allowed?: initially allows"
  (reset-rate-limit!)
  (check-true (rate-limit-allowed?)))

(test-case "rate-limit-allowed?: blocks after limit reached"
  (parameterize ([current-compact-rate-limit 2]
                 [current-compact-rate-window 60])
    (reset-rate-limit!)
    (check-true (rate-limit-allowed?))
    ;; Consume 2 slots
    (ctx-compact (make-test-messages 10) (compaction-strategy 5 3))
    (check-true (rate-limit-allowed?))
    (ctx-compact (make-test-messages 10) (compaction-strategy 5 3))
    ;; Should be blocked now
    (check-false (rate-limit-allowed?))))

(test-case "reset-rate-limit!: resets counter"
  (parameterize ([current-compact-rate-limit 1]
                 [current-compact-rate-window 60])
    (reset-rate-limit!)
    (ctx-compact (make-test-messages 10) (compaction-strategy 5 3))
    (check-false (rate-limit-allowed?))
    (reset-rate-limit!)
    (check-true (rate-limit-allowed?))))

;; ============================================================
;; ctx-compact-request struct
;; ============================================================

(test-case "ctx-compact-request: stores instructions and callbacks"
  (define on-ok (lambda (r) 'ok))
  (define on-err (lambda (r) 'err))
  (define req (ctx-compact-request "summarize focusing on code changes" on-ok on-err (current-seconds)))
  (check-equal? (ctx-compact-request-instructions req) "summarize focusing on code changes")
  (check-true (procedure? (ctx-compact-request-on-complete req)))
  (check-true (procedure? (ctx-compact-request-on-error req))))

(test-case "ctx-compact?: recognizes requests"
  (check-true (ctx-compact? (ctx-compact-request #f #f #f (current-seconds))))
  (check-false (ctx-compact? 'not-a-request)))

;; ============================================================
;; compact-callback-result struct
;; ============================================================

(test-case "compact-callback-result: success"
  (define r (compact-callback-result #t "summary text" #f 5))
  (check-true (compact-callback-result-success? r))
  (check-equal? (compact-callback-result-summary r) "summary text")
  (check-false (compact-callback-result-error r))
  (check-equal? (compact-callback-result-removed-count r) 5))

(test-case "compact-callback-result: failure"
  (define r (compact-callback-result #f #f "something went wrong" 0))
  (check-false (compact-callback-result-success? r))
  (check-equal? (compact-callback-result-error r) "something went wrong"))

;; ============================================================
;; ctx-compact: synchronous compaction
;; ============================================================

(test-case "ctx-compact: basic compaction"
  (reset-rate-limit!)
  (define msgs (make-test-messages 10))
  (define strategy (compaction-strategy 5 3))
  (define result (ctx-compact msgs strategy))
  (check-true (compact-callback-result? result))
  (check-true (compact-callback-result-success? result))
  (check-true (>= (compact-callback-result-removed-count result) 0)))

(test-case "ctx-compact: calls on-complete on success"
  (reset-rate-limit!)
  (define msgs (make-test-messages 10))
  (define strategy (compaction-strategy 5 3))
  (define callback-result #f)
  (ctx-compact msgs strategy
                #:on-complete (lambda (r) (set! callback-result r)))
  (check-true (compact-callback-result? callback-result))
  (check-true (compact-callback-result-success? callback-result)))

(test-case "ctx-compact: rate-limited returns error"
  (parameterize ([current-compact-rate-limit 0]
                 [current-compact-rate-window 60])
    (reset-rate-limit!)
    (define result (ctx-compact (make-test-messages 10) (compaction-strategy 5 3)))
    (check-false (compact-callback-result-success? result))
    (check-true (string? (compact-callback-result-error result)))))

(test-case "ctx-compact: calls on-error when rate-limited"
  (parameterize ([current-compact-rate-limit 0]
                 [current-compact-rate-window 60])
    (reset-rate-limit!)
    (define error-result #f)
    (ctx-compact (make-test-messages 10) (compaction-strategy 5 3)
                  #:on-error (lambda (r) (set! error-result r)))
    (check-true (compact-callback-result? error-result))
    (check-false (compact-callback-result-success? error-result))))

(test-case "ctx-compact: publishes events on bus"
  (reset-rate-limit!)
  (define bus (make-event-bus))
  (define captured (box '()))
  (subscribe! bus (lambda (evt) (set-box! captured (cons (event-ev evt) (unbox captured)))))
  (define msgs (make-test-messages 10))
  (define strategy (compaction-strategy 5 3))
  (ctx-compact msgs strategy #:bus bus #:session-id "test-session")
  (define topics (unbox captured))
  (check-true (and (member "compaction.start" topics) #t))
  (check-true (and (member "compaction.end" topics) #t)))

;; ============================================================
;; ctx-compact-async
;; ============================================================

(test-case "ctx-compact-async: returns thread"
  (reset-rate-limit!)
  (define msgs (make-test-messages 10))
  (define strategy (compaction-strategy 5 3))
  (define thd (ctx-compact-async msgs strategy))
  (check-true (thread? thd))
  ;; Wait for completion
  (thread-wait thd))

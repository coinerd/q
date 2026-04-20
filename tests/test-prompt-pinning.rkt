#lang racket/base

;; tests/test-prompt-pinning.rkt — Tests for first-user-prompt pinning (#1380)
;; Wave 4 of v0.13.0: Pin first user prompt in context assembly

(require rackunit
         "../util/protocol-types.rkt"
         "../runtime/context-builder.rkt")

;; Helper: create a simple message
(define (make-test-msg role kind text [id (format "msg-~a" (random 100000))])
  (make-message id #f role kind (list (make-text-part text)) (current-seconds) (hasheq)))

;; ── Test 1: First user message survives truncation ──
(test-case "first user message survives truncation to budget"
  ;; Build 10 messages where first is user, rest are alternating assistant/user
  (define msgs
    (cons (make-test-msg 'user 'message "ORIGINAL TASK: analyze the codebase")
          (for/list ([i (in-range 1 10)])
            (make-test-msg (if (odd? i) 'assistant 'user)
                           'message
                           (format "response ~a with some content" i)))))
  ;; Truncate to a tiny budget that will drop most messages
  (define result (truncate-messages-to-budget msgs 50))
  ;; First user message must be in result
  (define first-user (for/first ([m (in-list msgs)]
                                  #:when (eq? (message-role m) 'user))
                       m))
  (check-not-false (member first-user result)
                   "first user message must survive truncation"))

;; ── Test 2: First user message present when no truncation needed ──
(test-case "first user message present when no truncation needed"
  (define msgs (list (make-test-msg 'user 'message "hello")
                     (make-test-msg 'assistant 'message "hi")))
  (define result (truncate-messages-to-budget msgs 10000))
  (check-equal? result msgs "no truncation when within budget"))

;; ── Test 3: System messages still protected ──
(test-case "system messages and compaction summaries still protected"
  (define msgs (list (make-test-msg 'system 'system-instruction "system prompt")
                     (make-test-msg 'system 'compaction-summary "summary")
                     (make-test-msg 'user 'message "task")
                     (make-test-msg 'assistant 'message "response")))
  (define result (truncate-messages-to-budget msgs 50))
  ;; System and compaction-summary should be present
  (check-true (for/or ([m (in-list result)]
                        #:when (eq? (message-kind m) 'system-instruction))
                #t)
              "system-instruction preserved")
  (check-true (for/or ([m (in-list result)]
                        #:when (eq? (message-kind m) 'compaction-summary))
                #t)
              "compaction-summary preserved"))

;; ── Test 4: Pinning is identity when first user already present ──
(test-case "pinning is identity when first user already present"
  (define msgs (list (make-test-msg 'user 'message "first")
                     (make-test-msg 'assistant 'message "response")))
  (define result (truncate-messages-to-budget msgs 10000))
  (check-equal? (length result) 2))

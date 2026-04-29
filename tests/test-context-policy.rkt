#lang racket/base

;; tests/test-context-policy.rkt — Tests for runtime/context-policy.rkt
;; STABILITY: testing
;;
;; Issue #2402: W0 — Extract context-policy.rkt

(require rackunit
         racket/list
         "../runtime/context-policy.rkt"
         "../util/protocol-types.rkt"
         "../util/content-parts.rkt"
         "../llm/token-budget.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-test-message id role kind text #:parent-id [parent-id #f])
  (message id parent-id role kind (list (make-text-part text)) (current-seconds) (hasheq)))

(define (make-tool-call-msg id text tool-call-id #:parent-id [parent-id #f])
  (message id
           parent-id
           'assistant
           'message
           (list (make-text-part text)
                 (make-tool-call-part tool-call-id "function" (hasheq 'name "test")))
           (current-seconds)
           (hasheq)))

(define (make-tool-result-msg id tool-call-id text #:parent-id [parent-id #f])
  (message id
           parent-id
           'tool
           'message
           (list (make-tool-result-part tool-call-id text #f))
           (current-seconds)
           (hasheq)))

;; ============================================================
;; estimate-message-tokens
;; ============================================================

(test-case "estimate-message-tokens: basic text"
  (define msg (make-test-message "m1" 'user 'message "Hello world"))
  (define tokens (estimate-message-tokens msg))
  (check-true (positive? tokens)))

(test-case "estimate-message-tokens: empty content"
  (define msg (make-test-message "m2" 'user 'message ""))
  (define tokens (estimate-message-tokens msg))
  (check-equal? tokens 0))

(test-case "estimate-message-tokens: matches estimate-text-tokens"
  (define msg (make-test-message "m3" 'user 'message "This is a test message"))
  (define direct (estimate-text-tokens "This is a test message"))
  (define via-msg (estimate-message-tokens msg))
  (check-equal? via-msg direct))

;; ============================================================
;; Predicates
;; ============================================================

(test-case "system-message?"
  (check-true (system-message? (make-test-message "s1" 'system 'system-instruction "System prompt")))
  (check-false (system-message? (make-test-message "u1" 'user 'message "User msg"))))

(test-case "user-message?"
  (check-true (user-message? (make-test-message "u1" 'user 'message "User msg")))
  (check-false (user-message? (make-test-message "a1" 'assistant 'message "Assistant msg"))))

;; ============================================================
;; ensure-first-user-pinned
;; ============================================================

(test-case "ensure-first-user-pinned: already present"
  (define msgs
    (list (make-test-message "s" 'system 'system-instruction "sys")
          (make-test-message "u" 'user 'message "hello")))
  (define result (ensure-first-user-pinned msgs msgs))
  (check-equal? (length result) 2))

(test-case "ensure-first-user-pinned: missing, gets inserted"
  (define original
    (list (make-test-message "s" 'system 'system-instruction "sys")
          (make-test-message "u" 'user 'message "hello")
          (make-test-message "a" 'assistant 'message "hi")))
  (define result
    (list (make-test-message "s" 'system 'system-instruction "sys")
          (make-test-message "a" 'assistant 'message "hi")))
  (define pinned (ensure-first-user-pinned result original))
  (check-pred values (member (third original) pinned)))

(test-case "ensure-first-user-pinned: no user messages"
  (define msgs
    (list (make-test-message "s" 'system 'system-instruction "sys")
          (make-test-message "a" 'assistant 'message "hi")))
  (define result (ensure-first-user-pinned msgs msgs))
  (check-equal? (length result) 2))

;; ============================================================
;; build-pair-index
;; ============================================================

(test-case "build-pair-index: no tool pairs"
  (define msgs
    (list (make-test-message "u" 'user 'message "hi")
          (make-test-message "a" 'assistant 'message "hello")))
  (define-values (tr->a a->tr) (build-pair-index msgs))
  (check-equal? (hash-count tr->a) 0)
  (check-equal? (hash-count a->tr) 0))

(test-case "build-pair-index: with tool pairs"
  (define msgs
    (list (make-tool-call-msg "a1" "calling tool" "tc1")
          (make-tool-result-msg "t1" "tc1" "result" #:parent-id "a1")))
  (define-values (tr->a a->tr) (build-pair-index msgs))
  (check-equal? (hash-ref tr->a "t1" #f) "a1")
  (check-pred values (member "t1" (hash-ref a->tr "a1" '()))))

;; ============================================================
;; fit-messages-pair-preserving
;; ============================================================

(test-case "fit-messages-pair-preserving: all fit"
  (define msgs
    (list (make-test-message "u" 'user 'message "hi")
          (make-test-message "a" 'assistant 'message "hello")))
  (define result (fit-messages-pair-preserving msgs 100000))
  (check-equal? (length result) 2))

(test-case "fit-messages-pair-preserving: budget too small"
  (define msgs
    (list (make-test-message "u" 'user 'message "a longer message")
          (make-test-message "a" 'assistant 'message "another longer message")))
  (define result (fit-messages-pair-preserving msgs 1))
  (check-equal? (length result) 0))

(test-case "fit-messages-pair-preserving: tool pair kept together"
  (define msgs
    (list (make-test-message "u" 'user 'message "hi")
          (make-tool-call-msg "a1" "call" "tc1")
          (make-tool-result-msg "t1" "tc1" "result" #:parent-id "a1")))
  ;; Budget big enough for tool pair but maybe not the first user msg
  (define result (fit-messages-pair-preserving msgs 100000))
  ;; Both tool call and result should be present
  (define ids (map message-id result))
  (check-pred values (member "a1" ids) "assistant should be present")
  (check-pred values (member "t1" ids) "tool result should be present"))

(test-case "fit-messages-pair-preserving: preserves order"
  (define msgs
    (for/list ([i (in-range 10)])
      (make-test-message (format "m~a" i) 'user 'message (format "msg ~a" i))))
  (define result (fit-messages-pair-preserving msgs 100000))
  (define result-ids (map message-id result))
  (define original-ids (map message-id msgs))
  ;; Result ids should be a subsequence of original, in order
  (for ([rid (in-list result-ids)])
    (check-pred values (member rid original-ids) (format "~a should be in original" rid))))

;; ============================================================
;; requires-pair-inclusion?
;; ============================================================

(test-case "requires-pair-inclusion?: no pairs"
  (define msgs (list (make-test-message "u" 'user 'message "hi")))
  (define-values (tr->a a->tr) (build-pair-index msgs))
  (check-false (requires-pair-inclusion? "u" tr->a a->tr)))

(test-case "requires-pair-inclusion?: tool result has pair"
  (define msgs
    (list (make-tool-call-msg "a1" "call" "tc1")
          (make-tool-result-msg "t1" "tc1" "result" #:parent-id "a1")))
  (define-values (tr->a a->tr) (build-pair-index msgs))
  (check-true (requires-pair-inclusion? "t1" tr->a a->tr))
  (check-true (requires-pair-inclusion? "a1" tr->a a->tr)))

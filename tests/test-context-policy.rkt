#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-context-policy.rkt — Tests for runtime/context-policy.rkt
;; STABILITY: testing
;;
;; Issue #2402: W0 — Extract context-policy.rkt

(require rackunit
         racket/list
         "../runtime/context/context-policy.rkt"
         "../util/message/protocol-types.rkt"
         "../util/content/content-parts.rkt"
         "../llm/token-budget.rkt"
         racket/set
         (only-in "../runtime/context-assembly/serialization.rkt" compute-tier-c-count))

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
;; P6A: Tool-result part token estimation
;; ============================================================

(test-case "estimate-message-tokens: tool-result part returns > 0"
  (define content "This is the content of a file that was read")
  (define msg (make-tool-result-msg "tr1" "tc1" content #:parent-id "a1"))
  (define tokens (estimate-message-tokens msg))
  (check-true (positive? tokens)
              (format "tool-result message with content should have > 0 tokens, got ~a" tokens)))

(test-case "estimate-message-tokens: tool-result with empty content returns 0"
  (define msg (make-tool-result-msg "tr2" "tc2" "" #:parent-id "a2"))
  (define tokens (estimate-message-tokens msg))
  (check-equal? tokens 0 "tool-result message with empty content should have 0 tokens"))

(test-case "estimate-message-tokens: mixed text-part + tool-result-part"
  (define text-content "User asked a question")
  (define result-content "File contains important data")
  (define text-msg (make-test-message "m4" 'user 'message text-content))
  (define result-msg (make-tool-result-msg "tr3" "tc3" result-content #:parent-id "a3"))
  (define text-tokens (estimate-message-tokens text-msg))
  (define result-tokens (estimate-message-tokens result-msg))
  (define combined-tokens (+ text-tokens result-tokens))
  ;; Create message with both parts
  (define mixed-msg
    (message "m5"
             #f
             'assistant
             'message
             (list (make-text-part text-content) (make-tool-result-part "tc3" result-content #f))
             (current-seconds)
             (hasheq)))
  (define mixed-tokens (estimate-message-tokens mixed-msg))
  (check-equal? mixed-tokens
                combined-tokens
                (format "sum of individual tokens (~a + ~a = ~a) should equal mixed (~a)"
                        text-tokens
                        result-tokens
                        combined-tokens
                        mixed-tokens)))

(test-case "estimate-message-tokens-cached: matches uncached for tool-result"
  (define content "Some tool result content for caching test")
  (define msg (make-tool-result-msg "tr4" "tc4" content #:parent-id "a4"))
  (define uncached (estimate-message-tokens msg))
  (define cached (estimate-message-tokens-cached msg))
  (check-equal? cached uncached "cached version should match uncached for tool-result messages"))

(test-case "estimate-message-tokens-cached: matches uncached for mixed content"
  (define mixed-msg
    (message "m6"
             #f
             'assistant
             'message
             (list (make-text-part "Some text") (make-tool-result-part "tc5" "some result" #f))
             (current-seconds)
             (hasheq)))
  (define uncached (estimate-message-tokens mixed-msg))
  (define cached (estimate-message-tokens-cached mixed-msg))
  (check-equal? cached uncached "cached version should match uncached for mixed messages"))

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

(test-case "ensure-first-user-pinned: single user message"
  (define msgs (list (make-test-message "u1" 'user 'message "hello")))
  (define result (ensure-first-user-pinned msgs msgs))
  (check-equal? (length result) 1)
  (check-equal? (message-id (car result)) "u1"))

(test-case "ensure-first-user-pinned: result is empty list"
  (define original
    (list (make-test-message "u1" 'user 'message "hello")
          (make-test-message "a1" 'assistant 'message "hi")))
  (define result (ensure-first-user-pinned '() original))
  (check-equal? (length result) 1)
  (check-equal? (message-id (car result)) "u1"))

(test-case "ensure-user-messages-pinned re-injects missing users in order"
  (define u1 (make-test-message "u1" 'user 'message "first"))
  (define a1 (make-test-message "a1" 'assistant 'message "assistant 1"))
  (define u2 (make-test-message "u2" 'user 'message "second"))
  (define a2 (make-test-message "a2" 'assistant 'message "assistant 2"))
  (define u3 (make-test-message "u3" 'user 'message "third"))
  (define original (list u1 a1 u2 a2 u3))
  (define result (ensure-user-messages-pinned (list a1 a2) original))
  (check-equal? (map message-id result)
                '("u1" "a1" "u2" "a2" "u3")
                "missing user messages must be re-inserted at their original positions"))

(test-case "ensure-user-messages-pinned is idempotent"
  (define u1 (make-test-message "u1" 'user 'message "first"))
  (define u2 (make-test-message "u2" 'user 'message "second"))
  (define original (list u1 u2))
  (define result (ensure-user-messages-pinned original original))
  (check-equal? (map message-id result)
                '("u1" "u2")
                "calling ensure-user-messages-pinned on a complete list must not change order"))

(test-case "ensure-user-messages-pinned handles empty result"
  (define u1 (make-test-message "u1" 'user 'message "first"))
  (define u2 (make-test-message "u2" 'user 'message "second"))
  (define original (list u1 u2))
  (define result (ensure-user-messages-pinned '() original))
  (check-equal? (map message-id result)
                '("u1" "u2")
                "all user messages must be re-injected when result is empty"))

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
;; v0.45.6 (SAL-02/SAL-03/TEST-03): Importance + dynamic sizing tests
;; ============================================================

(test-case "fit-messages-pair-preserving empty list"
  (check-equal? (fit-messages-pair-preserving '() 1000) '()))

(test-case "fit-messages-pair-preserving single message"
  (define msgs (list (make-test-message "m1" 'user 'message "hello")))
  (check-equal? (length (fit-messages-pair-preserving msgs 1000)) 1))

(test-case "message-importance defaults to normal"
  (define msg (make-test-message "m1" 'user 'message "hello"))
  (check-equal? (message-importance msg) 'normal))

(test-case "message-importance reads from meta"
  (define msg
    (message "m1"
             #f
             'user
             'message
             (list (make-text-part "hello"))
             (current-seconds)
             (hasheq 'importance 'critical)))
  (check-equal? (message-importance msg) 'critical))

(test-case "message-elevated-importance? detects critical"
  (define msg
    (message "m1"
             #f
             'user
             'message
             (list (make-text-part "hello"))
             (current-seconds)
             (hasheq 'importance 'critical)))
  (check-not-false (message-elevated-importance? msg)))

(test-case "message-elevated-importance? normal is not elevated"
  (define msg (make-test-message "m1" 'user 'message "hello"))
  (check-false (message-elevated-importance? msg)))

(test-case "fit-messages-with-importance-rescue rescues critical messages"
  (define normal-msgs
    (for/list ([i (in-range 30)])
      (make-test-message (format "n~a" i)
                         'user
                         'message
                         (format "Normal message ~a with enough text to use tokens" i))))
  (define critical-msg
    (message "crit-1"
             #f
             'user
             'message
             (list (make-text-part "CRITICAL DECISION: use approach X"))
             (current-seconds)
             (hasheq 'importance 'critical)))
  (define msgs (append normal-msgs (list critical-msg)))
  ;; Small budget should still rescue the critical message
  (define fitted (fit-messages-with-importance-rescue msgs 500))
  (define fitted-ids (list->set (map message-id fitted)))
  (check-true (set-member? fitted-ids "crit-1")
              (format "Expected crit-1 in fitted, got ~a" (set->list fitted-ids))))

(test-case "fit-messages-with-importance-rescue no important messages"
  (define msgs
    (for/list ([i (in-range 10)])
      (make-test-message (format "m~a" i) 'user 'message (format "msg ~a" i))))
  (define result (fit-messages-with-importance-rescue msgs 100000))
  ;; Should behave same as regular fit
  (check-equal? (length result) 10))

(test-case "dynamic-tier-c-count scales with message count"
  (check-equal? (compute-tier-c-count 100) 4) ; 100/50 = 2, max(4,2) = 4
  (check-equal? (compute-tier-c-count 400) 8) ; 400/50 = 8
  (check-equal? (compute-tier-c-count 600) 12) ; 600/50 = 12
  (check-equal? (compute-tier-c-count 50) 4) ; 50/50 = 1, max(4,1) = 4
  (check-equal? (compute-tier-c-count 1000) 12)) ; 1000/50 = 20, min(12,20) = 12

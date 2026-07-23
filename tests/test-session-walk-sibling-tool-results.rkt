#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; W0: Sibling tool result tests for runtime/context-assembly/session-walk.rkt
;; Tests that build-session-context includes sibling tool results that share
;; a parentId with a tool-result already in the linear branch path.

(require rackunit
         "../runtime/session-index/schema.rkt"
         "../runtime/session-index/query.rkt"
         "../runtime/context-assembly/session-walk.rkt"
         (only-in "../util/message/message.rkt"
                  message-id
                  message-parent-id
                  message-kind
                  make-message))

;; ============================================================
;; Helper: build a session-index with controlled relationships
;; ============================================================

;; Creates a session-index from a list of entries.
;; Each entry is a message with explicit parentId.
;; The last entry becomes the active leaf.
(define (make-test-index entries)
  (define by-id
    (for/fold ([h (hash)]) ([e (in-list entries)])
      (hash-set h (message-id e) e)))
  (define children
    (for/fold ([h (hash)]) ([e (in-list entries)])
      (define pid (message-parent-id e))
      (if pid
          (hash-update h pid (lambda (lst) (cons e lst)) '())
          h)))
  (define all-ids
    (for/list ([e (in-list entries)])
      (message-id e)))
  (define leaf-id (last all-ids))
  (session-index by-id children (list->vector entries) (hash) (box leaf-id) (make-semaphore 1)))

;; ============================================================
;; Helper messages — build a chain: user → assistant → ...
;; ============================================================

(define id-counter 0)
(define (next-id)
  (set! id-counter (add1 id-counter))
  (format "id~a" id-counter))

(define (make-user-msg)
  (make-message (next-id) #f 'user 'normal '() (current-seconds) #f))

(define (make-assistant-msg parent-id tool-call-ids)
  (make-message (next-id)
                parent-id
                'assistant
                'normal
                (list (vector 'tool_calls tool-call-ids))
                (current-seconds)
                #f))

(define (make-tool-result-msg parent-id)
  (make-message (next-id) parent-id 'tool 'tool-result '() (current-seconds) #f))

(define (make-response-msg parent-id)
  (make-message (next-id) parent-id 'assistant 'normal '() (current-seconds) #f))

;; ============================================================
;; Tests
;; ============================================================

(test-case "W0: single tool result — unchanged"
  ;; user → assistant(tool_calls=[A]) → tool_result_A → response
  (define user (make-user-msg))
  (define asst (make-assistant-msg (message-id user) '("call_A")))
  ;; tool_result_A has parentId = asst
  (define tr-a (make-tool-result-msg (message-id asst)))
  (define resp (make-response-msg (message-id tr-a)))
  (define idx (make-test-index (list user asst tr-a resp)))
  (define ctx (build-session-context idx))
  (define tool-results (filter (λ (m) (eq? (message-kind m) 'tool-result)) ctx))
  (check-equal? (length tool-results) 1 "single tool result remains"))

(test-case "W0: sibling tool results — both included"
  ;; user → assistant(tool_calls=[A,B]) → tool_result_A → tool_result_B → response
  ;; Both tool results share parentId = assistant
  (define user (make-user-msg))
  (define asst (make-assistant-msg (message-id user) '("call_A" "call_B")))
  (define tr-a (make-tool-result-msg (message-id asst)))
  (define tr-b (make-tool-result-msg (message-id asst)))
  (define resp (make-response-msg (message-id tr-b)))
  (define idx (make-test-index (list user asst tr-a tr-b resp)))
  (define ctx (build-session-context idx))
  (define tool-results (filter (λ (m) (eq? (message-kind m) 'tool-result)) ctx))
  (check-equal? (length tool-results) 2 "both sibling tool results in context"))

(test-case "W0: sibling tool results — chronological order preserved"
  ;; Verify that tool_result_A appears before tool_result_B in the context
  (define user (make-user-msg))
  (define asst (make-assistant-msg (message-id user) '("call_A" "call_B")))
  (define tr-a (make-tool-result-msg (message-id asst)))
  (define tr-b (make-tool-result-msg (message-id asst)))
  (define resp (make-response-msg (message-id tr-b)))
  (define idx (make-test-index (list user asst tr-a tr-b resp)))
  (define ctx (build-session-context idx))
  (define tool-results (filter (λ (m) (eq? (message-kind m) 'tool-result)) ctx))
  ;; First tool result should be tr-a (earlier in entry-order)
  (check-equal? (message-id (car tool-results)) (message-id tr-a))
  (check-equal? (message-id (cadr tool-results)) (message-id tr-b)))

(test-case "W0: three sibling tool results — all included"
  (define user (make-user-msg))
  (define asst (make-assistant-msg (message-id user) '("call_A" "call_B" "call_C")))
  (define tr-a (make-tool-result-msg (message-id asst)))
  (define tr-b (make-tool-result-msg (message-id asst)))
  (define tr-c (make-tool-result-msg (message-id asst)))
  (define resp (make-response-msg (message-id tr-c)))
  (define idx (make-test-index (list user asst tr-a tr-b tr-c resp)))
  (define ctx (build-session-context idx))
  (define tool-results (filter (λ (m) (eq? (message-kind m) 'tool-result)) ctx))
  (check-equal? (length tool-results) 3 "all three sibling tool results in context"))

(test-case "W0: no tool calls — unchanged"
  (define user (make-user-msg))
  (define asst (make-assistant-msg (message-id user) '()))
  (define resp (make-response-msg (message-id asst)))
  (define idx (make-test-index (list user asst resp)))
  (define ctx (build-session-context idx))
  (check-equal? (length ctx) 3 "user, assistant, response all present"))

(test-case "W0: interleaved sibling tool results in linear chain"
  ;; user → assistant(tool_calls=[A,B]) → tool_result_A → response_2 → tool_result_B → response_3
  ;; Where tool_result_B is in the branch (response_3 → tr_b → asst → user)
  ;; but tool_result_A is a sibling NOT in the branch
  (define user (make-user-msg))
  (define asst (make-assistant-msg (message-id user) '("call_A" "call_B")))
  (define tr-a (make-tool-result-msg (message-id asst)))
  (define resp2 (make-response-msg (message-id tr-a)))
  (define tr-b (make-tool-result-msg (message-id asst)))
  (define resp3 (make-response-msg (message-id tr-b)))
  (define idx (make-test-index (list user asst tr-a resp2 tr-b resp3)))
  (define ctx (build-session-context idx))
  (define tool-results (filter (λ (m) (eq? (message-kind m) 'tool-result)) ctx))
  (check-equal? (length tool-results) 2 "both tool results present despite interleaving"))

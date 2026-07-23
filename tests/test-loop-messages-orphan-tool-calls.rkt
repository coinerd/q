#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; W1: Orphaned tool_call stripping tests for agent/loop-messages.rkt
;; Regression tests for v0.99.61 — orphaned tool call 400 error fix.

(require rackunit
         "../util/message/message.rkt"
         "../util/content/content-parts.rkt"
         "../agent/loop-messages.rkt")

;; ============================================================
;; Helpers for building test messages
;; ============================================================

(define (make-user text)
  (make-message (gensym 'id_) #f 'user 'normal (list (make-text-part text)) (current-seconds) #f))

(define (make-assistant text [tool-calls '()])
  (make-message (gensym 'id_)
                #f
                'assistant
                'normal
                (cons (make-text-part text)
                      (for/list ([tc (in-list tool-calls)])
                        (match-define (list id name args) tc)
                        (make-tool-call-part id name args)))
                (current-seconds)
                #f))

(define (make-tool-result tool-call-id content)
  (make-message (gensym 'id_)
                #f
                'tool
                'tool-result
                (list (make-tool-result-part tool-call-id content #f))
                (current-seconds)
                #f))

;; ============================================================
;; Test cases
;; ============================================================

(test-case "W1: paired tool_calls and tool results are preserved"
  (define msgs
    (list (make-user "hello")
          (make-assistant "searching" '(("call_1" "search" "{}")))
          (make-tool-result "call_1" "result")))
  (define result (build-raw-messages msgs))
  (check-equal? (length result) 3)
  (check-true (pair? (hash-ref (second result) 'tool_calls #f))))

(test-case "W1: orphaned tool_calls are stripped"
  (define msgs
    (list (make-user "hello")
          (make-assistant "searching" '(("call_orphan_1" "search" "{}")))
          (make-user "cancel")))
  (define result (build-raw-messages msgs))
  (check-equal? (length result) 3)
  (check-false (hash-ref (second result) 'tool_calls #f)))

(test-case "W1: partial orphans keep only answered tool_calls"
  (define msgs
    (list (make-user "do two things")
          (make-assistant "doing both" '(("call_a" "search" "{}") ("call_b" "calc" "{}")))
          (make-tool-result "call_a" "result a")))
  (define result (build-raw-messages msgs))
  (check-equal? (length result) 3)
  (define tcs (hash-ref (second result) 'tool_calls #f))
  (check-equal? (length tcs) 1)
  (check-equal? (hash-ref (first tcs) 'id #f) "call_a"))

(test-case "W1: text content preserved when tool_calls stripped"
  (define msgs
    (list (make-user "hello")
          (make-assistant "my text" '(("call_x" "search" "{}")))
          (make-user "never mind")))
  (define result (build-raw-messages msgs))
  (check-false (hash-ref (second result) 'tool_calls #f))
  (check-equal? (hash-ref (second result) 'content #f) "my text"))

(test-case "W1: orphaned tool results still dropped (existing behavior)"
  (define msgs (list (make-user "hello") (make-tool-result "call_nonexistent" "orphan result")))
  (define result (build-raw-messages msgs))
  (check-equal? (length result) 1))

(test-case "W1: trailing assistant trimmed (existing behavior)"
  (define msgs (list (make-user "hello") (make-assistant "trailing" '())))
  (define result (build-raw-messages msgs))
  (check-equal? (length result) 1))

(test-case "W1: multiple answered tool_calls preserved"
  (define msgs
    (list (make-user "do both")
          (make-assistant "doing both" '(("call_a" "search" "{}") ("call_b" "calc" "{}")))
          (make-tool-result "call_a" "result a")
          (make-tool-result "call_b" "result b")))
  (define result (build-raw-messages msgs))
  (check-equal? (length result) 4)
  (define tcs (hash-ref (second result) 'tool_calls #f))
  (check-equal? (length tcs) 2)
  (check-equal? (hash-ref (first tcs) 'id #f) "call_a")
  (check-equal? (hash-ref (second tcs) 'id #f) "call_b"))

(test-case "W1: no tool_calls — unchanged"
  (define msgs (list (make-user "hello") (make-assistant "just text" '())))
  (define result (build-raw-messages msgs))
  (check-equal? (length result) 1))

#lang racket/base

;; Test that context builder preserves tool_call/tool_result pairing
;; during truncation. Unpaired messages cause 400 "messages parameter illegal"
;; errors from the API.

(require rackunit
         racket/list
         "../util/protocol-types.rkt"
         "../runtime/context-builder.rkt"
         "../llm/token-budget.rkt")

(define (make-text-msg role text [kind 'message])
  (make-message (format "id-~a" (random 100000))
                #f
                role
                kind
                (list (make-text-part text))
                (current-seconds)
                (hasheq)))

(define (make-assistant-with-tool-call tool-call-id tool-name args)
  (define id (format "id-~a" (random 100000)))
  (make-message id
                #f
                'assistant
                'message
                (list (make-text-part "") (make-tool-call-part tool-call-id tool-name args))
                (current-seconds)
                (hasheq)))

(define (make-tool-result-msg tool-call-id parent-id result-text [is-error? #f])
  (make-message
   (format "id-~a" (random 100000))
   parent-id
   'tool
   'tool-result
   (list (make-tool-result-part tool-call-id (list (make-text-part result-text)) is-error?))
   (current-seconds)
   (hasheq)))

;; Helper: check that every tool result has a corresponding assistant with tool_calls
(define (check-pairing messages label)
  (for ([m (in-list messages)])
    (when (eq? (message-role m) 'tool)
      ;; Find the parent assistant
      (define pid (message-parent-id m))
      (define parent (findf (lambda (mm) (equal? (message-id mm) pid)) messages))
      (check-not-false
       parent
       (format "~a: tool result ~a has no parent assistant ~a" label (message-id m) pid)))))

;; Test 1: Simple pair preservation
(test-case "truncate preserves tool_call/tool_result pair"
  (define sys (make-text-msg 'system "system prompt" 'system-instruction))
  (define user (make-text-msg 'user "hello"))
  ;; Create 5 assistant+tool pairs with large tool results
  (define pairs
    (for/list ([i (in-range 5)])
      (define tc-id (format "tc-~a" i))
      (define ass (make-assistant-with-tool-call tc-id "bash" "{\"command\":\"ls\"}"))
      (define tr
        (make-tool-result-msg tc-id
                              (message-id ass)
                              (make-string (* 1000 (add1 i)) #\X))) ; large result
      (list ass tr)))

  (define all-msgs (append (list sys user) (append* pairs)))
  ;; Set a tight budget that only fits the last 2-3 pairs
  (define result (truncate-messages-to-budget all-msgs 5000))

  ;; Every tool result must have its parent assistant in the result
  (check-pairing result "tight-budget"))

;; Test 2: No orphan tool results
(test-case "no orphan tool results after truncation"
  (define sys (make-text-msg 'system "sys" 'system-instruction))
  (define user (make-text-msg 'user "do stuff"))
  (define pairs
    (for/list ([i (in-range 10)])
      (define tc-id (format "tc-~a" i))
      (define ass (make-assistant-with-tool-call tc-id "bash" (format "{\"command\":\"echo ~a\"}" i)))
      (define tr (make-tool-result-msg tc-id (message-id ass) (format "result-~a" i)))
      (list ass tr)))

  (define all-msgs (append (list sys user) (append* pairs)))
  ;; Very tight budget
  (define result (truncate-messages-to-budget all-msgs 500))

  (check-pairing result "very-tight-budget"))

;; Test 3: All pairs preserved when budget is sufficient
(test-case "all pairs preserved with sufficient budget"
  (define sys (make-text-msg 'system "sys" 'system-instruction))
  (define user (make-text-msg 'user "hello"))
  (define pairs
    (for/list ([i (in-range 3)])
      (define tc-id (format "tc-~a" i))
      (define ass (make-assistant-with-tool-call tc-id "read" "{\"path\":\"file.txt\"}"))
      (define tr (make-tool-result-msg tc-id (message-id ass) "file contents"))
      (list ass tr)))

  (define all-msgs (append (list sys user) (append* pairs)))
  ;; Large budget
  (define result (truncate-messages-to-budget all-msgs 1000000))

  (check-pairing result "large-budget")
  (check-equal? (length result) (length all-msgs)))

#lang racket/base

;; tests/test-record-conclusion.rkt — tests for record_conclusion tool
;; v0.76.0 W0: Tool implementation + persistence wiring

(require rackunit
         rackunit/text-ui
         "../tools/builtins/record-conclusion.rkt"
         "../tools/tool.rkt"
         (only-in "../agent/event-bus.rkt" make-event-bus publish!)
         (only-in "../util/event.rkt" make-event)
         "../runtime/session-events.rkt"
         "../runtime/session-types.rkt"
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion?
                  task-conclusion-text
                  task-conclusion-category
                  task-conclusion-fsm-state-origin
                  task-conclusion-relevance-tags)
         (submod "../runtime/session-types.rkt" internal))

;; Helper: call tool handler directly
(define (call-tool t args)
  ((tool-execute t) args #f))

;; Helper: call tool with exec-ctx that captures events
(define (call-tool-with-ctx t args)
  (define captured-events '())
  (define mock-ctx
    ((dynamic-require "../tools/exec-context.rkt" 'make-exec-context)
     #:event-publisher (lambda (event-type payload)
                         (set! captured-events (cons (cons event-type payload) captured-events)))))
  (define result ((tool-execute t) args mock-ctx))
  (values result captured-events))

(define suite
  (test-suite "record-conclusion"

    ;; ── Tool metadata ──

    (test-case "record_conclusion is a valid tool"
      (check-true (tool? record_conclusion))
      (check-equal? (tool-name record_conclusion) "record_conclusion"))

    ;; ── Handler validation ──

    (test-case "record_conclusion creates conclusion with text"
      (define result (call-tool record_conclusion (hasheq 'text "Found the bug")))
      (check-false (tool-result-is-error? result)))

    (test-case "record_conclusion rejects missing text"
      (define result (call-tool record_conclusion (hasheq)))
      (check-true (tool-result-is-error? result)))

    (test-case "record_conclusion rejects non-string text"
      (define result (call-tool record_conclusion (hasheq 'text 42)))
      (check-true (tool-result-is-error? result)))

    (test-case "record_conclusion rejects invalid category"
      (define result (call-tool record_conclusion (hasheq 'text "test" 'category "invalid-category")))
      (check-true (tool-result-is-error? result)))

    (test-case "record_conclusion accepts all valid categories"
      (for ([cat (in-list '("fact" "decision" "pattern" "error-cause" "test-result"))])
        (define result (call-tool record_conclusion (hasheq 'text "test" 'category cat)))
        (check-false (tool-result-is-error? result) (format "category ~a should be accepted" cat))))

    (test-case "record_conclusion defaults category to fact"
      (define result (call-tool record_conclusion (hasheq 'text "default test")))
      (check-false (tool-result-is-error? result)))

    (test-case "record_conclusion with tags"
      (define result (call-tool record_conclusion (hasheq 'text "test" 'tags '("bug" "pattern"))))
      (check-false (tool-result-is-error? result)))

    (test-case "record_conclusion returns success with conclusion_id"
      (define result (call-tool record_conclusion (hasheq 'text "test conclusion")))
      (check-false (tool-result-is-error? result))
      (define content (tool-result-content result))
      (check-true (list? content))
      (check-true (> (length content) 1)))

    ;; ── Event emission ──

    (test-case "record_conclusion emits tool.record_conclusion.completed event"
      (define-values (result events)
        (call-tool-with-ctx record_conclusion (hasheq 'text "event test" 'category "fact")))
      (check-false (tool-result-is-error? result))
      (check-true (> (length events) 0))
      (define ev (assoc "tool.record_conclusion.completed" events))
      (check-not-false ev)
      (check-equal? (hash-ref (cdr ev) 'text #f) "event test")
      (check-equal? (hash-ref (cdr ev) 'category #f) "fact"))

    (test-case "record_conclusion event includes tags"
      (define-values (result events)
        (call-tool-with-ctx record_conclusion (hasheq 'text "tagged" 'tags '("a" "b"))))
      (define ev (assoc "tool.record_conclusion.completed" events))
      (check-not-false ev)
      (define tags (hash-ref (cdr ev) 'tags #f))
      (check-equal? tags '(a b)))

    ;; ── Session event handler integration ──

    (test-case "session handler persists conclusion from event"
      (define bus (make-event-bus))
      (define sess
        (agent-session "test-sess"
                       "/tmp/q-test-record"
                       #f
                       #f
                       bus
                       #f
                       #f
                       '()
                       #f
                       #f
                       #f
                       #f
                       #t
                       0
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       'idle
                       '()
                       '()))
      (wire-session-event-handlers! sess (lambda (s e) s))
      ;; Publish a record_conclusion event
      (publish! bus
                (make-event
                 "tool.record_conclusion.completed"
                 (current-seconds)
                 "test-sess"
                 #f
                 (hasheq 'text "handler test" 'conclusion-id "c123" 'category "fact" 'tags '())))
      ;; Allow event processing (synchronous bus)
      (define conclusions (agent-session-task-conclusions sess))
      (check-true (> (length conclusions) 0))
      (define c (car conclusions))
      (check-true (task-conclusion? c))
      (check-equal? (task-conclusion-text c) "handler test")
      (check-equal? (task-conclusion-category c) 'fact)
      (check-equal? (task-conclusion-fsm-state-origin c) 'idle))

    (test-case "session handler sets fsm-state-origin to current state"
      (define bus (make-event-bus))
      (define sess
        (agent-session "test-sess"
                       "/tmp/q-test-record"
                       #f
                       #f
                       bus
                       #f
                       #f
                       '()
                       #f
                       #f
                       #f
                       #f
                       #t
                       0
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       'implementation
                       '()
                       '()))
      (wire-session-event-handlers! sess (lambda (s e) s))
      (publish! bus
                (make-event "tool.record_conclusion.completed"
                            (current-seconds)
                            "test-sess"
                            #f
                            (hasheq 'text "state test" 'category "decision" 'tags '())))
      (define conclusions (agent-session-task-conclusions sess))
      (check-true (> (length conclusions) 0))
      (check-equal? (task-conclusion-fsm-state-origin (car conclusions)) 'implementation))))

(run-tests suite)

#lang racket/base

;; tests/test-event-roundtrip.rkt — Round-trip tests for event serialization
;; M-12: Verify that all registered events survive JSON serialization round-trip.

(require rackunit
         rackunit/text-ui
         racket/string
         "../agent/event-json.rkt"
         "../agent/event-structs/typed-event-predicates.rkt"
         "../util/event-macro.rkt")

(define (check-round-trip type-str make-fn fields-spec)
  (define evt (apply make-fn type-str 100 "session-1" "turn-1" fields-spec))
  (define jsexpr (typed-event->jsexpr evt))
  (define result (jsexpr->typed-event jsexpr))
  (check-equal? (typed-event-type result)
                type-str
                (format "round-trip type mismatch for ~a" type-str))
  (check-equal? (typed-event-session-id result)
                "session-1"
                (format "round-trip session-id mismatch for ~a" type-str))
  (check-equal? (typed-event-turn-id result)
                "turn-1"
                (format "round-trip turn-id mismatch for ~a" type-str)))

(define-test-suite
 event-roundtrip-suite
 ;; Turn events
 (test-case "turn.started round-trip"
   (check-round-trip "turn.started" turn-start-event '("gpt-4" "openai")))
 (test-case "turn.completed round-trip"
   (check-round-trip "turn.completed" turn-end-event '("done" 1500)))
 ;; Message events
 (test-case "message.started round-trip"
   (check-round-trip "message.started" message-start-event '("assistant" "gpt-4")))
 (test-case "message.updated round-trip"
   (check-round-trip "message.updated" message-update-event '("hello" " world")))
 (test-case "message.completed round-trip"
   (check-round-trip "message.completed" message-end-event '("assistant" 42)))
 ;; Tool execution events
 (test-case "tool.execution.started round-trip"
   (check-round-trip "tool.execution.started" tool-execution-start-event '("bash" "tc-1")))
 (test-case "tool.execution.completed round-trip"
   (check-round-trip "tool.execution.completed" tool-execution-end-event '("bash" 200 "ok")))
 ;; Tool call/result
 (test-case "tool.called round-trip"
   (check-round-trip "tool.called" tool-call-event '("bash" (hasheq 'cmd "ls") "tc-1")))
 (test-case "tool.result round-trip"
   (check-round-trip "tool.result" tool-result-event '("tc-1" "output" #f)))
 ;; Provider events
 (test-case "model.request.started round-trip"
   (check-round-trip "model.request.started" provider-request-event '("gpt-4" "openai")))
 (test-case "model.request.completed round-trip"
   (check-round-trip "model.request.completed" provider-response-event '("gpt-4" "openai" 350)))
 ;; Session events
 (test-case "session.started round-trip"
   (check-round-trip "session.started" session-start-event '("gpt-4")))
 (test-case "agent.completed round-trip"
   (check-round-trip "agent.completed" agent-end-event '("done" 5000)))
 (test-case "context.built round-trip"
   (check-round-trip "context.built" context-event '(4096 8192)))
 ;; Stream events
 (test-case "model.stream.delta round-trip"
   (check-round-trip "model.stream.delta" stream-delta-event '("hello")))
 (test-case "model.stream.completed round-trip"
   (check-round-trip "model.stream.completed"
                     stream-completed-event
                     (list (hasheq 'tokens 100) "stop" #f)))
 (test-case "model.stream.completed truncated round-trip"
   (check-round-trip "model.stream.completed" stream-completed-event (list (hasheq) "length" #t)))
 ;; Blocked/cancelled events
 (test-case "model.request.blocked round-trip"
   (check-round-trip "model.request.blocked" model-request-blocked-event '("rate-limited")))
 (test-case "turn.cancelled round-trip"
   (check-round-trip "turn.cancelled" turn-cancelled-event '("user-interrupt" #f)))
 ;; Input event
 (test-case "input round-trip"
   (check-round-trip "input" input-event '("text" "hello")))
 ;; Model select
 (test-case "model.selected round-trip"
   (check-round-trip "model.selected" model-select-event '("gpt-4" "openai")))
 ;; Streaming provider events
 (test-case "provider.stream.delta round-trip"
   (check-round-trip "provider.stream.delta" model-stream-delta-event '("hello" "gpt-4")))
 (test-case "provider.stream.completed round-trip"
   (check-round-trip "provider.stream.completed" model-stream-completed-event '("gpt-4" "openai")))
 ;; Tool-specific events via type string
 (test-case "tool.bash.called round-trip"
   (define evt
     (bash-tool-call-event "tool.bash.called"
                           100
                           "s1"
                           "t1"
                           "bash"
                           (hasheq)
                           "tc-1"
                           "ls -la"
                           30
                           "/tmp"))
   (define j (typed-event->jsexpr evt))
   (define r (jsexpr->typed-event j))
   (check-equal? (typed-event-type r) "tool.bash.called")
   (check-equal? (bash-tool-call-event-command r) "ls -la")
   (check-equal? (bash-tool-call-event-timeout r) 30)
   (check-equal? (bash-tool-call-event-cwd r) "/tmp"))
 (test-case "tool.edit.called round-trip"
   (define evt
     (edit-tool-call-event "tool.edit.called" 100 "s1" "t1" "edit" (hasheq) "tc-1" "file.rkt" '()))
   (define j (typed-event->jsexpr evt))
   (define r (jsexpr->typed-event j))
   (check-equal? (typed-event-type r) "tool.edit.called")
   (check-equal? (edit-tool-call-event-path r) "file.rkt"))
 (test-case "unknown event falls back to typed-event"
   (define j (hasheq 'type "unknown.event" 'timestamp 100 'sessionId "s" 'turnId "t"))
   (define r (jsexpr->typed-event j))
   (check-equal? (typed-event-type r) "unknown.event")
   (check-true (typed-event? r)))
 ;; Registry completeness check
 (test-case "all known event types have registered serializers"
   (for ([type (all-known-event-types)])
     (check-not-false (lookup-event-serializer type) (format "missing serializer for ~a" type))
     (check-not-false (lookup-event-deserializer type) (format "missing deserializer for ~a" type)))))

(run-tests event-roundtrip-suite)

#lang racket/base

;; test-typed-event-emission.rkt — Tests for typed event emission bridge
;; v0.29.2 W0: Test scaffolding (bridge function does not exist yet — tests should FAIL)

(require rackunit
         racket/base
         (only-in "../agent/event-structs/base.rkt"
                  typed-event typed-event? typed-event-type typed-event-timestamp)
         (only-in "../agent/event-structs/turn-events.rkt"
                  turn-start-event turn-start-event?
                  turn-end-event turn-end-event?)
         (only-in "../agent/event-structs/message-events.rkt"
                  message-start-event message-start-event?
                  message-end-event message-end-event?)
         (only-in "../agent/event-structs/provider-events.rkt"
                  provider-request-event provider-request-event?)
         (only-in "../agent/event-structs/tool-events.rkt"
                  tool-execution-start-event tool-execution-start-event?
                  tool-execution-end-event tool-execution-end-event?
                  tool-call-event tool-call-event?)
         (only-in "../agent/event-structs/session-events.rkt"
                  session-start-event session-start-event?
                  context-event context-event?)
         (only-in "../agent/event-bus.rkt"
                  make-event-bus
                  event-bus?
                  subscribe!)
         (only-in "../agent/event-emitter.rkt"
                  emit-typed-event!
                  event-struct->hasheq))

;; ── Helper: capture events ──

(define (make-capturing-bus)
  (define bus (make-event-bus))
  (define captured '())
  (subscribe! bus (λ (evt) (set! captured (cons evt captured))))
  (values bus (λ () (reverse captured))))

;; ── event-struct->hasheq serialization ──

(test-case "event-struct->hasheq: turn-start-event preserves fields"
  (define evt (turn-start-event "turn.started" 1000 "sess1" "turn1" "gpt-4" "openai"))
  (define h (event-struct->hasheq evt))
  (check-equal? (hash-ref h 'type #f) "turn.started")
  (check-equal? (hash-ref h 'model #f) "gpt-4")
  (check-equal? (hash-ref h 'provider #f) "openai"))

(test-case "event-struct->hasheq: tool-call-event preserves fields"
  (define evt (tool-call-event "tool.call" 1000 "sess1" "turn1" "bash" (hasheq 'cmd "ls") "tc1"))
  (define h (event-struct->hasheq evt))
  (check-equal? (hash-ref h 'tool-name #f) "bash")
  (check-equal? (hash-ref h 'tool-call-id #f) "tc1"))

(test-case "event-struct->hasheq: message-end-event preserves fields"
  (define evt (message-end-event "message.end" 1000 "sess1" "turn1" "assistant" 42))
  (define h (event-struct->hasheq evt))
  (check-equal? (hash-ref h 'role #f) "assistant")
  (check-equal? (hash-ref h 'content-length #f) 42))

;; ── emit-typed-event! integration ──

(test-case "emit-typed-event!: publishes typed event on bus"
  (define-values (bus get-events) (make-capturing-bus))
  (define evt (turn-start-event "turn.started" 1000 "sess1" "turn1" "gpt-4" "openai"))
  (emit-typed-event! bus evt)
  (define events (get-events))
  (check-equal? (length events) 1)
  (check-equal? (event-type (car events)) "turn.started"))

(test-case "emit-typed-event!: turn-end-event"
  (define-values (bus get-events) (make-capturing-bus))
  (define evt (turn-end-event "turn.completed" 1000 "sess1" "turn1" "completed" 500))
  (emit-typed-event! bus evt)
  (define events (get-events))
  (check-equal? (length events) 1)
  (check-equal? (event-type (car events)) "turn.completed"))

(test-case "emit-typed-event!: tool-execution-start-event"
  (define-values (bus get-events) (make-capturing-bus))
  (define evt (tool-execution-start-event "tool.execution.started" 1000 "sess1" "turn1" "bash" "tc1"))
  (emit-typed-event! bus evt)
  (define events (get-events))
  (check-equal? (length events) 1))

;; ── No raw hasheq at emission sites (requires migration, tested in coverage) ──

(test-case "emit-typed-event!: context-event"
  (define-values (bus get-events) (make-capturing-bus))
  (define evt (context-event "context.built" 1000 "sess1" "turn1" 500 10))
  (emit-typed-event! bus evt)
  (define events (get-events))
  (check-equal? (length events) 1))

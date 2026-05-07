#lang racket/base

;; agent/event-emitter.rkt -- typed event emission bridge
;; v0.29.2: Bridge between typed event structs and raw event bus.
;; v0.32.2: Replace manual field-name registry with macro-generated constants.

(require (only-in racket/string string-prefix?)
         (only-in "../util/event.rkt" make-event)
         (only-in "../agent/event-bus.rkt" publish!)
         (only-in "../agent/state.rkt" state-add-event!)
         (only-in "../agent/event-structs/base.rkt"
                  typed-event?
                  typed-event-type
                  typed-event-timestamp
                  typed-event-session-id
                  typed-event-turn-id)
         ;; Import event-struct modules for *-event-fields constants
         (only-in "../agent/event-structs/turn-events.rkt"
                  turn-start-event-fields
                  turn-end-event-fields)
         (only-in "../agent/event-structs/message-events.rkt"
                  message-start-event-fields
                  message-update-event-fields
                  message-end-event-fields)
         (only-in "../agent/event-structs/iteration-events.rkt"
                  auto-retry-event-fields
                  compaction-event-fields
                  injection-event-fields)
         (only-in "../agent/event-structs/provider-events.rkt"
                  provider-request-event-fields
                  provider-response-event-fields
                  model-stream-delta-event-fields
                  model-stream-thinking-event-fields
                  model-stream-completed-event-fields)
         (only-in "../agent/event-structs/session-events.rkt"
                  session-start-event-fields
                  session-shutdown-event-fields
                  input-event-fields
                  model-select-event-fields
                  agent-start-event-fields
                  agent-end-event-fields
                  context-event-fields)
         (only-in "../agent/event-structs/tool-events.rkt"
                  tool-execution-start-event-fields
                  tool-execution-update-event-fields
                  tool-execution-end-event-fields
                  tool-call-event-fields
                  tool-result-event-fields
                  bash-tool-call-event-fields
                  edit-tool-call-event-fields
                  write-tool-call-event-fields
                  read-tool-call-event-fields
                  grep-tool-call-event-fields
                  find-tool-call-event-fields
                  custom-tool-call-event-fields)
         (only-in "../agent/event-structs/hook-events.rkt"
                  model-request-blocked-event-fields
                  message-blocked-event-fields
                  turn-cancelled-event-fields
                  assistant-message-completed-event-fields)
         (only-in "../agent/event-structs/stream-events.rkt"
                  stream-completed-event-fields
                  stream-delta-event-fields
                  stream-tool-call-delta-event-fields
                  stream-thinking-event-fields
                  stream-message-start-event-fields
                  stream-message-delta-event-fields
                  stream-message-end-event-fields
                  stream-turn-completed-event-fields
                  stream-turn-cancelled-event-fields
                  stream-tool-call-started-event-fields
                  stream-assistant-msg-completed-event-fields))

;; NOTE (v0.29.14): emit-typed-event! has 2+ production callers (runtime/session-switch.rkt).
;; Adoption is tracked by IVG check `session-switch-typed-events`.
(provide emit-typed-event!
         event-struct->hasheq
         struct-field-names)

;; Field name mappings: auto-synced from *-event-fields constants.
;; Per-tool events append tool-call-event-fields to include parent fields.
(define struct-field-names
  (hasheq 'message-start-event
          message-start-event-fields
          'message-update-event
          message-update-event-fields
          'message-end-event
          message-end-event-fields
          'provider-request-event
          provider-request-event-fields
          'provider-response-event
          provider-response-event-fields
          'model-stream-delta-event
          model-stream-delta-event-fields
          'model-stream-thinking-event
          model-stream-thinking-event-fields
          'model-stream-completed-event
          model-stream-completed-event-fields
          'session-start-event
          session-start-event-fields
          'session-shutdown-event
          session-shutdown-event-fields
          'input-event
          input-event-fields
          'model-select-event
          model-select-event-fields
          'agent-start-event
          agent-start-event-fields
          'agent-end-event
          agent-end-event-fields
          'turn-cancelled-event
          turn-cancelled-event-fields
          'context-event
          context-event-fields
          'tool-execution-start-event
          tool-execution-start-event-fields
          'tool-execution-update-event
          tool-execution-update-event-fields
          'tool-execution-end-event
          tool-execution-end-event-fields
          'tool-call-event
          tool-call-event-fields
          'tool-result-event
          tool-result-event-fields
          'bash-tool-call-event
          bash-tool-call-event-fields
          'edit-tool-call-event
          edit-tool-call-event-fields
          'write-tool-call-event
          write-tool-call-event-fields
          'read-tool-call-event
          read-tool-call-event-fields
          'grep-tool-call-event
          grep-tool-call-event-fields
          'find-tool-call-event
          find-tool-call-event-fields
          'custom-tool-call-event
          custom-tool-call-event-fields
          'turn-start-event
          turn-start-event-fields
          'turn-end-event
          turn-end-event-fields
          'auto-retry-event
          auto-retry-event-fields
          'compaction-event
          compaction-event-fields
          'injection-event
          injection-event-fields
          'model-request-blocked-event
          model-request-blocked-event-fields
          'message-blocked-event
          message-blocked-event-fields
          'assistant-message-completed-event
          assistant-message-completed-event-fields
          'stream-completed-event
          stream-completed-event-fields
          'stream-delta-event
          stream-delta-event-fields
          'stream-tool-call-delta-event
          stream-tool-call-delta-event-fields
          'stream-thinking-event
          stream-thinking-event-fields
          'stream-message-start-event
          stream-message-start-event-fields
          'stream-message-delta-event
          stream-message-delta-event-fields
          'stream-message-end-event
          stream-message-end-event-fields
          'stream-turn-completed-event
          stream-turn-completed-event-fields
          'stream-turn-cancelled-event
          stream-turn-cancelled-event-fields
          'stream-tool-call-started-event
          stream-tool-call-started-event-fields
          'stream-assistant-msg-completed-event
          stream-assistant-msg-completed-event-fields))

;; Extract struct name, stripping the struct: prefix from struct->vector
(define (struct-name evt)
  (define raw (vector-ref (struct->vector evt) 0))
  (define str (symbol->string raw))
  (if (string-prefix? str "struct:")
      (string->symbol (substring str 7))
      raw))

;; Serialize a typed event struct to a hasheq payload.
(define (event-struct->hasheq evt)
  (define vec (struct->vector evt))
  (define base
    (hasheq 'type
            (vector-ref vec 1)
            'timestamp
            (vector-ref vec 2)
            'session-id
            (vector-ref vec 3)
            'turn-id
            (vector-ref vec 4)))
  (define name (struct-name evt))
  (define fields (hash-ref struct-field-names name '()))
  (for/fold ([h base])
            ([i (in-naturals)]
             [fname (in-list fields)])
    (hash-set h fname (vector-ref vec (+ 5 i)))))

;; Emit a typed event on the bus.
;; Optional #:state for state accumulation (mirrors emit! from loop-messages).
(define (emit-typed-event! bus evt #:state [state #f])
  (define event-type-str (typed-event-type evt))
  (define payload (event-struct->hasheq evt))
  (define raw-event
    (make-event event-type-str
                (typed-event-timestamp evt)
                (typed-event-session-id evt)
                (typed-event-turn-id evt)
                payload))
  (publish! bus raw-event)
  (when state
    (state-add-event! state raw-event))
  raw-event)

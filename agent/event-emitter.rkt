#lang racket/base

;; agent/event-emitter.rkt -- typed event emission bridge
;; v0.29.2: Bridge between typed event structs and raw event bus.

(require (only-in racket/string string-prefix?)
         (only-in "../util/event.rkt" make-event)
         (only-in "../agent/event-bus.rkt" publish!)
         (only-in "../agent/state.rkt" state-add-event!)
         (only-in "../agent/event-structs/base.rkt"
                  typed-event?
                  typed-event-type
                  typed-event-timestamp
                  typed-event-session-id
                  typed-event-turn-id))

(provide emit-typed-event!
         event-struct->hasheq)

;; Field name mappings: sub-struct fields after typed-event base (type timestamp session-id turn-id)
(define struct-field-names
  (hasheq 'message-start-event
          '(role model)
          'message-update-event
          '(content delta)
          'message-end-event
          '(role content-length)
          'provider-request-event
          '(model provider)
          'provider-response-event
          '(model provider latency-ms)
          'session-start-event
          '(model)
          'session-shutdown-event
          '(reason)
          'input-event
          '(input-type content)
          'model-select-event
          '(model provider)
          'agent-start-event
          '(model)
          'agent-end-event
          '(reason duration-ms)
          'context-event
          '(token-count window-size)
          'tool-execution-start-event
          '(tool-name tool-call-id)
          'tool-execution-update-event
          '(tool-name progress)
          'tool-execution-end-event
          '(tool-name duration-ms result-summary)
          'tool-call-event
          '(tool-name arguments tool-call-id)
          'tool-result-event
          '(tool-call-id content is-error?)
          'bash-tool-call-event
          '(command timeout cwd)
          'edit-tool-call-event
          '(path edits)
          'write-tool-call-event
          '(path content)
          'read-tool-call-event
          '(path offset limit)
          'grep-tool-call-event
          '(pattern path)
          'find-tool-call-event
          '(pattern path)
          'custom-tool-call-event
          '()
          'turn-start-event
          '(model provider)
          'turn-end-event
          '(reason duration-ms)))

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

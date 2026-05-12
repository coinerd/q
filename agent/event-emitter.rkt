#lang racket/base

;; agent/event-emitter.rkt -- typed event emission bridge
;; v0.29.2: Bridge between typed event structs and raw event bus.
;; v0.32.2: Replace manual field-name registry with macro-generated constants.

(require racket/contract
         (only-in racket/string string-prefix?)
         (only-in "../util/event.rkt" make-event event?)
         (only-in "../agent/event-bus.rkt" publish! event-bus?)
         (only-in "../agent/state.rkt" state-add-event! loop-state?)
         (only-in "../agent/event-structs/base.rkt"
                  typed-event?
                  typed-event-type
                  typed-event-timestamp
                  typed-event-session-id
                  typed-event-turn-id)
         ;; Auto-populated field registry from define-typed-event (I-12)
         (only-in "../util/event-macro.rkt" lookup-event-fields lookup-event-serializer))

;; NOTE (v0.29.14): emit-typed-event! has 2+ production callers (runtime/session-switch.rkt).
;; Adoption is tracked by IVG check `session-switch-typed-events`.
(provide (contract-out [emit-typed-event!
                        (->* (event-bus? typed-event?) (#:state (or/c loop-state? #f)) event?)]
                       [event-struct->hasheq (-> typed-event? hash?)]
                       [get-struct-field-names (-> symbol? (or/c (listof symbol?) #f))])
         typed-event-base-field-count)

;; Look up the field name list for a given event struct symbol.
;; Uses auto-populated registry from define-typed-event macro.
(define (get-struct-field-names name)
  (lookup-event-fields name))

;; Extract struct name, stripping the struct: prefix from struct->vector
(define (struct-name evt)
  (define raw (vector-ref (struct->vector evt) 0))
  (define str (symbol->string raw))
  (if (string-prefix? str "struct:")
      (string->symbol (substring str 7))
      raw))

;; Serialize a typed event struct to a hasheq payload.
;; W-02: Use accessors instead of hardcoded vector offsets for base fields.
;; typed-event has 4 base fields: type, timestamp, session-id, turn-id.
;; Subclass app fields start at vector index 5 (= 1 struct-type + 4 base fields).
(define typed-event-base-field-count 4)

(define (event-struct->hasheq evt)
  ;; R-13: Try auto-generated serializer first (fast path, no struct->vector)
  (define type-str (typed-event-type evt))
  (define serializer (lookup-event-serializer type-str))
  (if serializer
      ;; Fast path: merge base fields + serializer output
      (let ([subclass-data (serializer evt)])
        (hash-set* subclass-data
                   'type
                   (typed-event-type evt)
                   'timestamp
                   (typed-event-timestamp evt)
                   'session-id
                   (typed-event-session-id evt)
                   'turn-id
                   (typed-event-turn-id evt)))
      ;; Legacy fallback: struct->vector reflection
      (let ()
        (define vec (struct->vector evt))
        (define app-start (+ 1 typed-event-base-field-count))
        (define base
          (hasheq 'type
                  (typed-event-type evt)
                  'timestamp
                  (typed-event-timestamp evt)
                  'session-id
                  (typed-event-session-id evt)
                  'turn-id
                  (typed-event-turn-id evt)))
        (define name (struct-name evt))
        (define fields (get-struct-field-names name))
        (unless fields
          (log-warning
           (format "event-struct->hasheq: no field registry for ~a, dropping subclass fields" name)))
        (for/fold ([h base])
                  ([i (in-naturals)]
                   [fname (in-list (or fields '()))])
          (hash-set h fname (vector-ref vec (+ app-start i)))))))

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

#lang racket/base

;; util/event.rkt — Event envelope struct and serialization
;;
;; Extracted from protocol-types.rkt (ARCH-05 split).
;; Event struct with JSON serialization/deserialization.

(require racket/contract)

(provide (struct-out event)
         event-event
         make-event
         event->jsexpr
         jsexpr->event
         CURRENT-EVENT-VERSION)

;; ============================================================
;; Event envelope struct
;; ============================================================

(struct event (version ev time session-id turn-id payload) #:transparent)

(define (make-event ev time session-id turn-id payload #:version [version 1])
  (event version ev time session-id turn-id payload))

;; Accessor alias: the field is called `ev` internally to avoid
;; name collision with the struct, but the logical name is `event`.
;; Racket generates `event-ev`; we re-export as a procedure.
(define event-event event-ev)

;; Serialize event to jsexpr (hash)
(define (event->jsexpr evt)
  (hasheq 'version
          (event-version evt)
          'event
          (event-ev evt)
          'time
          (event-time evt)
          'sessionId
          (event-session-id evt)
          'turnId
          (event-turn-id evt)
          'payload
          (event-payload evt)))

;; Current event schema version produced by this runtime.
(define CURRENT-EVENT-VERSION 1)

;; Deserialize jsexpr (hash) to event.
;; For typed event deserialization, see agent/event-types.rkt:jsexpr->typed-event
;; which handles type-tagged events with structured fields.
(define (jsexpr->event h)
  (define ver (hash-ref h 'version 1))
  (when (> ver CURRENT-EVENT-VERSION)
    (log-warning "jsexpr->event: event version ~a exceeds current ~a (event: ~a)"
                 ver
                 CURRENT-EVENT-VERSION
                 (hash-ref h 'event "<unknown>")))
  (event ver
         (hash-ref h 'event)
         (hash-ref h 'time)
         (hash-ref h 'sessionId)
         (hash-ref h 'turnId)
         (hash-ref h 'payload)))

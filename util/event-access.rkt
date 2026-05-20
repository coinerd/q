#lang racket/base

;; util/event-access.rkt — selector functions for event structs
;; Provides an abstraction layer over direct struct field access.
;; Use these selectors instead of calling event-ev, event-time, etc. directly.

(require racket/contract
         "event.rkt")

;; Selectors (contract-out)
(provide (contract-out [event-type-ref (-> event? any/c)]
                       [event-timestamp-ref (-> event? any/c)]
                       [event-session-id-ref (-> event? (or/c string? #f))]
                       [event-turn-id-ref (-> event? (or/c string? #f))]
                       [event-payload-ref (-> event? any/c)])
         ;; Re-export predicates and constructors for convenience (from typed/racket module)
         event?
         make-event)

;; Selector functions
;; The struct field for event type is 'ev' (aliased as event-event).
(define (event-type-ref evt)
  (event-ev evt))

(define (event-timestamp-ref evt)
  (event-time evt))

(define (event-session-id-ref evt)
  (event-session-id evt))

(define (event-turn-id-ref evt)
  (event-turn-id evt))

(define (event-payload-ref evt)
  (event-payload evt))

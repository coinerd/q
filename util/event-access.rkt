#lang racket/base

;; util/event-access.rkt — selector functions for event structs
;; Provides an abstraction layer over direct struct field access.
;; Use these selectors instead of calling event-ev, event-time, etc. directly.

(require "event.rkt")

(provide
 ;; Selectors
 event-type-ref
 event-timestamp-ref
 event-session-id-ref
 event-turn-id-ref
 event-payload-ref
 ;; Re-export predicates and constructors for convenience
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

#lang typed/racket

;; util/event-access.rkt — selector functions for event structs
;;
;; Migrated to #lang typed/racket in v0.70.9 W1.
;; Previously used racket/contract; now relies on TR boundary contracts
;; for untyped consumers.
;;
;; Provides an abstraction layer over direct struct field access.
;; Use these selectors instead of calling event-ev, event-time, etc. directly.

(require "event.rkt")

(provide event-type-ref
         event-timestamp-ref
         event-session-id-ref
         event-turn-id-ref
         event-payload-ref
         event?
         make-event)

;; Selector functions
;; The struct field for event type is 'ev' (aliased as event-event).

(: event-type-ref : (-> event Any))
(define (event-type-ref evt)
  (event-ev evt))

(: event-timestamp-ref : (-> event Real))
(define (event-timestamp-ref evt)
  (event-time evt))

(: event-session-id-ref : (-> event (Option String)))
(define (event-session-id-ref evt)
  (event-session-id evt))

(: event-turn-id-ref : (-> event (Option String)))
(define (event-turn-id-ref evt)
  (event-turn-id evt))

(: event-payload-ref : (-> event Any))
(define (event-payload-ref evt)
  (event-payload evt))

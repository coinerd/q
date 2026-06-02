#lang typed/racket

;; util/event.rkt — Event envelope struct and serialization
;;
;; Extracted from protocol-types.rkt (ARCH-05 split).
;; Event struct with JSON serialization/deserialization.
;; Migrated to #lang typed/racket in v0.28.9 W0 (TR expansion).
;;
;; TR BOUNDARY:
;; This is a #lang typed/racket module. Untyped consumers receive
;; auto-generated contracts from TR boundary system. Struct
;; constructors enforce field types at call sites in untyped modules.

(provide event
         event?
         event-version
         event-ev
         event-time
         event-session-id
         event-turn-id
         event-payload
         event-event
         make-event
         event->jsexpr
         jsexpr->event
         CURRENT-EVENT-VERSION)

;; ============================================================
;; Event envelope struct
;; ============================================================

(struct event
        ([version : Integer] [ev : Any]
                             [time : Real]
                             [session-id : (Option String)]
                             [turn-id : (Option String)]
                             [payload : Any])
  #:transparent)

(: make-event (->* (Any Real (Option String) (Option String) Any) (Integer) event))
(define (make-event ev time session-id turn-id payload [version 1])
  (event version ev time session-id turn-id payload))

;; Accessor alias: the field is called `ev` internally to avoid
;; name collision with the struct, but the logical name is `event`.
(: event-event : (-> event Any))
(define event-event event-ev)

;; Serialize event to jsexpr (hash)
(: event->jsexpr : (-> event (HashTable Symbol Any)))
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
(: jsexpr->event : (-> (HashTable Symbol Any) event))
(define (jsexpr->event h)
  ;; I-10: Explicit field extraction replaces cast
  (define raw-ver (hash-ref h 'version (lambda () 1)))
  (define ver
    :
    Integer
    (if (exact-integer? raw-ver) raw-ver 1))
  (when (> ver CURRENT-EVENT-VERSION)
    (log-warning "jsexpr->event: event version ~a exceeds current ~a (event: ~a)"
                 ver
                 CURRENT-EVENT-VERSION
                 (hash-ref h 'event (lambda () "<unknown>"))))
  (define raw-time (hash-ref h 'time))
  (define raw-session-id (hash-ref h 'sessionId))
  (define raw-turn-id (hash-ref h 'turnId (lambda () #f)))
  (event ver
         (hash-ref h 'event)
         (if (real? raw-time) raw-time 0.0)
         (if (or (string? raw-session-id) (not raw-session-id)) raw-session-id #f)
         (if (or (string? raw-turn-id) (not raw-turn-id)) raw-turn-id #f)
         (hash-ref h 'payload)))

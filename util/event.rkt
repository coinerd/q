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

(provide (struct-out event)
         event-event
         make-event
         event->jsexpr
         jsexpr->event
         CURRENT-EVENT-VERSION)

;; ============================================================
;; Event envelope struct
;; ============================================================

(struct event ([version : Integer]
               [ev : Symbol]
               [time : Integer]
               [session-id : String]
               [turn-id : (Option String)]
               [payload : Any]) #:transparent)

(: make-event (->* (Symbol Integer String (Option String) Any) (Integer) event))
(define (make-event ev time session-id turn-id payload [version 1])
  (event version ev time session-id turn-id payload))

;; Accessor alias: the field is called `ev` internally to avoid
;; name collision with the struct, but the logical name is `event`.
(: event-event : (-> event Symbol))
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
  (define ver : Integer (cast (hash-ref h 'version (lambda () 1)) Integer))
  (when (> ver CURRENT-EVENT-VERSION)
    (log-warning "jsexpr->event: event version ~a exceeds current ~a (event: ~a)"
                 ver
                 CURRENT-EVENT-VERSION
                 (cast (hash-ref h (quote event) (lambda () "<unknown>")) String)))
  (event ver
         (cast (hash-ref h (quote event)) Symbol)
         (cast (hash-ref h (quote time)) Integer)
         (cast (hash-ref h (quote sessionId)) String)
         (cast (hash-ref h (quote turnId) (lambda () #f)) (Option String))
         (hash-ref h (quote payload))))

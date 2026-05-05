#lang racket/base

;; agent/event-structs/hook-events.rkt — hook-blocked and cancellation events
;;
;; Events emitted when hooks block operations (model requests, messages)
;; or when a turn is cancelled.

(require "base.rkt")

(provide
 ;; Blocked events
 (struct-out model-request-blocked-event)
 (struct-out message-blocked-event)
 make-model-request-blocked-event
 make-message-blocked-event
 model-request-blocked-event?
 message-blocked-event?
 ;; Turn cancelled
 (struct-out turn-cancelled-event)
 make-turn-cancelled-event
 turn-cancelled-event?
 ;; Assistant message completed
 (struct-out assistant-message-completed-event)
 make-assistant-message-completed-event
 assistant-message-completed-event?)

;; ============================================================
;; Blocked events
;; ============================================================

(struct model-request-blocked-event typed-event (reason) #:transparent)

(define (make-model-request-blocked-event #:session-id session-id
                                          #:turn-id turn-id
                                          #:timestamp timestamp
                                          #:reason reason)
  (model-request-blocked-event "model.request.blocked" timestamp session-id turn-id reason))

(struct message-blocked-event typed-event (hook reason) #:transparent)

(define (make-message-blocked-event #:session-id session-id
                                    #:turn-id turn-id
                                    #:timestamp timestamp
                                    #:hook hook
                                    #:reason reason)
  (message-blocked-event "message.blocked" timestamp session-id turn-id hook reason))

;; ============================================================
;; Turn cancelled
;; ============================================================

(struct turn-cancelled-event typed-event (reason) #:transparent)

(define (make-turn-cancelled-event #:session-id session-id
                                   #:turn-id turn-id
                                   #:timestamp timestamp
                                   #:reason reason)
  (turn-cancelled-event "turn.cancelled" timestamp session-id turn-id reason))

;; ============================================================
;; Assistant message completed
;; ============================================================

(struct assistant-message-completed-event typed-event (content-length) #:transparent)

(define (make-assistant-message-completed-event #:session-id session-id
                                                 #:turn-id turn-id
                                                 #:timestamp timestamp
                                                 #:content-length content-length)
  (assistant-message-completed-event "assistant.message.completed" timestamp session-id turn-id content-length))

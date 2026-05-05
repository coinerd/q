#lang racket/base

;; agent/event-structs/turn-events.rkt — turn lifecycle events

(require "base.rkt")

(provide (struct-out turn-start-event)
         (struct-out turn-end-event)
         make-turn-start-event
         make-turn-end-event
         turn-start-event?
         turn-end-event?)

(struct turn-start-event typed-event (model provider) #:transparent)
(struct turn-end-event typed-event (reason duration-ms) #:transparent)

(define (make-turn-start-event #:session-id session-id
                               #:turn-id turn-id
                               #:timestamp timestamp
                               #:model model
                               #:provider provider)
  (turn-start-event "turn.started" timestamp session-id turn-id model provider))

(define (make-turn-end-event #:session-id session-id
                             #:turn-id turn-id
                             #:timestamp timestamp
                             #:reason reason
                             #:duration-ms duration-ms)
  (turn-end-event "turn.completed" timestamp session-id turn-id reason duration-ms))

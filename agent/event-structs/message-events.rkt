#lang racket/base

;; agent/event-structs/message-events.rkt — message lifecycle events

(require "base.rkt")

(provide (struct-out message-start-event)
         (struct-out message-update-event)
         (struct-out message-end-event)
         make-message-start-event
         make-message-update-event
         make-message-end-event
         message-start-event?
         message-update-event?
         message-end-event?)

(struct message-start-event typed-event (role model) #:transparent)
(struct message-update-event typed-event (content delta) #:transparent)
(struct message-end-event typed-event (role content-length) #:transparent)

(define (make-message-start-event #:session-id session-id
                                  #:turn-id turn-id
                                  #:timestamp timestamp
                                  #:role role
                                  #:model model)
  (message-start-event "message-start" timestamp session-id turn-id role model))

(define (make-message-update-event #:session-id session-id
                                   #:turn-id turn-id
                                   #:timestamp timestamp
                                   #:content content
                                   #:delta delta)
  (message-update-event "message-update" timestamp session-id turn-id content delta))

(define (make-message-end-event #:session-id session-id
                                #:turn-id turn-id
                                #:timestamp timestamp
                                #:role role
                                #:content-length content-length)
  (message-end-event "message-end" timestamp session-id turn-id role content-length))

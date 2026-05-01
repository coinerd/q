#lang racket/base

;; agent/event-structs/provider-events.rkt — provider/LLM request/response events

(require "base.rkt")

(provide (struct-out provider-request-event)
         (struct-out provider-response-event)
         make-provider-request-event
         make-provider-response-event
         provider-request-event?
         provider-response-event?)

(struct provider-request-event typed-event (model provider) #:transparent)
(struct provider-response-event typed-event (model provider latency-ms) #:transparent)

(define (make-provider-request-event #:session-id session-id
                                     #:turn-id turn-id
                                     #:timestamp timestamp
                                     #:model model
                                     #:provider provider)
  (provider-request-event "provider-request" timestamp session-id turn-id model provider))

(define (make-provider-response-event #:session-id session-id
                                      #:turn-id turn-id
                                      #:timestamp timestamp
                                      #:model model
                                      #:provider provider
                                      #:latency-ms latency-ms)
  (provider-response-event "provider-response"
                           timestamp
                           session-id
                           turn-id
                           model
                           provider
                           latency-ms))

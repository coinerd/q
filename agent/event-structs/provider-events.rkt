#lang racket/base

;; agent/event-structs/provider-events.rkt — provider/LLM request/response events

(require "base.rkt")

(provide (struct-out provider-request-event)
         (struct-out provider-response-event)
         make-provider-request-event
         make-provider-response-event
         provider-request-event?
         provider-response-event?
         ;; Streaming events
         (struct-out model-stream-delta-event)
         (struct-out model-stream-thinking-event)
         (struct-out model-stream-completed-event)
         make-model-stream-delta-event
         make-model-stream-thinking-event
         make-model-stream-completed-event
         model-stream-delta-event?
         model-stream-thinking-event?
         model-stream-completed-event?)

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


;; ============================================================
;; Streaming events (LLM streaming responses)
;; ============================================================

(struct model-stream-delta-event typed-event (delta model) #:transparent)

(define (make-model-stream-delta-event #:session-id session-id
                                       #:turn-id turn-id
                                       #:timestamp timestamp
                                       #:delta delta
                                       #:model model)
  (model-stream-delta-event "model.stream.delta" timestamp session-id turn-id delta model))

(struct model-stream-thinking-event typed-event (thinking model) #:transparent)

(define (make-model-stream-thinking-event #:session-id session-id
                                          #:turn-id turn-id
                                          #:timestamp timestamp
                                          #:thinking thinking
                                          #:model model)
  (model-stream-thinking-event "model.stream.thinking" timestamp session-id turn-id thinking model))

(struct model-stream-completed-event typed-event (model provider) #:transparent)

(define (make-model-stream-completed-event #:session-id session-id
                                           #:turn-id turn-id
                                           #:timestamp timestamp
                                           #:model model
                                           #:provider provider)
  (model-stream-completed-event "model.stream.completed" timestamp session-id turn-id model provider))

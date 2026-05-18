#lang racket/base

;; agent/event-structs/provider-events.rkt — provider/LLM request/response events

(require "base.rkt"
         "../../util/event-macro.rkt")

(define-typed-event provider-request-event "model.request.started" (model provider) #:schema-version 1)

(define-typed-event provider-response-event
                    "model.request.completed"
                    (model provider latency-ms)
                    #:defaults (latency-ms 0) #:schema-version 1)

;; Streaming events (LLM streaming responses)

(define-typed-event model-stream-delta-event "provider.stream.delta" (delta model) #:schema-version 1)

(define-typed-event model-stream-thinking-event "provider.stream.thinking" (thinking model) #:schema-version 1)

(define-typed-event model-stream-completed-event "provider.stream.completed" (model provider) #:schema-version 1)


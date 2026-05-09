#lang racket/base

;; agent/event-structs/provider-events.rkt — provider/LLM request/response events

(require "base.rkt"
         "../../util/event-macro.rkt")

(define-typed-event provider-request-event "model.request.started" (model provider))

(define-typed-event provider-response-event "model.request.completed" (model provider latency-ms))

;; Streaming events (LLM streaming responses)

(define-typed-event model-stream-delta-event "provider.stream.delta" (delta model))

(define-typed-event model-stream-thinking-event "provider.stream.thinking" (thinking model))

(define-typed-event model-stream-completed-event "provider.stream.completed" (model provider))

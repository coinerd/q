#lang racket/base

;; agent/event-structs/hook-events.rkt — hook-blocked and cancellation events
;;
;; Events emitted when hooks block operations (model requests, messages)
;; or when a turn is cancelled.

(require "base.rkt"
         "../../util/event-macro.rkt")

;; Blocked events

(define-typed-event model-request-blocked-event "model.request.blocked"
  (reason))

(define-typed-event message-blocked-event "message.blocked"
  (hook reason))

;; Turn cancelled

(define-typed-event turn-cancelled-event "turn.cancelled"
  (reason)
  #:optional ([iteration #f]))

;; Assistant message completed

(define-typed-event assistant-message-completed-event "assistant.message.completed"
  (content-length))

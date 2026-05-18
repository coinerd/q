#lang racket/base

;; agent/event-structs/hook-events.rkt — hook-blocked and cancellation events
;;
;; Events emitted when hooks block operations (model requests, messages)
;; or when a turn is cancelled.

(require "base.rkt"
         "../../util/event-macro.rkt")

;; Blocked events

(define-typed-event model-request-blocked-event "model.request.blocked" (reason) #:schema-version 1)

(define-typed-event message-blocked-event "message.blocked" (hook reason) #:schema-version 1)

;; Turn cancelled

(define-typed-event turn-cancelled-event "turn.cancelled" (reason) #:optional ([iteration #f]) #:schema-version 1)

;; Assistant message completed

(define-typed-event assistant-message-completed-event
                    "assistant.message.completed"
                    (content-length)
                    #:defaults (content-length 0) #:schema-version 1)


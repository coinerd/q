#lang racket/base

;; agent/event-structs/message-events.rkt — message lifecycle events

(require "base.rkt"
         "../../util/event-macro.rkt")

(define-typed-event message-start-event "message.started"
  (role model))

(define-typed-event message-update-event "message.updated"
  (content delta))

(define-typed-event message-end-event "message.completed"
  (role content-length))

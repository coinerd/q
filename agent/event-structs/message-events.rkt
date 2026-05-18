#lang racket/base

;; agent/event-structs/message-events.rkt — message lifecycle events

(require "base.rkt"
         "../../util/event-macro.rkt")

(define-typed-event message-start-event "message.started" (role model) #:schema-version 1)

(define-typed-event message-update-event "message.updated" (content delta) #:schema-version 1)

(define-typed-event message-end-event
                    "message.completed"
                    (role content-length)
                    #:defaults (content-length 0) #:schema-version 1)


#lang racket/base

;; agent/event-structs/turn-events.rkt — turn lifecycle events

(require "base.rkt"
         "../../util/event-macro.rkt")

(define-typed-event turn-start-event "turn.started" (model provider) #:schema-version 1)

(define-typed-event turn-end-event "turn.completed" (reason duration-ms) #:defaults (duration-ms 0) #:schema-version 1)


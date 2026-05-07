#lang racket/base

;; agent/event-structs/turn-events.rkt — turn lifecycle events

(require "base.rkt"
         "../../util/event-macro.rkt")

(define-typed-event turn-start-event "turn.started"
  (model provider))

(define-typed-event turn-end-event "turn.completed"
  (reason duration-ms))

#lang racket/base

;; agent/event-structs/session-events.rkt — session, input, model, agent, context events

(require "base.rkt"
         "../../util/event-macro.rkt")

;; Session lifecycle

(define-typed-event session-start-event "session.started"
  (model))

(define-typed-event session-shutdown-event "session.shutdown"
  (reason))

;; Input events

(define-typed-event input-event "input"
  (input-type content))

;; Model events

(define-typed-event model-select-event "model.selected"
  (model provider))

;; Agent events

(define-typed-event agent-start-event "agent.started"
  (model))

(define-typed-event agent-end-event "agent.completed"
  (reason duration-ms))

;; Context events

(define-typed-event context-event "context.built"
  (token-count window-size))

#lang racket/base

;; agent/event-structs/session-events.rkt — session, input, model, agent, context events

(require "base.rkt"
         "../../util/event-macro.rkt")

;; Session lifecycle

(define-typed-event session-start-event "session.started" (model))

(define-typed-event session-shutdown-event "session.shutdown" (reason))

;; Input events

(define-typed-event input-event "input" (input-type content))

;; Model events

(define-typed-event model-select-event "model.selected" (model provider))

;; Agent events

(define-typed-event agent-start-event "agent.started" (model))

(define-typed-event agent-end-event "agent.completed" (reason duration-ms) #:defaults (duration-ms 0))

;; Context events

(define-typed-event context-event
                    "context.built"
                    (token-count window-size)
                    #:defaults (token-count 0 window-size 0))

;; v0.44.3 (R2): Context assembly lifecycle events — replaces raw hasheq payloads
(define-typed-event context-assembled-event
                    "context.assembled"
                    (iteration total-messages assembled-messages token-count
                     working-set-entries working-set-tokens)
                    #:defaults (working-set-entries 0 working-set-tokens 0))

(define-typed-event context-blocked-event
                    "context.assembly.blocked"
                    (reason))

(define-typed-event working-set-injected-event
                    "working-set.injected"
                    (entries tokens))

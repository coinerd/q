#lang racket/base

;; agent/event-structs/session-events.rkt — session, input, model, agent, context events

(require "base.rkt"
         "../../util/event-macro.rkt")

;; Session lifecycle

(define-typed-event session-start-event "session.started" (model) #:schema-version 1)

(define-typed-event session-shutdown-event "session.shutdown" (reason) #:schema-version 1)

;; Input events

(define-typed-event input-event "input" (input-type content) #:schema-version 1)

;; Model events

(define-typed-event model-select-event "model.selected" (model provider) #:schema-version 1)

;; Agent events

(define-typed-event agent-start-event "agent.started" (model) #:schema-version 1)

(define-typed-event agent-end-event "agent.completed" (reason duration-ms) #:defaults (duration-ms 0) #:schema-version 1)

;; Context events

(define-typed-event context-event
                    "context.built"
                    (token-count window-size)
                    #:defaults (token-count 0 window-size 0) #:schema-version 1)

;; v0.44.3 (R2): Context assembly lifecycle events — replaces raw hasheq payloads
(define-typed-event
 context-assembled-event
 "context.assembled"
 (iteration total-messages assembled-messages token-count working-set-entries working-set-tokens)
 #:defaults (working-set-entries 0 working-set-tokens 0))

(define-typed-event context-blocked-event "context.assembly.blocked" (reason) #:schema-version 1)

(define-typed-event working-set-injected-event "working-set.injected" (entries tokens) #:schema-version 1)

;; v0.45.5 (OBS-01/02/03): Detailed assembly metrics for observability
(define-typed-event context-assembly-detail-event
                    "context.assembly.detail"
                    (total-messages tier-a-count
                                    tier-b-count
                                    tier-c-count
                                    excluded-count
                                    excluded-ids
                                    summary-length
                                    gsd-pinned-count
                                    ws-entry-count
                                    ws-tokens
                                    cache-hit-p)
                    #:defaults (tier-a-count 0
                                             tier-b-count
                                             0
                                             tier-c-count
                                             0
                                             excluded-count
                                             0
                                             excluded-ids
                                             ""
                                             summary-length
                                             0
                                             gsd-pinned-count
                                             0
                                             ws-entry-count
                                             0
                                             ws-tokens
                                             0
                                             cache-hit-p
                                             #f) #:schema-version 1)


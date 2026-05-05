#lang racket/base

;; agent/event-structs/session-events.rkt — session, input, model, agent, context events

(require "base.rkt")

(provide
 ;; Session lifecycle
 (struct-out session-start-event)
 (struct-out session-shutdown-event)
 make-session-start-event
 make-session-shutdown-event
 session-start-event?
 session-shutdown-event?

 ;; Input events
 (struct-out input-event)
 make-input-event
 input-event?

 ;; Model events
 (struct-out model-select-event)
 make-model-select-event
 model-select-event?

 ;; Agent events
 (struct-out agent-start-event)
 (struct-out agent-end-event)
 make-agent-start-event
 make-agent-end-event
 agent-start-event?
 agent-end-event?

 ;; Context events
 (struct-out context-event)
 make-context-event
 context-event?)

;; ============================================================
;; Session lifecycle
;; ============================================================

(struct session-start-event typed-event (model) #:transparent)
(struct session-shutdown-event typed-event (reason) #:transparent)

(define (make-session-start-event #:session-id session-id
                                  #:turn-id turn-id
                                  #:timestamp timestamp
                                  #:model model)
  (session-start-event "session.started" timestamp session-id turn-id model))

(define (make-session-shutdown-event #:session-id session-id
                                     #:turn-id turn-id
                                     #:timestamp timestamp
                                     #:reason reason)
  (session-shutdown-event "session.shutdown" timestamp session-id turn-id reason))

;; ============================================================
;; Input events
;; ============================================================

(struct input-event typed-event (input-type content) #:transparent)

(define (make-input-event #:session-id session-id
                          #:turn-id turn-id
                          #:timestamp timestamp
                          #:input-type input-type
                          #:content content)
  (input-event "input" timestamp session-id turn-id input-type content))

;; ============================================================
;; Model events
;; ============================================================

(struct model-select-event typed-event (model provider) #:transparent)

(define (make-model-select-event #:session-id session-id
                                 #:turn-id turn-id
                                 #:timestamp timestamp
                                 #:model model
                                 #:provider provider)
  (model-select-event "model.selected" timestamp session-id turn-id model provider))

;; ============================================================
;; Agent events
;; ============================================================

(struct agent-start-event typed-event (model) #:transparent)
(struct agent-end-event typed-event (reason duration-ms) #:transparent)

(define (make-agent-start-event #:session-id session-id
                                #:turn-id turn-id
                                #:timestamp timestamp
                                #:model model)
  (agent-start-event "agent.started" timestamp session-id turn-id model))

(define (make-agent-end-event #:session-id session-id
                              #:turn-id turn-id
                              #:timestamp timestamp
                              #:reason reason
                              #:duration-ms duration-ms)
  (agent-end-event "agent.completed" timestamp session-id turn-id reason duration-ms))

;; ============================================================
;; Context events
;; ============================================================

(struct context-event typed-event (token-count window-size) #:transparent)

(define (make-context-event #:session-id session-id
                            #:turn-id turn-id
                            #:timestamp timestamp
                            #:token-count token-count
                            #:window-size window-size)
  (context-event "context.built" timestamp session-id turn-id token-count window-size))

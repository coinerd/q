#lang racket/base

;; agent/loop-phases.rkt — Pure phase functions for agent loop (F1)
;; STABILITY: evolving
;;
;; Each phase function is a PURE function that returns
;; (values result (listof effect?)). Effects describe what should
;; happen; the orchestrator (loop.rkt) decides how to execute them.

(require racket/match
         racket/list
         "../util/ids.rkt"
         "../util/protocol-types.rkt"
         "../llm/model.rkt"
         "../llm/token-budget.rkt"
         (only-in "../llm/provider.rkt" provider-name provider?)
         "effect-types.rkt"
         "loop-messages.rkt"
         "loop-fsm.rkt"
         "state.rkt"
         "event-emitter.rkt"
         (only-in "event-structs.rkt"
                  make-turn-start-event
                  make-context-event
                  make-provider-request-event
                  make-model-request-blocked-event
                  make-turn-end-event))

(provide phase-emit-start
         phase-build-context
         phase-build-request
         phase-pre-hook)

;; ---------------------------------------------------------------------------
;; Phase 1: Emit turn-started event
;; ---------------------------------------------------------------------------

;; Returns (values context (listof effect?))
(define (phase-emit-start session-id turn-id st context)
  (define start-event
    (make-turn-start-event #:session-id session-id
                           #:turn-id turn-id
                           #:timestamp (current-inexact-milliseconds)
                           #:model ""
                           #:provider ""))
  (define effects
    (list (effect:emit-event 'turn-start start-event)
          (effect:update-fsm turn-state-emit-start turn-event-start)))
  (values context effects))

;; ---------------------------------------------------------------------------
;; Phase 2: Build context
;; ---------------------------------------------------------------------------

;; Returns (values raw-messages (listof effect?))
(define (phase-build-context bus session-id turn-id st context)
  (define raw-messages (build-raw-messages context))
  (define token-count (estimate-context-tokens raw-messages))
  (define ctx-event
    (make-context-event #:session-id session-id
                        #:turn-id turn-id
                        #:timestamp (current-inexact-milliseconds)
                        #:token-count token-count
                        #:window-size (length raw-messages)))
  ;; Emit the context event directly (it needs the bus)
  (emit-typed-event! bus ctx-event #:state st)
  (define effects (list (effect:update-fsm turn-state-build-context turn-event-context-built)))
  (values raw-messages effects))

;; ---------------------------------------------------------------------------
;; Phase 3: Build model request
;; ---------------------------------------------------------------------------

;; Returns (values model-request (listof effect?))
(define (phase-build-request raw-messages tools provider-settings)
  (define req (make-model-request raw-messages tools (or provider-settings (hasheq))))
  (values req '()))

;; ---------------------------------------------------------------------------
;; Phase 4: Pre-hook dispatch
;; ---------------------------------------------------------------------------

;; Returns (values hook-result (listof effect?))
;; hook-result is #f if no hook dispatcher, or the hook result
(define (phase-pre-hook hook-dispatcher provider raw-messages req session-id turn-id)
  (define result
    (and hook-dispatcher
         (hook-dispatcher 'model-request-pre
                          (hasheq 'model-name
                                  (provider-name provider)
                                  'message-count
                                  (length raw-messages)
                                  'messages
                                  raw-messages
                                  'settings
                                  (model-request-settings req)))))
  (values result '()))

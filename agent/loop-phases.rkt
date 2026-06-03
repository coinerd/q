#lang racket/base

;; agent/loop-phases.rkt — Pure phase functions for agent loop (F1)
;; STABILITY: evolving
;;
;; Each phase function is a PURE function that returns
;; (values result (listof effect?)). Effects describe what should
;; happen; the orchestrator (loop.rkt) decides how to execute them.

(require racket/contract
         racket/match
         racket/list
         "../util/ids.rkt"
         (only-in "../util/loop-result.rkt" loop-result)
         "../llm/model.rkt"
         "../llm/token-budget.rkt"
         (only-in "../llm/provider.rkt" provider-name provider?)
         (only-in "../llm/stream.rkt" accumulate-tool-call-deltas)
         "effect-types.rkt"
         "loop-messages.rkt"
         "loop-stream.rkt"
         "loop-fsm.rkt"
         "state.rkt"
         (only-in "event-structs.rkt"
                  make-turn-start-event
                  make-context-event
                  make-provider-request-event
                  make-model-request-blocked-event
                  make-message-blocked-event
                  make-turn-end-event)
         (only-in "turn-reducer.rkt" decide-after-pre-hook decide-after-msg-hook decide-after-stream)
         (only-in "turn-model.rkt"
                  make-stream-completion
                  turn-decision-tag
                  make-turn-start
                  make-turn-hook-result
                  make-turn-stream-complete
                  hook-stage-payload)
         (only-in "../llm/token-budget.rkt" estimate-context-tokens)
         (only-in "../util/tool/tool-types.rkt" tool?)
         (only-in "event-bus.rkt" event-bus?)
         (only-in "loop-stream.rkt" stream-from-provider handle-cancellation build-stream-result)
         (only-in "../util/cancellation.rkt" cancellation-token?)
         (only-in "../util/message/message.rkt" message?)
         "../util/loop-result.rkt")

(provide (contract-out [phase-emit-start
                        (-> string?
                            string?
                            loop-state?
                            (listof message?)
                            (values (listof message?) (listof effect?)))])
         (contract-out [phase-build-context
                        (-> event-bus?
                            string?
                            string?
                            loop-state?
                            (listof message?)
                            (values (listof hash?) (listof effect?)))])
         (contract-out [phase-build-request
                        (-> (listof hash?)
                            (or/c (listof hash?) #f)
                            hash?
                            (values model-request? (listof effect?)))])
         (contract-out [phase-pre-hook
                        (-> (or/c procedure? #f)
                            provider?
                            (listof hash?)
                            model-request?
                            string?
                            string?
                            (values (or/c any/c #f) (listof effect?)))])
         (contract-out [phase-msg-hook
                        (-> (or/c procedure? #f)
                            provider?
                            model-request?
                            (listof hash?)
                            string?
                            string?
                            loop-state?
                            (values (or/c any/c #f) (listof effect?)))])
         (contract-out [phase-stream
                        (-> provider?
                            model-request?
                            event-bus?
                            string?
                            string?
                            loop-state?
                            (or/c procedure? #f)
                            (or/c cancellation-token? #f)
                            (values hash? (listof effect?)))]))

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
  ;; Return effect instead of emitting directly (purity fix W2-T1)
  (define effects
    (list (effect:emit-event 'context ctx-event)
          (effect:update-fsm turn-state-build-context turn-event-context-built)))
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

;; ---------------------------------------------------------------------------
;; Phase 5: Message-start hook dispatch
;; ---------------------------------------------------------------------------

;; Returns (values msg-hook-result (listof effect?))
;; msg-hook-result: the raw hook result (for reducer)
(define (phase-msg-hook hook-dispatcher provider req raw-messages session-id turn-id st)
  (define msg-start-result
    (and hook-dispatcher
         (hook-dispatcher 'message-start
                          (hasheq 'session-id
                                  session-id
                                  'turn-id
                                  turn-id
                                  'model-name
                                  (provider-name provider)
                                  'message-count
                                  (length raw-messages)))))
  (values msg-start-result '()))

;; ---------------------------------------------------------------------------
;; Phase 6: Stream from provider + completion dispatch
;; ---------------------------------------------------------------------------

;; Returns (values stream-result (listof effect?))
;; stream-result is a hasheq with keys: cancelled?, cancel-reason, text, tool-calls
(define (phase-stream provider req bus session-id turn-id st hook-dispatcher cancellation-token)
  (define stream-data
    (stream-from-provider provider req bus session-id turn-id st hook-dispatcher cancellation-token))
  (values stream-data '()))

;; ---------------------------------------------------------------------------

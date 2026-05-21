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
         "../util/protocol-types.rkt"
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
         (only-in "event-emitter.rkt" emit-typed-event!)
         (only-in "../tools/tool.rkt" tool?)
         (only-in "event-bus.rkt" event-bus?)
         (only-in "loop-stream.rkt" stream-from-provider handle-cancellation build-stream-result)
         (only-in "state.rkt" current-loop-state-for-error-recovery)
         "../util/loop-result.rkt")

(provide (contract-out [phase-emit-start
                        (-> any/c any/c any/c any/c (values any/c (listof effect?)))])
         (contract-out [phase-build-context
                        (-> any/c any/c any/c any/c any/c (values any/c (listof effect?)))])
         (contract-out [phase-build-request (-> any/c any/c any/c (values any/c (listof effect?)))])
         (contract-out [phase-pre-hook
                        (-> any/c any/c any/c any/c any/c any/c (values any/c (listof effect?)))])
         (contract-out
          [phase-msg-hook
           (-> any/c any/c any/c any/c any/c any/c any/c (values any/c (listof effect?)))])
         (contract-out
          [phase-stream
           (-> any/c any/c any/c any/c any/c any/c any/c any/c (values any/c (listof effect?)))])
         (contract-out [run-streaming-phase
                        (-> any/c any/c any/c any/c any/c any/c any/c any/c any/c any/c any/c)])
         (contract-out
          [compute-streaming-plan
           (-> any/c any/c any/c any/c any/c any/c any/c any/c any/c any/c streaming-plan?)]))

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
;; ---------------------------------------------------------------------------
;; Phase 6b: Pure streaming plan computation
;; ---------------------------------------------------------------------------

;; compute-streaming-plan : ... -> streaming-plan
;; Pure: computes what the streaming phase needs to do without doing it.
;; Validates messages, builds the request, and records expected effects.
(define (compute-streaming-plan provider
                                req
                                bus
                                session-id
                                turn-id
                                st
                                raw-messages
                                tools
                                hook-dispatcher
                                cancellation-token)
  (define pre-effects
    (list (effect:emit-event 'provider-request
                             (hasheq 'session-id
                                     session-id
                                     'turn-id
                                     turn-id
                                     'model
                                     (hash-ref (model-request-settings req)
                                               'model
                                               (lambda () (format "~a" (provider-name provider))))
                                     'provider
                                     (format "~a" (provider-name provider))))
          (effect:update-fsm turn-state-pre-hook turn-event-hook-pass)))
  (streaming-plan session-id
                  turn-id
                  raw-messages
                  req
                  provider
                  tools
                  hook-dispatcher
                  cancellation-token
                  pre-effects))

;; ---------------------------------------------------------------------------

;; Phase 7: Full post-pre-hook streaming dispatch
;; ---------------------------------------------------------------------------

;; Returns loop-result directly (this is an effectful dispatch function)
;; Handles: msg-hook dispatch, streaming, cancellation, result building
(define (run-streaming-phase provider
                             req
                             bus
                             session-id
                             turn-id
                             st
                             raw-messages
                             tools
                             hook-dispatcher
                             cancellation-token)
  (parameterize ([current-turn-fsm-state (current-turn-fsm-state)])
    ;; FSM: pre-hook -> stream
    (current-turn-fsm-state (next-turn-state turn-state-pre-hook turn-event-hook-pass))

    ;; DEBUG: validate raw-messages before sending
    (unless (valid-api-message-sequence? raw-messages)
      (log-warning "INVALID message sequence detected! Dumping raw messages:")
      (for ([rm (in-list raw-messages)]
            [i (in-naturals)])
        (log-warning "  msg[~a]: role=~a keys=~a" i (hash-ref rm 'role #f) (hash-keys rm)))
      (log-warning "End of invalid sequence dump"))

    (emit-typed-event! bus
                       (make-provider-request-event
                        #:session-id session-id
                        #:turn-id turn-id
                        #:timestamp (current-inexact-milliseconds)
                        #:model (hash-ref (model-request-settings req)
                                          'model
                                          (lambda () (format "~a" (provider-name provider))))
                        #:provider (format "~a" (provider-name provider)))
                       #:state st)

    ;; Phase 5: Message-start hook
    (define-values (msg-start-result _fx5)
      (phase-msg-hook hook-dispatcher provider req raw-messages session-id turn-id st))

    (define d-msg (decide-after-msg-hook msg-start-result))
    (match (turn-decision-tag d-msg)
      ['blocked
       (current-turn-fsm-state (next-turn-state turn-state-stream turn-event-msg-hook-block))
       (emit-typed-event! bus
                          (make-message-blocked-event #:session-id session-id
                                                      #:turn-id turn-id
                                                      #:timestamp (current-inexact-milliseconds)
                                                      #:hook "message-start"
                                                      #:reason "blocked"))
       (emit-typed-event! bus
                          (make-turn-end-event #:session-id session-id
                                               #:turn-id turn-id
                                               #:timestamp (current-inexact-milliseconds)
                                               #:reason "hook-blocked"
                                               #:duration-ms 0))
       (loop-result raw-messages 'hook-blocked (hasheq 'hook 'message-start))]
      [_
       ;; Phase 6: Stream from provider
       (define-values (stream-data _fx6)
         (phase-stream provider req bus session-id turn-id st hook-dispatcher cancellation-token))

       (define sc
         (make-stream-completion #:cancelled? (hash-ref stream-data 'cancelled? #f)
                                 #:cancel-reason (hash-ref stream-data 'cancel-reason #f)
                                 #:text (hash-ref stream-data 'text "")
                                 #:tool-calls (hash-ref stream-data 'tool-calls '())))
       (define d-stream (decide-after-stream sc))
       (match (turn-decision-tag d-stream)
         ['cancelled
          (current-turn-fsm-state (next-turn-state turn-state-stream turn-event-stream-cancel))
          (handle-cancellation bus session-id turn-id st #:hook-dispatcher hook-dispatcher)]
         [_
          (build-stream-result stream-data
                               raw-messages
                               bus
                               session-id
                               turn-id
                               st
                               tools
                               provider
                               hook-dispatcher)])])))

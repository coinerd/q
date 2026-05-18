#lang racket/base

;; agent/loop.rkt — effectful agent turn orchestrator
;;
;; Orchestrates the agent turn lifecycle: emit-start, build-context,
;; build-request, pre-hook, streaming, and turn completion.
;; This module is the public API surface for the agent loop.
;; v0.32.4: Flattened CPS hook dispatch to data-return classify-hook-result + match.
;;
;; Architecture (v0.25.0 decomposition):
;;   loop-messages.rkt — message helpers (build-raw-messages, emit!, etc.)
;;   loop-stream.rkt   — streaming, cancellation, result building
;;   loop.rkt          — facade: re-exports + run-agent-turn orchestrator

(require racket/contract
         racket/match
         (only-in "loop-fsm.rkt"
                  turn-state-emit-start
                  turn-state-build-context
                  turn-state-pre-hook
                  turn-state-stream
                  turn-state-post-hook
                  turn-state-complete
                  turn-state-blocked
                  turn-event-start
                  turn-event-context-built
                  turn-event-hook-pass
                  turn-event-hook-block
                  turn-event-stream-complete
                  turn-event-stream-cancel
                  turn-event-post-hook-done
                  turn-event-msg-hook-block
                  next-turn-state
                  turn-state->symbol
                  current-turn-fsm-state)
         racket/string
         racket/list
         racket/date
         racket/set
         "../util/ids.rkt"
         "../util/protocol-types.rkt"
         "event-bus.rkt"
         "state.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../llm/stream.rkt" accumulate-tool-call-deltas)
         (only-in "../llm/token-budget.rkt" estimate-turn-tokens estimate-context-tokens)
         (only-in "../util/cancellation.rkt" cancellation-token? cancellation-token-cancelled?)
         (only-in "../util/hook-types.rkt" hook-result? hook-result-action hook-result-payload)
         (only-in "../util/content-helpers.rkt" result-content->string)
         "streaming-message.rkt"
         "loop-messages.rkt"
         "loop-stream.rkt"
         (only-in "event-emitter.rkt" emit-typed-event!)
         (only-in "turn-reducer.rkt"
                  decide-after-pre-hook
                  decide-after-msg-hook
                  decide-after-stream
                  decide-after-start
                  decide-after-context
                  decide-turn-step)
         (only-in "turn-model.rkt"
                  make-stream-completion
                  turn-decision-tag
                  make-turn-start
                  make-turn-hook-result
                  make-turn-stream-complete
                  hook-stage-payload)
         (only-in "event-structs.rkt"
                  make-turn-start-event
                  make-turn-end-event
                  make-provider-request-event
                  make-context-event
                  make-model-request-blocked-event
                  make-message-blocked-event)
         "effect-executor.rkt"
         (only-in "loop-phases.rkt"
                  phase-emit-start
                  phase-build-context
                  phase-build-request
                  phase-pre-hook
                  phase-msg-hook
                  phase-stream
                  phase-dispatch-streaming))

(provide (contract-out [run-agent-turn
                        (->i ([ctx (listof message?)] [prov provider?] [bus event-bus?])
                             (#:session-id [session-id string?]
                              #:turn-id [turn-id string?]
                              #:state [state (or/c loop-state? #f)]
                              #:tools [tools (or/c (listof hash?) #f)]
                              #:cancellation-token [cancellation-token (or/c cancellation-token? #f)]
                              #:hook-dispatcher [hook-dispatcher (or/c procedure? #f)]
                              #:provider-settings [provider-settings (or/c hash? #f)])
                             [result loop-result?])]
                       [build-raw-messages (-> (listof message?) (listof hash?))]
                       [stream-from-provider
                        (-> provider?
                            model-request?
                            event-bus?
                            string?
                            string?
                            loop-state?
                            (or/c procedure? #f)
                            (or/c cancellation-token? #f)
                            hash?)]
                       [handle-cancellation (-> event-bus? string? string? loop-state? loop-result?)]
                       [build-stream-result
                        (-> hash?
                            (listof hash?)
                            event-bus?
                            string?
                            string?
                            loop-state?
                            (or/c (listof hash?) #f)
                            provider?
                            (or/c procedure? #f)
                            loop-result?)])
         MAX-STREAM-CHUNKS
         usage-empty?
         parts->text-string
         classify-hook-result)
;; ============================================================
;; Extracted helpers (I-01)
;; ============================================================

;; DEPRECATED v0.46.10: Use phase-emit-start from loop-phases.rkt instead
;; Phase 1: Emit turn-started event and dispatch agent-start hook
(define (emit-turn-start! bus session-id turn-id st hook-dispatcher context)
  (emit-typed-event! bus
                     (make-turn-start-event #:session-id session-id
                                            #:turn-id turn-id
                                            #:timestamp (current-inexact-milliseconds)
                                            #:model ""
                                            #:provider "")
                     #:state st)
  (when hook-dispatcher
    (hook-dispatcher
     'agent-start
     (hasheq 'session-id session-id 'turn-id turn-id 'message-count (length context)))))

;; DEPRECATED v0.46.10: Use phase-build-context from loop-phases.rkt instead
;; Phase 2: Build normalized context and emit context.built event
(define (build-turn-context bus session-id turn-id st context)
  (define raw-messages (build-raw-messages context))
  (define token-count (estimate-context-tokens raw-messages))
  (emit-typed-event! bus
                     (make-context-event #:session-id session-id
                                         #:turn-id turn-id
                                         #:timestamp (current-inexact-milliseconds)
                                         #:token-count token-count
                                         #:window-size (length raw-messages))
                     #:state st)
  raw-messages)

;; ============================================================
;; Main entry point — thin orchestrator
;; ============================================================

;; run-agent-turn : (listof message?) provider? event-bus?
;;                  #:session-id string?
;;                  #:turn-id string?
;;                  #:state (or/c loop-state? #f)
;;                  #:tools (or/c (listof hash?) #f)
;;                  #:cancellation-token (or/c cancellation-token? #f)
;;                  #:hook-dispatcher (or/c procedure? #f)
;;               -> loop-result?
;; R-06/R-07: FSM state parameter now lives in loop-fsm.rkt

(define (run-agent-turn context
                        provider
                        bus
                        #:session-id [session-id "session"]
                        #:turn-id [turn-id "turn"]
                        #:state [state #f]
                        #:tools [tools #f]
                        #:cancellation-token [cancellation-token #f]
                        #:hook-dispatcher [hook-dispatcher #f]
                        #:provider-settings [provider-settings #f])
  ;; Ensure we have a state for accumulation
  (define st (or state (make-loop-state session-id turn-id)))

  ;; v0.45.10 NF1: Set loop-state parameter so dispatch-iteration's error
  ;; handler can recover partial messages from stream errors.
  (parameterize ([current-loop-state-for-error-recovery st])

    ;; Phase 1: Emit turn-started (via phase pipeline + effect executor)
    (define-values (ctx1 fx1) (phase-emit-start session-id turn-id st context))
    (execute-effects! fx1 #:bus bus #:state st #:hook-dispatcher hook-dispatcher)

    ;; Phase 2: Build context (via phase pipeline + effect executor)
    (define-values (raw-messages fx2) (phase-build-context bus session-id turn-id st ctx1))
    (execute-effects! fx2 #:bus bus #:state st)

    ;; Phase 3: Build model-request (via phase pipeline)
    (define-values (req _fx3) (phase-build-request raw-messages tools provider-settings))

    ;; Phase 4: Pre-hook (via phase pipeline)
    (define-values (pre-hook-result _)
      (phase-pre-hook hook-dispatcher provider raw-messages req session-id turn-id))

    ;; v0.43.0: Reducer-driven dispatch
    (define d-pre (decide-after-pre-hook pre-hook-result))
    (match (turn-decision-tag d-pre)
      ['blocked
       ;; R-06/R-07: FSM: pre-hook -> blocked
       (current-turn-fsm-state (next-turn-state turn-state-pre-hook turn-event-hook-block))
       (emit-typed-event! bus
                          (make-model-request-blocked-event #:session-id session-id
                                                            #:turn-id turn-id
                                                            #:timestamp (current-inexact-milliseconds)
                                                            #:reason "hook"))
       (emit-typed-event! bus
                          (make-turn-end-event #:session-id session-id
                                               #:turn-id turn-id
                                               #:timestamp (current-inexact-milliseconds)
                                               #:reason "hook-blocked"
                                               #:duration-ms 0))
       (loop-result raw-messages 'hook-blocked (hasheq 'hook 'model-request-pre))]
      [_
       ;; v0.46.10 (I-1): Streaming dispatch extracted to phase-dispatch-streaming
       (phase-dispatch-streaming provider
                                 req
                                 bus
                                 session-id
                                 turn-id
                                 st
                                 raw-messages
                                 tools
                                 hook-dispatcher
                                 cancellation-token)])))

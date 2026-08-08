#lang racket/base
;;
;; COMPOSITION ROOT: This module wires together dependencies from
;; lower layers. It should not be imported by other production modules.
;;

;; runtime/iteration/main-loop.rkt — main iteration loop orchestrator
;;
;; Extracted from runtime/iteration.rkt (v0.34.6 W0a — A-01 decomposition).
;;
;; Provides:

(define-logger q-main-loop)
;;   run-iteration-loop — main iteration loop entry point

(require racket/match
         racket/list
         racket/contract
         (only-in "loop-state.rkt"
                  loop-infra
                  iteration-snapshot
                  make-initial-counters
                  loop-counters-iteration
                  loop-counters-consecutive-tool-count
                  loop-counters-explore-count
                  loop-counters
                  loop-counters-stall-retry-count)
         (only-in "../../util/loop-result.rkt" make-loop-result loop-result-messages)
         (only-in "../../agent/state.rkt" current-empty-response-retried?)
         (only-in "../../util/message/message.rkt" message?)
         (only-in "../../util/loop-result.rkt" loop-result-termination-reason loop-result-metadata)
         (only-in "../../util/ids.rkt" generate-id)
         (only-in "../event-emitter.rkt" emit-typed-event!)
         (only-in "../event-structs/hook-events.rkt" turn-cancelled-event)
         (only-in "../event-structs/iteration-events.rkt" make-iteration-decision-event)
         (only-in "../queue.rkt" queue-status queue? dequeue-followup! dequeue-all-followups!)
         (only-in "tool-turn-bridge.rkt" dequeue-all-steering! drain-injected-messages!)
         (only-in "../event-emitter.rkt" emit-session-event!)
         (only-in "../../extensions/hooks.rkt" maybe-dispatch-hooks)
         (only-in "../../util/hook-types.rkt" hook-result-action hook-result?)
         (only-in "../../util/event/event-contracts.rkt"
                  injection-count-payload/c
                  iteration-decision-payload/c
                  reason-payload/c)
         ;; v0.99.85: build-assembled-context and run-provider-turn are now
         ;; injected via loop-config instead of imported from the runtime
         ;; orchestration layer.
         (only-in "../../runtime/working-set.rkt"
                  working-set?
                  make-working-set
                  compute-working-set-budget)
         (only-in "../../util/cancellation.rkt" cancellation-token?)
         (only-in "../../runtime/session/session-types.rkt" agent-session?)
         (only-in "../../llm/provider.rkt" provider?)
         (only-in "../../runtime/layer-adapters.rkt" tool-registry? extension-registry?)
         (only-in "../event-bus.rkt" event-bus?)
         (only-in "../../util/loop-result.rkt" loop-result?)
         (only-in "counters.rkt" check-cancellation)
         (only-in "../../util/iteration/decision.rkt"
                  iteration-ctx
                  step-result-metadata
                  step-result-new-counters)
         (only-in "decision.rkt" compute-step-result)
         (only-in "../../util/iteration/directive.rkt"
                  directive-recurse
                  directive-stop
                  directive-yield)
         (only-in "../../util/iteration/fsm-types.rkt"
                  state-idle
                  state-provider-turn
                  state-decision
                  state-complete
                  state-aborted
                  event-start-loop
                  event-model-response
                  event-tool-calls-present
                  event-termination-reason
                  event-hook-block
                  event-cancel
                  next-iteration-state
                  state->symbol
                  iteration-state?)
         (only-in "../../util/iteration/internal.rkt" assert-payload)
         (only-in "loop-phases.rkt" prepare-iteration-context dispatch-turn-start-hooks)
         (only-in "loop-config.rkt"
                  loop-config?
                  loop-config-context
                  loop-config-provider
                  loop-config-bus
                  loop-config-registry
                  loop-config-ext-registry
                  loop-config-log-path
                  loop-config-session-id
                  loop-config-max-iterations
                  loop-config-cancellation-token
                  loop-config-max-iterations-hard
                  loop-config-context-budget
                  loop-config-queue
                  loop-config-injected-box
                  loop-config-shutdown-check
                  loop-config-force-shutdown-check
                  loop-config-working-set
                  loop-config-session
                  loop-config-build-context-fn
                  loop-config-run-provider-turn-fn
                  loop-config-interpret-step-fn
                  make-loop-config))

(provide (contract-out [run-iteration-loop/v2 (-> loop-config? loop-result?)]))

(provide (contract-out [current-iteration-fsm-state (parameter/c iteration-state?)]))

;; ============================================================
;; run-iteration-loop
;; ============================================================

;; R-06/R-07: FSM state tracking parameter
(define current-iteration-fsm-state (make-parameter state-idle))

(define (run-iteration-loop/v2 cfg)
  ;; Unpack loop-config fields into local bindings
  (let ([context (loop-config-context cfg)]
        [prov (loop-config-provider cfg)]
        [bus (loop-config-bus cfg)]
        [reg (loop-config-registry cfg)]
        [ext-reg (loop-config-ext-registry cfg)]
        [log-path (loop-config-log-path cfg)]
        [session-id (loop-config-session-id cfg)]
        [max-iterations (loop-config-max-iterations cfg)]
        [token (loop-config-cancellation-token cfg)]
        [max-iterations-hard (loop-config-max-iterations-hard cfg)]
        [context-budget (loop-config-context-budget cfg)]
        [steering-queue (loop-config-queue cfg)]
        [injected-box (loop-config-injected-box cfg)]
        [shutdown-check (loop-config-shutdown-check cfg)]
        [force-shutdown-check (loop-config-force-shutdown-check cfg)]
        [initial-ws (loop-config-working-set cfg)]
        [sess (loop-config-session cfg)])
    ;; v0.99.85: Injected runtime operations — no direct orchestration import
    (define build-assembled-context-fn
      (or (loop-config-build-context-fn cfg)
          (error 'run-iteration-loop/v2
                 "build-context-fn not supplied; use wiring layer to construct loop-config")))
    (define run-provider-turn-fn
      (or (loop-config-run-provider-turn-fn cfg)
          (error 'run-iteration-loop/v2
                 "run-provider-turn-fn not supplied; use wiring layer to construct loop-config")))
    (define interpret-step-fn
      (or (loop-config-interpret-step-fn cfg)
          (error 'run-iteration-loop/v2
                 "interpret-step-fn not supplied; use wiring layer to construct loop-config")))
    (define max-iterations-hard-val max-iterations-hard)
    (define ws
      (or initial-ws (make-working-set #:max-tokens (compute-working-set-budget context-budget))))
    ;; v0.99.83 W2: Local helper for counter increment
    (define (make-next-counters base)
      (struct-copy loop-counters
                   base
                   [iteration (add1 (loop-counters-iteration base))]
                   [stall-retry-count 0]))
    (define agent-start-payload
      (hasheq 'session-id
              session-id
              'max-iterations
              max-iterations
              'context-message-count
              (length context)))
    (define-values (amended-start start-hook-res)
      (maybe-dispatch-hooks ext-reg 'before-agent-start agent-start-payload))
    (if (and start-hook-res (eq? (hook-result-action start-hook-res) 'block))
        (begin
          (emit-session-event!
           bus
           session-id
           "agent.blocked"
           (assert-payload "agent.blocked"
                           (hasheq 'reason "extension-block" 'hook 'before-agent-start)
                           reason-payload/c))
          (make-loop-result '() 'completed (hasheq 'reason "extension-block")))
        (let ([infra (loop-infra context ext-reg reg bus session-id log-path token)])
          ;; R-06/R-07: Track FSM state: idle -> provider-turn
          (current-iteration-fsm-state (next-iteration-state state-idle event-start-loop))
          (log-q-main-loop-info "iteration start")
          (parameterize ([current-empty-response-retried? #f])
            (let loop ([ctx context]
                       [counters (make-initial-counters)]
                       [ws ws])

              (define ctx-with-injected
                (prepare-iteration-context ctx steering-queue injected-box bus ext-reg session-id))

              (define cancel-result
                (check-cancellation token
                                    force-shutdown-check
                                    shutdown-check
                                    bus
                                    session-id
                                    (loop-counters-iteration counters)
                                    ctx-with-injected))
              (cond
                [cancel-result cancel-result]
                [else
                 (define turn-id (generate-id))
                 (define-values (ctx-to-use turn-blocked?)
                   (dispatch-turn-start-hooks ctx-with-injected ext-reg))
                 (cond
                   [turn-blocked?
                    ;; R-06/R-07: FSM: provider-turn + hook-block -> aborted
                    (current-iteration-fsm-state (next-iteration-state state-provider-turn
                                                                       event-hook-block))
                    (log-q-main-loop-info "turn blocked at iteration ~a"
                                          (loop-counters-iteration counters))
                    (emit-session-event! bus
                                         session-id
                                         "turn.blocked"
                                         (assert-payload "turn.blocked"
                                                         (hasheq 'reason "extension-block")
                                                         reason-payload/c))
                    (make-loop-result '() 'completed (hasheq 'reason "extension-block"))]
                   [else
                    (define ctx-final
                      (build-assembled-context-fn ctx-to-use
                                                  ws
                                                  ext-reg
                                                  bus
                                                  session-id
                                                  (loop-counters-iteration counters)
                                                  #:session sess))
                    (define result
                      (run-provider-turn-fn ctx-final prov bus reg ext-reg session-id turn-id token))
                    (define termination (loop-result-termination-reason result))
                    (define new-msgs (loop-result-messages result))
                    ;; R-06/R-07: FSM: provider-turn + model-response -> decision
                    (current-iteration-fsm-state (next-iteration-state state-provider-turn
                                                                       event-model-response))
                    (log-q-main-loop-info "model response received at iteration ~a"
                                          (loop-counters-iteration counters))
                    (emit-typed-event! bus
                                       (make-iteration-decision-event
                                        #:session-id session-id
                                        #:turn-id ""
                                        #:timestamp (current-inexact-milliseconds)
                                        #:iteration (add1 (loop-counters-iteration counters))
                                        #:termination termination
                                        #:consecutive-tools
                                        (loop-counters-consecutive-tool-count counters)
                                        #:max-iterations max-iterations
                                        #:max-iterations-hard max-iterations-hard-val))
                    (define step-res
                      (compute-step-result
                       (iteration-ctx (loop-counters-iteration counters)
                                      (loop-counters-consecutive-tool-count counters)
                                      (loop-counters-explore-count counters)
                                      max-iterations
                                      max-iterations-hard)
                       result
                       counters))
                    ;; v0.99.78 FIX: surface the consecutive-tool circuit breaker
                    ;; (tool-loop-limit) as a session event so the TUI/caller can
                    ;; distinguish a circle-stop from a normal turn completion.
                    (when (hash-ref (step-result-metadata step-res) 'toolLoopLimit #f)
                      (emit-session-event!
                       bus
                       session-id
                       "tool-loop.limit-reached"
                       (hasheq
                        'iteration
                        (loop-counters-iteration counters)
                        'consecutive-tools
                        (loop-counters-consecutive-tool-count counters)
                        'message
                        "Consecutive tool-call limit reached; stopping to avoid an unbounded tool loop.")))
                    (define snapshot
                      (iteration-snapshot counters ws #f sess max-iterations max-iterations-hard-val))
                    (define directive
                      (interpret-step-fn step-res
                                         result
                                         new-msgs
                                         (struct-copy loop-infra infra [ctx ctx-with-injected])
                                         snapshot))
                    (match directive
                      [(directive-stop final-result)
                       ;; R-06/R-07: FSM: decision + termination -> complete
                       (current-iteration-fsm-state (next-iteration-state state-decision
                                                                          event-termination-reason))
                       (log-q-main-loop-info "iteration complete: ~a at iteration ~a"
                                             termination
                                             (loop-counters-iteration counters))
                       final-result]
                      [(directive-recurse new-ctx new-counters ws2)
                       (loop new-ctx new-counters ws2)])])])))))))

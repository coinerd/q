#lang racket/base

;; runtime/iteration/main-loop.rkt — main iteration loop orchestrator
;;
;; Extracted from runtime/iteration.rkt (v0.34.6 W0a — A-01 decomposition).
;;
;; Provides:
;;   run-iteration-loop — main iteration loop entry point

(require racket/dict
         racket/match
         racket/list
         racket/contract
         (only-in "loop-state.rkt"
                  loop-infra
                  iteration-snapshot
                  make-initial-counters
                  loop-counters-iteration
                  loop-counters-consecutive-tool-count
                  loop-counters-explore-count)
         (only-in "../../util/protocol-types.rkt"
                  message?
                  make-message
                  make-text-part
                  make-loop-result
                  loop-result-termination-reason
                  loop-result-messages
                  loop-result-metadata)
         (only-in "../../util/ids.rkt" generate-id)
         (only-in "../../agent/event-emitter.rkt" emit-typed-event!)
         (only-in "../../agent/event-structs/hook-events.rkt" turn-cancelled-event)
         (only-in "../../agent/queue.rkt"
                  queue-status
                  queue?
                  dequeue-followup!
                  dequeue-all-followups!)
         (only-in "tool-turn-bridge.rkt" dequeue-all-steering! drain-injected-messages!)
         (only-in "../../runtime/runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks)
         (only-in "../../util/hook-types.rkt" hook-result-action hook-result?)
         (only-in "../../util/event-contracts.rkt"
                  injection-count-payload/c
                  iteration-decision-payload/c
                  reason-payload/c)
         (only-in "../../runtime/turn-orchestrator.rkt" run-provider-turn build-assembled-context)
         (only-in "../../runtime/working-set.rkt" working-set? make-working-set)
         (only-in "../../util/cancellation.rkt" cancellation-token?)
         (only-in "../../runtime/session-types.rkt" agent-session?)
         (only-in "../../runtime/session-config.rkt"
                  session-config?
                  hash->session-config
                  resolve-max-iterations-hard)
         (only-in "../../llm/provider.rkt" provider?)
         (only-in "../../tools/registry.rkt" tool-registry?)
         (only-in "../../extensions/api.rkt" extension-registry?)
         (only-in "../../agent/event-bus.rkt" event-bus?)
         (only-in "../../util/loop-result.rkt" loop-result?)
         (only-in "counters.rkt" check-cancellation)
         (only-in "decision.rkt" iteration-ctx compute-step-result)
         (only-in "step-interpreter.rkt" interpret-step)
         (only-in "directive.rkt" directive-recurse directive-stop directive-yield)
         (only-in "fsm-types.rkt"
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
                  state->symbol)
         (only-in "internal.rkt" assert-payload)
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
                  loop-config-config
                  loop-config-queue
                  loop-config-follow-up-delivery-mode
                  loop-config-injected-box
                  loop-config-shutdown-check
                  loop-config-force-shutdown-check
                  loop-config-working-set
                  loop-config-session
                  make-loop-config))

(provide (contract-out [run-iteration-loop
                        (->* ((listof message?) (or/c provider? #f)
                                                event-bus?
                                                (or/c tool-registry? #f)
                                                (or/c extension-registry? #f)
                                                (or/c path-string? path?)
                                                string?
                                                exact-nonnegative-integer?)
                             (#:cancellation-token (or/c cancellation-token? #f)
                              #:config session-config?
                              #:queue (or/c queue? #f)
                              #:follow-up-delivery-mode (or/c 'all 'one-at-a-time)
                              #:injected-box (or/c box? #f)
                              #:shutdown-check (or/c procedure? #f)
                              #:force-shutdown-check (or/c procedure? #f)
                              #:working-set (or/c working-set? #f)
                              #:session (or/c agent-session? #f))
                             loop-result?)]
                       [run-iteration-loop/v2 (-> loop-config? loop-result?)]))

(provide current-iteration-fsm-state)

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
        [config-raw (loop-config-config cfg)]
        [steering-queue (loop-config-queue cfg)]
        [injected-box (loop-config-injected-box cfg)]
        [shutdown-check (loop-config-shutdown-check cfg)]
        [force-shutdown-check (loop-config-force-shutdown-check cfg)]
        [initial-ws (loop-config-working-set cfg)]
        [sess (loop-config-session cfg)])
    (define config config-raw)
    (define max-iterations-hard (resolve-max-iterations-hard config max-iterations))
    (define ws (or initial-ws (make-working-set)))
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
          (let loop ([ctx context]
                     [counters (make-initial-counters)]
                     [ws ws])

            (define ctx-with-steering
              (if steering-queue
                  (let ([steering-msgs (dequeue-all-steering! steering-queue)])
                    (let ([qs (queue-status steering-queue)])
                      (when (or (> (hash-ref qs 'steering 0) 0) (> (hash-ref qs 'followup 0) 0))
                        (emit-session-event! bus session-id "queue.status-update" qs)))
                    (if (null? steering-msgs)
                        ctx
                        (append ctx steering-msgs)))
                  ctx))
            (define ctx-with-injected
              (if injected-box
                  (let ([injected (drain-injected-messages! bus injected-box session-id)])
                    (if (null? injected)
                        ctx-with-steering
                        (begin
                          (emit-session-event! bus
                                               session-id
                                               "message.injected.drain"
                                               (assert-payload "message.injected.drain"
                                                               (hasheq 'count (length injected))
                                                               injection-count-payload/c))
                          (append ctx-with-steering injected))))
                  ctx-with-steering))

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
                 (let-values ([(amended-ctx hook-res)
                               (maybe-dispatch-hooks ext-reg 'turn-start ctx-with-injected)])
                   (if (and hook-res (eq? (hook-result-action hook-res) 'block))
                       (values ctx-with-injected #t)
                       (values amended-ctx #f))))
               (cond
                 [turn-blocked?
                  ;; R-06/R-07: FSM: provider-turn + hook-block -> aborted
                  (current-iteration-fsm-state (next-iteration-state state-provider-turn
                                                                     event-hook-block))
                  (emit-session-event! bus
                                       session-id
                                       "turn.blocked"
                                       (assert-payload "turn.blocked"
                                                       (hasheq 'reason "extension-block")
                                                       reason-payload/c))
                  (make-loop-result '() 'completed (hasheq 'reason "extension-block"))]
                 [else
                  (define config-with-ws (dict-set config 'working-set ws))
                  (define ctx-final
                    (build-assembled-context ctx-to-use
                                             config-with-ws
                                             ext-reg
                                             bus
                                             session-id
                                             (loop-counters-iteration counters)))
                  (define result
                    (run-provider-turn ctx-final
                                       prov
                                       bus
                                       reg
                                       ext-reg
                                       session-id
                                       turn-id
                                       token
                                       config))
                  (define termination (loop-result-termination-reason result))
                  (define new-msgs (loop-result-messages result))
                  ;; R-06/R-07: FSM: provider-turn + model-response -> decision
                  (current-iteration-fsm-state (next-iteration-state state-provider-turn
                                                                     event-model-response))
                  (emit-session-event!
                   bus
                   session-id
                   "iteration.decision"
                   (assert-payload "iteration.decision"
                                   (hasheq 'iteration
                                           (add1 (loop-counters-iteration counters))
                                           'termination
                                           termination
                                           'consecutive_tools
                                           (loop-counters-consecutive-tool-count counters)
                                           'max_iterations
                                           max-iterations
                                           'max_iterations_hard
                                           max-iterations-hard)
                                   iteration-decision-payload/c))
                  (define step-res
                    (compute-step-result
                     (iteration-ctx (loop-counters-iteration counters)
                                    (loop-counters-consecutive-tool-count counters)
                                    (loop-counters-explore-count counters)
                                    max-iterations
                                    max-iterations-hard)
                     result
                     counters))
                  (define snapshot
                    (iteration-snapshot counters ws config sess max-iterations max-iterations-hard))
                  (define directive
                    (interpret-step step-res
                                    result
                                    new-msgs
                                    (struct-copy loop-infra infra [ctx ctx-with-injected])
                                    snapshot))
                  (match directive
                    [(directive-stop final-result)
                     ;; R-06/R-07: FSM: decision + termination -> complete
                     (current-iteration-fsm-state (next-iteration-state state-decision
                                                                        event-termination-reason))
                     final-result]
                    [(directive-recurse new-ctx new-counters ws2)
                     (loop new-ctx new-counters ws2)])])]))))))

;; ============================================================
;; run-iteration-loop (backward-compatible wrapper)
;; ============================================================

(define (run-iteration-loop context
                            prov
                            bus
                            reg
                            ext-reg
                            log-path
                            session-id
                            max-iterations
                            #:cancellation-token [token #f]
                            #:config [config-raw (hash->session-config (hash))]
                            #:queue [steering-queue #f]
                            #:follow-up-delivery-mode [follow-up-mode 'all]
                            #:injected-box [injected-box #f]
                            #:shutdown-check [shutdown-check #f]
                            #:force-shutdown-check [force-shutdown-check #f]
                            #:working-set [initial-ws #f]
                            #:session [sess #f])
  (run-iteration-loop/v2 (make-loop-config context
                                           prov
                                           bus
                                           reg
                                           ext-reg
                                           log-path
                                           session-id
                                           max-iterations
                                           #:cancellation-token token
                                           #:config config-raw
                                           #:queue steering-queue
                                           #:follow-up-delivery-mode follow-up-mode
                                           #:injected-box injected-box
                                           #:shutdown-check shutdown-check
                                           #:force-shutdown-check force-shutdown-check
                                           #:working-set initial-ws
                                           #:session sess)))

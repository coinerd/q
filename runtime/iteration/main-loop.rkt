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
         (only-in "../../runtime/session-config.rkt" session-config?)
         (only-in "../../llm/provider.rkt" provider?)
         (only-in "../../tools/registry.rkt" tool-registry?)
         (only-in "../../extensions/api.rkt" extension-registry?)
         (only-in "../../agent/event-bus.rkt" event-bus?)
         (only-in "../../util/loop-result.rkt" loop-result?)
         (only-in "counters.rkt" check-cancellation)
         (only-in "decision.rkt" iteration-ctx compute-step-result)
         (only-in "step-interpreter.rkt" interpret-step)
         (only-in "directive.rkt" directive-recurse directive-stop directive-yield)
         (only-in "internal.rkt" assert-payload))

(provide (contract-out [run-iteration-loop
                        (->* ((listof message?) (or/c provider? #f)
                                                event-bus?
                                                (or/c tool-registry? #f)
                                                (or/c extension-registry? #f)
                                                (or/c path-string? path?)
                                                string?
                                                exact-nonnegative-integer?)
                             (#:cancellation-token (or/c cancellation-token? #f)
                              #:config (or/c hash? session-config?)
                              #:queue (or/c queue? #f)
                              #:follow-up-delivery-mode (or/c 'all 'one-at-a-time)
                              #:injected-box (or/c box? #f)
                              #:shutdown-check (or/c procedure? #f)
                              #:force-shutdown-check (or/c procedure? #f)
                              #:working-set (or/c working-set? #f)
                              #:session (or/c agent-session? #f))
                             loop-result?)]))

;; ============================================================
;; run-iteration-loop
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
                            #:config [config (hash)]
                            #:queue [steering-queue #f]
                            #:follow-up-delivery-mode [follow-up-mode 'all]
                            #:injected-box [injected-box #f]
                            #:shutdown-check [shutdown-check #f]
                            #:force-shutdown-check [force-shutdown-check #f]
                            #:working-set [initial-ws #f]
                            #:session [sess #f])
  (define max-iterations-hard
    (dict-ref config 'max-iterations-hard (max (inexact->exact (floor (* max-iterations 1.6))) 80)))
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
                (emit-session-event!
                 bus
                 session-id
                 "turn.blocked"
                 (assert-payload "turn.blocked" (hasheq 'reason "extension-block") reason-payload/c))
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
                  (run-provider-turn ctx-final prov bus reg ext-reg session-id turn-id token config))
                (define termination (loop-result-termination-reason result))
                (define new-msgs (loop-result-messages result))
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
                  (compute-step-result (iteration-ctx (loop-counters-iteration counters)
                                                      (loop-counters-consecutive-tool-count counters)
                                                      (loop-counters-explore-count counters)
                                                      max-iterations
                                                      max-iterations-hard)
                                       result
                                       counters))
                (define directive
                  (interpret-step step-res
                                  result
                                  new-msgs
                                  (struct-copy loop-infra infra [ctx ctx-with-injected])
                                  counters
                                  ws
                                  config
                                  sess
                                  max-iterations
                                  max-iterations-hard))
                (match directive
                  [(directive-stop final-result) final-result]
                  [(directive-recurse new-ctx new-counters ws2)
                   (loop new-ctx new-counters ws2)])])])))))

#lang racket/base

;; runtime/iteration.rkt -- agent iteration loop FACADE
;; STABILITY: stable
;;
;; M-07 DEPRECATION NOTICE: This module is a re-export facade. New code should
;; import directly from sub-modules:
;;   runtime/iteration/counters.rkt
;;   runtime/iteration/decision.rkt
;;   runtime/iteration/step-interpreter.rkt
;;   runtime/iteration/main-loop.rkt
;; This facade will be removed in v0.38.0.
;;
;; Thin re-export facade over runtime/iteration/*. Sub-modules:
;;   counters.rkt          -- compute-next-counters, check-cancellation
;;   decision.rkt          -- iteration-ctx, step-result, decide-next-action,
;;                           compute-step-result
;;   step-interpreter.rkt  -- interpret-step, handle-stop-action,
;;                           execute-pending-tool-calls
;;   main-loop.rkt         -- run-iteration-loop
;;
;; Also re-exports shared helpers from runtime-helpers.rkt and
;; tool-turn-bridge.rkt for backward compatibility.
;; -- LAYER NOTE -----------------------------------------------
;; Since v0.22.4 (MOD-01): Upward imports limited to a single constant from
;; extensions/message-inject.rkt. All behavioral upward imports moved to
;; turn-orchestrator.rkt. Listed as known exception in arch-boundaries tests.
;; --------------------------------------------------------------

(require racket/contract
         (only-in "iteration/counters.rkt" compute-next-counters check-cancellation)
         "iteration/decision.rkt"
         (only-in "iteration/step-interpreter.rkt"
                  interpret-step
                  handle-stop-action
                  execute-pending-tool-calls)
         (only-in "iteration/main-loop.rkt" run-iteration-loop)
         ;; Shared helpers (backward compat)
         (only-in "runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks)
         (only-in "../util/json-helpers.rkt" ensure-hash-args)
         (only-in "iteration/tool-turn-bridge.rkt"
                  extract-tool-target-path
                  update-seen-paths
                  make-injected-collector!
                  drain-injected-messages!)
         ;; Re-export retry-policy symbols for backward compat
         (only-in "iteration/retry-policy.rkt"
                  call-with-overflow-recovery
                  check-mid-turn-budget!
                  estimate-mid-turn-tokens
                  maybe-compact-mid-turn)
         ;; Re-export turn-orchestrator symbol for backward compat
         (only-in "turn-orchestrator.rkt" register-session-extensions!)
         ;; Types needed by contract
         (only-in "../util/loop-result.rkt" loop-result?)
         (only-in "../util/protocol-types.rkt" message?)
         (only-in "../llm/provider.rkt" provider?)
         (only-in "../tools/registry.rkt" tool-registry?)
         (only-in "../extensions/api.rkt" extension-registry?)
         (only-in "../runtime/session-types.rkt" agent-session?)
         (only-in "../runtime/session-config.rkt" session-config?)
         (only-in "../agent/event-bus.rkt" event-bus?)
         (only-in "../util/cancellation.rkt" cancellation-token?)
         (only-in "../agent/queue.rkt" queue?)
         (only-in "../runtime/working-set.rkt" working-set?))

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
                             loop-result?)]
                       [decide-next-action (-> iteration-ctx? loop-result? step-action?)]
                       [step-action? contract?])
         emit-session-event!
         maybe-dispatch-hooks
         ensure-hash-args
         extract-tool-target-path
         update-seen-paths
         make-injected-collector!
         drain-injected-messages!
         call-with-overflow-recovery
         check-mid-turn-budget!
         estimate-mid-turn-tokens
         maybe-compact-mid-turn
         register-session-extensions!
         (struct-out iteration-ctx)
         known-termination-reasons
         (struct-out step-result)
         compute-step-result
         interpret-step)

;; ============================================================
;; for-testing submodule: expose internals for unit testing
;; ============================================================
(module+ for-testing
  (provide handle-stop-action
           interpret-step
           compute-next-counters
           execute-pending-tool-calls
           decide-next-action
           check-cancellation
           compute-step-result
           iteration-ctx
           step-result))

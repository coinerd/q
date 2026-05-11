#lang racket/base

;; runtime/iteration/step-interpreter.rkt — effectful step interpreter
;;
;; Extracted from runtime/iteration.rkt (v0.34.6 W0a — A-01 decomposition).
;;
;; Provides:
;;   interpret-step         — effectful interpreter for step-result
;;   handle-stop-action     — handle 'stop action from decide-next-action
;;   execute-pending-tool-calls — execute pending tool calls, update working set

(require racket/match
         racket/list
         racket/dict
         (only-in "loop-state.rkt"
                  loop-infra
                  loop-infra-ctx
                  loop-infra-ext-reg
                  loop-infra-reg
                  loop-infra-bus
                  loop-infra-session-id
                  loop-infra-log-path
                  loop-infra-token
                  loop-counters
                  loop-counters-iteration
                  loop-counters-consecutive-tool-count
                  loop-counters-explore-count
                  loop-counters-seen-paths
                  loop-counters-consecutive-error-count
                  loop-counters-recent-tool-names)
         (only-in "../../util/protocol-types.rkt"
                  message-role
                  message-id
                  make-message
                  make-text-part
                  tool-call-name
                  tool-call-arguments
                  tool-result-part?
                  tool-result-part-is-error?)
         (only-in "../../runtime/tool-coordinator.rkt"
                  handle-tool-calls-pending
                  extract-tool-calls-from-messages)
         (only-in "../../runtime/runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks)
         (only-in "../../util/hook-types.rkt" hook-result-action hook-result?)
         (only-in "../../agent/event-emitter.rkt" emit-typed-event!)
         (only-in "../../util/event-contracts.rkt"
                  error-detail-payload/c
                  iteration-decision-payload/c
                  reason-payload/c
                  injection-count-payload/c)
         (only-in "../../util/loop-result.rkt"
                  make-loop-result
                  loop-result-metadata
                  loop-result-termination-reason
                  loop-result-messages)
         (only-in "../../util/ids.rkt" generate-id now-seconds)
         (only-in "../../runtime/session-store.rkt" append-entries!)
         (only-in "../../runtime/session-config.rkt" config-max-context-tokens)
         (only-in "../../agent/queue.rkt" queue-status)
         (only-in "../../runtime/working-set.rkt"
                  working-set-update!
                  ws-entry-path
                  working-set-entries
                  working-set-entry-count
                  working-set-token-count)
         (only-in "tool-turn-bridge.rkt" extract-tool-target-path)
         (only-in "../../runtime/context-policy.rkt" estimate-message-tokens)
         (only-in "../../runtime/session-compaction.rkt" compact-context-mid-turn)
         (only-in "retry-policy.rkt"
                  estimate-mid-turn-tokens
                  maybe-compact-mid-turn
                  detect-exploration-loop)
         (only-in "decision.rkt" step-result step-result-action step-result-new-counters)
         (only-in "internal.rkt" assert-payload)
         (only-in "directive.rkt" directive-recurse directive-stop directive-yield))

(provide interpret-step
         handle-stop-action
         execute-pending-tool-calls)

;; ============================================================
;; execute-pending-tool-calls
;; ============================================================

(define (execute-pending-tool-calls new-msgs infra config ws)
  (define updated-ctx
    (handle-tool-calls-pending new-msgs
                               (loop-infra-ctx infra)
                               (loop-infra-ext-reg infra)
                               (loop-infra-reg infra)
                               (loop-infra-bus infra)
                               (loop-infra-session-id infra)
                               (loop-infra-log-path infra)
                               (loop-infra-token infra)
                               config))
  (define current-tool-calls (extract-tool-calls-from-messages new-msgs))
  (define tool-result-msgs (filter (lambda (m) (eq? (message-role m) 'tool)) updated-ctx))
  (define current-tool-calls-hashes
    (for/list ([tc (in-list current-tool-calls)])
      (hasheq 'name (tool-call-name tc) 'arguments (tool-call-arguments tc))))
  (define read-spiral-paths
    (for/list ([tc (in-list current-tool-calls)]
               #:when (equal? (tool-call-name tc) "read"))
      (define path (extract-tool-target-path tc))
      (and path (member path (map ws-entry-path (working-set-entries ws))) path)))
  (define valid-spiral-paths (filter string? read-spiral-paths))
  (when (> (length valid-spiral-paths) 0)
    (emit-session-event! (loop-infra-bus infra)
                         (loop-infra-session-id infra)
                         "working-set.read-spiral-detected"
                         (hasheq 'paths valid-spiral-paths 'count (length valid-spiral-paths))))
  (working-set-update! ws
                       current-tool-calls-hashes
                       tool-result-msgs
                       message-id
                       estimate-message-tokens)
  (emit-session-event! (loop-infra-bus infra)
                       (loop-infra-session-id infra)
                       "working-set.update"
                       (hasheq 'entry-count
                               (working-set-entry-count ws)
                               'token-count
                               (working-set-token-count ws)
                               'paths
                               (map ws-entry-path (working-set-entries ws))))
  updated-ctx)

;; ============================================================
;; handle-stop-action
;; ============================================================

(define (handle-stop-action result new-msgs infra counters ws config)
  (define termination (loop-result-termination-reason result))
  (if (eq? termination 'completed)
      (begin
        (append-entries! (loop-infra-log-path infra) new-msgs)
        (let-values ([(amended-result after-hook-res)
                      (maybe-dispatch-hooks (loop-infra-ext-reg infra) 'turn-end result)])
          (if (and after-hook-res (eq? (hook-result-action after-hook-res) 'amend))
              amended-result
              result)))
      (begin
        (append-entries! (loop-infra-log-path infra) new-msgs)
        result)))

;; ============================================================
;; interpret-step
;; ============================================================

(define (interpret-step step-res
                        result
                        new-msgs
                        infra
                        counters
                        ws
                        config
                        sess
                        max-iterations
                        max-iterations-hard)
  ;; v0.35.3 (W-02): Returns step-directive? instead of calling on-recurse callback
  (define action (step-result-action step-res))
  (match action
    ['stop
     (define stop-result (handle-stop-action result new-msgs infra counters ws config))
     (directive-stop stop-result)]
    ['stop-hard-limit
     (append-entries! (loop-infra-log-path infra) new-msgs)
     (emit-session-event! (loop-infra-bus infra)
                          (loop-infra-session-id infra)
                          "runtime.error"
                          (assert-payload "runtime.error"
                                          (hasheq 'error
                                                  "max-iterations-exceeded"
                                                  'iteration
                                                  (loop-counters-iteration counters)
                                                  'maxIterations
                                                  max-iterations-hard)
                                          error-detail-payload/c))
     (directive-stop (make-loop-result
                      new-msgs
                      'max-iterations-exceeded
                      (hash-set (loop-result-metadata result) 'maxIterationsReached #t)))]
    ['stop-soft-limit
     (append-entries! (loop-infra-log-path infra) new-msgs)
     (emit-session-event! (loop-infra-bus infra)
                          (loop-infra-session-id infra)
                          "iteration.soft-warning"
                          (hasheq 'iteration
                                  (add1 (loop-counters-iteration counters))
                                  'soft-limit
                                  max-iterations
                                  'hard-limit
                                  max-iterations-hard
                                  'remaining
                                  (- max-iterations-hard (add1 (loop-counters-iteration counters)))))
     (define updated-ctx (execute-pending-tool-calls new-msgs infra config ws))
     (define budget-config (hasheq 'max-context-tokens (config-max-context-tokens config)))
     (define emit-fn
       (lambda (name payload)
         (emit-session-event! (loop-infra-bus infra) (loop-infra-session-id infra) name payload)))
     (define ctx-after-budget
       (if sess
           (maybe-compact-mid-turn sess
                                   updated-ctx
                                   (loop-infra-session-id infra)
                                   budget-config
                                   #:emit-event emit-fn
                                   #:compact-proc (lambda (ctx) (compact-context-mid-turn sess ctx)))
           (begin
             (estimate-mid-turn-tokens updated-ctx
                                       (loop-infra-session-id infra)
                                       budget-config
                                       #:emit-event emit-fn)
             updated-ctx)))
     (directive-recurse ctx-after-budget
                        (struct-copy loop-counters
                                     counters
                                     [iteration (add1 (loop-counters-iteration counters))]
                                     [consecutive-tool-count
                                      (add1 (loop-counters-consecutive-tool-count counters))]
                                     [stall-retry-count 0])
                        ws)]
    ['continue
     (append-entries! (loop-infra-log-path infra) new-msgs)
     (define updated-ctx (execute-pending-tool-calls new-msgs infra config ws))
     (define new-counters (step-result-new-counters step-res))
     (define loop-warning
       (detect-exploration-loop (filter string? (loop-counters-recent-tool-names new-counters))))
     (when loop-warning
       (emit-session-event! (loop-infra-bus infra)
                            (loop-infra-session-id infra)
                            "iteration.exploration-loop"
                            (hasheq 'pattern
                                    loop-warning
                                    'recent-tools
                                    (loop-counters-recent-tool-names new-counters)
                                    'iteration
                                    (loop-counters-iteration counters))))
     (directive-recurse updated-ctx
                        (struct-copy loop-counters
                                     new-counters
                                     [iteration (add1 (loop-counters-iteration counters))]
                                     [consecutive-tool-count
                                      (add1 (loop-counters-consecutive-tool-count counters))]
                                     [stall-retry-count 0])
                        ws)]))

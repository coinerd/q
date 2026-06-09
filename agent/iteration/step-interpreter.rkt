#lang racket/base

;; runtime/iteration/step-interpreter.rkt — effectful step interpreter
;;
;; Extracted from runtime/iteration.rkt (v0.34.6 W0a — A-01 decomposition).
;;
;; Provides:
;;   interpret-step         — effectful interpreter for step-result
;;   handle-stop-action     — handle 'stop action from decide-next-action
;;   execute-pending-tool-calls — execute pending tool calls, update working set

(require racket/contract
         racket/string
         racket/match
         racket/class
         racket/list
         (only-in "loop-state.rkt"
                  loop-infra
                  loop-infra?
                  iteration-snapshot
                  iteration-snapshot?
                  loop-counters?
                  iteration-snapshot-counters
                  iteration-snapshot-ws
                  iteration-snapshot-config
                  iteration-snapshot-sess
                  iteration-snapshot-max-iterations
                  iteration-snapshot-max-iterations-hard
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
                  loop-counters-recent-tool-names)
         (only-in "../../util/message/message.rkt" message-role message-id message-content)
         (only-in "../../util/content/content-parts.rkt"
                  tool-result-part?
                  tool-result-part-tool-call-id
                  tool-result-part-content)
         (only-in "../../util/tool/tool-types.rkt" tool-call-name tool-call-arguments tool-call-id)
         (only-in "../../runtime/layer-adapters.rkt" permission-config?)
         (only-in "../../runtime/tool-coordinator.rkt"
                  handle-tool-calls-pending
                  extract-tool-calls-from-messages)
         (only-in "../../runtime/runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks)
         (only-in "../../runtime/iteration/effect-executor.rkt"
                  step-effect:append-entries
                  step-effect:emit-event
                  run-step-effects!)
         (only-in "../../util/hook-types.rkt" hook-result-action hook-result?)
         (only-in "../event-emitter.rkt" emit-typed-event!)
         (only-in "../../util/event/event-contracts.rkt"
                  error-detail-payload/c
                  iteration-decision-payload/c
                  reason-payload/c
                  injection-count-payload/c)
         (only-in "../../util/loop-result.rkt"
                  make-loop-result
                  loop-result?
                  loop-result-metadata
                  loop-result-termination-reason
                  loop-result-messages)
         (only-in "../../runtime/session/session-store.rkt" append-entries!)
         (only-in "../../runtime/session/session-config.rkt" config-max-context-tokens)
         (only-in "../../runtime/working-set.rkt"
                  working-set-update!
                  ws-entry-path
                  working-set-entries
                  working-set-entry-count
                  working-set-token-count)
         (only-in "tool-turn-bridge.rkt" extract-tool-target-path)
         (only-in "../../runtime/context/context-policy.rkt" estimate-message-tokens)
         (only-in "../../runtime/compaction/session-compaction.rkt" compact-context-mid-turn)
         (only-in "../../runtime/iteration/retry-policy.rkt"
                  estimate-mid-turn-tokens
                  maybe-compact-mid-turn
                  detect-exploration-loop)
         (only-in "../../runtime/context-assembly/rollback-actions.rkt" increment-loop-warning-count!)
         (only-in "../../runtime/context-assembly/state-aware-builder.rkt" current-reflection-event)
         (only-in "../../runtime/memory/auto-extraction.rkt"
                  maybe-auto-extract-tool-results!
                  current-auto-extraction-enabled)
         (only-in "../../runtime/iteration/decision.rkt"
                  step-result
                  step-result?
                  step-result-action
                  step-result-new-counters)
         (only-in "../../runtime/iteration/internal.rkt" assert-payload)
         (only-in "../../runtime/iteration/directive.rkt" directive-recurse directive-stop))

(provide (contract-out
          [interpret-step
           (-> step-result? loop-result? (listof any/c) loop-infra? iteration-snapshot? any/c)]
          [handle-stop-action
           (-> loop-result? (listof any/c) loop-infra? loop-counters? any/c any/c loop-result?)]
          [execute-pending-tool-calls (-> (listof any/c) loop-infra? any/c any/c (listof any/c))]
          [sink-append-entries! (->* (loop-infra? (listof any/c)) ((or/c any/c #f)) void?)]
          [current-reflection-prompt-enabled (parameter/c boolean?)]
          [REFLECTION-THRESHOLD-CHARS exact-positive-integer?]))

;; ── v0.96.13 W3: Reflection prompt ──
(define current-reflection-prompt-enabled (make-parameter #f))
(define REFLECTION-THRESHOLD-CHARS 4000)

;; ============================================================
;; R-09/R-10: Sink-aware append helper
;; ============================================================

;; When a sink is provided, use it instead of the path-based API.
;; This enables test isolation and effect boundary extraction.
(define (sink-append-entries! infra new-msgs [sink #f])
  (if sink
      (send sink sink-append-entries! new-msgs)
      (append-entries! (loop-infra-log-path infra) new-msgs)))

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
  ;; v0.96.13 W3: Post-tool reflection — emit event if large results detected
  (when (current-reflection-prompt-enabled)
    (define large-results
      (for/list ([m (in-list tool-result-msgs)]
                 #:when (let ([content-str (format "~a" (message-content m))])
                          (> (string-length content-str) REFLECTION-THRESHOLD-CHARS)))
        (or (message-id m) "unknown")))
    (when (pair? large-results)
      (define payload
        (hasheq
         'tools
         large-results
         'message
         "Large tool results received. Consider using record_conclusion to persist key findings before proceeding."))
      (emit-session-event! (loop-infra-bus infra)
                           (loop-infra-session-id infra)
                           "reflection-suggested"
                           payload)
      ;; v0.96.14 F3: Wire reflection event → parameter for preamble consumption
      (current-reflection-event payload)))
  ;; GAP-3: Auto-extract from tool results (ADR-0023)
  (when (current-auto-extraction-enabled)
    ;; GAP-2 fix: Build tool-call-id → tool-name lookup from current tool calls
    (define tcid->name
      (for/hash ([tc (in-list current-tool-calls)])
        (define tcid (tool-call-id tc))
        (values (or tcid "") (tool-call-name tc))))
    (define extractable-msgs
      (for/list ([m (in-list tool-result-msgs)])
        ;; Extract tool-call-id from tool-result-part content
        (define parts (message-content m))
        (define tcid
          (for/or ([p (in-list parts)]
                   #:when (tool-result-part? p))
            (tool-result-part-tool-call-id p)))
        (define tool-name (hash-ref tcid->name (or tcid "") "unknown"))
        ;; Extract actual content from tool-result-parts
        (define content-str
          (string-join (for/list ([p (in-list parts)]
                                  #:when (tool-result-part? p))
                         (define c (tool-result-part-content p))
                         (if (string? c)
                             c
                             (format "~a" c)))
                       "\n"))
        (hasheq 'content content-str 'name tool-name)))
    (maybe-auto-extract-tool-results! extractable-msgs
                                      #:session-id (loop-infra-session-id infra)
                                      #:project-root (loop-infra-log-path infra)))
  updated-ctx)

;; ============================================================
;; handle-stop-action
;; ============================================================

(define success-completion-reasons '(completed))

(define (handle-stop-action result new-msgs infra counters ws config)
  (define termination (loop-result-termination-reason result))
  (if (member termination success-completion-reasons)
      (begin
        (sink-append-entries! infra new-msgs)
        (let-values ([(amended-result after-hook-res)
                      (maybe-dispatch-hooks (loop-infra-ext-reg infra) 'turn-end result)])
          (if (and after-hook-res (eq? (hook-result-action after-hook-res) 'amend))
              amended-result
              result)))
      (begin
        (sink-append-entries! infra new-msgs)
        result)))

;; ============================================================
;; Shared helpers
;; ============================================================

;; FA-02: Shared counter increment logic for recurse branches
(define (make-next-counters base)
  (struct-copy loop-counters
               base
               [iteration (add1 (loop-counters-iteration base))]
               [consecutive-tool-count (add1 (loop-counters-consecutive-tool-count base))]
               [stall-retry-count 0]))

;; ============================================================
;; interpret-step
;; ============================================================

;; v0.37.4 (FA-04): Bundle evolving parameters into iteration-snapshot
;; to avoid threading 6+ positional parameters.
(define (interpret-step step-res result new-msgs infra snapshot)
  (define counters (iteration-snapshot-counters snapshot))
  (define ws (iteration-snapshot-ws snapshot))
  (define config (iteration-snapshot-config snapshot))
  (define sess (iteration-snapshot-sess snapshot))
  (define max-iterations (iteration-snapshot-max-iterations snapshot))
  (define max-iterations-hard (iteration-snapshot-max-iterations-hard snapshot))
  (define action (step-result-action step-res))
  ;; Local emit helper — avoids repeating bus/session-id everywhere
  (define (emit name payload)
    (emit-session-event! (loop-infra-bus infra) (loop-infra-session-id infra) name payload))

  (match action
    ['stop
     (define stop-result (handle-stop-action result new-msgs infra counters ws config))
     (directive-stop stop-result)]
    ['stop-hard-limit
     ;; effect extraction for fire-and-forget side effects
     (run-step-effects!
      (list (step-effect:append-entries new-msgs)
            (step-effect:emit-event "runtime.error"
                                    (assert-payload "runtime.error"
                                                    (hasheq 'error
                                                            "max-iterations-exceeded"
                                                            'iteration
                                                            (loop-counters-iteration counters)
                                                            'maxIterations
                                                            max-iterations-hard)
                                                    error-detail-payload/c)))
      infra)
     (directive-stop (make-loop-result
                      new-msgs
                      'max-iterations-exceeded
                      (hash-set (loop-result-metadata result) 'maxIterationsReached #t)))]
    ['stop-soft-limit
     (sink-append-entries! infra new-msgs)
     (emit "iteration.soft-warning"
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
     (define ctx-after-budget
       (if sess
           (maybe-compact-mid-turn sess
                                   updated-ctx
                                   (loop-infra-session-id infra)
                                   budget-config
                                   #:emit-event emit
                                   #:compact-proc (lambda (ctx) (compact-context-mid-turn sess ctx)))
           (begin
             (estimate-mid-turn-tokens updated-ctx
                                       (loop-infra-session-id infra)
                                       budget-config
                                       #:emit-event emit)
             updated-ctx)))
     (directive-recurse ctx-after-budget (make-next-counters counters) ws)]
    ['continue
     ;; fire-and-forget effect for entry persistence
     (run-step-effects! (list (step-effect:append-entries new-msgs)) infra)
     (define updated-ctx (execute-pending-tool-calls new-msgs infra config ws))
     (define new-counters (step-result-new-counters step-res))
     (define loop-warning
       (detect-exploration-loop (filter string? (loop-counters-recent-tool-names new-counters))))
     (when loop-warning
       (emit "iteration.exploration-loop"
             (hasheq 'pattern
                     loop-warning
                     'recent-tools
                     (loop-counters-recent-tool-names new-counters)
                     'iteration
                     (loop-counters-iteration counters)))
       ;; v0.96.14 F2: Feed exploration loop into rollback pipeline
       ;; by incrementing the warning counter (triggers escalation on next check)
       (increment-loop-warning-count!))
     ;; Reuse make-next-counters for consistent counter increment
     (directive-recurse updated-ctx
                        (struct-copy loop-counters
                                     (make-next-counters counters)
                                     [recent-tool-names
                                      (loop-counters-recent-tool-names new-counters)])
                        ws)]))

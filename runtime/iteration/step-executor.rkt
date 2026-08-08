#lang racket/base

;; runtime/iteration/step-executor.rkt — effectful step executor
;;
;; v0.99.86: Relocated from agent/iteration/step-interpreter.rkt.
;;
;; This module performs EFFECTFUL EXECUTION of step-result directives.
;; It coordinates tool execution, session persistence, context compaction,
;; rollback actions, working-set mutation, and event emission.
;;
;; ARCHITECTURAL BOUNDARY:
;;   The PURE DECISION of what to do next lives in decision.rkt
;;   (compute-step-result, decide-next-action).
;;
;;   The EFFECTFUL EXECUTION of that decision lives here.
;;
;;   The Agent iteration loop calls compute-step-result to decide,
;;   then calls interpret-step (below) to execute. This separation
;;   means the Agent iteration algorithm can be understood without
;;   reading tool, persistence, compaction, or rollback code.
;;
;; Provides:
;;   interpret-step             — effectful executor for step-result
;;   handle-stop-action         — handle 'stop action
;;   execute-pending-tool-calls — execute pending tool calls, update working set

(require racket/contract
         racket/string
         racket/match
         racket/class
         racket/list
         (only-in "../../agent/iteration/loop-state.rkt"
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
                  loop-counters-recent-tool-names
                  loop-counters-recent-error-classes
                  loop-counters-last-corrected-error-class)
         (only-in "../../util/message/message.rkt"
                  make-message
                  message?
                  message-id
                  message-content
                  message-meta-safe)
         (only-in "../../util/content/content-parts.rkt"
                  make-text-part
                  tool-result-part?
                  tool-result-part-tool-call-id
                  tool-result-part-content
                  tool-result-part-is-error?)
         (only-in "../../util/tool/tool-types.rkt" tool-call-name tool-call-id)
         (only-in "../layer-adapters.rkt" permission-config?)
         (only-in "../tool-coordinator.rkt"
                  handle-tool-calls-pending/outcome
                  tool-batch-outcome-updated-context
                  tool-batch-outcome-effective-current-calls
                  tool-batch-outcome-current-result-messages)
         (only-in "../runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks)
         (only-in "effect-executor.rkt"
                  step-effect:append-entries
                  step-effect:emit-event
                  run-step-effects!)
         (only-in "../../util/hook-types.rkt" hook-result-action hook-result?)
         (only-in "../../agent/event-emitter.rkt" emit-typed-event!)
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
         (only-in "../session/session-store.rkt" append-entries!)
         (only-in "../session/session-config.rkt" config-max-context-tokens)
         (only-in "../working-set.rkt"
                  working-set-update!/actions
                  ws-entry-path
                  ws-entry-token-estimate
                  ws-entry-budget-action
                  working-set-entries
                  working-set-entry-count
                  working-set-token-count)
         (only-in "../../agent/iteration/tool-turn-bridge.rkt" extract-tool-target-path)
         (only-in "../context/context-policy.rkt" estimate-message-tokens)
         (only-in "../compaction/session-compaction.rkt" compact-context-mid-turn)
         (only-in "retry-policy.rkt"
                  estimate-mid-turn-tokens
                  maybe-compact-mid-turn
                  detect-exploration-loop)
         (only-in "../context-assembly/rollback-actions.rkt"
                  increment-loop-warning-count!
                  current-loop-warning-count
                  escalation-threshold
                  tool-error-class->string
                  error-class->signal
                  warnings->actions
                  select-highest-priority-action
                  maybe-execute-action
                  rollback-action-type)
         (only-in "decision.rkt" step-result step-result? step-result-action step-result-new-counters)
         (only-in "../../agent/state.rkt" current-reflection-event)
         (only-in "internal.rkt" assert-payload)
         (only-in "directive.rkt"
                  directive-recurse
                  directive-recurse?
                  directive-stop
                  directive-stop?))

(provide (contract-out
          [interpret-step
           (-> step-result?
               loop-result?
               (listof message?)
               loop-infra?
               iteration-snapshot?
               (or/c directive-recurse? directive-stop?))]
          [handle-stop-action
           (-> loop-result? (listof message?) loop-infra? loop-counters? any/c any/c loop-result?)]
          [execute-pending-tool-calls
           (-> (listof message?) loop-infra? any/c any/c (listof message?))]
          [sink-append-entries! (->* (loop-infra? (listof message?)) ((or/c any/c #f)) void?)]
          [current-reflection-prompt-enabled (parameter/c boolean?)]
          [REFLECTION-THRESHOLD-CHARS exact-positive-integer?]
          [current-post-tool-result-hook (parameter/c procedure?)]))

;; ── v0.96.13 W3: Reflection prompt ──
(define current-reflection-prompt-enabled (make-parameter #f))
(define REFLECTION-THRESHOLD-CHARS 4000)

;; ── v0.99.84: Tool-result extraction hook (dependency inversion) ──
;; Called after tool execution with extractable tool-result messages.
;; Default is a no-op. Runtime sets this to wire memory extraction.
(define current-post-tool-result-hook (make-parameter (lambda (msgs session-id project-root) (void))))

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

(define (execute-pending-tool-calls/observed new-msgs infra config ws)
  (define outcome
    (handle-tool-calls-pending/outcome new-msgs
                                       (loop-infra-ctx infra)
                                       (loop-infra-ext-reg infra)
                                       (loop-infra-reg infra)
                                       (loop-infra-bus infra)
                                       (loop-infra-session-id infra)
                                       (loop-infra-log-path infra)
                                       (loop-infra-token infra)
                                       config))
  (define updated-ctx (tool-batch-outcome-updated-context outcome))
  ;; Only this batch's effective calls and results may feed ephemeral state.
  ;; updated-ctx intentionally also contains history for the next model turn.
  (define current-tool-calls (tool-batch-outcome-effective-current-calls outcome))
  (define tool-result-msgs (tool-batch-outcome-current-result-messages outcome))
  ;; One observation per tool result keeps the frozen eight-result window
  ;; truthful: successful results occupy a slot rather than disappearing.
  (define result-classes
    (for/list ([m (in-list tool-result-msgs)])
      (define error-part
        (findf (lambda (part) (and (tool-result-part? part) (tool-result-part-is-error? part)))
               (message-content m)))
      (if error-part
          (tool-error-class->string (format "~a" (tool-result-part-content error-part)))
          "success")))
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
  (define budget-actions
    (working-set-update!/actions ws
                                 current-tool-calls
                                 tool-result-msgs
                                 message-id
                                 estimate-message-tokens))
  (emit-session-event! (loop-infra-bus infra)
                       (loop-infra-session-id infra)
                       "working-set.update"
                       (hasheq 'entry-count
                               (working-set-entry-count ws)
                               'token-count
                               (working-set-token-count ws)
                               'paths
                               (map ws-entry-path (working-set-entries ws))
                               'budget-actions
                               (for/list ([entry (in-list budget-actions)])
                                 (hasheq 'path
                                         (ws-entry-path entry)
                                         'action
                                         (ws-entry-budget-action entry)
                                         'tokens
                                         (ws-entry-token-estimate entry)))))
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
  ;; v0.99.84: Tool-result extraction delegated to Runtime via hook parameter.
  ;; The hook is set by the wiring layer to call maybe-auto-extract-tool-results!.
  ;; Agent Core constructs the extractable messages but does not own the extraction logic.
  (define tcid->name
    (for/hash ([tc (in-list current-tool-calls)])
      (define tcid (tool-call-id tc))
      (values (or tcid "") (tool-call-name tc))))
  (define extractable-msgs
    (for/list ([m (in-list tool-result-msgs)])
      (define parts (message-content m))
      (define tcid
        (for/or ([p (in-list parts)]
                 #:when (tool-result-part? p))
          (tool-result-part-tool-call-id p)))
      (define tool-name (hash-ref tcid->name (or tcid "") "unknown"))
      (define content-str
        (string-join (for/list ([p (in-list parts)]
                                #:when (tool-result-part? p))
                       (define c (tool-result-part-content p))
                       (if (string? c)
                           c
                           (format "~a" c)))
                     "\n"))
      (hasheq 'content content-str 'name tool-name)))
  ((current-post-tool-result-hook) extractable-msgs
                                   (loop-infra-session-id infra)
                                   (loop-infra-log-path infra))
  (values updated-ctx result-classes))

(define (execute-pending-tool-calls new-msgs infra config ws)
  (define-values (updated-ctx _result-classes)
    (execute-pending-tool-calls/observed new-msgs infra config ws))
  updated-ctx)

(define MAX-RECENT-ERROR-CLASSES 8)
(define ERROR-CORRECTION-THRESHOLD 3)
(define SUCCESS-RESULT-CLASS "success")

(define (bounded-result-history prior current)
  (define combined (append prior current))
  (take-right combined (min MAX-RECENT-ERROR-CLASSES (length combined))))

(define (process-result-classes prior-history prior-corrected current)
  ;; Process a parallel batch in scheduler result order. A threshold crossing
  ;; may emit at most one action for the turn; a later success/different class
  ;; still closes that episode before the next provider request.
  (let loop ([remaining current]
             [history prior-history]
             [corrected prior-corrected]
             [crossing #f])
    (cond
      [(null? remaining) (values history corrected crossing)]
      [else
       (define item (car remaining))
       (define recovered? (and corrected (not (string=? item corrected))))
       (define base-history
         (if recovered?
             '()
             history))
       (define base-corrected (if recovered? #f corrected))
       (define next-history (bounded-result-history base-history (list item)))
       (define crosses?
         (and (not crossing)
              (not base-corrected)
              (not (string=? item SUCCESS-RESULT-CLASS))
              (>= (count (lambda (seen) (string=? seen item)) next-history)
                  ERROR-CORRECTION-THRESHOLD)))
       (loop (cdr remaining)
             next-history
             (if crosses? item base-corrected)
             (if crosses? item crossing))])))

(define (remove-error-corrections ctx)
  (filter (lambda (m) (not (and (message? m) (hash-ref (message-meta-safe m) 'error-correction #f))))
          ctx))

(define (inject-error-correction ctx error-class signal iteration)
  (append
   ctx
   (list (make-message
          (format "error-correction-~a-~a" iteration error-class)
          #f
          'system
          'system-instruction
          (list (make-text-part
                 (format (string-append
                          "Corrective checkpoint: error class ~a repeated at least ~a times "
                          "within the last ~a tool results. Signal: ~a. Do not repeat the "
                          "equivalent call. Re-establish the canonical repository/path/tool "
                          "coordinate, choose a materially different action, and continue only "
                          "after that corrective check.")
                         error-class
                         ERROR-CORRECTION-THRESHOLD
                         MAX-RECENT-ERROR-CLASSES
                         signal)))
          (current-seconds)
          (hasheq 'ephemeral
                  #t
                  'gsd-pin
                  #t
                  'error-correction
                  #t
                  'error-class
                  error-class
                  'signal
                  signal)))))

(define (apply-error-correction updated-ctx result-classes counters emit)
  (define event-history
    (bounded-result-history (loop-counters-recent-error-classes counters) result-classes))
  (define-values (history corrected crossing)
    (process-result-classes (loop-counters-recent-error-classes counters)
                            (loop-counters-last-corrected-error-class counters)
                            result-classes))
  (define next-counters
    (struct-copy loop-counters
                 counters
                 [recent-error-classes history]
                 [last-corrected-error-class corrected]))
  ;; Keep at most one ephemeral correction and remove it immediately when a
  ;; later result in the same batch demonstrates recovery/different behavior.
  (define cleaned-ctx (remove-error-corrections updated-ctx))
  (define reconciled-ctx
    (if corrected
        (inject-error-correction cleaned-ctx
                                 corrected
                                 (error-class->signal corrected)
                                 (loop-counters-iteration counters))
        cleaned-ctx))
  (if crossing
      (let* ([signal (error-class->signal crossing)]
             [reason (format "error ~a repeated ~a times within ~a tool results"
                             crossing
                             ERROR-CORRECTION-THRESHOLD
                             (length event-history))]
             [action (select-highest-priority-action (warnings->actions (list (list signal
                                                                                    reason))))])
        (maybe-execute-action action)
        (increment-loop-warning-count!)
        (emit "iteration.error-correction"
              (hasheq 'error-class
                      crossing
                      'signal
                      signal
                      'history
                      event-history
                      'corrective-calls
                      1
                      'max-corrective-calls
                      1
                      'action
                      (and action (rollback-action-type action))))
        (values reconciled-ctx next-counters))
      (values reconciled-ctx next-counters)))

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
     (define-values (raw-updated-ctx result-classes)
       (execute-pending-tool-calls/observed new-msgs infra config ws))
     (define-values (updated-ctx corrected-counters)
       (apply-error-correction raw-updated-ctx
                               result-classes
                               (step-result-new-counters step-res)
                               emit))
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
     (directive-recurse ctx-after-budget (make-next-counters corrected-counters) ws)]
    ['continue
     ;; fire-and-forget effect for entry persistence
     (run-step-effects! (list (step-effect:append-entries new-msgs)) infra)
     (define-values (raw-updated-ctx result-classes)
       (execute-pending-tool-calls/observed new-msgs infra config ws))
     (define new-counters (step-result-new-counters step-res))
     (define-values (updated-ctx corrected-counters)
       (apply-error-correction raw-updated-ctx result-classes new-counters emit))
     (define loop-warning
       (detect-exploration-loop (filter string?
                                        (loop-counters-recent-tool-names corrected-counters))))
     (when loop-warning
       (emit "iteration.exploration-loop"
             (hasheq 'pattern
                     loop-warning
                     'recent-tools
                     (loop-counters-recent-tool-names corrected-counters)
                     'iteration
                     (loop-counters-iteration counters)))
       ;; Feed exploration loop into rollback pipeline
       ;; by incrementing the warning counter (triggers escalation on next check)
       (increment-loop-warning-count!)
       ;; exploration-loop escalation to corrective steering.
       (when (>= (current-loop-warning-count) escalation-threshold)
         (current-loop-warning-count 0)
         (emit "iteration.exploration-loop.corrected"
               (hasheq 'pattern loop-warning 'iteration (loop-counters-iteration counters)))
         (set!
          updated-ctx
          (append
           updated-ctx
           (list (make-message
                  (format "exploration-corrective-~a" (loop-counters-iteration counters))
                  #f
                  'user
                  'message
                  (list (make-text-part
                         (format (string-append
                                  "STEERING: You have called the same tools repeatedly (~a) without "
                                  "producing text or progress. Stop exploring and re-reading files. "
                                  "Produce a concrete implementation step now (an edit, a test run, "
                                  "a file write, or a clear statement of the blocker), or the loop "
                                  "will be terminated.")
                                 loop-warning)))
                  (current-seconds)
                  (hasheq 'source 'steering)))))))
     ;; Reuse make-next-counters for consistent counter increment
     (directive-recurse updated-ctx
                        (struct-copy loop-counters
                                     corrected-counters
                                     [iteration (add1 (loop-counters-iteration corrected-counters))]
                                     [stall-retry-count 0])
                        ws)]))

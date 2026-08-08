#lang racket/base

;; runtime/context-assembly/rollback-actions.rkt — Bounded rollback action model
;; STABILITY: evolving
;; CONSUMERS: state-aware-builder, step-executor, loop
;;
;; Represents rollback actions as pure data. Actions are prioritized:
;; warn-only < expand-context < force-distill < revert-state.
;; At most one action is executed per turn to prevent recursive loops.
;;
;; v0.77.10 M2: Real execution via injectable callbacks.
;; Default callbacks are no-ops. The runtime (state-aware-builder) wires
;; real callbacks that set feature flags and budgets.

(require racket/contract
         racket/list
         racket/string
         racket/format)

;; ── Action Types ──

(define-struct rollback-action
               (type ; symbol: warn-only, expand-context, force-distill, revert-state
                reason ; string: human-readable reason
                severity ; integer: 0-3
                metadata) ; hash: additional context
  #:transparent)

(define valid-action-types '(warn-only expand-context force-distill revert-state))

(define (rollback-action-type? v)
  (and (symbol? v) (memq v valid-action-types) #t))

;; ── Constructors ──

(define (make-warn-action reason)
  (rollback-action 'warn-only reason 0 (hasheq)))

(define (make-expand-context-action reason metadata)
  (rollback-action 'expand-context reason 1 metadata))

(define (make-force-distill-action reason metadata)
  (rollback-action 'force-distill reason 2 metadata))

(define (make-revert-state-action reason metadata)
  (rollback-action 'revert-state reason 3 metadata))

;; ── Rollback Plan (Phase 3-4 extraction) ──

;; A rollback-plan bundles trigger warnings with the recommended action (or #f).
;; This is the output of the pure detection phase (detect-rollback-plan).
;; The effectful execution phase (execute-rollback-plan!) consumes it.
(struct rollback-plan
        (warnings ; (listof (list/c symbol? string?))
         recommended-action) ; (or/c rollback-action? #f)
  #:transparent)

;; ── Explicit Rollback State (cross-turn) ──
;;
;; rollback-state bundles the cross-turn consequences of rollback actions.
;; It replaces scattered global parameters (current-loop-warning-count,
;; current-rollback-action-log) with a single explicit value object.
;;
;; Semantic fields (not mechanical copies of parameter names):
;; - warning-count: loop-warning escalation accumulator (resets on escalation or FSM transition)
;; - force-distill-active?: has force-distill ever fired? Persists — makes auto-distill permanent for the session.
;; - budget-expansion-level: how many times expand-context has doubled the budget (0 = base)
;; - action-log: observability log of executed actions (ring buffer, max 100)

(struct rollback-state
        (warning-count ; exact-nonnegative-integer?
         force-distill-active? ; boolean
         budget-expansion-level ; exact-nonnegative-integer?
         action-log) ; (listof hash?)
  #:transparent)

(define (make-default-rollback-state)
  (rollback-state 0 #f 0 '()))

;; Per-session canonical state parameter.
;; Initialized at session start, reset on new session.
;; All rollback-related cross-turn state lives here.
(define current-rollback-state (make-parameter (make-default-rollback-state)))

;; Effective values derived from rollback-state + base configuration.
;; Callers use these instead of directly reading rollback-state fields.

;; Whether auto-distillation should run: base config OR rollback forced it.
(define (effective-auto-distill? base-config? state)
  (or base-config? (rollback-state-force-distill-active? state)))

;; Effective conclusion token budget: base × 2^expansion-level.
(define (effective-conclusion-budget base-budget state)
  (* base-budget (expt 2 (rollback-state-budget-expansion-level state))))

;; ── Pure State Transition ──
;;
;; advance-rollback-state: plan + old state → new state.
;; Pure — no I/O, no parameter mutation, no logging.
;; Computes the semantic consequences of executing a rollback plan.
(define (advance-rollback-state state plan)
  (cond
    [(not plan) state]
    [else
     (define warnings (rollback-plan-warnings plan))
     ;; Recompute warning count using pure escalation logic
     (define new-warning-count
       (for/fold ([count (rollback-state-warning-count state)]) ([w (in-list warnings)])
         (define-values (_action nc) (pure-warnings->action w count))
         nc))
     ;; Determine rollback consequences from recommended action
     (define recommended (rollback-plan-recommended-action plan))
     (define new-force-distill?
       (or (rollback-state-force-distill-active? state)
           (and recommended (eq? (rollback-action-type recommended) 'force-distill))))
     (define new-expansion-level
       (if (and recommended (eq? (rollback-action-type recommended) 'expand-context))
           (add1 (rollback-state-budget-expansion-level state))
           (rollback-state-budget-expansion-level state)))
     (rollback-state new-warning-count
                     new-force-distill?
                     new-expansion-level
                     (rollback-state-action-log state))]))

;; ── Effectful Application ──
;;
;; apply-rollback-plan!: executes external effects (callbacks, logging) and
;; updates current-rollback-state. Returns the action type if executed, #f.
(define (apply-rollback-plan! plan)
  (cond
    [(not plan) #f]
    [else
     (define warnings (rollback-plan-warnings plan))
     (define recommended (rollback-plan-recommended-action plan))
     (when (pair? warnings)
       (log-warning "context-assembly: rollback triggers fired: ~a" warnings))
     (cond
       [recommended
        ;; Execute via callbacks (legacy parameterize-based wiring)
        (define executed (maybe-execute-action recommended))
        (when executed
          (log-warning "context-assembly: executed rollback action: ~a" executed))
        ;; Advance state and store
        (define old-state (current-rollback-state))
        (define new-state (advance-rollback-state old-state plan))
        ;; Sync warning count to legacy parameter for step-executor compat
        (current-loop-warning-count (rollback-state-warning-count new-state))
        ;; Append to action log in state
        (define logged-state
          (struct-copy rollback-state
                       new-state
                       [action-log
                        (let ([entry (hasheq 'type
                                             (rollback-action-type recommended)
                                             'reason
                                             (rollback-action-reason recommended)
                                             'timestamp
                                             (current-seconds))])
                          (define new-log (append (rollback-state-action-log new-state) (list entry)))
                          (if (> (length new-log) max-rollback-log-size)
                              (drop new-log (- (length new-log) max-rollback-log-size))
                              new-log))]))
        (current-rollback-state logged-state)
        executed]
       [else #f])]))

;; ── Prioritization ──

;; Select the highest-severity action from a list (at most one).
(define (select-highest-priority-action actions)
  (cond
    [(null? actions) #f]
    [else (car (sort actions > #:key rollback-action-severity))]))

;; ── Execution Guard ──

;; H1 v0.97.13: Config struct for snapshot/grouped access.
;; Individual parameters still exist for backward compat with parameterize.
(struct rollback-actions-config
        (execution? ; boolean: enable action execution
         force-distill ; (or/c (-> rollback-action? void?) #f)
         expand-context ; (or/c (-> rollback-action? void?) #f)
         revert-state) ; (or/c (-> rollback-action? void?) #f)
  #:transparent)

(define (make-default-rollback-config)
  (rollback-actions-config #f #f #f #f))

;; Feature flag for action execution (disabled by default — warn-only mode)
(define current-rollback-action-execution? (make-parameter #f))

;; Injectable execution callbacks.
(define current-force-distill-fn (make-parameter #f))
(define current-expand-context-fn (make-parameter #f))
(define current-revert-state-fn (make-parameter #f))

;; Snapshot current state as config struct
(define (current-rollback-actions-config)
  (rollback-actions-config (current-rollback-action-execution?)
                           (current-force-distill-fn)
                           (current-expand-context-fn)
                           (current-revert-state-fn)))

;; v0.77.10 M2: Action execution log for observability.
;; Each entry is a hash with 'type, 'reason, 'timestamp.
(define current-rollback-action-log (make-parameter '()))

;; v0.96.13 W2: Loop warning counter for escalation.
;; Incremented each time a "repeat" or "loop" warning fires.
;; When >= escalation-threshold, warnings->actions escalates to force-distill.
(define current-loop-warning-count (make-parameter 0))

;; v0.96.14 F4: Named constant for escalation threshold (was magic number 2).
(define escalation-threshold 2)

;; Centralized counter mutation: all increments go through this function.
;; step-interpreter calls it for exploration-loop detection.
;; warnings->actions calls it for repeat-pattern escalation.
;; They are non-overlapping: exploration-loop fires from detect-exploration-loop
;; on consecutive identical tool names; repeat fires from check-rollback-triggers
;; on high repeat-tool-count. Both increment toward the same escalation threshold.
;;
;; v0.99.85: Also updates current-rollback-state to keep it in sync.
(define (increment-loop-warning-count! [amount 1])
  (define new-count (+ (current-loop-warning-count) amount))
  (current-loop-warning-count new-count)
  ;; Sync to rollback-state
  (define old-state (current-rollback-state))
  (current-rollback-state (struct-copy rollback-state old-state [warning-count new-count])))

;; v0.77.10 M2: Execute force-distill action.
;; Calls the injectable callback if available, then logs.
(define (execute-force-distill! action)
  (define fn (current-force-distill-fn))
  (when fn
    (fn action))
  (log-action! action))

;; v0.77.10 M2: Execute expand-context action.
;; Calls the injectable callback if available, then logs.
(define (execute-expand-context! action)
  (define fn (current-expand-context-fn))
  (when fn
    (fn action))
  (log-action! action))

;; v0.79.1 GAP-6: Execute revert-state action.
;; Calls the injectable callback if available, then logs.
;; Only executes when current-revert-state-fn is wired.
(define (execute-revert-state! action)
  (define fn (current-revert-state-fn))
  (when fn
    (fn action))
  (log-action! action))

;; Log an executed action to the rollback action log.
;; GAP-M v0.97.12: Cap log at 100 entries (ring buffer semantics).
(define max-rollback-log-size 100)

(define (log-action! action)
  (define entry
    (hasheq 'type
            (rollback-action-type action)
            'reason
            (rollback-action-reason action)
            'timestamp
            (current-seconds)))
  (define current-log (current-rollback-action-log))
  (define new-log (append current-log (list entry)))
  (current-rollback-action-log (if (> (length new-log) max-rollback-log-size)
                                   (drop new-log (- (length new-log) max-rollback-log-size))
                                   new-log)))

;; Execute a rollback action if execution is enabled.
;; v0.77.10 M2: Now dispatches to real execution functions.
;; Returns the action type if executed, #f otherwise.
;; Never executes 'revert-state unless explicitly enabled.
(define (maybe-execute-action action)
  (cond
    [(not action) #f]
    [(not (current-rollback-action-execution?)) #f]
    [(eq? (rollback-action-type action) 'revert-state)
     (if (current-revert-state-fn)
         (begin
           (execute-revert-state! action)
           'revert-state)
         #f)]
    [(eq? (rollback-action-type action) 'force-distill)
     (execute-force-distill! action)
     'force-distill]
    [(eq? (rollback-action-type action) 'expand-context)
     (execute-expand-context! action)
     'expand-context]
    [(eq? (rollback-action-type action) 'warn-only)
     (log-action! action)
     'warn-only]
    [else (rollback-action-type action)]))

;; ── Trigger to Action Mapping ──

;; Convert rollback trigger warnings to recommended actions.
;; Each warning is a (list symbol string) pair from check-rollback-triggers.
;; GAP-H v0.97.11: Symbol-based matching replaces fragile string-contains?.
(define (warnings->actions warnings)
  (for/list ([w (in-list warnings)])
    (define sym
      (if (pair? w)
          (car w)
          #f))
    (define msg
      (if (pair? w)
          (cadr w)
          (format "~a" w)))
    (cond
      [(eq? sym 'amnesia-risk) (make-force-distill-action msg (hasheq 'trigger 'amnesia))]
      [(eq? sym 'excessive-savings)
       (make-expand-context-action msg (hasheq 'trigger 'excessive-savings))]
      [(eq? sym 'exploration-loop)
       (make-force-distill-action msg (hasheq 'trigger 'exploration-loop))]
      [(eq? sym 'stuck-detected) (make-expand-context-action msg (hasheq 'trigger 'stuck))]
      [(eq? sym 'stuck-path) (make-expand-context-action msg (hasheq 'trigger 'stuck-path))]
      [(memq sym '(missing-tool access-denied command-failure timeout generic-error))
       (make-force-distill-action msg (hasheq 'trigger sym))]
      [(eq? sym 'task-amnesia-detected)
       ;; Repeated tool calls indicate amnesia — escalate to distill if threshold reached
       (if (>= (current-loop-warning-count) escalation-threshold)
           (begin
             (current-loop-warning-count 0)
             (make-force-distill-action msg (hasheq 'trigger 'task-amnesia-escalation)))
           (begin
             (increment-loop-warning-count!)
             (make-warn-action msg)))]
      [(eq? sym 'repeat-tool)
       (if (>= (current-loop-warning-count) escalation-threshold)
           (begin
             (current-loop-warning-count 0) ; reset after escalation
             (make-force-distill-action msg (hasheq 'trigger 'repeat-escalation)))
           (begin
             (increment-loop-warning-count!)
             (make-warn-action msg)))]
      ;; Fallback: string-based matching for backwards compatibility
      [(and (string? w) (string-contains? w "amnesia"))
       (make-force-distill-action w (hasheq 'trigger 'amnesia))]
      [(and (string? w) (string-contains? w "excessive"))
       (make-expand-context-action w (hasheq 'trigger 'excessive-savings))]
      [(and (string? w) (string-contains? (string-downcase w) "exploration loop"))
       (make-force-distill-action w (hasheq 'trigger 'exploration-loop))]
      [(and (string? w) (string-contains? (string-downcase w) "stuck"))
       (make-expand-context-action w (hasheq 'trigger 'stuck))]
      [(and (string? w) (string-contains? (string-downcase w) "repeat"))
       (if (>= (current-loop-warning-count) escalation-threshold)
           (begin
             (current-loop-warning-count 0)
             (make-force-distill-action w (hasheq 'trigger 'repeat-escalation)))
           (begin
             (increment-loop-warning-count!)
             (make-warn-action w)))]
      [else (make-warn-action (if (pair? w) msg w))])))

;; ── Pure Detection (Phase 3 extraction) ──
;;
;; detect-rollback-plan is a PURE function that evaluates trigger conditions
;; and returns a rollback-plan (or #f if no triggers fire).
;;
;; It does NOT mutate current-loop-warning-count or any other parameter.
;; The escalation logic from warnings->actions is replicated here in pure form:
;; the caller (execute-rollback-plan!) handles the counter mutation.

;; Pure version of the escalation logic from warnings->actions.
;; Returns (values action new-warning-count) without mutating anything.
(define (pure-warnings->action w current-warning-count)
  (define sym
    (if (pair? w)
        (car w)
        #f))
  (define msg
    (if (pair? w)
        (cadr w)
        (format "~a" w)))
  (define (escalation-for trigger-sym)
    (if (>= current-warning-count escalation-threshold)
        (values (make-force-distill-action msg (hasheq 'trigger trigger-sym)) 0)
        (values (make-warn-action msg) (add1 current-warning-count))))
  (cond
    [(eq? sym 'amnesia-risk)
     (values (make-force-distill-action msg (hasheq 'trigger 'amnesia)) current-warning-count)]
    [(eq? sym 'excessive-savings)
     (values (make-expand-context-action msg (hasheq 'trigger 'excessive-savings))
             current-warning-count)]
    [(eq? sym 'exploration-loop)
     (values (make-force-distill-action msg (hasheq 'trigger 'exploration-loop))
             current-warning-count)]
    [(eq? sym 'stuck-detected)
     (values (make-expand-context-action msg (hasheq 'trigger 'stuck)) current-warning-count)]
    [(eq? sym 'stuck-path)
     (values (make-expand-context-action msg (hasheq 'trigger 'stuck-path)) current-warning-count)]
    [(memq sym '(missing-tool access-denied command-failure timeout generic-error))
     (values (make-force-distill-action msg (hasheq 'trigger sym)) current-warning-count)]
    [(eq? sym 'task-amnesia-detected) (escalation-for 'task-amnesia-escalation)]
    [(eq? sym 'repeat-tool) (escalation-for 'repeat-escalation)]
    [(and (string? w) (string-contains? w "amnesia"))
     (values (make-force-distill-action w (hasheq 'trigger 'amnesia)) current-warning-count)]
    [(and (string? w) (string-contains? w "excessive"))
     (values (make-expand-context-action w (hasheq 'trigger 'excessive-savings))
             current-warning-count)]
    [(and (string? w) (string-contains? (string-downcase w) "exploration loop"))
     (values (make-force-distill-action w (hasheq 'trigger 'exploration-loop)) current-warning-count)]
    [(and (string? w) (string-contains? (string-downcase w) "stuck"))
     (values (make-expand-context-action w (hasheq 'trigger 'stuck)) current-warning-count)]
    [(and (string? w) (string-contains? (string-downcase w) "repeat"))
     (escalation-for 'repeat-escalation)]
    [else (values (make-warn-action (if (pair? w) msg w)) current-warning-count)]))

;; Pure trigger evaluation: computes warnings and recommended action WITHOUT
;; mutating any parameter. Returns rollback-plan or #f if no warnings.
;; The new-warning-count is stored inside the plan for the execution phase.
;;
;; NOTE: This function requires importing check-rollback-triggers from
;; state-aware-helpers.rkt. To avoid a circular dependency, the trigger
;; detection logic is imported lazily via the caller.
(define (detect-rollback-plan* warnings current-warning-count)
  (if (null? warnings)
      #f
      (let loop ([ws warnings]
                 [count current-warning-count]
                 [actions '()])
        (cond
          [(null? ws)
           (define recommended (select-highest-priority-action (reverse actions)))
           (rollback-plan warnings recommended)]
          [else
           (define-values (action new-count) (pure-warnings->action (car ws) count))
           (loop (cdr ws) new-count (cons action actions))]))))

;; ── Effectful Execution (Phase 4 extraction) ──
;;
;; execute-rollback-plan! consumes a rollback-plan and performs the side effects:
;; - Logs warnings
;; - Executes the recommended action via callbacks
;; - Mutates current-loop-warning-count (escalation/reset)
;; Returns the action type symbol if executed, #f otherwise.

(define (execute-rollback-plan! plan)
  (cond
    [(not plan) #f]
    [else
     (define warnings (rollback-plan-warnings plan))
     (define recommended (rollback-plan-recommended-action plan))
     (when (pair? warnings)
       (log-warning "context-assembly: rollback triggers fired: ~a" warnings))
     (cond
       [recommended
        ;; Sync the loop-warning-count by replaying escalation through warnings->actions
        ;; (which has the real side effects). This preserves existing behavior.
        (warnings->actions warnings) ; side effect: increments/resets counter
        (define executed (maybe-execute-action recommended))
        (when executed
          (log-warning "context-assembly: executed rollback action: ~a" executed))
        executed]
       [else #f])]))

;; ── Error-Class Detection (W11, R3/R5) ──
;; v0.99.73 W11: Classify tool error output into signal classes for
;; semantic loop detection. Maps common error patterns to symbols.

(define (tool-error-class->string error-output)
  ;; Extract a high-level error class from tool output text
  (define lower (string-downcase (or error-output "")))
  (cond
    [(regexp-match? #rx"not a git repository" lower) "git-not-found"]
    [(regexp-match? #rx"no such file or directory" lower) "file-not-found"]
    [(regexp-match? #rx"command not found" lower) "command-not-found"]
    [(regexp-match? #rx"permission denied" lower) "permission-denied"]
    [(regexp-match? #rx"exit code|exit status" lower) "non-zero-exit"]
    [(regexp-match? #rx"timeout" lower) "timeout"]
    [(regexp-match? #rx"not found" lower) "not-found"]
    [else "generic-error"]))

(define (error-class->signal error-class)
  ;; Map an error-class string to a warning signal symbol
  (case (string->symbol error-class)
    [(git-not-found file-not-found not-found) 'stuck-path]
    [(command-not-found) 'missing-tool]
    [(permission-denied) 'access-denied]
    [(non-zero-exit) 'command-failure]
    [(timeout) 'timeout]
    [else 'generic-error]))

;; Track error class history across turns
(define current-error-class-history (make-parameter '()))

;; Detect repeated error class: returns the (signal error-class) pair
;; if the same error class appears >= min-repeats times consecutively.
(define (detect-repeated-error-class history [min-repeats 3])
  (define latest (and (pair? history) (caar history)))
  (if latest
      (let ([count (for/fold ([n 0]) ([entry (in-list history)])
                     (if (equal? (car entry) latest)
                         (add1 n)
                         n))])
        (if (>= count min-repeats)
            (list latest (format "error ~a repeated ~a times" latest count))
            #f))
      #f))

;; ── Exports ──

;; W3 v0.99.36: Explicit exports replace struct-out for rollback-action and
;; rollback-actions-config. Documents the exact public surface.
(provide rollback-action
         rollback-action?
         rollback-action-type
         rollback-action-reason
         rollback-action-severity
         rollback-action-metadata
         rollback-actions-config
         rollback-actions-config?
         rollback-actions-config-execution?
         rollback-actions-config-force-distill
         rollback-actions-config-expand-context
         rollback-actions-config-revert-state
         current-rollback-actions-config
         make-default-rollback-config
         current-rollback-action-execution?
         current-rollback-action-log
         current-loop-warning-count
         escalation-threshold
         increment-loop-warning-count!
         current-force-distill-fn
         current-expand-context-fn
         current-revert-state-fn
         rollback-action-type?
         rollback-plan
         rollback-plan?
         rollback-plan-warnings
         rollback-plan-recommended-action
         detect-rollback-plan*
         execute-rollback-plan!
         ;; v0.99.85: Explicit rollback state
         rollback-state
         rollback-state?
         rollback-state-warning-count
         rollback-state-force-distill-active?
         rollback-state-budget-expansion-level
         rollback-state-action-log
         make-default-rollback-state
         current-rollback-state
         apply-rollback-plan!
         (contract-out
          [make-warn-action (-> string? rollback-action?)]
          [make-expand-context-action (-> string? hash? rollback-action?)]
          [make-force-distill-action (-> string? hash? rollback-action?)]
          [make-revert-state-action (-> string? hash? rollback-action?)]
          [select-highest-priority-action (-> (listof rollback-action?) (or/c rollback-action? #f))]
          [maybe-execute-action (-> (or/c rollback-action? #f) (or/c symbol? #f))]
          [warnings->actions
           (-> (listof (or/c string? (list/c symbol? string?))) (listof rollback-action?))]
          [error-class->signal (-> string? symbol?)]
          [current-error-class-history (parameter/c (listof (list/c symbol? string?)))]
          [detect-repeated-error-class
           (-> (listof (list/c symbol? string?)) (or/c #f (list/c symbol? string?)))]
          [tool-error-class->string (-> string? string?)]
          [effective-auto-distill? (-> boolean? rollback-state? boolean?)]
          [effective-conclusion-budget
           (-> exact-nonnegative-integer? rollback-state? exact-nonnegative-integer?)]
          [advance-rollback-state (-> rollback-state? (or/c rollback-plan? #f) rollback-state?)]))

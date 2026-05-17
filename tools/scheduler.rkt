#lang racket/base

;;; tools/scheduler.rkt — preflight, validation, scheduling, ordered result commit.
;;;
;;; Exports:
;;;   - run-tool-batch       — main entry point
;;;   - scheduler-result     — result struct
;;;
;;; Behavior (from ARCHITECTURE.md section 6.4):
;;;   1. preserve source order
;;;   2. run preflight hooks serially
;;;   3. revalidate arguments after hook mutation
;;;   4. execute approved calls, optionally in parallel
;;;   5. emit final results in source order

(require racket/contract
         (only-in "tool.rkt"
                  tool?
                  tool-name
                  tool-schema
                  make-tool
                  make-tool-registry
                  tool-registry?
                  make-exec-context
                  register-tool!
                  lookup-tool
                  validate-tool-args
                  format-tool-schema-hint
                  tool-result?
                  tool-result-content
                  tool-result-details
                  tool-result-is-error?
                  make-error-result
                  make-success-result
                  validate-tool-result
                  exec-context?
                  exec-context-event-publisher
                  exec-context-permission-config
                  tool-call
                  tool-call?
                  make-tool-call
                  tool-call-id
                  tool-call-name
                  tool-call-arguments)
         (only-in "tool-struct.rkt" tool-execute tool-dangerous?)
         (only-in "scheduler-strategy.rkt"
                  scheduler-strategy?
                  scheduler-strategy-preflight-filter
                  scheduler-strategy-execution-order
                  default-scheduler-strategy)
         (only-in "../util/hook-types.rkt" hook-result? hook-result-action hook-result-payload)
         (only-in "../util/safe-mode-predicates.rkt"
                  safe-mode?
                  allowed-tool?
                  allowed-path?
                  safe-mode-project-root)
         (only-in "file-mutation-queue.rkt" with-file-mutation-queue)
         (only-in "permission-gate.rkt" permission-config? tool-needs-approval? request-approval))

;; ── Result struct ──
(provide (struct-out scheduler-result)
         ;; R-15: Strategy support
         (struct-out preflight-entry)
         ;; v0.44.2: Planning structs
         (struct-out scheduler-problem)
         (struct-out scheduler-plan)
         ;; v0.44.2: Typed payloads
         (struct-out tool-pre-hook-payload)
         (struct-out tool-post-hook-payload)
         (struct-out scheduler-batch-stats)
         scheduler-batch-stats->hash
         plan-tool-batch
         execute-tool-plan
         run-preflight
         (contract-out [run-tool-batch
                        (->* ((listof tool-call?) tool-registry?)
                             (#:hook-dispatcher (or/c procedure? #f)
                                                #:exec-context (or/c any/c #f)
                                                #:parallel? (or/c boolean? #f))
                             scheduler-result?)]
                       [max-parallel-tools (parameter/c exact-positive-integer?)]))

;; ============================================================
;; Scheduler result struct
;; ============================================================

(struct scheduler-result (results metadata) #:transparent)

;; v0.21.5 (F6): Maximum parallel tool execution threads.
;; Default 8 — prevents unbounded thread spawning.
(define max-parallel-tools (make-parameter 8))

;; ============================================================
;; Internal: preflight outcome for a single tool call
;; ============================================================

;; An entry in the preflight result list.
;; One of three states:
;;   - 'ready  + tool-call + tool struct   → proceed to execution
;;   - 'blocked + tool-call + #f           → hook blocked it
;;   - 'error   + tool-call + error-msg    → post-hook validation failed / not found

;; v0.35.2 (W-10): Typed struct replaces ad-hoc hasheq
(struct preflight-entry (status tool-call tool error-message) #:transparent)

;; v0.44.2 (R3): Planning-phase structs for scheduler observability
(struct scheduler-problem (calls registry strategy hook-dispatcher exec-context parallel?)
  #:transparent)
(struct scheduler-plan (entries ordered-calls execution-order metadata) #:transparent)

;; v0.44.2 (R5): Typed hook payloads and batch stats
(struct tool-pre-hook-payload (tool-name args entry-id) #:transparent)
(struct tool-post-hook-payload (tool-name result entry-id arguments) #:transparent)
(struct scheduler-batch-stats (total executed blocked errors) #:transparent)

;; Convert scheduler-batch-stats to a hash for event emission.
;; Preserves struct for internal use while providing serializable hash
;; for event bus consumers.
(define (scheduler-batch-stats->hash s)
  (hasheq 'total
          (scheduler-batch-stats-total s)
          'executed
          (scheduler-batch-stats-executed s)
          'blocked
          (scheduler-batch-stats-blocked s)
          'errors
          (scheduler-batch-stats-errors s)))

;; status: 'ready | 'blocked | 'error
;; tool: tool? (#f for blocked/error)
;; error-message: string? (#f for ready)

;; ============================================================
;; Internal: extract path argument from tool call args
;; ============================================================

(define (extract-path-arg args)
  (or (hash-ref args 'path #f) (hash-ref args 'root #f) (hash-ref args 'directory #f)))

;; ============================================================
;; Preflight stage (serial)
;; ============================================================

(define (run-preflight tool-calls registry hook-dispatcher)
  ;; Returns a list of preflight entries, one per tool-call, in order.
  (for/list ([tc (in-list tool-calls)])
    (define tc-after-hook
      (if hook-dispatcher
          (with-handlers ([exn:fail? (lambda (e)
                                       ;; Hook itself threw → treat as block with error
                                       (define msg (format "hook error: ~a" (exn-message e)))
                                       (preflight-entry 'error tc #f msg))])
            (define result (hook-dispatcher 'tool-call tc))
            (cond
              [(not result) tc] ; no handler → pass through
              [(hook-result? result)
               (case (hook-result-action result)
                 [(block) 'blocked]
                 [(amend) (hook-result-payload result)]
                 [else tc])]
              [else result]))
          tc))
    (cond
      ;; Hook returned 'blocked
      [(eq? tc-after-hook 'blocked)
       (preflight-entry 'blocked tc #f (format "tool call '~a' blocked by hook" (tool-call-name tc)))]
      ;; Hook threw an exception (returned as list)
      [(and (preflight-entry? tc-after-hook) (eq? (preflight-entry-status tc-after-hook) 'error))
       tc-after-hook]
      ;; Hook returned a (possibly modified) tool-call
      [(tool-call? tc-after-hook)
       (define t (lookup-tool registry (tool-call-name tc-after-hook)))
       (cond
         [(not t)
          ;; Tool not found in registry
          (preflight-entry 'error
                           tc-after-hook
                           #f
                           (format "unknown tool: '~a'" (tool-call-name tc-after-hook)))]
         [else
          ;; Check safe-mode tool restrictions (SEC-01)
          (cond
            [(and (safe-mode?) (not (allowed-tool? (tool-call-name tc-after-hook))))
             (preflight-entry 'blocked
                              tc-after-hook
                              #f
                              (format "tool '~a' blocked by safe-mode"
                                      (tool-call-name tc-after-hook)))]
            ;; Check safe-mode path restrictions (ARCH-02)
            [(and (safe-mode?)
                  (let ([path-arg (extract-path-arg (tool-call-arguments tc-after-hook))])
                    (and path-arg (not (allowed-path? path-arg)))))
             (define path-arg (extract-path-arg (tool-call-arguments tc-after-hook)))
             (preflight-entry
              'blocked
              tc-after-hook
              #f
              (format
               "Access denied: ~a is outside project root (~a). Safe mode restricts file access to the project directory."
               path-arg
               (safe-mode-project-root)))]
            [else
             ;; Revalidate arguments after potential hook mutation (v0.19.3 W1)
             ;; Capture exception detail to produce actionable error feedback
             (define validation-result
               (with-handlers ([exn:fail? (lambda (e) e)])
                 (validate-tool-args t (tool-call-arguments tc-after-hook))
                 #f))
             (if (not validation-result)
                 (preflight-entry 'ready tc-after-hook t #f)
                 (preflight-entry 'error
                                  tc-after-hook
                                  #f
                                  (format "~a. Usage: ~a"
                                          (exn-message validation-result)
                                          (format-tool-schema-hint t))))])])]
      ;; Unexpected hook return value — treat as error
      [else (preflight-entry 'error tc #f (format "unexpected hook return: ~v" tc-after-hook))])))

;; ============================================================
;; Execute a single tool call (with exception handling)
;; Includes tool-call-pre and tool-result-post hooks (R2-7)
;; ============================================================

(define (execute-single tc t exec-ctx hook-dispatcher)
  ;; Dispatch 'tool-call-pre hook
  (define tc-id (tool-call-id tc))
  (define tc-name (tool-call-name tc))
  (define tc-args (tool-call-arguments tc))

  ;; FEAT-73: emit tool.execution.started lifecycle event
  ;; W-05: include per-tool start-ms for accurate duration tracking
  (define tool-start-ms (current-inexact-milliseconds))
  (define ev-pub (and exec-ctx (exec-context-event-publisher exec-ctx)))
  (when ev-pub
    (ev-pub "tool.execution.started"
            (hasheq 'tool-name tc-name 'tool-call-id tc-id 'start-ms tool-start-ms)))

  (define pre-payload (tool-pre-hook-payload tc-name tc-args tc-id))

  ;; Check if tool-call-pre hook blocks or amends
  (define pre-hook-result
    (if hook-dispatcher
        (with-handlers ([exn:fail? (lambda (e)
                                     (log-warning "tool-call-pre hook threw: ~a" (exn-message e))
                                     #f)])
          (hook-dispatcher 'tool-call-pre pre-payload))
        #f))

  (cond
    [(and (hook-result? pre-hook-result) (eq? (hook-result-action pre-hook-result) 'block))
     ;; Return early with blocked result
     (make-error-result (format "tool '~a' blocked by tool-call-pre hook" tc-name))]
    [else
     ;; Determine which tool call to execute (possibly amended args)
     ;; Validate that hook-amended args is a hash before use
     (define tc-to-execute
       (if (and (hook-result? pre-hook-result)
                (eq? (hook-result-action pre-hook-result) 'amend)
                (hash? (hook-result-payload pre-hook-result))
                (hash-has-key? (hook-result-payload pre-hook-result) 'args))
           (let ([amended-args (hash-ref (hook-result-payload pre-hook-result) 'args)])
             (if (hash? amended-args)
                 (make-tool-call tc-id tc-name amended-args)
                 tc))
           tc))

     ;; G3.4: Permission gate — check if tool needs approval
     (define perm-cfg (exec-context-permission-config exec-ctx))
     (cond
       [(and (permission-config? perm-cfg)
             (tool-needs-approval? perm-cfg tc-name)
             (not (request-approval perm-cfg tc-name (tool-call-arguments tc-to-execute))))
        (make-error-result (format "tool '~a' blocked — approval denied" tc-name))]
       [else
        ;; R-03/R-22: Use tool-dangerous? metadata instead of hardcoded list
        (define args (tool-call-arguments tc-to-execute))
        (define exec-result
          (with-handlers ([exn:fail? (lambda (e)
                                       (make-error-result
                                        (format "tool '~a' raised: ~a" tc-name (exn-message e))))])
            (define path-arg (and (tool-dangerous? t) (hash? args) (hash-ref args 'path #f)))
            (with-file-mutation-queue path-arg (lambda () ((tool-execute t) args exec-ctx)))))

        ;; Dispatch 'tool-result-post hook
        (define post-payload (tool-post-hook-payload tc-name exec-result tc-id args))

        (define post-hook-result
          (if hook-dispatcher
              (with-handlers ([exn:fail? (lambda (e)
                                           (log-warning "tool-result-post hook threw: ~a"
                                                        (exn-message e))
                                           #f)])
                (hook-dispatcher 'tool-result-post post-payload))
              #f))

        (cond
          [(and (hook-result? post-hook-result) (eq? (hook-result-action post-hook-result) 'block))
           ;; Treat block as error
           (make-error-result (format "tool '~a' result blocked by tool-result-post hook" tc-name))]
          [(and (hook-result? post-hook-result)
                (eq? (hook-result-action post-hook-result) 'amend)
                (hash? (hook-result-payload post-hook-result))
                (hash-has-key? (hook-result-payload post-hook-result) 'result))
           ;; Validate post-hook amended result is a valid tool-result
           (let ([amended-result (hash-ref (hook-result-payload post-hook-result) 'result)])
             (if (validate-tool-result amended-result) amended-result exec-result))]
          ;; Return original result
          [else exec-result])])]))

;; ============================================================
;; Execution stage
;; ============================================================

(define (run-execution preflight-entries exec-ctx parallel? hook-dispatcher)
  ;; Returns a list of tool-result in the same order as preflight-entries.
  ;; For 'blocked and 'error entries, produces error results directly.
  ;; For 'ready entries, executes the tool.

  ;; Collect indices and ready entries for execution
  (define indexed-ready
    (for/list ([entry (in-list preflight-entries)]
               [idx (in-naturals)]
               #:when (eq? (preflight-entry-status entry) 'ready))
      (cons idx entry)))

  ;; Execute ready calls
  (define execution-results
    (if parallel?
        ;; Parallel execution using threads with bounded pool (F6)
        (let* ([sem (make-semaphore (max-parallel-tools))]
               [channels (for/list ([ie (in-list indexed-ready)])
                           (define ch (make-channel))
                           (define idx (car ie))
                           (define entry (cdr ie))
                           (define tc (preflight-entry-tool-call entry))
                           (define t (preflight-entry-tool entry))
                           (thread (lambda ()
                                     (semaphore-wait sem)
                                     (define result
                                       (with-handlers ([exn:fail? (lambda (e)
                                                                    (make-error-result
                                                                     (format "tool '~a' raised: ~a"
                                                                             (tool-call-name tc)
                                                                             (exn-message e))))])
                                         (execute-single tc t exec-ctx hook-dispatcher)))
                                     (semaphore-post sem)
                                     (channel-put ch (cons idx result))))
                           ch)])
          (for/list ([ch (in-list channels)])
            (channel-get ch)))
        ;; Serial execution
        (for/list ([ie (in-list indexed-ready)])
          (define idx (car ie))
          (define entry (cdr ie))
          (define tc (preflight-entry-tool-call entry))
          (define t (preflight-entry-tool entry))
          (cons idx (execute-single tc t exec-ctx hook-dispatcher)))))

  ;; Build a map from index → result
  (define results-by-idx
    (for/hasheq ([pair (in-list execution-results)])
      (values (car pair) (cdr pair))))

  ;; Build final ordered list
  (for/list ([entry (in-list preflight-entries)]
             [idx (in-naturals)])
    (define status (preflight-entry-status entry))
    (case status
      [(ready)
       (hash-ref results-by-idx
                 idx
                 (lambda () (make-error-result "internal: missing execution result")))]
      [(blocked) (make-error-result (preflight-entry-error-message entry))]
      [(error) (make-error-result (preflight-entry-error-message entry))])))

;; ============================================================
;; Compute metadata
;; ============================================================

(define (compute-metadata results preflight-entries)
  (define total (length results))
  (define blocked
    (for/sum ([entry (in-list preflight-entries)])
             (if (eq? (preflight-entry-status entry) 'blocked) 1 0)))
  ;; Count errors that are NOT from blocked entries
  (define blocked-indices
    (for/list ([entry (in-list preflight-entries)]
               [idx (in-naturals)]
               #:when (eq? (preflight-entry-status entry) 'blocked))
      idx))
  (define errors
    (for/sum ([r (in-list results)] [idx (in-naturals)]
                                    #:when (and (tool-result-is-error? r)
                                                (not (member idx blocked-indices))))
             1))
  (define executed (- total blocked errors))
  ;; v0.44.4: Return typed struct directly (was dead-unpacking to hasheq)
  (scheduler-batch-stats total executed blocked errors))

;; ============================================================
;; Main entry point
;; ============================================================

;; v0.44.2 (R3): Pure planning phase — constructs scheduler-plan from scheduler-problem
(define (plan-tool-batch problem)
  (define calls (scheduler-problem-calls problem))
  (define registry (scheduler-problem-registry problem))
  (define hook-dispatcher (scheduler-problem-hook-dispatcher problem))
  (define parallel? (scheduler-problem-parallel? problem))
  (define strat (or (scheduler-problem-strategy problem) (default-scheduler-strategy)))
  (define filtered-calls ((scheduler-strategy-preflight-filter strat) calls))
  (define ordered-calls ((scheduler-strategy-execution-order strat) filtered-calls))
  (define entries (run-preflight ordered-calls registry hook-dispatcher))
  (scheduler-plan entries ordered-calls (if parallel? 'parallel 'serial) #f))

;; v0.44.2 (R3): Effectful execution phase — runs a scheduler-plan
(define (execute-tool-plan plan exec-ctx hook-dispatcher parallel? ev-pub)
  (define entries (scheduler-plan-entries plan))
  (when ev-pub
    (ev-pub "tool.batch.preflight.started"
            (hasheq 'toolCount
                    (length (scheduler-plan-ordered-calls plan))
                    'toolNames
                    (map tool-call-name (scheduler-plan-ordered-calls plan)))))
  (define results (run-execution entries exec-ctx parallel? hook-dispatcher))
  (define metadata (compute-metadata results entries))
  (when ev-pub
    (ev-pub "tool.batch.completed" (scheduler-batch-stats->hash metadata)))
  (scheduler-result results metadata))

;; Main entry point — backward compatible
(define (run-tool-batch tool-calls
                        registry
                        #:hook-dispatcher [hook-dispatcher #f]
                        #:exec-context [exec-ctx (make-exec-context)]
                        #:parallel? [parallel? #f]
                        #:strategy [strategy #f])
  (define problem (scheduler-problem tool-calls registry strategy hook-dispatcher exec-ctx parallel?))
  (define plan (plan-tool-batch problem))
  (define ev-pub (and exec-ctx (exec-context-event-publisher exec-ctx)))
  (execute-tool-plan plan exec-ctx hook-dispatcher parallel? ev-pub))

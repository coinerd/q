#lang racket/base

;; Subagent coordinator — execution planning, dispatch, result aggregation.
;; Extracted from spawn-subagent.rkt (v0.99.50+).
;; Owns effectful orchestration: approval, rate limiting, planning, dispatch.

(require racket/contract
         racket/match
         racket/list
         racket/string
         "../../tools/tool.rkt"
         (only-in "../../tools/exec-context.rkt" exec-context-browser-service)
         (only-in "../../util/capability.rkt" valid-capability? current-session-capabilities)
         (only-in "../../util/safe-mode/safe-mode-predicates.rkt" safe-mode? allowed-tool?)
         (only-in "../../runtime/runtime-helpers.rkt" emit-session-event! make-event-bus)
         (only-in "../../runtime/settings.rkt" q-settings? setting-ref)
         (only-in "../../runtime/auto-retry.rkt" with-auto-retry)
         "../model-bridge.rkt"
         (only-in "provider-hash-bridge.rkt" messages->provider-hashes)
         "../../util/ids.rkt"
         (only-in "../../util/content/content-parts.rkt"
                  make-text-part
                  tool-call-part?
                  tool-call-part-id
                  tool-call-part-name
                  tool-call-part-arguments
                  make-tool-call-part
                  make-tool-result-part
                  tool-result-part-tool-call-id
                  tool-result-part-content
                  tool-result-part-is-error?
                  text-part?
                  text-part-text
                  content-part?)
         (only-in "../../util/message/message.rkt"
                  make-message
                  message-role
                  message-content
                  message-id)
         (only-in "../../util/tool/tool-types.rkt" make-tool-call tool-call-id tool-result-content)
         (only-in "../../util/json/json-helpers.rkt" ensure-hash-args)
         (only-in "../../util/error/error-sanitizer.rkt" sanitize-error-message)
         "../../tools/scheduler.rkt"
         ;; Individual builtins for child-safe tool registration (no circular dep)
         (only-in "../builtins/read.rkt" tool-read)
         (only-in "../builtins/write.rkt" tool-write)
         (only-in "../builtins/edit.rkt" tool-edit)
         (only-in "../builtins/bash.rkt" tool-bash)
         (only-in "../builtins/grep.rkt" tool-grep)
         (only-in "../builtins/find.rkt" tool-find)
         (only-in "../builtins/ls.rkt" tool-ls)
         (only-in "../builtins/skill-router.rkt" tool-skill-route)
         ;; v0.99.21 §4.3: Blackboard context injection for subagents
         (only-in "../../runtime/context-assembly/blackboard-context.rkt"
                  build-blackboard-context-snippet)
         ;; W2 v0.99.35: Pure helpers extracted for testability
         "spawn-subagent-helpers.rkt"
         "spawn-execution-plan.rkt"
         (only-in (submod "spawn-execution-plan.rkt" orchestration)
                  batch-execution-plan-job-arguments)
         (prefix-in approval: "spawn-approval.rkt")
         (prefix-in rate: "spawn-rate-limit.rkt")
         (only-in "../../util/cancellation.rkt"
                  cancellation-token-cancelled?
                  make-cancellation-token
                  cancel-token!)
         (only-in "../../util/message/provider-transport.rkt"
                  provider-tool-call-type?
                  provider-tool-stop-reason?
                  provider-completion-stop-reason?)
         (only-in "../../runtime/approval/broker.rkt" call-with-approval-grant)
         ;; Import run-subagent and child-safe-tools from execution module
         (only-in "spawn-execution.rkt" run-subagent child-safe-tools child-safe-tools-filtered))

(provide (contract-out [resolve-role-prompt (-> (or/c string? #f) string?)]
                       [parse-subagent-config (-> any/c subagent-config?)])
         ;; Tool entries (direct — used by scheduler)
         tool-spawn-subagent
         tool-spawn-subagents
         ;; Child-safe tool list (for testing and capability facade)
         child-safe-tools
         ;; Struct (direct for match compatibility)
         subagent-config
         subagent-config?
         subagent-config-task
         subagent-config-role
         subagent-config-max-turns
         subagent-config-tools
         subagent-config-model
         subagent-config-capabilities
         ;; v0.99.21 §4.2: Capability-aware tool filtering
         child-safe-tools-filtered
         ;; v0.99.21 §4.3: Blackboard context injection for subagents
         build-subagent-blackboard-context
         ;; v0.99.22 A-2: Batch capabilities parsing
         parse-job-capabilities
         ;; v0.99.23 §5.3: HITL approval for dangerous spawns
         requires-hitl-approval?
         request-spawn-approval
         request-batch-spawn-approval
         run-subagent-with-config
         ;; v0.99.23 §5.1: Session-wide agent pool limit
         current-agent-pool-limit
         ;; W1: deterministic rate-boundary testing/introspection
         current-spawn-timestamps
         ;; W2 v0.99.35: Re-export pure helpers for backward compat
         normalize-capabilities
         extract-assistant-text
         extract-text-summary
         ;; v0.99.50 W1 (TMUX-04): Typed terminal outcomes and safe metadata
         classify-terminal-status
         make-safe-result-metadata
         result-has-content?
         ;; v0.99.63 W0: Batch timeout configuration and execution
         DEFAULT-BATCH-DEADLINE-MS
         run-jobs-parallel)

;; v0.99.23 §5.1: Session-wide agent pool limit parameter.
;; Default 3 (matching the previous hardcoded max). CLI --agent-pool overrides.
(define current-agent-pool-limit (make-parameter 3))

;; Re-exported compatibility handles owned by focused boundaries.
(define current-spawn-timestamps rate:current-spawn-timestamps)
(define spawn-rate-capacity? rate:spawn-rate-capacity?)
(define reserve-spawn-rate! rate:reserve-spawn-rate!)

;; Focused approval transport owns correlation, preview safety, and headless policy.
(define request-spawn-approval approval:request-spawn-approval)
(define request-batch-spawn-approval approval:request-batch-spawn-approval)

(define (make-denied-spawn-result message)
  (make-tool-result (list (hasheq 'type "text" 'text message))
                    (hasheq 'terminal-status "denied" 'child-created? #f)
                    #t))

;; Resolve a role value: if it matches a skill name, load that skill as the prompt.
;; Otherwise use it as a literal prompt string.
(define (resolve-role-prompt role-value)
  (if (string? role-value)
      (let* ([result (tool-skill-route (hasheq 'action "context" 'name role-value))])
        (if (and (tool-result? result) (not (tool-result-is-error? result)))
            ;; Skill found — use its content as the role prompt
            (let ([content (hash-ref (car (tool-result-content result)) 'text "")])
              (if (non-empty-string? content) content default-subagent-role-prompt))
            ;; Not a skill name — use as literal prompt
            (if (non-empty-string? role-value) role-value default-subagent-role-prompt)))
      default-subagent-role-prompt))

;; The spawn-subagent tool implementation
(define (tool-spawn-subagent args [exec-ctx #f])
  (with-handlers ([exn:fail? (lambda (e)
                               (make-error-result (format "spawn-subagent failed: ~a"
                                                          (exn-message e))))])
    ;; Validate the complete request before consuming rate budget or performing
    ;; approval/provider/session effects.
    (define cfg (parse-subagent-config args))
    (run-subagent-with-config cfg exec-ctx)))

;; Public compatibility parser; normalization lives at the planning boundary.
(define (parse-subagent-config args)
  (parse-subagent-config/request args (map tool-name (child-safe-tools))))

;; Execute a normalized, digest-bound plan. Effect resolution remains here;
;; normalization, binding assembly, redaction, and commitment live below it.
(define (run-subagent-with-config cfg exec-ctx)
  (with-handlers ([exn:fail? (lambda (e)
                               (make-error-result (format "invalid subagent configuration: ~a"
                                                          (exn-message e))))])
    (define planned-safe-mode? (safe-mode?))
    (define request
      (normalize-subagent-request cfg
                                  (child-safe-tools)
                                  (current-session-capabilities)
                                  planned-safe-mode?))
    (define resolved-role-prompt (resolve-role-prompt (normalized-subagent-request-role request)))
    (define planned-blackboard-context (build-subagent-blackboard-context))
    (define planned-provider (resolve-provider exec-ctx))
    (define resolved-model
      (resolve-model-name exec-ctx (hasheq 'model (normalized-subagent-request-model request))))
    (define plan
      (build-single-execution-plan request
                                   resolved-role-prompt
                                   planned-blackboard-context
                                   exec-ctx
                                   planned-provider
                                   resolved-model))
    (define approval-plan (single-execution-plan-approval-plan plan))
    (define dangerous? (requires-hitl-approval? (single-execution-plan-capabilities plan)))
    (define (execute!)
      (if (reserve-spawn-rate! 1)
          (run-subagent (single-execution-plan-execution-arguments plan)
                        (single-execution-plan-role-prompt plan)
                        (single-execution-plan-max-turns plan)
                        (single-execution-plan-effective-tools plan)
                        exec-ctx)
          (make-error-result "spawn rate limit exceeded (30/min)")))
    (cond
      [(not (spawn-rate-capacity? 1)) (make-error-result "spawn rate limit exceeded (30/min)")]
      [(not dangerous?) (execute!)]
      [else
       (define grant (request-spawn-approval approval-plan exec-ctx))
       (if (not grant)
           (make-denied-spawn-result "subagent spawn blocked — HITL approval denied")
           (or (call-with-approval-grant grant (spawn-execution-plan-digest approval-plan) execute!)
               (make-denied-spawn-result
                "subagent spawn blocked — approval grant stale or revoked")))])))

;; Resolve the provider from exec-context runtime-settings.
;; Returns the real provider if available, or falls back to a mock.
(define (rt-settings-ref rt key [default #f])
  (cond
    [(hash? rt) (hash-ref rt key default)]
    [(q-settings? rt) (setting-ref rt key default)]
    [else default]))

;; #1204: Provider injection from parent session.
;; BUG: make-minimal-settings stores provider under 'default-provider,
;; not 'provider. Must check both keys to avoid silent mock fallback.
(define (resolve-provider exec-ctx)
  (define rt (and exec-ctx (exec-context-runtime-settings exec-ctx)))
  (define prov
    (or (and rt (rt-settings-ref rt 'provider #f))
        (and rt (rt-settings-ref rt 'default-provider #f))))
  (cond
    [(provider? prov) prov]
    ;; Settings loaded from config.json store provider names as strings.
    ;; Never pass those names directly to provider-send; if the parent runtime
    ;; did not inject a provider struct, fall back safely to the mock provider.
    [(string? prov) (build-mock-provider-for-subagent)]
    [else (build-mock-provider-for-subagent)]))

;; Resolve model name from exec-context or fall back to "mock-model".
;; BUG: make-minimal-settings stores model under 'default-model, not 'model.
(define (resolve-model-name exec-ctx args)
  (or (hash-ref args 'model #f)
      (let ([rt (and exec-ctx (exec-context-runtime-settings exec-ctx))])
        (or (and rt (rt-settings-ref rt 'model #f)) (and rt (rt-settings-ref rt 'default-model #f))))
      "mock-model"))

;; v0.99.21 §4.3: Build blackboard context string for subagent injection.
;; Reads the parent session's blackboard via current-blackboard parameter.
;; Returns a formatted string with '## Parent Session Context' header when
;; the blackboard has content, or "" when no blackboard is available.
;; Uses the existing build-blackboard-context-snippet which handles:
;;   - #f when no blackboard or empty
;;   - Token budget guard (capped at 500 chars)
(define (build-subagent-blackboard-context)
  (define snippet (build-blackboard-context-snippet))
  (if (and snippet (> (string-length snippet) 0))
      (string-append "## Parent Session Context
" snippet "
")
      ""))

;; Helper: create a mock provider for subagent testing (fallback when no real provider)
(define (build-mock-provider-for-subagent)
  (define mock-response
    (make-model-response (list (hasheq 'type "text" 'text "Subagent task completed."))
                         (hasheq 'prompt-tokens 0 'completion-tokens 0 'total-tokens 0)
                         "mock-model"
                         'stop))
  (make-mock-provider mock-response #:name "subagent-mock"))

;; ============================================================
;; spawn-subagents: Batch parallel execution
;; ============================================================

;; Batch request parsing and immutable commitment construction are owned by
;; spawn-execution-plan.rkt; this module only coordinates their effects.

;; Run a single already-planned job and return (cons job-id result).
(define (run-single-job job plan parent-ctx)
  (define job-id (hash-ref job 'job-id))
  (define task (hash-ref job 'task #f))
  (define role-prompt (or (hash-ref job 'role #f) default-subagent-role-prompt))
  (define max-turns (hash-ref job 'max-turns))
  (define result
    (if (not task)
        (make-error-result "task is required for each job")
        (run-subagent (batch-execution-plan-job-arguments plan job)
                      role-prompt
                      max-turns
                      (hash-ref job 'effective-tools)
                      parent-ctx)))
  (cons job-id result))

;; Batch subagent execution with immutable planning and one all-or-nothing
;; dangerous-capability approval gate.
(define (tool-spawn-subagents args [exec-ctx #f])
  (define-values (request validation-error) (normalize-batch-request args))
  (if validation-error
      (make-error-result validation-error)
      (with-handlers ([exn:fail? (lambda (e)
                                   (make-error-result (format "spawn-subagents failed: ~a"
                                                              (exn-message e))))])
        (define planned-provider (resolve-provider exec-ctx))
        (define resolved-model (resolve-model-name exec-ctx (hasheq)))
        (define planned-safe-mode? (safe-mode?))
        (define planned-blackboard-context (build-subagent-blackboard-context))
        (define plan
          (build-batch-execution-plan request
                                      exec-ctx
                                      planned-provider
                                      resolved-model
                                      planned-safe-mode?
                                      (child-safe-tools)
                                      (current-session-capabilities)
                                      (current-agent-pool-limit)
                                      planned-blackboard-context))
        (define spawn-count (length (batch-execution-plan-jobs plan)))
        (define dangerous? (pair? (batch-execution-plan-dangerous-jobs plan)))
        (define approval-plan (batch-execution-plan-approval-plan plan))
        (define (execute!)
          (cond
            [(not (batch-execution-plan-request-matches? plan args))
             (make-error-result "subagent batch request changed after approval")]
            [(not (reserve-spawn-rate! spawn-count))
             (make-error-result "spawn rate limit exceeded (30/min)")]
            [else (finish-batch plan exec-ctx)]))
        (cond
          [(not (spawn-rate-capacity? spawn-count))
           (make-error-result "spawn rate limit exceeded (30/min)")]
          [(not dangerous?) (execute!)]
          [else
           (define grant (request-batch-spawn-approval approval-plan exec-ctx))
           (if (not grant)
               (make-denied-spawn-result "subagent batch blocked — HITL approval denied")
               (or
                (call-with-approval-grant grant (spawn-execution-plan-digest approval-plan) execute!)
                (make-denied-spawn-result
                 "subagent batch blocked — approval grant stale or revoked")))]))))

(define (finish-batch plan exec-ctx)
  (define results
    (run-jobs-parallel (batch-execution-plan-jobs plan)
                       plan
                       exec-ctx
                       (batch-execution-plan-max-parallel plan)
                       #:batch-deadline-ms (batch-execution-plan-batch-timeout-ms plan)))
  (define job-results
    (for/list ([entry (in-list results)]
               [job (in-list (batch-execution-plan-jobs plan))])
      (define result (cdr entry))
      (define metadata (or (tool-result-details result) (hasheq)))
      (hasheq 'jobId
              (car entry)
              'success
              (not (tool-result-is-error? result))
              'terminalStatus
              (hash-ref metadata 'terminal-status "failed")
              'turnsUsed
              (hash-ref metadata 'turns-used 0)
              'resultPresent
              (hash-ref metadata 'result-present? #f)
              'contentSize
              (hash-ref metadata 'content-size 0)
              'contentDigest
              (hash-ref metadata 'content-digest "")
              'taskDigest
              (hash-ref job 'task-digest)
              'capabilities
              (hash-ref job 'effective-capabilities)
              'toolCallId
              (hash-ref job 'tool-call-id)
              'childId
              (hash-ref job 'child-id)
              'sessionId
              (hash-ref job 'session-id))))
  ;; Raw child output belongs only to the explicit tool-result content channel.
  ;; Retained details/events stay digest-only and safe for logs/traces.
  (define visible-job-results
    (for/list ([job-result (in-list job-results)]
               [entry (in-list results)])
      (hasheq 'jobId
              (hash-ref job-result 'jobId)
              'success
              (hash-ref job-result 'success #f)
              'content
              (tool-result-content (cdr entry)))))
  (define success-count (count (lambda (result) (hash-ref result 'success #f)) job-results))
  (define fail-count (- (length job-results) success-count))
  (define summary-text
    (if (batch-execution-plan-aggregate? plan)
        (format "Batch complete: ~a/~a succeeded, ~a failed.

~a"
                success-count
                (length job-results)
                fail-count
                (string-join
                 (for/list ([job-result (in-list job-results)]
                            [entry (in-list results)])
                   (format "[~a] ~a"
                           (hash-ref job-result 'jobId)
                           (if (hash-ref job-result 'success #f)
                               (extract-text-summary (tool-result-content (cdr entry)))
                               (string-upcase (hash-ref job-result 'terminalStatus "failed")))))
                 "
---
"))
        (format "Batch complete: ~a/~a succeeded, ~a failed."
                success-count
                (length job-results)
                fail-count)))
  (make-success-result (list (hasheq 'type "text" 'text summary-text)
                             (hasheq 'type "batch-results" 'jobs visible-job-results))
                       (hasheq 'batch-id
                               (batch-execution-plan-batch-id plan)
                               'batchId
                               (batch-execution-plan-batch-id plan)
                               'snapshot-digest
                               (spawn-execution-plan-digest (batch-execution-plan-approval-plan plan))
                               'total-jobs
                               (length job-results)
                               'succeeded
                               success-count
                               'failed
                               fail-count
                               'jobs
                               job-results)))

;; Default batch timeout: 5 minutes (300000 ms)
(define DEFAULT-BATCH-DEADLINE-MS 300000)

;; Run jobs in parallel with bounded concurrency using threads.
;; If #:batch-deadline-ms is provided, the batch is cancelled if all jobs
;; don't complete within the deadline. Timed-out jobs get error results.
(define (run-jobs-parallel jobs
                           plan
                           exec-ctx
                           max-parallel
                           #:batch-deadline-ms [deadline-ms DEFAULT-BATCH-DEADLINE-MS])
  (define n (length jobs))
  (cond
    ;; Single job: run directly (no thread overhead, no timeout needed)
    [(= n 1) (list (run-single-job (car jobs) plan exec-ctx))]
    ;; Multiple jobs: use threads with bounded concurrency. Each worker owns a
    ;; distinct vector slot, so completion timing cannot reorder output.
    ;; Race thread completion against a deadline to prevent hanging.
    [else
     (define ordered-results (make-vector n #f))
     (define concurrency-sem (make-semaphore max-parallel))
     ;; Create a dedicated cancellation token for batch-level timeout
     (define batch-token (make-cancellation-token))
     ;; Build child exec-context with batch token (replacing parent token)
     ;; Handle #f exec-ctx for testing compatibility
     (define child-ctx
       (if exec-ctx
           (make-exec-context #:working-directory (exec-context-working-directory exec-ctx)
                              #:cancellation-token batch-token
                              #:event-publisher (exec-context-event-publisher exec-ctx)
                              #:runtime-settings (exec-context-runtime-settings exec-ctx)
                              #:call-id (exec-context-call-id exec-ctx)
                              #:session-metadata (exec-context-session-metadata exec-ctx)
                              #:progress-callback (exec-context-progress-callback exec-ctx)
                              #:permission-config (exec-context-permission-config exec-ctx)
                              #:browser-service (exec-context-browser-service exec-ctx))
           (make-exec-context #:cancellation-token batch-token)))
     (define threads
       (for/list ([job (in-list jobs)]
                  [index (in-naturals)])
         (thread (lambda ()
                   (call-with-semaphore
                    concurrency-sem
                    (lambda ()
                      (vector-set! ordered-results index (run-single-job job plan child-ctx))))))))
     ;; Wait for ALL threads to complete with wall-clock deadline.
     ;; BUGFIX v0.99.64: Previously used (apply choice-evt (map thread-dead-evt threads))
     ;; which fires on the FIRST thread death, not ALL. Now loops through threads
     ;; sequentially with a shrinking remaining-time budget.
     (define deadline-ms* (+ (current-inexact-milliseconds) deadline-ms))
     (define timed-out? #f)
     (for ([t (in-list threads)])
       (unless timed-out?
         (define remaining (- deadline-ms* (current-inexact-milliseconds)))
         (cond
           [(<= remaining 0) (set! timed-out? #t)]
           [else
            (define result (sync/timeout (/ remaining 1000.0) (thread-dead-evt t)))
            (when (not result)
              (set! timed-out? #t))])))
     ;; If timed out, cancel remaining threads
     (when timed-out?
       (log-warning "SPAWN-BATCH: timeout after ~a ms, cancelling ~a remaining jobs" deadline-ms n)
       (cancel-token! batch-token)
       ;; Grace period for cooperative cancellation, then force-kill orphaned threads
       (for ([t (in-list threads)]
             [i (in-naturals)])
         (unless (vector-ref ordered-results i)
           (unless (sync/timeout 5 (thread-dead-evt t))
             (kill-thread t)
             (log-warning "SPAWN-BATCH: force-killed thread ~a after grace period" i)))))
     ;; Fill in error results for any unfinished jobs
     (for ([i (in-range n)])
       (unless (vector-ref ordered-results i)
         (define job (list-ref jobs i))
         (define job-id (hash-ref job 'job-id))
         (vector-set! ordered-results
                      i
                      (cons job-id
                            (make-error-result
                             (format "subagent cancelled: batch deadline (~a ms) exceeded"
                                     deadline-ms))))))
     (vector->list ordered-results)]))

#lang racket/base

;; Immutable planning boundary for single and batch subagent execution.
;; This module normalizes requests and commits execution-effective data, but
;; performs no transport, concurrency, approval, rate, or session effects.

(require racket/contract
         racket/list
         racket/string
         "spawn-subagent-helpers.rkt"
         (only-in "../../tools/tool.rkt"
                  exec-context-working-directory
                  exec-context-session-metadata
                  exec-context-call-id
                  tool-name
                  tool-required-capability)
         (only-in "../model-bridge.rkt" provider-name)
         (only-in "../../util/ids.rkt" generate-id)
         (only-in "../../util/safe-mode/safe-mode-predicates.rkt" allowed-tool?)
         (only-in "../../util/credential-redaction.rkt" redact-secrets))

(provide (contract-out [make-spawn-execution-plan (-> symbol? hash? hash? spawn-execution-plan?)]
                       [spawn-execution-plan? (-> any/c boolean?)]
                       [spawn-execution-plan-kind (-> spawn-execution-plan? symbol?)]
                       [spawn-execution-plan-snapshot (-> spawn-execution-plan? hash?)]
                       [spawn-execution-plan-digest (-> spawn-execution-plan? string?)]
                       [spawn-execution-plan-presentation (-> spawn-execution-plan? hash?)]
                       [spawn-execution-plan-presentation-digest (-> spawn-execution-plan? string?)]
                       [spawn-execution-plan-matches? (-> spawn-execution-plan? hash? boolean?)])
         redacted-approval-preview
         default-subagent-role-prompt
         ;; Public compatibility data type, re-exported by spawn-subagent.rkt.
         subagent-config
         subagent-config?
         subagent-config-task
         subagent-config-role
         subagent-config-max-turns
         subagent-config-tools
         subagent-config-model
         subagent-config-capabilities
         parse-subagent-config/request
         parse-job-capabilities
         ;; Single-spawn normalization and commitment.
         normalize-subagent-request
         normalized-subagent-request-role
         normalized-subagent-request-model
         build-single-execution-plan
         single-execution-plan-capabilities
         single-execution-plan-approval-plan
         single-execution-plan-role-prompt
         single-execution-plan-max-turns
         single-execution-plan-effective-tools
         single-execution-plan-execution-arguments
         ;; Batch request normalization and immutable execution commitment.
         normalize-batch-request
         build-batch-execution-plan
         batch-execution-plan-batch-id
         batch-execution-plan-jobs
         batch-execution-plan-snapshot
         batch-execution-plan-dangerous-jobs
         batch-execution-plan-max-parallel
         batch-execution-plan-aggregate?
         batch-execution-plan-batch-timeout-ms
         batch-execution-plan-approval-plan
         batch-execution-plan-request-matches?)

;; ============================================================
;; Generic immutable approval commitment
;; ============================================================

(struct spawn-execution-plan (kind snapshot digest presentation presentation-digest)
  #:transparent
  #:constructor-name make-raw-spawn-execution-plan)

(define (redacted-approval-preview value [limit 200])
  (define redacted (redact-secrets (if (string? value) value "")))
  (define control-safe
    (list->string (for/list ([char (in-string redacted)])
                    (define code (char->integer char))
                    (if (or (< code 32) (and (>= code 127) (<= code 159))) #\space char))))
  (define compact (string-join (string-split control-safe) " "))
  (if (> (string-length compact) limit)
      (substring compact 0 limit)
      compact))

(define (make-spawn-execution-plan kind snapshot presentation)
  (unless (memq kind '(single batch))
    (raise-argument-error 'make-spawn-execution-plan "'single or 'batch" kind))
  (define frozen-snapshot (immutable-canonical-copy snapshot))
  (define frozen-presentation (immutable-canonical-copy presentation))
  (make-raw-spawn-execution-plan kind
                                 frozen-snapshot
                                 (sha256-digest frozen-snapshot)
                                 frozen-presentation
                                 (sha256-digest frozen-presentation)))

(define (spawn-execution-plan-matches? plan candidate-snapshot)
  (string=? (spawn-execution-plan-digest plan)
            (sha256-digest (immutable-canonical-copy candidate-snapshot))))

;; ============================================================
;; Shared request and binding normalization
;; ============================================================

(define default-subagent-role-prompt
  "You are a focused assistant executing a specific delegated task. Complete the task efficiently and return the result.")

;; Kept transparent because callers historically construct and match this
;; compatibility type through spawn-subagent.rkt.
(struct subagent-config (task role max-turns tools model capabilities) #:transparent)

(define (immutable-string value)
  (and (string? value) (string->immutable-string (string-copy value))))

(define (validate-tool-allowlist tools available-tool-names who)
  (unless (or (not tools) (and (list? tools) (andmap string? tools)))
    (raise-argument-error who "#f or a list of tool-name strings" tools))
  (when tools
    (for ([name (in-list tools)])
      (unless (member name available-tool-names)
        (raise-arguments-error who "unknown or unsafe child tool" "tool" name))))
  tools)

;; Parse the legacy public configuration while leaving concrete child-tool
;; ownership in the orchestrator.
(define (parse-subagent-config/request args available-tool-names)
  (unless (hash? args)
    (raise-argument-error 'parse-subagent-config "hash?" args))
  (define task (hash-ref args 'task #f))
  (unless (and (string? task) (not (string=? (string-trim task) "")))
    (raise-argument-error 'parse-subagent-config "a non-empty task string" task))
  (define role (hash-ref args 'role default-subagent-role-prompt))
  (unless (string? role)
    (raise-argument-error 'parse-subagent-config "a role string" role))
  (define max-turns (hash-ref args 'max-turns 10))
  (unless (and (exact-integer? max-turns) (positive? max-turns))
    (raise-argument-error 'parse-subagent-config "a positive exact max-turns integer" max-turns))
  (define tools
    (validate-tool-allowlist (hash-ref args 'tools #f) available-tool-names 'parse-subagent-config))
  (define model (hash-ref args 'model #f))
  (unless (or (not model) (string? model))
    (raise-argument-error 'parse-subagent-config "#f or a model string" model))
  (define capabilities
    (if (hash-has-key? args 'capabilities)
        (normalize-capabilities/strict (hash-ref args 'capabilities))
        #f))
  (subagent-config (immutable-string task)
                   (immutable-string role)
                   max-turns
                   (and tools (map immutable-string tools))
                   (immutable-string model)
                   capabilities))

(define (parse-job-capabilities job)
  (if (hash-has-key? job 'capabilities)
      (normalize-capabilities/strict (hash-ref job 'capabilities))
      #f))

(define (capture-working-directory exec-ctx)
  (define cwd
    (if exec-ctx
        (exec-context-working-directory exec-ctx)
        (current-directory)))
  (cond
    [(path? cwd) (immutable-string (path->string cwd))]
    [(string? cwd) (immutable-string cwd)]
    [else #f]))

(define (capture-parent-session-id exec-ctx)
  (define metadata (and exec-ctx (exec-context-session-metadata exec-ctx)))
  (define session-id (and (hash? metadata) (hash-ref metadata 'session-id #f)))
  (if (string? session-id)
      (immutable-string session-id)
      ""))

(define (capture-parent-call-id exec-ctx)
  (if exec-ctx
      (immutable-string (exec-context-call-id exec-ctx))
      ""))

(define (capture-provider-binding provider)
  (define name
    (with-handlers ([exn:fail? (lambda (_) "unknown-provider")])
      (provider-name provider)))
  ;; Identity commits the selected object without retaining or presenting it.
  (hasheq 'name (immutable-string name) 'identity (eq-hash-code provider)))

(define (capture-execution-bindings exec-ctx provider)
  (hasheq 'cwd
          (capture-working-directory exec-ctx)
          'parent-session-id
          (capture-parent-session-id exec-ctx)
          'parent-call-id
          (capture-parent-call-id exec-ctx)
          'provider-binding
          (capture-provider-binding provider)))

(define (child-tool-names-for child-tools capabilities planned-safe-mode?)
  (define capability-tools
    (cond
      [(not capabilities) child-tools]
      [(null? capabilities) '()]
      [(memq 'any capabilities)
       (error 'child-tool-names-for "delegated any wildcard is not permitted")]
      [else
       (filter (lambda (child-tool)
                 (define required (tool-required-capability child-tool))
                 (or (eq? required 'any) (memq required capabilities)))
               child-tools)]))
  (for/list ([child-tool (in-list capability-tools)]
             #:when (or (not planned-safe-mode?) (allowed-tool? (tool-name child-tool))))
    (tool-name child-tool)))

(define (role-with-context blackboard-context role)
  (immutable-string (if (> (string-length blackboard-context) 0)
                        (string-append blackboard-context "\n" role)
                        role)))

;; ============================================================
;; Single-spawn plan
;; ============================================================

(struct normalized-subagent-request
        (task role max-turns requested-tools model capabilities effective-tools safe-mode?))

(struct single-execution-plan
        (task role-prompt
              max-turns
              effective-tools
              capabilities
              model
              safe-mode?
              cwd
              parent-session-id
              parent-call-id
              tool-call-id
              child-id
              session-id
              approval-plan
              provider))

(define (normalize-subagent-request cfg child-tools parent-capabilities planned-safe-mode?)
  (unless (subagent-config? cfg)
    (raise-argument-error 'normalize-subagent-request "subagent-config?" cfg))
  (define task (immutable-string (subagent-config-task cfg)))
  (unless (and task (not (string=? (string-trim task) "")))
    (error 'run-subagent-with-config "task is required"))
  (define role (subagent-config-role cfg))
  (unless (string? role)
    (error 'run-subagent-with-config "role must be a string"))
  (define max-turns (subagent-config-max-turns cfg))
  (unless (and (exact-integer? max-turns) (positive? max-turns))
    (error 'run-subagent-with-config "max-turns must be a positive exact integer"))
  (define available-names (map tool-name child-tools))
  (define tools
    (validate-tool-allowlist (subagent-config-tools cfg) available-names 'run-subagent-with-config))
  (define model (subagent-config-model cfg))
  (unless (or (not model) (string? model))
    (error 'run-subagent-with-config "model must be #f or a string"))
  (define frozen-role (immutable-string role))
  (define frozen-tools (and tools (map immutable-string tools)))
  (define declared-capabilities
    (if (eq? (subagent-config-capabilities cfg) #f)
        #f
        (normalize-capabilities/strict (subagent-config-capabilities cfg))))
  (define capabilities (bounded-delegated-capabilities declared-capabilities parent-capabilities))
  (define capability-tools (child-tool-names-for child-tools capabilities planned-safe-mode?))
  (define effective-tools
    (map immutable-string
         (if frozen-tools
             (filter (lambda (name) (member name frozen-tools)) capability-tools)
             capability-tools)))
  (normalized-subagent-request task
                               frozen-role
                               max-turns
                               frozen-tools
                               (immutable-string model)
                               capabilities
                               effective-tools
                               planned-safe-mode?))

(define (build-single-execution-plan request
                                     resolved-role-prompt
                                     blackboard-context
                                     exec-ctx
                                     planned-provider
                                     resolved-model)
  (unless (normalized-subagent-request? request)
    (raise-argument-error 'build-single-execution-plan "normalized-subagent-request?" request))
  (unless (string? resolved-role-prompt)
    (error 'run-subagent-with-config "resolved role prompt must be a string"))
  (unless (string? blackboard-context)
    (raise-argument-error 'build-single-execution-plan "string?" blackboard-context))
  (unless (string? resolved-model)
    (error 'run-subagent-with-config "resolved model must be a string"))
  (define task (normalized-subagent-request-task request))
  (define role-prompt (role-with-context blackboard-context resolved-role-prompt))
  (define model (immutable-string resolved-model))
  (define max-turns (normalized-subagent-request-max-turns request))
  (define capabilities (normalized-subagent-request-capabilities request))
  (define effective-tools (normalized-subagent-request-effective-tools request))
  (define planned-safe-mode? (normalized-subagent-request-safe-mode? request))
  (define bindings (capture-execution-bindings exec-ctx planned-provider))
  (define tool-call-id (immutable-string (generate-id)))
  (define child-id (immutable-string (generate-id)))
  (define session-id (immutable-string (generate-id)))
  (define snapshot
    (hasheq 'task-digest
            (sha256-digest task)
            'role-digest
            (sha256-digest role-prompt)
            'model
            model
            'effective-capabilities
            capabilities
            'effective-tools
            effective-tools
            'max-turns
            max-turns
            'cwd
            (hash-ref bindings 'cwd)
            'parent-session-id
            (hash-ref bindings 'parent-session-id)
            'parent-call-id
            (hash-ref bindings 'parent-call-id)
            'safe-mode
            planned-safe-mode?
            'provider-binding
            (hash-ref bindings 'provider-binding)
            'tool-call-id
            tool-call-id
            'child-id
            child-id
            'session-id
            session-id))
  (define provider-binding (hash-ref bindings 'provider-binding))
  (define presentation
    (hasheq 'task-preview
            (redacted-approval-preview task)
            'task-digest
            (sha256-digest task)
            'role-preview
            (redacted-approval-preview role-prompt)
            'role-digest
            (sha256-digest role-prompt)
            'model-preview
            (redacted-approval-preview model)
            'capabilities
            capabilities
            'effective-tools
            effective-tools
            'max-turns
            max-turns
            'cwd-preview
            (redacted-approval-preview (or (hash-ref bindings 'cwd) ""))
            'provider-preview
            (redacted-approval-preview (hash-ref provider-binding 'name ""))
            'parent-session-preview
            (redacted-approval-preview (hash-ref bindings 'parent-session-id))
            'parent-call-preview
            (redacted-approval-preview (hash-ref bindings 'parent-call-id))
            'safe-mode
            planned-safe-mode?
            'tool-call-id
            tool-call-id
            'child-id
            child-id
            'session-id
            session-id))
  (single-execution-plan task
                         role-prompt
                         max-turns
                         effective-tools
                         capabilities
                         model
                         planned-safe-mode?
                         (hash-ref bindings 'cwd)
                         (hash-ref bindings 'parent-session-id)
                         (hash-ref bindings 'parent-call-id)
                         tool-call-id
                         child-id
                         session-id
                         (make-spawn-execution-plan 'single snapshot presentation)
                         planned-provider))

(define (single-execution-plan-execution-arguments plan)
  (hasheq 'task
          (single-execution-plan-task plan)
          'role
          (single-execution-plan-role-prompt plan)
          'max-turns
          (single-execution-plan-max-turns plan)
          'model
          (single-execution-plan-model plan)
          'capabilities
          (single-execution-plan-capabilities plan)
          'planned-provider
          (single-execution-plan-provider plan)
          'planned-safe-mode
          (single-execution-plan-safe-mode? plan)
          'planned-working-directory
          (single-execution-plan-cwd plan)
          'planned-parent-session-id
          (single-execution-plan-parent-session-id plan)
          'planned-parent-call-id
          (single-execution-plan-parent-call-id plan)
          'planned-tool-call-id
          (single-execution-plan-tool-call-id plan)
          'planned-child-id
          (single-execution-plan-child-id plan)
          'planned-session-id
          (single-execution-plan-session-id plan)))

;; ============================================================
;; Batch-spawn plan
;; ============================================================

(struct batch-request (raw jobs max-parallel aggregate?))

;; Opaque by design: the selected provider is an execution-only field. Its
;; binding metadata, never the object, is committed to snapshot/presentation.
(struct batch-execution-plan
        (batch-id jobs
                  snapshot
                  raw-digest
                  dangerous-jobs
                  max-parallel
                  aggregate?
                  batch-timeout-ms
                  approval-plan
                  provider))

;; Returns a normalized request and #f, or #f and the exact legacy validation
;; message. Keeping transport formatting in the orchestrator preserves its API.
(define (normalize-batch-request args)
  (define jobs (and (hash? args) (hash-ref args 'jobs #f)))
  (define max-parallel (and (hash? args) (hash-ref args 'maxParallel 3)))
  (define aggregate? (and (hash? args) (hash-ref args 'aggregate #t)))
  (cond
    [(not jobs) (values #f "jobs array is required")]
    [(not (list? jobs)) (values #f "jobs must be an array")]
    [(null? jobs) (values #f "jobs array must not be empty")]
    [(> (length jobs) 12) (values #f "jobs array must not exceed 12 items")]
    [(not (and (exact-integer? max-parallel) (>= max-parallel 1)))
     (values #f "maxParallel must be at least 1")]
    [(> max-parallel 3) (values #f "maxParallel must be at most 3")]
    [(not (boolean? aggregate?)) (values #f "aggregate must be boolean")]
    [else (values (batch-request args jobs max-parallel aggregate?) #f)]))

(define (normalize-batch-job job
                             index
                             default-model
                             planned-safe-mode?
                             child-tools
                             parent-capabilities)
  (unless (hash? job)
    (error 'tool-spawn-subagents "job ~a must be an object" index))
  (define task (hash-ref job 'task #f))
  (unless (and (string? task) (not (string=? (string-trim task) "")) (<= (string-length task) 100000))
    (error 'tool-spawn-subagents "task for job ~a must be a non-empty bounded string" index))
  (define explicit-job-id (hash-ref job 'jobId #f))
  (when (and explicit-job-id (not (valid-plan-id? explicit-job-id)))
    (error 'tool-spawn-subagents "jobId for job ~a must be a bounded terminal-safe identifier" index))
  (define role-raw (hash-ref job 'role (hash-ref job 'rolePrompt #f)))
  (when (and role-raw (or (not (string? role-raw)) (> (string-length role-raw) 10000)))
    (error 'tool-spawn-subagents "role for job ~a must be a bounded string" index))
  (define model-raw (hash-ref job 'model #f))
  (when (and model-raw (or (not (string? model-raw)) (> (string-length model-raw) 200)))
    (error 'tool-spawn-subagents "model for job ~a must be a bounded string" index))
  (define max-turns (hash-ref job 'max-turns 10))
  (unless (and (exact-integer? max-turns) (> max-turns 0))
    (error 'tool-spawn-subagents "max-turns for job ~a must be a positive integer" index))
  (define declared-capabilities
    (if (hash-has-key? job 'capabilities)
        (with-handlers ([exn:fail:contract? (lambda (_)
                                              (error 'tool-spawn-subagents
                                                     "invalid capability declaration for job ~a"
                                                     index))])
          (normalize-capabilities/strict (hash-ref job 'capabilities)))
        #f))
  (define capabilities (bounded-delegated-capabilities declared-capabilities parent-capabilities))
  (hasheq 'job-id
          (immutable-string explicit-job-id)
          'task
          (immutable-string task)
          'role
          (immutable-string (or role-raw default-subagent-role-prompt))
          'model
          (immutable-string (or model-raw default-model))
          'max-turns
          max-turns
          'effective-capabilities
          capabilities
          'effective-tools
          (child-tool-names-for child-tools capabilities planned-safe-mode?)))

(define (build-batch-execution-plan request
                                    exec-ctx
                                    planned-provider
                                    default-model
                                    planned-safe-mode?
                                    child-tools
                                    parent-capabilities
                                    agent-pool-limit
                                    blackboard-context)
  (unless (batch-request? request)
    (raise-argument-error 'build-batch-execution-plan "batch-request?" request))
  (unless (string? default-model)
    (error 'tool-spawn-subagents "resolved model must be a string"))
  (unless (string? blackboard-context)
    (raise-argument-error 'build-batch-execution-plan "string?" blackboard-context))
  ;; Complete validation before allocating identities.
  (define normalized
    (for/list ([job (in-list (batch-request-jobs request))]
               [index (in-naturals)])
      (normalize-batch-job job
                           index
                           default-model
                           planned-safe-mode?
                           child-tools
                           parent-capabilities)))
  (define explicit-ids (filter values (map (lambda (job) (hash-ref job 'job-id)) normalized)))
  (unless (= (length explicit-ids) (length (remove-duplicates explicit-ids string=?)))
    (error 'tool-spawn-subagents "jobId values must be unique within a batch"))
  (define args (batch-request-raw request))
  (define batch-id-raw (hash-ref args 'batchId (hash-ref args 'batch-id #f)))
  (when (and batch-id-raw (not (valid-plan-id? batch-id-raw)))
    (error 'tool-spawn-subagents "batchId must be a bounded terminal-safe identifier"))
  (define batch-id (or (immutable-string batch-id-raw) (immutable-string (generate-id))))
  (define batch-timeout-ms
    (let ([raw (hash-ref args 'batch-timeout-ms (hash-ref args 'batchTimeoutMs #f))])
      (if (and (exact-integer? raw) (positive? raw)) raw 300000)))
  (define planned-jobs
    (for/list ([job (in-list normalized)]
               [batch-order (in-naturals)])
      (define effective-role (role-with-context blackboard-context (hash-ref job 'role)))
      (hasheq 'job-id
              (or (hash-ref job 'job-id) (generate-id))
              'batch-order
              batch-order
              'task
              (hash-ref job 'task)
              'task-digest
              (sha256-digest (hash-ref job 'task))
              'role
              effective-role
              'role-digest
              (sha256-digest effective-role)
              'model
              (hash-ref job 'model)
              'max-turns
              (hash-ref job 'max-turns)
              'effective-capabilities
              (hash-ref job 'effective-capabilities)
              'effective-tools
              (hash-ref job 'effective-tools)
              'tool-call-id
              (generate-id)
              'child-id
              (generate-id)
              'session-id
              (generate-id))))
  (define effective-parallel
    (min (batch-request-max-parallel request) (length planned-jobs) agent-pool-limit))
  (define bindings (capture-execution-bindings exec-ctx planned-provider))
  (define snapshot
    (immutable-canonical-copy (hasheq 'batch-id
                                      batch-id
                                      'jobs
                                      planned-jobs
                                      'max-parallel
                                      effective-parallel
                                      'aggregate
                                      (and (batch-request-aggregate? request) #t)
                                      'batch-timeout-ms
                                      batch-timeout-ms
                                      'cwd
                                      (hash-ref bindings 'cwd)
                                      'parent-session-id
                                      (hash-ref bindings 'parent-session-id)
                                      'parent-call-id
                                      (hash-ref bindings 'parent-call-id)
                                      'safe-mode
                                      planned-safe-mode?
                                      'provider-binding
                                      (hash-ref bindings 'provider-binding))))
  (define dangerous-jobs
    (filter (lambda (job) (requires-hitl-approval? (hash-ref job 'effective-capabilities #f)))
            (hash-ref snapshot 'jobs)))
  (define presentation-jobs
    (for/list ([job (in-list (hash-ref snapshot 'jobs))])
      (hasheq 'job-id
              (hash-ref job 'job-id)
              'batch-order
              (hash-ref job 'batch-order)
              'task-digest
              (hash-ref job 'task-digest)
              'task-preview
              (redacted-approval-preview (hash-ref job 'task))
              'role-digest
              (hash-ref job 'role-digest)
              'role-preview
              (redacted-approval-preview (hash-ref job 'role))
              'model-preview
              (redacted-approval-preview (hash-ref job 'model))
              'effective-capabilities
              (hash-ref job 'effective-capabilities)
              'effective-tools
              (hash-ref job 'effective-tools)
              'max-turns
              (hash-ref job 'max-turns)
              'tool-call-id
              (hash-ref job 'tool-call-id)
              'child-id
              (hash-ref job 'child-id)
              'session-id
              (hash-ref job 'session-id))))
  (define dangerous-presentation-jobs
    (filter (lambda (job) (requires-hitl-approval? (hash-ref job 'effective-capabilities #f)))
            presentation-jobs))
  (define dangerous-capabilities
    (remove-duplicates (append* (map (lambda (job) (hash-ref job 'effective-capabilities '()))
                                     dangerous-jobs))
                       eq?))
  (define provider-binding (hash-ref bindings 'provider-binding))
  (define presentation
    (hasheq 'batch-id
            batch-id
            'task-preview
            (redacted-approval-preview
             (string-join (map (lambda (job) (hash-ref job 'task)) dangerous-jobs) "; "))
            'jobs
            presentation-jobs
            'dangerous-jobs
            dangerous-presentation-jobs
            'capabilities
            dangerous-capabilities
            'max-parallel
            effective-parallel
            'aggregate
            (and (batch-request-aggregate? request) #t)
            'cwd-preview
            (redacted-approval-preview (or (hash-ref bindings 'cwd) ""))
            'provider-preview
            (redacted-approval-preview (hash-ref provider-binding 'name ""))
            'parent-session-preview
            (redacted-approval-preview (hash-ref bindings 'parent-session-id))
            'parent-call-preview
            (redacted-approval-preview (hash-ref bindings 'parent-call-id))
            'safe-mode
            planned-safe-mode?))
  (define approval-plan (make-spawn-execution-plan 'batch snapshot presentation))
  (batch-execution-plan batch-id
                        (hash-ref snapshot 'jobs)
                        snapshot
                        (sha256-digest args)
                        dangerous-jobs
                        effective-parallel
                        (batch-request-aggregate? request)
                        batch-timeout-ms
                        approval-plan
                        planned-provider))

(define (batch-execution-plan-request-matches? plan raw-request)
  (string=? (batch-execution-plan-raw-digest plan) (sha256-digest raw-request)))

;; Assemble one execution handoff without exposing the provider through the
;; public plan API or committing the object to either safe approval view.
(define (batch-execution-plan-job-arguments plan job)
  (define snapshot (batch-execution-plan-snapshot plan))
  (hasheq 'task
          (hash-ref job 'task)
          'role
          (or (hash-ref job 'role #f) default-subagent-role-prompt)
          'model
          (hash-ref job 'model)
          'max-turns
          (hash-ref job 'max-turns)
          'capabilities
          (hash-ref job 'effective-capabilities #f)
          'planned-provider
          (batch-execution-plan-provider plan)
          'planned-safe-mode
          (hash-ref snapshot 'safe-mode)
          'planned-working-directory
          (hash-ref snapshot 'cwd)
          'planned-parent-session-id
          (hash-ref snapshot 'parent-session-id)
          'planned-parent-call-id
          (hash-ref snapshot 'parent-call-id)
          'planned-tool-call-id
          (hash-ref job 'tool-call-id)
          'planned-child-id
          (hash-ref job 'child-id)
          'planned-session-id
          (hash-ref job 'session-id)))

;; The provider-bearing handoff is intentionally absent from the public
;; planning API. Only the effectful spawn orchestrator imports this submodule.
(module+ orchestration
  (provide batch-execution-plan-job-arguments))

#lang racket/base

;; tools/builtins/spawn-subagent.rkt — Subagent spawning tool
;;
;; Spawns an isolated child agent process to execute a delegated task.
;; The child gets a fresh context window, optional role prompt, and
;; scoped tool access. Returns the child's final text result.
;;
;; #1193: Subagent Spawning — Isolated Child Agent Processes
;; #1203/#1204: Real provider injection — uses parent's provider

(require racket/list
         racket/port
         racket/string
         "../../tools/tool.rkt"
         "../../agent/event-bus.rkt"
         "../../llm/provider.rkt"
         "../../llm/model.rkt"
         "../../util/ids.rkt"
         (only-in "../../util/protocol-types.rkt"
                  make-message
                  message-role
                  message-content
                  message-id
                  tool-call-part?
                  tool-call-part-id
                  tool-call-part-name
                  tool-call-part-arguments
                  make-tool-call
                  make-tool-call-part
                  make-tool-result-part
                  tool-call-id
                  tool-result-content
                  tool-result-is-error?)
         (only-in "../../util/json-helpers.rkt" ensure-hash-args)
         (only-in "../../util/error-sanitizer.rkt" sanitize-error-message)
         "../../tools/scheduler.rkt"
         ;; Individual builtins for child-safe tool registration (no circular dep)
         (only-in "../builtins/read.rkt" tool-read)
         (only-in "../builtins/write.rkt" tool-write)
         (only-in "../builtins/edit.rkt" tool-edit)
         (only-in "../builtins/bash.rkt" tool-bash)
         (only-in "../builtins/grep.rkt" tool-grep)
         (only-in "../builtins/find.rkt" tool-find)
         (only-in "../builtins/ls.rkt" tool-ls)
         (only-in "../builtins/skill-router.rkt" tool-skill-route))

(provide tool-spawn-subagent
         tool-spawn-subagents
         resolve-role-prompt)

;; Default role prompt when no role is specified
(define default-role-prompt
  "You are a focused assistant executing a specific delegated task. Complete the task efficiently and return the result.")

;; Resolve a role value: if it matches a skill name, load that skill as the prompt.
;; Otherwise use it as a literal prompt string.
(define (resolve-role-prompt role-value)
  (if (string? role-value)
      (let* ([result (tool-skill-route (hasheq 'action "context" 'name role-value))])
        (if (and (tool-result? result) (not (tool-result-is-error? result)))
            ;; Skill found — use its content as the role prompt
            (let ([content (hash-ref (car (tool-result-content result)) 'text "")])
              (if (non-empty-string? content) content default-role-prompt))
            ;; Not a skill name — use as literal prompt
            (if (non-empty-string? role-value) role-value default-role-prompt)))
      default-role-prompt))

;; The spawn-subagent tool implementation
(define (tool-spawn-subagent args [exec-ctx #f])
  (define task (hash-ref args 'task #f))
  (define role-arg (hash-ref args 'role default-role-prompt))
  (define role-prompt (resolve-role-prompt role-arg))
  (define max-turns (hash-ref args 'max-turns 5))
  (define allowed-tools (hash-ref args 'tools #f)) ;; optional list of tool names

  (if (not task)
      (make-error-result "task is required")
      (run-subagent args role-prompt max-turns allowed-tools exec-ctx)))

;; Resolve the provider from exec-context runtime-settings.
;; Returns the real provider if available, or falls back to a mock.
;; Extract a value from runtime-settings which may be a q-settings struct or a plain hash.
;; #1241: After settings-contract fix, runtime-settings is a q-settings struct.
;; Uses struct->vector for transparent struct access (no runtime import needed).
(define (rt-settings-ref rt key [default #f])
  (cond
    [(hash? rt) (hash-ref rt key default)]
    [(struct? rt)
     ;; q-settings is transparent: fields are global, project, merged
     ;; merged is at vector index 3 (0=name, 1=global, 2=project, 3=merged)
     (define merged (vector-ref (struct->vector rt) 3))
     (if (hash? merged)
         (hash-ref merged key default)
         default)]
    [else default]))

;; #1204: Provider injection from parent session.
(define (resolve-provider exec-ctx)
  (define rt (and exec-ctx (exec-context-runtime-settings exec-ctx)))
  (define prov (and rt (rt-settings-ref rt 'provider #f)))
  (if prov
      prov
      (build-mock-provider-for-subagent)))

;; Resolve model name from exec-context or fall back to "mock-model".
(define (resolve-model-name exec-ctx args)
  (or (hash-ref args 'model #f)
      (let ([rt (and exec-ctx (exec-context-runtime-settings exec-ctx))])
        (and rt (rt-settings-ref rt 'model #f)))
      "mock-model"))

;; Child-safe tools: read-only + safe write/edit/bash for subagent children.
;; Schemas match those in registry-defaults.rkt.
(define (child-safe-tools)
  (list (make-tool "read"
                   "Read file contents"
                   (hasheq 'type
                           "object"
                           'required
                           '("path")
                           'properties
                           (hasheq 'path
                                   (hasheq 'type "string" 'description "Path to file")
                                   'offset
                                   (hasheq 'type "integer" 'description "Line offset")
                                   'limit
                                   (hasheq 'type "integer" 'description "Max lines")))
                   tool-read)
        (make-tool "write"
                   "Write content to a file"
                   (hasheq 'type
                           "object"
                           'required
                           '("path" "content")
                           'properties
                           (hasheq 'path
                                   (hasheq 'type "string" 'description "Path to file")
                                   'content
                                   (hasheq 'type "string" 'description "Content to write")))
                   tool-write)
        (make-tool "edit"
                   "Edit a file with exact text replacement"
                   (hasheq 'type
                           "object"
                           'required
                           '("path" "edits")
                           'properties
                           (hasheq 'path
                                   (hasheq 'type "string" 'description "Path to file")
                                   'edits
                                   (hasheq 'type "array" 'description "Edit operations")))
                   tool-edit)
        (make-tool "bash"
                   "Execute a bash command"
                   (hasheq 'type
                           "object"
                           'required
                           '("command")
                           'properties
                           (hasheq 'command
                                   (hasheq 'type "string" 'description "Bash command")
                                   'timeout
                                   (hasheq 'type "integer" 'description "Timeout in seconds")))
                   tool-bash)
        (make-tool "grep"
                   "Search file contents with regex"
                   (hasheq 'type
                           "object"
                           'required
                           '("pattern")
                           'properties
                           (hasheq 'pattern
                                   (hasheq 'type "string" 'description "Regex pattern")
                                   'path
                                   (hasheq 'type "string" 'description "File or directory path")))
                   tool-grep)
        (make-tool "find"
                   "Find files by name pattern"
                   (hasheq 'type
                           "object"
                           'required
                           '("pattern")
                           'properties
                           (hasheq 'pattern
                                   (hasheq 'type "string" 'description "Name pattern")
                                   'path
                                   (hasheq 'type "string" 'description "Search directory")))
                   tool-find)
        (make-tool "ls"
                   "List directory contents"
                   (hasheq 'type
                           "object"
                           'required
                           '("path")
                           'properties
                           (hasheq 'path (hasheq 'type "string" 'description "Directory path")))
                   tool-ls)))

;; Internal: run the subagent
(define (run-subagent args role-prompt max-turns allowed-tools exec-ctx)
  (define task (hash-ref args 'task))
  (with-handlers ([exn:fail? (lambda (e)
                               (make-error-result (sanitize-error-message
                                                   (format "subagent failed: ~a" (exn-message e)))))])
    ;; #1204: Resolve real provider from parent's runtime-settings
    (define provider (resolve-provider exec-ctx))
    (define model-name (resolve-model-name exec-ctx args))

    ;; Create scoped tool registry with child-safe tools
    (define registry (make-tool-registry))
    ;; Register tools directly to avoid circular dependency with registry-defaults
    (for ([t (child-safe-tools)])
      (register-tool! registry t))

    ;; Build child session
    (define session-id (generate-id))
    (define bus (make-event-bus))
    (define settings (hasheq 'model model-name))

    ;; Create execution context for child
    (define child-ctx
      (make-exec-context #:working-directory (if exec-ctx
                                                 (exec-context-working-directory exec-ctx)
                                                 (current-directory))
                         #:event-publisher (lambda (event) (publish! bus event))
                         #:runtime-settings settings
                         #:call-id (generate-id)
                         #:session-metadata (hasheq 'session-id session-id 'role "subagent")))

    ;; Build messages for child
    (define system-msg
      (make-message (generate-id) #f 'system 'message (list role-prompt) (current-seconds) (hasheq)))
    (define user-msg
      (make-message (generate-id) #f 'user 'message (list task) (current-seconds) (hasheq)))

    ;; Run the child agent loop
    (define-values (result-messages final-status)
      (run-subagent-loop provider registry (list system-msg user-msg) child-ctx max-turns))

    ;; Extract final text from result
    (define result-text
      (string-join (for/list ([m (in-list result-messages)]
                              #:when (eq? (message-role m) 'assistant))
                     (define content (message-content m))
                     (if (string? content)
                         content
                         (format "~a" content)))
                   "\n"))

    (make-success-result
     (list (hasheq 'type "text" 'text result-text))
     (hasheq 'turns-used max-turns 'status (symbol->string final-status) 'session-id session-id))))

;; Run a simple agent loop for the subagent
;; v0.19.4 GAP-1 fix: actually dispatch tool_calls instead of returning 'stopped
(define (run-subagent-loop provider registry messages ctx max-turns)
  (let loop ([msgs messages]
             [turns-remaining max-turns]
             [all-results '()])
    (cond
      [(<= turns-remaining 0) (values all-results 'max-turns-reached)]
      [else
       (define req (make-model-request msgs (list-active-tools-jsexpr registry) (hasheq)))
       (define resp (provider-send provider req))
       (define content (model-response-content resp))
       ;; Build assistant message from response content parts
       (define content-parts
         (for/list ([c (in-list content)])
           (cond
             [(hash-ref c 'type #f)
              =>
              (lambda (t)
                (cond
                  [(equal? t "text") (hash-ref c 'text (format "~a" c))]
                  [(equal? t "tool_call")
                   (make-tool-call-part (hash-ref c 'id (generate-id))
                                        (hash-ref c 'name "")
                                        (ensure-hash-args (hash-ref c 'arguments "{}")))]
                  [else (format "~a" c)]))]
             [else (hash-ref c 'text (format "~a" c))])))
       (define assistant-msg
         (make-message (generate-id) #f 'assistant 'message content-parts (current-seconds) (hasheq)))
       (define new-all (append all-results (list assistant-msg)))
       (cond
         ;; Dispatch tool calls and continue the loop
         [(eq? (model-response-stop-reason resp) 'tool_calls)
          (define tool-calls
            (for/list ([part (in-list content-parts)]
                       #:when (tool-call-part? part))
              (make-tool-call (tool-call-part-id part)
                              (tool-call-part-name part)
                              (tool-call-part-arguments part))))
          (if (null? tool-calls)
              (values new-all 'complete)
              (let ()
                ;; Run tool batch
                (define sched-result (run-tool-batch tool-calls registry #:exec-context ctx))
                (define results (scheduler-result-results sched-result))
                ;; Build tool-result messages
                (define tool-result-msgs
                  (for/list ([tc (in-list tool-calls)]
                             [tr (in-list results)])
                    (make-message
                     (generate-id)
                     (message-id assistant-msg)
                     'tool
                     'tool-result
                     (list (make-tool-result-part (tool-call-id tc)
                                                  (tool-result-content tr)
                                                  (tool-result-is-error? tr)))
                     (current-seconds)
                     (hasheq 'toolCallId (tool-call-id tc) 'isError (tool-result-is-error? tr)))))
                (loop (append msgs (list assistant-msg) tool-result-msgs)
                      (sub1 turns-remaining)
                      (append new-all tool-result-msgs))))]
         [else (values new-all 'complete)])])))

;; ============================================================
;; spawn-subagents: Batch parallel execution
;; ============================================================

;; Run a single job and return (cons job-id result) or (cons job-id error)
(define (run-single-job job provider parent-ctx)
  (define job-id (hash-ref job 'jobId #f))
  (define task (hash-ref job 'task #f))
  (define role-prompt (hash-ref job 'role (hash-ref job 'rolePrompt #f)))
  (define model-name (or (hash-ref job 'model #f) (resolve-model-name parent-ctx job)))
  (define max-turns (hash-ref job 'max-turns 5))
  (define result
    (if (not task)
        (make-error-result "task is required for each job")
        (run-subagent (hasheq 'task
                              task
                              'role
                              (or role-prompt default-role-prompt)
                              'model
                              model-name
                              'max-turns
                              max-turns)
                      (or role-prompt default-role-prompt)
                      max-turns
                      #f ;; allowed-tools
                      parent-ctx)))
  (cons job-id result))

;; Batch subagent execution with concurrency control
(define (tool-spawn-subagents args [exec-ctx #f])
  (define jobs (hash-ref args 'jobs #f))
  (define max-parallel (hash-ref args 'maxParallel 3))
  (define aggregate? (hash-ref args 'aggregate #t))

  (cond
    [(not jobs) (make-error-result "jobs array is required")]
    [(not (list? jobs)) (make-error-result "jobs must be an array")]
    [(= (length jobs) 0) (make-error-result "jobs array must not be empty")]
    [(> (length jobs) 12) (make-error-result "jobs array must not exceed 12 items")]
    [(< max-parallel 1) (make-error-result "maxParallel must be at least 1")]
    [(> max-parallel 3) (make-error-result "maxParallel must not exceed 3")]
    [else
     (define effective-parallel (min max-parallel (length jobs) 3))
     (with-handlers ([exn:fail? (lambda (e)
                                  (make-error-result (format "spawn-subagents failed: ~a"
                                                             (exn-message e))))])
       ;; Resolve provider once for all jobs
       (define provider (resolve-provider exec-ctx))

       ;; Execute jobs with bounded parallelism using futures/threads
       (define results (run-jobs-parallel jobs provider exec-ctx effective-parallel))

       ;; Aggregate results
       (define job-results
         (for/list ([r (in-list results)])
           (define job-id (car r))
           (define result (cdr r))
           (hasheq 'jobId
                   job-id
                   'success
                   (not (tool-result-is-error? result))
                   'content
                   (tool-result-content result)
                   'details
                   (tool-result-details result))))

       (define success-count (count (lambda (r) (hash-ref r 'success #f)) job-results))
       (define fail-count (- (length job-results) success-count))

       (define summary-text
         (if aggregate?
             (format "Batch complete: ~a/~a succeeded, ~a failed.\n\n~a"
                     success-count
                     (length job-results)
                     fail-count
                     (string-join (for/list ([jr (in-list job-results)])
                                    (format "[~a] ~a"
                                            (or (hash-ref jr 'jobId #f) "unnamed")
                                            (if (hash-ref jr 'success #f)
                                                (extract-text-summary (hash-ref jr 'content '()))
                                                "FAILED")))
                                  "\n---\n"))
             (format "Batch complete: ~a/~a succeeded, ~a failed."
                     success-count
                     (length job-results)
                     fail-count)))

       (make-success-result (list (hasheq 'type "text" 'text summary-text))
                            (hasheq 'total-jobs
                                    (length job-results)
                                    'succeeded
                                    success-count
                                    'failed
                                    fail-count
                                    'jobs
                                    job-results)))]))

;; Extract a short text summary from tool result content
(define (extract-text-summary content)
  (define full-text
    (string-join (for/list ([c (in-list (if (list? content)
                                            content
                                            '()))]
                            #:when (and (hash? c) (hash-ref c 'text #f)))
                   (hash-ref c 'text ""))
                 "\n"))
  ;; Truncate to 200 chars for summary
  (if (> (string-length full-text) 200)
      (string-append (substring full-text 0 200) "...")
      full-text))

;; Run jobs in parallel with bounded concurrency using threads + channel
(define (run-jobs-parallel jobs provider exec-ctx max-parallel)
  (define n (length jobs))
  (cond
    ;; Single job: run directly (no thread overhead)
    [(= n 1) (list (run-single-job (car jobs) provider exec-ctx))]
    ;; Multiple jobs: use threads with bounded concurrency
    [else
     (define results-box (box '()))
     (define concurrency-sem (make-semaphore max-parallel))
     (define mutex-sem (make-semaphore 1))
     (define threads
       (for/list ([job (in-list jobs)])
         (thread (lambda ()
                   ;; Acquire concurrency slot
                   (call-with-semaphore
                    concurrency-sem
                    (lambda ()
                      (define r (run-single-job job provider exec-ctx))
                      ;; Thread-safe append to results (shared mutex)
                      (call-with-semaphore
                       mutex-sem
                       (lambda () (set-box! results-box (cons r (unbox results-box)))))))))))
     ;; Wait for all threads to complete
     (for-each thread-wait threads)
     ;; Results are in reverse order (last completed first), restore job order
     (reverse (unbox results-box))]))

;; Helper: create a mock provider for subagent testing (fallback when no real provider)
(define (build-mock-provider-for-subagent)
  (define mock-response
    (make-model-response (list (hasheq 'type "text" 'text "Subagent task completed."))
                         (hasheq 'prompt-tokens 0 'completion-tokens 0 'total-tokens 0)
                         "mock-model"
                         'stop))
  (make-mock-provider mock-response #:name "subagent-mock"))

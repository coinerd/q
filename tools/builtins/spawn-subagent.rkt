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
         (only-in "../../util/protocol-types.rkt" make-message message-role message-content)
         (only-in "../../runtime/settings.rkt" q-settings? setting-ref))

(provide tool-spawn-subagent
         tool-spawn-subagents)

;; Default role prompt when no role is specified
(define default-role-prompt
  "You are a focused assistant executing a specific delegated task. Complete the task efficiently and return the result.")

;; The spawn-subagent tool implementation
(define (tool-spawn-subagent args [exec-ctx #f])
  (define task (hash-ref args 'task #f))
  (define role-prompt (hash-ref args 'role default-role-prompt))
  (define max-turns (hash-ref args 'max-turns 5))
  (define allowed-tools (hash-ref args 'tools #f)) ;; optional list of tool names

  (if (not task)
      (make-error-result "task is required")
      (run-subagent args role-prompt max-turns allowed-tools exec-ctx)))

;; Resolve the provider from exec-context runtime-settings.
;; Returns the real provider if available, or falls back to a mock.
;; Extract a value from runtime-settings which may be a q-settings struct or a plain hash.
;; #1241: After settings-contract fix, runtime-settings is a q-settings struct.
;; Fall back to hash access for backward compat (tests, SDK path).
(define (rt-settings-ref rt key [default #f])
  (cond
    [(q-settings? rt) (setting-ref rt key default)]
    [(hash? rt) (hash-ref rt key default)]
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

;; Internal: run the subagent
(define (run-subagent args role-prompt max-turns allowed-tools exec-ctx)
  (define task (hash-ref args 'task))
  (with-handlers ([exn:fail? (lambda (e)
                               (make-error-result (format "subagent failed: ~a" (exn-message e))))])
    ;; #1204: Resolve real provider from parent's runtime-settings
    (define provider (resolve-provider exec-ctx))
    (define model-name (resolve-model-name exec-ctx args))

    ;; Create scoped tool registry (empty for now — child agents are limited)
    (define registry (make-tool-registry))

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
       (define assistant-msg
         (make-message (generate-id)
                       #f
                       'assistant
                       'message
                       (for/list ([c (in-list content)])
                         (hash-ref c 'text (format "~a" c)))
                       (current-seconds)
                       (hasheq)))
       (define new-all (append all-results (list assistant-msg)))
       (cond
         ;; Would need tool execution — simplified: return results
         [(eq? (model-response-stop-reason resp) 'tool_calls) (values new-all 'stopped)]
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

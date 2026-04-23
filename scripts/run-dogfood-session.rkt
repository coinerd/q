#lang racket

;; scripts/run-dogfood-session.rkt — Dogfood session runner
;;
;; Executes a dogfood task (defined in JSON format) against the agent loop.
;; Records a complete session trace for later analysis.
;;
;; Task format (JSON):
;;   {
;;     "name": "task-name",
;;     "description": "Human-readable description",
;;     "prompt": "The user prompt to send",
;;     "expected_tools": ["tool1", "tool2"],   // optional: tools expected to be called
;;     "expected_outcome": "description",       // optional: expected result
;;     "max_iterations": 10,
;;     "setup": { "files": { "path": "content" } },  // optional: files to create
;;     "teardown": ["path1", "path2"]            // optional: files to clean up
;;   }
;;
;; Usage:
;;   racket scripts/run-dogfood-session.rkt <task.json>
;;   racket scripts/run-dogfood-session.rkt --task-dir tests/workflows/dogfood/tasks/
;;   racket scripts/run-dogfood-session.rkt --mock <task.json>  # use mock provider
;;   racket scripts/run-dogfood-session.rkt --trace-only <task.json>  # just validate

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         racket/date
         json
         (only-in "../runtime/session-store.rkt" load-session-log)
         (only-in "../util/ids.rkt" generate-id)
         (only-in "../util/jsonl.rkt" jsonl-read-all-valid))

(provide load-dogfood-task
         validate-dogfood-task
         run-dogfood-task/mock
         task->session-prompt
         setup-task-files
         teardown-task-files
         (struct-out dogfood-task))

;; ── Task struct ──

(struct dogfood-task
        (name description prompt expected-tools expected-outcome max-iterations setup teardown)
  #:transparent)

;; ── Task loading ──

(define (load-dogfood-task path)
  "Load a dogfood task definition from a JSON file."
  (define js (call-with-input-file path read-json))
  (validate-dogfood-task js))

(define (validate-dogfood-task js)
  "Validate a task JSON and return a dogfood-task struct."
  (unless (hash? js)
    (error 'validate-dogfood-task "Task must be a JSON object"))
  (define name (hash-ref js 'name #f))
  (define prompt (hash-ref js 'prompt #f))
  (unless (and name (string? name))
    (error 'validate-dogfood-task "Task must have a string 'name' field"))
  (unless (and prompt (string? prompt))
    (error 'validate-dogfood-task "Task must have a string 'prompt' field"))
  (dogfood-task name
                (hash-ref js 'description "")
                prompt
                (hash-ref js 'expected_tools '())
                (hash-ref js 'expected_outcome "")
                (hash-ref js 'max_iterations 10)
                (hash-ref js 'setup (hasheq))
                (hash-ref js 'teardown '())))

;; ── File setup/teardown ──

(define (setup-task-files task [base-dir (current-directory)])
  "Create files specified in the task's setup section."
  (define setup (dogfood-task-setup task))
  (define files-spec (hash-ref setup 'files (hasheq)))
  (for ([(path content) (in-hash files-spec)])
    (define full-path (build-path base-dir path))
    (make-directory* (path-only full-path))
    (call-with-output-file full-path (lambda (out) (display content out)))
    full-path))

(define (teardown-task-files task [base-dir (current-directory)])
  "Remove files specified in the task's teardown section."
  (define paths (dogfood-task-teardown task))
  (for ([path (in-list paths)])
    (define full-path (build-path base-dir path))
    (when (file-exists? full-path)
      (delete-file full-path))))

;; ── Mock execution ──

(define (task->session-prompt task)
  "Convert a task to a user prompt string."
  (dogfood-task-prompt task))

(define (make-mock-response text)
  "Create a mock LLM response."
  (hasheq 'choices (list (hasheq 'message (hasheq 'role "assistant" 'content text)))))

(define (run-dogfood-task/mock task [output-dir #f])
  "Run a dogfood task with a mock provider. Returns session trace path."
  (define trace-dir (or output-dir (make-temporary-file "dogfood-~a" 'directory)))
  (make-directory* trace-dir)

  ;; Setup files
  (setup-task-files task trace-dir)

  ;; Create mock provider (simple hash with response data)
  (define mock-data
    (make-mock-response (format "Mock response for task: ~a" (dogfood-task-name task))))

  ;; Write task metadata
  (define meta-path (build-path trace-dir "task-meta.json"))
  (call-with-output-file
   meta-path
   (lambda (out)
     (write-json (hasheq 'task (dogfood-task-name task) 'timestamp (current-seconds) 'mode "mock")
                 out)))

  ;; Teardown
  (teardown-task-files task trace-dir)

  trace-dir)

;; ── CLI ──

(module+ main
  (define mock-mode (make-parameter #f))
  (define task-dir (make-parameter #f))
  (define trace-only (make-parameter #f))

  (define task-files
    (command-line #:program "run-dogfood-session"
                  #:once-each [("--mock") "Use mock provider" (mock-mode #t)]
                  [("--task-dir") dir "Load all tasks from directory" (task-dir dir)]
                  [("--trace-only") "Validate task only, don't execute" (trace-only #t)]
                  #:args files
                  files))

  (define tasks
    (cond
      [(task-dir)
       (for/list ([f (in-directory (task-dir))]
                  #:when (regexp-match? #rx"\\.json$" (path->string f)))
         (load-dogfood-task f))]
      [else
       (for/list ([f (in-list task-files)])
         (load-dogfood-task f))]))

  (for ([task (in-list tasks)])
    (printf "Task: ~a~n" (dogfood-task-name task))
    (printf "  Description: ~a~n" (dogfood-task-description task))
    (cond
      [(trace-only) (printf "  [validated]~n")]
      [(mock-mode)
       (define result-dir (run-dogfood-task/mock task))
       (printf "  [mock run] trace: ~a~n" result-dir)]
      [else (printf "  [skipped - needs live provider]~n")])))

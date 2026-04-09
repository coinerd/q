#lang racket/base

(require racket/string
         racket/list)

;; benchmarks/tasks.rkt — benchmark task definitions
;;
;; Each benchmark task has:
;;   - name:          short identifier
;;   - description:   what the task tests
;;   - prompt:        the user prompt sent to the agent
;;   - validation:    procedure to check if the result indicates success
;;
;; Tasks cover the core coding-agent capabilities:
;;   1. explain-code       — comprehension
;;   2. write-test         — test generation
;;   3. edit-function      — code editing
;;   4. search-codebase    — codebase navigation
;;   5. session-resume     — session continuity

(require "metrics.rkt")

(provide (struct-out benchmark-task)
         all-tasks
         validate-task-result)

;; ============================================================
;; Task struct
;; ============================================================

(struct benchmark-task (name description prompt validation) #:transparent)

;; ============================================================
;; Validation helpers
;; ============================================================

;; Default validation: check that output is a non-empty string
(define (non-empty-string? output)
  (and (string? output) (> (string-length output) 0)))

;; Check output contains a substring (case-insensitive)
(define ((contains-any . keywords) output)
  (and (string? output)
       (for/or ([kw (in-list keywords)])
         (string-contains? (string-downcase output) (string-downcase kw)))))

;; Check output looks like a test definition
(define ((looks-like-test) output)
  (and (string? output)
       (ormap (λ (s) (string-contains? output s))
              '("test-case" "check-" "rackunit" "check-equal?" "check-true" "assert"))))

;; Check output looks like a function definition
(define ((looks-like-definition) output)
  (and (string? output)
       (ormap (λ (s) (string-contains? output s))
              '("define" "defun" "function" "clear-all"))))

;; Check output lists files
(define ((looks-like-file-list) output)
  (and (string? output)
       (ormap (λ (s) (string-contains? output s))
              '(".rkt" "import" "require" "file" "module"))))

;; ============================================================
;; Task definitions
;; ============================================================

(define tasks
  (list
   (benchmark-task
    'explain-code
    "Explain a code module in natural language"
    "Explain what q's event-bus.rkt does in 2-3 sentences."
    (contains-any "event" "publish" "subscribe" "bus" "notification"))

   (benchmark-task
    'write-test
    "Generate a rackunit test for an existing function"
    "Write a rackunit test for the `subscribe!` function in q/agent/event-bus.rkt"
    (looks-like-test))

   (benchmark-task
    'edit-function
    "Add a new function to an existing module"
    "Add a `clear-all-subscribers!` function to event-bus.rkt"
    (looks-like-definition))

   (benchmark-task
    'search-codebase
    "Find files that import from a given module"
    "Find all files that import from agent/types.rkt and list them"
    (looks-like-file-list))

   (benchmark-task
    'session-resume
    "Resume a session and summarize prior context"
    "Resume this session and summarize what we discussed"
    non-empty-string?)))

;; ============================================================
;; Public API
;; ============================================================

(define (all-tasks) tasks)

;; Validate a task-result against its task's validation predicate.
;; Returns 'pass, 'fail, or 'error.
(define (validate-task-result result)
  (define task (task-result-task result))
  (define m (task-result-metrics result))
  (define output (task-result-output result))
  (cond
    [(eq? (metrics-status m) 'error) 'error]
    [((benchmark-task-validation task) output) 'pass]
    [else 'fail]))

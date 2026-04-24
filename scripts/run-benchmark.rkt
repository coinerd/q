#lang racket

;; scripts/run-benchmark.rkt — CLI entry point for the benchmark suite
;;
;; Runs benchmark tasks against live or mock LLM providers.
;; Supports batch execution, scoring, and baseline comparison.
;;
;; Usage:
;;   racket scripts/run-benchmark.rkt --live --task-dir scripts/benchmark/tasks/
;;   racket scripts/run-benchmark.rkt --live scripts/benchmark/tasks/add-simple-function.json
;;   racket scripts/run-benchmark.rkt --mock scripts/benchmark/tasks/add-simple-function.json
;;   racket scripts/run-benchmark.rkt --scoring-only traces/task.jsonl --task task.json
;;   racket scripts/run-benchmark.rkt --live --task-dir scripts/benchmark/tasks/ --json

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         json
         "benchmark/task.rkt"
         "benchmark/executor.rkt")

;; ============================================================
;; CLI parameters
;; ============================================================

(define live-mode (make-parameter #f))
(define mock-mode (make-parameter #f))
(define task-dir (make-parameter #f))
(define output-dir (make-parameter #f))
(define keep-on-failure (make-parameter #f))
(define provider-override (make-parameter #f))
(define scoring-only (make-parameter #f))
(define scoring-task (make-parameter #f))
(define json-output (make-parameter #f))
(define parallel-count (make-parameter 1))
(define baseline-path (make-parameter #f))

(define task-files
  (command-line
   #:program "run-benchmark"
   #:once-each [("--live") "Run with real LLM provider" (live-mode #t)]
   [("--mock") "Run with mock provider (default)" (mock-mode #t)]
   [("--task-dir") dir "Load all tasks from directory" (task-dir dir)]
   [("--output-dir") dir "Output directory for traces and reports" (output-dir dir)]
   [("--keep-on-failure") "Preserve temp project dirs for failed tasks" (keep-on-failure #t)]
   [("--provider") name "Override provider (format: provider/model)" (provider-override name)]
   [("--scoring-only") trace "Score an existing trace without re-running" (scoring-only trace)]
   [("--task") path "Task definition for scoring-only mode" (scoring-task path)]
   [("--json") "Output as JSON" (json-output #t)]
   [("--parallel") n "Run N tasks in parallel (default: 1)" (parallel-count (string->number n))]
   [("--baseline") path "Compare against baseline report" (baseline-path path)]
   #:args files
   files))

;; ============================================================
;; Load tasks
;; ============================================================

(define tasks
  (cond
    [(task-dir) (all-benchmark-tasks (task-dir))]
    [(not (null? task-files))
     (for/list ([f (in-list task-files)])
       (load-benchmark-task f))]
    [else
     (eprintf "Error: No tasks specified. Use --task-dir or provide task files.\n")
     (exit 1)]))

;; ============================================================
;; Execute
;; ============================================================

(define (run-single-task task)
  (define use-live? (and (live-mode) (not (mock-mode))))
  (define out-dir (and (output-dir) (build-path (output-dir) (benchmark-task-name task))))
  (when out-dir
    (make-directory* out-dir))
  (define result
    (if use-live?
        (execute-task/live task (provider-override) out-dir)
        (execute-task/mock task out-dir)))
  ;; Cleanup unless keep-on-failure and task failed
  (when (and (not (keep-on-failure)) (execution-result-project-dir result))
    (task-teardown task (execution-result-project-dir result)))
  result)

(define results
  (for/list ([task (in-list tasks)])
    (printf "Running task: ~a (~a) ... " (benchmark-task-name task) (benchmark-task-category task))
    (flush-output)
    (define result (run-single-task task))
    (printf "~a (~a ms, ~a iterations)~n"
            (execution-result-outcome result)
            (execution-result-duration-ms result)
            (execution-result-iterations-used result))
    (cons task result)))

;; ============================================================
;; Output formatting
;; ============================================================

(define (difficulty-str d)
  (case d
    [(1) "★☆☆"]
    [(2) "★★☆"]
    [(3) "★★★"]))

(define (format-results-human results-list)
  (define lines
    (for/list ([r (in-list results-list)])
      (match-define (cons task result) r)
      (format "  ~a ~a | ~a | ~a ms | ~a iter | ~a"
              (benchmark-task-name task)
              (difficulty-str (benchmark-task-difficulty task))
              (execution-result-outcome result)
              (execution-result-duration-ms result)
              (execution-result-iterations-used result)
              (or (execution-result-error-msg result) ""))))
  (string-join (append (list "══════════════════════════════════════════════════════════════"
                             "  BENCHMARK RESULTS"
                             "══════════════════════════════════════════════════════════════"
                             "")
                       lines
                       (list ""
                             (format "  Total tasks: ~a" (length results-list))
                             (format "  Completed: ~a"
                                     (count (lambda (r)
                                              (eq? (execution-result-outcome (cdr r)) 'completed))
                                            results-list))
                             "══════════════════════════════════════════════════════════════"))
               "\n"))

(define (results->json results-list)
  (define results-data
    (for/list ([r (in-list results-list)])
      (match-define (cons task result) r)
      (hasheq 'task
              (benchmark-task-name task)
              'category
              (symbol->string (benchmark-task-category task))
              'difficulty
              (benchmark-task-difficulty task)
              'outcome
              (symbol->string (execution-result-outcome result))
              'duration_ms
              (execution-result-duration-ms result)
              'iterations
              (execution-result-iterations-used result)
              'trace_path
              (and (execution-result-trace-path result)
                   (path->string (execution-result-trace-path result)))
              'error
              (execution-result-error-msg result))))
  (hasheq 'total_tasks (length results-list) 'results results-data))

;; ── Print output ──

(cond
  [(json-output) (displayln (jsexpr->string (results->json results)))]
  [else (displayln (format-results-human results))])

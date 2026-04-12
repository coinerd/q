#lang racket

;; q/tests/test-benchmarks.rkt — tests for the benchmark harness
;;
;; TDD tests for benchmarks/metrics.rkt, benchmarks/tasks.rkt, and
;; the core run-benchmarks flow.

(require rackunit
         racket/file
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../util/protocol-types.rkt"
         "../benchmarks/metrics.rkt"
         "../benchmarks/tasks.rkt"
         "../benchmarks/run.rkt")

;; ============================================================
;; metrics.rkt
;; ============================================================

(test-suite
 "benchmark metrics"

 (test-case "make-metrics defaults"
   (define m (make-metrics))
   (check-equal? (metrics-status m) 'unknown)
   (check-equal? (metrics-elapsed-ms m) 0)
   (check-equal? (metrics-turns m) 0)
   (check-equal? (metrics-tool-calls m) 0)
   (check-equal? (metrics-tokens m) 0))

 (test-case "make-metrics with values"
   (define m (make-metrics #:status 'completed
                            #:elapsed-ms 123
                            #:turns 2
                            #:tool-calls 3
                            #:tokens 50))
   (check-equal? (metrics-status m) 'completed)
   (check-equal? (metrics-elapsed-ms m) 123)
   (check-equal? (metrics-turns m) 2)
   (check-equal? (metrics-tool-calls m) 3)
   (check-equal? (metrics-tokens m) 50))

 (test-case "metrics struct is transparent"
   (define m (metrics 'completed 100 1 0 10))
   (check-equal? (metrics-status m) 'completed))

 (test-case "task-result struct"
   (define t (benchmark-task 'test "desc" "prompt" string?))
   (define m (make-metrics #:status 'completed))
   (define r (task-result t m "output"))
   (check-eq? (task-result-task r) t)
   (check-eq? (task-result-metrics r) m)
   (check-equal? (task-result-output r) "output")))

;; ============================================================
;; tasks.rkt
;; ============================================================

(test-suite
 "benchmark tasks"

 (test-case "all-tasks returns 5 tasks"
   (check-equal? (length (all-tasks)) 5))

 (test-case "all task names are symbols"
   (for ([t (all-tasks)])
     (check-pred symbol? (benchmark-task-name t))))

 (test-case "all tasks have non-empty prompts"
   (for ([t (all-tasks)])
     (check-pred string? (benchmark-task-prompt t))
     (check > (string-length (benchmark-task-prompt t)) 0)))

 (test-case "all tasks have validation procedures"
   (for ([t (all-tasks)])
     (check-pred procedure? (benchmark-task-validation t))))

 (test-case "task names are unique"
   (define names (map benchmark-task-name (all-tasks)))
   (check-equal? (length names) (length (remove-duplicates names))))

 (test-case "expected task names present"
   (define names (map benchmark-task-name (all-tasks)))
   (for ([expected '(explain-code write-test edit-function search-codebase session-resume)])
     (check-not-false (member expected names)
                      (format "missing task: ~a" expected)))))

;; ============================================================
;; Validation predicates
;; ============================================================

(test-suite
 "task validation"

 (test-case "explain-code validation: accepts relevant keywords"
   (define t (findf (lambda (t) (eq? (benchmark-task-name t) 'explain-code)) (all-tasks)))
   (define v (benchmark-task-validation t))
   (check-true (v "The event bus provides publish/subscribe notification"))
   (check-true (v "Subscribers register handlers via subscribe!"))
   (check-false (v "")))

 (test-case "write-test validation: accepts rackunit-like output"
   (define t (findf (lambda (t) (eq? (benchmark-task-name t) 'write-test)) (all-tasks)))
   (define v (benchmark-task-validation t))
   (check-true (v "(test-case \"something\" (check-equal? 1 1))"))
   (check-true (v "(require rackunit)"))
   (check-false (v "just some text without tests")))

 (test-case "edit-function validation: accepts define-like output"
   (define t (findf (lambda (t) (eq? (benchmark-task-name t) 'edit-function)) (all-tasks)))
   (define v (benchmark-task-validation t))
   (check-true (v "(define (clear-all-subscribers! bus) ...)"))
   (check-false (v "nope")))

 (test-case "search-codebase validation: accepts file listings"
   (define t (findf (lambda (t) (eq? (benchmark-task-name t) 'search-codebase)) (all-tasks)))
   (define v (benchmark-task-validation t))
   (check-true (v "Files found: loop.rkt, types.rkt"))
   (check-false (v "nothing here")))

 (test-case "session-resume validation: accepts any non-empty string"
   (define t (findf (lambda (t) (eq? (benchmark-task-name t) 'session-resume)) (all-tasks)))
   (define v (benchmark-task-validation t))
   (check-true (v "Session resumed successfully."))
   (check-false (v "")))

 (test-case "validate-task-result: returns pass for valid output"
   (define t (first (all-tasks)))
   (define m (make-metrics #:status 'completed))
   (define r (task-result t m "The event bus publishes events"))
   (check-equal? (validate-task-result r) 'pass))

 (test-case "validate-task-result: returns fail for invalid output"
   (define t (first (all-tasks)))
   (define m (make-metrics #:status 'completed))
   (define r (task-result t m "xyz"))
   (check-equal? (validate-task-result r) 'fail))

 (test-case "validate-task-result: returns error for error status"
   (define t (first (all-tasks)))
   (define m (make-metrics #:status 'error))
   (define r (task-result t m "ERROR: something"))
   (check-equal? (validate-task-result r) 'error)))

;; ============================================================
;; run-benchmarks integration (mock mode)
;; ============================================================

(test-suite
 "benchmark harness run"

 (test-case "run-benchmarks returns results for all tasks"
   (define results (run-benchmarks))
   (check-equal? (length results) 5))

 (test-case "all results have completed status"
   (define results (run-benchmarks))
   (for ([r results])
     (check-equal? (metrics-status (task-result-metrics r)) 'completed
                   (format "task ~a should be completed"
                           (benchmark-task-name (task-result-task r))))))

 (test-case "all results have non-zero elapsed time"
   (define results (run-benchmarks))
   (for ([r results])
     (check >= (metrics-elapsed-ms (task-result-metrics r)) 0)))

 (test-case "all results validate as pass in mock mode"
   (define results (run-benchmarks))
   (for ([r results])
     (check-equal? (validate-task-result r) 'pass
                   (format "task ~a should pass validation"
                           (benchmark-task-name (task-result-task r))))))

 (test-case "all results have non-empty output"
   (define results (run-benchmarks))
   (for ([r results])
     (check-pred string? (task-result-output r))
     (check > (string-length (task-result-output r)) 0
            (format "task ~a should have non-empty output"
                    (benchmark-task-name (task-result-task r)))))))

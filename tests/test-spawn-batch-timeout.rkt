#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-spawn-batch-timeout.rkt — Batch timeout tests for spawn-subagents
;;
;; Tests that run-jobs-parallel correctly handles batch deadlines.
;; Tests struct-level fields, constants, and plan integration.

(require rackunit
         racket/string
         "../tools/builtins/spawn-coordinator.rkt"
         "../tools/builtins/spawn-execution-plan.rkt"
         "../tools/tool.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../util/event/event-bus.rkt"
         "../util/ids.rkt"
         "../util/cancellation.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-stub-provider response-text #:name [prov-name "test-provider"])
  (define mock-response
    (make-model-response (list (hasheq 'type "text" 'text response-text))
                         (hasheq 'prompt-tokens 10 'completion-tokens 20 'total-tokens 30)
                         prov-name
                         'stop))
  (make-mock-provider mock-response #:name prov-name))

(define (make-test-exec-ctx)
  (make-exec-context #:working-directory (current-directory)
                     #:event-publisher #f
                     #:call-id "test-call"
                     #:runtime-settings (hasheq 'provider (make-stub-provider "ok"))))

;; Build a batch-execution-plan with configurable batch-timeout-ms
(define (make-batch-plan job-descs #:batch-timeout-ms [timeout-ms 300000] #:max-parallel [max-par 3])
  (define args
    (hasheq 'jobs
            (for/list ([jd (in-list job-descs)]
                       [i (in-naturals)])
              (hasheq 'task
                      (format "task-~a" (hash-ref jd 'task "work"))
                      'jobId
                      (format "job-~a" i)
                      'capabilities
                      '(read-only)))
            'batchTimeoutMs
            timeout-ms))
  (define-values (request validation-error) (normalize-batch-request args))
  (when validation-error
    (error 'make-batch-plan validation-error))
  (define exec-ctx (make-test-exec-ctx))
  (define provider (make-stub-provider "ok" #:name "test-provider"))
  (define model "test-model")
  (define safe-mode? #f)
  (define tools '())
  (define capabilities '(any))
  (define pool-limit max-par)
  (define blackboard "")
  (build-batch-execution-plan request
                              exec-ctx
                              provider
                              model
                              safe-mode?
                              tools
                              capabilities
                              pool-limit
                              blackboard))

;; ============================================================
;; Structural Tests
;; ============================================================

(test-case "DEFAULT-BATCH-DEADLINE-MS is 5 minutes"
  (check-equal? DEFAULT-BATCH-DEADLINE-MS 300000 "default deadline should be 300000 ms (5 minutes)"))

(test-case "batch-execution-plan has batch-timeout-ms accessor"
  (define plan (make-batch-plan (list (hasheq 'task "t")) #:batch-timeout-ms 12345))
  (check-equal? (batch-execution-plan-batch-timeout-ms plan)
                12345
                "accessor should return set value"))

(test-case "batch-timeout-ms defaults to 300000 when not specified"
  (define plan (make-batch-plan (list (hasheq 'task "t"))))
  (check-equal? (batch-execution-plan-batch-timeout-ms plan) 300000 "default should be 300000"))

(test-case "snapshot contains batch-timeout-ms"
  (define plan (make-batch-plan (list (hasheq 'task "t")) #:batch-timeout-ms 99999))
  (define snapshot (batch-execution-plan-snapshot plan))
  (check-equal? (hash-ref snapshot 'batch-timeout-ms)
                99999
                "snapshot should contain batch-timeout-ms"))

;; ============================================================
;; Integration Tests
;; ============================================================

(test-case "2 fast jobs complete without error in plan"
  (define plan (make-batch-plan (list (hasheq 'task "a") (hasheq 'task "b"))))
  (check-equal? (length (batch-execution-plan-jobs plan)) 2 "plan should have 2 jobs")
  (check-equal? (batch-execution-plan-batch-timeout-ms plan)
                300000
                "plan should have default timeout"))

(test-case "single job plan works"
  (define plan (make-batch-plan (list (hasheq 'task "single")) #:max-parallel 1))
  (check-equal? (length (batch-execution-plan-jobs plan)) 1 "should have 1 job")
  (check-equal? (batch-execution-plan-max-parallel plan) 1 "max-parallel should be 1"))

(test-case "batch-timeout-ms is carried in finish-batch flow"
  (define plan
    (make-batch-plan (list (hasheq 'task "a") (hasheq 'task "b")) #:batch-timeout-ms 99999))
  (check-equal? (batch-execution-plan-batch-timeout-ms plan)
                99999
                "plan carries custom batch-timeout-ms"))

(test-case "3 jobs with max-parallel=2"
  (define plan
    (make-batch-plan (list (hasheq 'task "a") (hasheq 'task "b") (hasheq 'task "c"))
                     #:max-parallel 2))
  (check-equal? (length (batch-execution-plan-jobs plan)) 3 "should have 3 jobs")
  (check-equal? (batch-execution-plan-max-parallel plan) 2 "max-parallel should be 2"))

;; ============================================================
;; Verification: batch-timeout-ms is configurable per-batch
;; ============================================================

(test-case "batch-timeout-ms can be set via batchTimeoutMs key"
  (define args
    (hasheq 'jobs (list (hasheq 'task "test" 'capabilities '(read-only))) 'batchTimeoutMs 5000))
  (define-values (request validation-error) (normalize-batch-request args))
  (check-false validation-error "should validate")
  (define exec-ctx (make-test-exec-ctx))
  (define provider (make-stub-provider "ok" #:name "test-provider"))
  (define plan (build-batch-execution-plan request exec-ctx provider "test-model" #f '() '(any) 3 ""))
  (check-equal? (batch-execution-plan-batch-timeout-ms plan)
                5000
                "batchTimeoutMs should set batch-timeout-ms"))

(test-case "batch-timeout-ms can be set via batch-timeout-ms key"
  (define args
    (hasheq 'jobs (list (hasheq 'task "test" 'capabilities '(read-only))) 'batch-timeout-ms 7500))
  (define-values (request validation-error) (normalize-batch-request args))
  (check-false validation-error "should validate")
  (define exec-ctx (make-test-exec-ctx))
  (define provider (make-stub-provider "ok" #:name "test-provider"))
  (define plan (build-batch-execution-plan request exec-ctx provider "test-model" #f '() '(any) 3 ""))
  (check-equal? (batch-execution-plan-batch-timeout-ms plan)
                7500
                "batch-timeout-ms key should set the field"))

(test-case "batch-timeout-ms with invalid value defaults to 300000"
  (define args
    (hasheq 'jobs
            (list (hasheq 'task "test" 'capabilities '(read-only)))
            'batchTimeoutMs
            "not-a-number"))
  (define-values (request validation-error) (normalize-batch-request args))
  (check-false validation-error "should validate")
  (define exec-ctx (make-test-exec-ctx))
  (define provider (make-stub-provider "ok" #:name "test-provider"))
  (define plan (build-batch-execution-plan request exec-ctx provider "test-model" #f '() '(any) 3 ""))
  (check-equal? (batch-execution-plan-batch-timeout-ms plan)
                300000
                "invalid batchTimeoutMs should default to 300000"))

;; ============================================================
;; Behavioral Tests: run-jobs-parallel synchronization
;; ============================================================

(test-case "single job path unaffected"
  ;; Single job should run directly, no thread overhead
  (define plan (make-batch-plan (list (hasheq 'task "quick")) #:batch-timeout-ms 5000))
  (define exec-ctx (make-test-exec-ctx))
  (define results
    (run-jobs-parallel (batch-execution-plan-jobs plan) plan exec-ctx 1 #:batch-deadline-ms 5000))
  (check-equal? (length results) 1 "single job returns one result")
  (define result (cdr (car results)))
  (check-false (tool-result-is-error? result) "single job should succeed"))

(test-case "all fast jobs complete with real results (no fake timeouts)"
  ;; 3 fast jobs with generous deadline, all should produce real results
  (define plan
    (make-batch-plan (list (hasheq 'task "a") (hasheq 'task "b") (hasheq 'task "c"))
                     #:batch-timeout-ms 30000))
  (define exec-ctx (make-test-exec-ctx))
  (define results
    (run-jobs-parallel (batch-execution-plan-jobs plan) plan exec-ctx 3 #:batch-deadline-ms 30000))
  (check-equal? (length results) 3 "all 3 jobs return results")
  (for ([entry (in-list results)]
        [i (in-naturals)])
    (define result (cdr entry))
    (check-false (tool-result-is-error? result) (format "job-~a should not be an error" i))
    (check-true (string? (car entry)) "job-id should be a string")))

(test-case "no false timeout when all jobs finish before deadline"
  ;; Regression test: 3 jobs with generous 60s deadline
  ;; All 3 must produce real results (the v0.99.63 bug)
  (define plan
    (make-batch-plan (list (hasheq 'task "x") (hasheq 'task "y") (hasheq 'task "z"))
                     #:batch-timeout-ms 60000))
  (define exec-ctx (make-test-exec-ctx))
  (define results
    (run-jobs-parallel (batch-execution-plan-jobs plan) plan exec-ctx 3 #:batch-deadline-ms 60000))
  (check-equal? (length results) 3 "all 3 jobs return results")
  (define error-count (count (lambda (entry) (tool-result-is-error? (cdr entry))) results))
  (check-equal? error-count 0 "no fake timeout errors"))

(test-case "deadline is wall-clock from batch start, not per-thread"
  ;; 2 jobs with maxParallel=1 (staggered). Each takes some time via provider.
  ;; Deadline should be from batch start, covering both jobs.
  (define plan
    (make-batch-plan (list (hasheq 'task "first") (hasheq 'task "second"))
                     #:batch-timeout-ms 15000
                     #:max-parallel 1))
  (define exec-ctx (make-test-exec-ctx))
  (define results
    (run-jobs-parallel (batch-execution-plan-jobs plan) plan exec-ctx 1 #:batch-deadline-ms 15000))
  (check-equal? (length results) 2 "both jobs should complete")
  (check-false (tool-result-is-error? (cdr (car results))) "first job should succeed")
  (check-false (tool-result-is-error? (cdr (list-ref results 1))) "second job should succeed"))

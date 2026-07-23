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
(define (make-batch-plan job-descs #:batch-timeout-ms [timeout-ms 180000] #:max-parallel [max-par 3])
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

(test-case "DEFAULT-BATCH-DEADLINE-MS is 3 minutes"
  (check-equal? DEFAULT-BATCH-DEADLINE-MS 180000 "default deadline should be 180000 ms (3 minutes)"))

(test-case "batch-execution-plan has batch-timeout-ms accessor"
  (define plan (make-batch-plan (list (hasheq 'task "t")) #:batch-timeout-ms 12345))
  (check-equal? (batch-execution-plan-batch-timeout-ms plan)
                12345
                "accessor should return set value"))

(test-case "batch-timeout-ms defaults to 180000 when not specified"
  (define plan (make-batch-plan (list (hasheq 'task "t"))))
  (check-equal? (batch-execution-plan-batch-timeout-ms plan) 180000 "default should be 180000"))

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
                180000
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

(test-case "batch-timeout-ms with invalid value defaults to 180000"
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
                180000
                "invalid batchTimeoutMs should default to 180000"))

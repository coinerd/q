#lang racket

;; tests/test-spawn-subagents-batch.rkt — Batch parallel subagent tests
;;
;; Tests for spawn-subagents tool: parallel execution with bounded
;; concurrency, output aggregation, and partial failure handling.

(require rackunit
         racket/string
         "../tools/builtins/spawn-subagent.rkt"
         "../tools/tool.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../agent/event-bus.rkt"
         "../util/ids.rkt")

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

(define (make-test-exec-ctx provider
                            #:model [model "test-model"]
                            #:working-directory [wd (current-directory)])
  (make-exec-context #:working-directory wd
                     #:event-publisher #f
                     #:runtime-settings (hasheq 'provider provider 'model model)
                     #:call-id (generate-id)
                     #:session-metadata (hasheq 'session-id "test-batch-session" 'role "parent")))

(define (result-text result)
  (string-join (for/list ([c (in-list (tool-result-content result))]
                          #:when (and (hash? c) (hash-ref c 'text #f)))
                 (hash-ref c 'text ""))
               ""))

;; ============================================================
;; Validation tests
;; ============================================================

(test-case "spawn-subagents requires jobs array"
  (define result (tool-spawn-subagents (hasheq)))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "jobs")))

(test-case "spawn-subagents rejects empty jobs"
  (define result (tool-spawn-subagents (hasheq 'jobs '())))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "empty")))

(test-case "spawn-subagents rejects non-list jobs"
  (define result (tool-spawn-subagents (hasheq 'jobs "not-a-list")))
  (check-true (tool-result-is-error? result)))

(test-case "spawn-subagents rejects jobs > 12"
  (define big-jobs
    (for/list ([i (in-range 13)])
      (hasheq 'task (format "task ~a" i))))
  (define result (tool-spawn-subagents (hasheq 'jobs big-jobs)))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "12")))

(test-case "spawn-subagents rejects maxParallel < 1"
  (define result (tool-spawn-subagents (hasheq 'jobs (list (hasheq 'task "test")) 'maxParallel 0)))
  (check-true (tool-result-is-error? result)))

(test-case "spawn-subagents rejects maxParallel > 3"
  (define result (tool-spawn-subagents (hasheq 'jobs (list (hasheq 'task "test")) 'maxParallel 5)))
  (check-true (tool-result-is-error? result)))

;; ============================================================
;; Single job execution
;; ============================================================

(test-case "spawn-subagents runs single job"
  (define prov (make-stub-provider "single job result"))
  (define ctx (make-test-exec-ctx prov))
  (define result
    (tool-spawn-subagents (hasheq 'jobs (list (hasheq 'task "do the thing" 'jobId "job-1"))) ctx))
  (check-false (tool-result-is-error? result) (format "expected success: ~a" (result-text result)))
  (define details (tool-result-details result))
  (check-equal? (hash-ref details 'total-jobs #f) 1)
  (check-equal? (hash-ref details 'succeeded #f) 1)
  (check-equal? (hash-ref details 'failed #f) 0))

;; ============================================================
;; Multi-job parallel execution
;; ============================================================

(test-case "spawn-subagents runs 2 jobs in parallel"
  (define prov (make-stub-provider "parallel result"))
  (define ctx (make-test-exec-ctx prov))
  (define jobs (list (hasheq 'task "task A" 'jobId "job-a") (hasheq 'task "task B" 'jobId "job-b")))
  (define result (tool-spawn-subagents (hasheq 'jobs jobs 'maxParallel 2) ctx))
  (check-false (tool-result-is-error? result) (format "expected success: ~a" (result-text result)))
  (define details (tool-result-details result))
  (check-equal? (hash-ref details 'total-jobs #f) 2)
  (check-equal? (hash-ref details 'succeeded #f) 2)
  (check-equal? (hash-ref details 'failed #f) 0))

(test-case "spawn-subagents runs 3 jobs with maxParallel=3"
  (define prov (make-stub-provider "triple result"))
  (define ctx (make-test-exec-ctx prov))
  (define jobs
    (for/list ([i (in-range 3)])
      (hasheq 'task (format "task ~a" i) 'jobId (format "job-~a" i))))
  (define result (tool-spawn-subagents (hasheq 'jobs jobs 'maxParallel 3) ctx))
  (check-false (tool-result-is-error? result))
  (check-equal? (hash-ref (tool-result-details result) 'total-jobs #f) 3)
  (check-equal? (hash-ref (tool-result-details result) 'succeeded #f) 3))

(test-case "spawn-subagents clamps maxParallel to job count"
  (define prov (make-stub-provider "clamped"))
  (define ctx (make-test-exec-ctx prov))
  ;; 1 job but maxParallel=3: should still work, effectively sequential
  (define result
    (tool-spawn-subagents (hasheq 'jobs (list (hasheq 'task "only job")) 'maxParallel 3) ctx))
  (check-false (tool-result-is-error? result))
  (check-equal? (hash-ref (tool-result-details result) 'succeeded #f) 1))

;; ============================================================
;; Aggregation
;; ============================================================

(test-case "spawn-subagents aggregate=true includes summary"
  (define prov (make-stub-provider "summary content here"))
  (define ctx (make-test-exec-ctx prov))
  (define result
    (tool-spawn-subagents (hasheq 'jobs (list (hasheq 'task "t1" 'jobId "j1")) 'aggregate #t) ctx))
  (check-false (tool-result-is-error? result))
  (define text (result-text result))
  (check-true (string-contains? text "Batch complete"))
  (check-true (string-contains? text "j1")))

(test-case "spawn-subagents aggregate=false omits details"
  (define prov (make-stub-provider "minimal"))
  (define ctx (make-test-exec-ctx prov))
  (define result (tool-spawn-subagents (hasheq 'jobs (list (hasheq 'task "t1")) 'aggregate #f) ctx))
  (check-false (tool-result-is-error? result))
  (define text (result-text result))
  (check-true (string-contains? text "Batch complete"))
  ;; With aggregate=false, should NOT contain per-job details
  (check-false (string-contains? text "---")))

;; ============================================================
;; Partial failure
;; ============================================================

(test-case "spawn-subagents handles job missing task"
  (define prov (make-stub-provider "ok"))
  (define ctx (make-test-exec-ctx prov))
  (define jobs (list (hasheq 'task "valid task" 'jobId "good") (hasheq 'jobId "bad"))) ;; no task
  (define result (tool-spawn-subagents (hasheq 'jobs jobs) ctx))
  (check-false (tool-result-is-error? result))
  (define details (tool-result-details result))
  (check-equal? (hash-ref details 'succeeded #f) 1)
  (check-equal? (hash-ref details 'failed #f) 1))

;; ============================================================
;; Backward compat
;; ============================================================

(test-case "spawn-subagents works without exec-context (mock provider)"
  (define result (tool-spawn-subagents (hasheq 'jobs (list (hasheq 'task "no ctx test")))))
  (check-false (tool-result-is-error? result)
               (format "expected success with mock: ~a" (result-text result))))

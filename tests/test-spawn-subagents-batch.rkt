#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

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
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "at most 3")))

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
;; Fail-closed whole-batch preflight
;; ============================================================

(test-case "spawn-subagents rejects every job when one task is missing"
  (define prov (make-stub-provider "ok"))
  (define ctx (make-test-exec-ctx prov))
  (define jobs (list (hasheq 'task "valid task" 'jobId "good") (hasheq 'jobId "bad")))
  (define result (tool-spawn-subagents (hasheq 'jobs jobs) ctx))
  (check-true (tool-result-is-error? result)))

;; ============================================================
;; Truthful unsuccessful child aggregation
;; ============================================================

(test-case "timed-out child is a batch failure with safe metadata only"
  (define looping-response
    (make-model-response
     (list
      (hasheq 'type "tool-call" 'id "batch-loop" 'name "read" 'arguments (hasheq 'path "README.md")))
     (hasheq)
     "test-provider"
     'tool-calls))
  (define provider (make-mock-provider looping-response #:name "looping"))
  (define ctx (make-test-exec-ctx provider))
  (define result
    (tool-spawn-subagents (hasheq 'jobs (list (hasheq 'task "loop" 'jobId "timed" 'max-turns 1)))
                          ctx))
  (check-false (tool-result-is-error? result))
  (define details (tool-result-details result))
  (check-equal? (hash-ref details 'succeeded #f) 0)
  (check-equal? (hash-ref details 'failed #f) 1)
  (check-false (hash-has-key? details 'snapshot))
  (define job (car (hash-ref details 'jobs)))
  (check-false (hash-ref job 'success #t))
  (check-equal? (hash-ref job 'terminalStatus #f) "timed-out")
  (check-false (hash-has-key? job 'content))
  (check-false (hash-has-key? job 'details)))

(test-case "batch result content preserves job correlation while metadata stays digest-only"
  (define first-sentinel "FIRST-CHILD-RAW-SENTINEL")
  (define second-sentinel "SECOND-CHILD-RAW-SENTINEL")
  (define provider
    (make-provider (lambda () "correlated")
                   (lambda () (hasheq))
                   (lambda (request)
                     (define rendered (format "~s" (model-request-messages request)))
                     (make-model-response (list (hasheq 'type
                                                        "text"
                                                        'text
                                                        (if (string-contains? rendered "first task")
                                                            first-sentinel
                                                            second-sentinel)))
                                          (hasheq)
                                          "correlated"
                                          'stop))
                   (lambda (_request) '())))
  (define events (box '()))
  (define ctx
    (make-exec-context #:event-publisher
                       (lambda (event-type payload)
                         (set-box! events (cons (cons event-type payload) (unbox events))))
                       #:runtime-settings (hasheq 'provider provider 'model "correlated")))
  (define result
    (tool-spawn-subagents
     (hasheq 'jobs
             (list (hasheq 'jobId "first" 'task "first task" 'capabilities '(read-only))
                   (hasheq 'jobId "second" 'task "second task" 'capabilities '(read-only)))
             'maxParallel
             2)
     ctx))
  (define visible
    (findf (lambda (part) (equal? (hash-ref part 'type #f) "batch-results"))
           (tool-result-content result)))
  (define visible-jobs (hash-ref visible 'jobs))
  (check-equal? (map (lambda (job) (hash-ref job 'jobId)) visible-jobs) '("first" "second"))
  (check-true (string-contains? (format "~s" (hash-ref (car visible-jobs) 'content)) first-sentinel))
  (check-true (string-contains? (format "~s" (hash-ref (cadr visible-jobs) 'content))
                                second-sentinel))
  (define metadata-rendered (format "~s" (tool-result-details result)))
  (define events-rendered (format "~s" (unbox events)))
  (for ([sentinel (in-list (list first-sentinel second-sentinel))])
    (check-false (string-contains? metadata-rendered sentinel))
    (check-false (string-contains? events-rendered sentinel))))

(test-case "failed child is a batch failure"
  (define malformed
    (make-model-response (list (hasheq 'type "text" 'text "not executable"))
                         (hasheq)
                         "test-provider"
                         'tool-calls))
  (define result
    (tool-spawn-subagents (hasheq 'jobs (list (hasheq 'task "fail" 'jobId "failed")))
                          (make-test-exec-ctx (make-mock-provider malformed))))
  (define details (tool-result-details result))
  (check-equal? (hash-ref details 'succeeded) 0)
  (check-equal? (hash-ref details 'failed) 1)
  (check-equal? (hash-ref (car (hash-ref details 'jobs)) 'terminalStatus) "failed"))

(test-case "cancelled child is a batch failure"
  (define token (make-cancellation-token))
  (cancel-token! token)
  (define provider (make-stub-provider "must not complete"))
  (define ctx
    (make-exec-context #:cancellation-token token
                       #:runtime-settings (hasheq 'provider provider 'model "test-model")))
  (define result
    (tool-spawn-subagents (hasheq 'jobs (list (hasheq 'task "cancel" 'jobId "cancelled"))) ctx))
  (define details (tool-result-details result))
  (check-equal? (hash-ref details 'succeeded) 0)
  (check-equal? (hash-ref details 'failed) 1)
  (check-equal? (hash-ref (car (hash-ref details 'jobs)) 'terminalStatus) "cancelled"))

;; ============================================================
;; Backward compat
;; ============================================================

(test-case "spawn-subagents works without exec-context (mock provider)"
  (define result (tool-spawn-subagents (hasheq 'jobs (list (hasheq 'task "no ctx test")))))
  (check-false (tool-result-is-error? result)
               (format "expected success with mock: ~a" (result-text result))))

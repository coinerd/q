#lang racket

;; @speed fast
;; @suite skills

;; tests/test-workflow-executor.rkt
;; v0.99.26 W3: Workflow Executor tests.

(require rackunit
         rackunit/text-ui
         json
         racket/string
         "../skills/workflow-executor.rkt"
         "../skills/mas-workflow.rkt"
         (only-in "../tools/builtins/spawn-subagent.rkt" current-spawn-timestamps)
         (only-in "../runtime/approval/broker.rkt"
                  make-approval-channel
                  set-approval-channel!
                  clear-approval-channel!
                  approval-decide!)
         "../tools/tool.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt")

;; ── Mock providers ──

(define (make-static-text-provider text)
  (make-mock-provider
   (make-model-response (list (hasheq 'type "text" 'text text))
                        (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                        "mock-model"
                        'stop)
   #:name "mock"))

(define (make-task-aware-provider)
  (make-provider (lambda () "task-aware")
                 (lambda () (hasheq))
                 (lambda (request)
                   (define request-text (format "~s" (model-request-messages request)))
                   (define response-text
                     (cond
                       [(string-contains? request-text "Parallel A") "PARALLEL-A-SENTINEL"]
                       [(string-contains? request-text "Parallel B") "PARALLEL-B-SENTINEL"]
                       [(string-contains? request-text "Prepare") "PREP-SENTINEL"]
                       [else "FINAL-SENTINEL"]))
                   (make-model-response (list (hasheq 'type "text" 'text response-text))
                                        (hasheq)
                                        "task-aware"
                                        'stop))
                 (lambda (_request) '())))

(define (make-failing-provider)
  (make-provider (lambda () "failing-mock")
                 (lambda () (hasheq 'streaming #t))
                 (lambda (req) (error 'failing-provider "simulated failure"))
                 (lambda (req) (error 'failing-provider "streaming failed"))))

(define (make-counting-provider sends)
  (make-provider (lambda () "counting-mock")
                 (lambda () (hasheq 'streaming #f))
                 (lambda (_request)
                   (set-box! sends (add1 (unbox sends)))
                   (make-model-response (list (hasheq 'type "text" 'text "Done"))
                                        (hasheq 'prompt-tokens 1 'completion-tokens 1 'total-tokens 2)
                                        "counting-mock"
                                        'stop))
                 (lambda (_request) (error 'counting-mock "streaming not expected"))))

;; Provider that succeeds N times, then fails on subsequent calls.
;; Used to test partial workflow results.
(define (make-succeed-then-fail-provider n)
  (define counter (box 0))
  (make-provider
   (lambda () "hybrid-mock")
   (lambda () (hasheq 'streaming #t))
   (lambda (req)
     (define c (unbox counter))
     (set-box! counter (add1 c))
     (if (< c n)
         (make-model-response (list (hasheq 'type "text" 'text (format "Success ~a" c)))
                              (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                              "mock-model"
                              'stop)
         (error 'hybrid-provider "failure after ~a successes" n)))
   (lambda (req) (error 'hybrid-provider "streaming failed"))))

(define (make-test-exec-ctx provider #:publisher [publisher #f])
  (make-exec-context #:working-directory (current-directory)
                     #:event-publisher publisher
                     #:runtime-settings (hasheq 'provider provider 'model "mock-model")))

;; ── Test helpers ──

(define (call-with-approved-context provider proc)
  (dynamic-wind (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 1000)))
                (lambda ()
                  (define publisher
                    (lambda (type payload)
                      (when (string=? type "mas.spawn-approval-requested")
                        (check-true (approval-decide! (hash-ref payload 'request-id)
                                                      (hash-ref payload 'commitment-digest)
                                                      #t)))))
                  (proc (make-test-exec-ctx provider #:publisher publisher)))
                clear-approval-channel!))

(define (make-simple-workflow steps)
  (define wf-steps
    (for/list ([s (in-list steps)])
      (workflow-step (hash-ref s 'role "assistant")
                     (hash-ref s 'task "")
                     (hash-ref s 'capabilities '(read-only))
                     (hash-ref s 'parallel #f))))
  (mas-workflow "test-wf" "test desc" wf-steps '()))

(define suite
  (test-suite "Workflow Executor (v0.99.26 W3)"

    ;; ── render-step-task ──

    (test-case "render-step-task substitutes user variables"
      (define step (workflow-step "analyst" "Analyze {{file}}" #f #f))
      (define rendered (render-step-task step (hasheq 'file "test.rkt") #f))
      (check-equal? rendered "Analyze test.rkt"))

    (test-case "render-step-task substitutes {{result}} from previous step"
      (define step (workflow-step "reviewer" "Review: {{result}}" #f #f))
      (define rendered (render-step-task step (hasheq) "Previous output"))
      (check-equal? rendered "Review: Previous output"))

    (test-case "render-step-task leaves missing variables as-is"
      (define step (workflow-step "analyst" "Analyze {{missing}}" #f #f))
      (define rendered (render-step-task step (hasheq) #f))
      (check-equal? rendered "Analyze {{missing}}"))

    (test-case "render-step-task handles both user var and result"
      (define step (workflow-step "analyst" "{{action}}: {{result}}" #f #f))
      (define rendered (render-step-task step (hasheq 'action "Summarize") "the content"))
      (check-equal? rendered "Summarize: the content"))

    ;; ── extract-result-text ──

    (test-case "extract-result-text from text content list"
      (define result (make-success-result (list (hasheq 'type "text" 'text "Hello world"))))
      (check-equal? (extract-result-text result) "Hello world"))

    (test-case "extract-result-text from error result"
      (define result (make-error-result "Something failed"))
      (check-true (string? (extract-result-text result))))

    (test-case "extract-result-text from #f"
      (check-equal? (extract-result-text #f) ""))

    ;; ── execute-workflow: integration with mock providers ──

    (test-case "execute 2-step workflow with sequential chaining"
      (define provider (make-static-text-provider "Step done"))
      (define exec-ctx (make-test-exec-ctx provider))
      (define wf
        (make-simple-workflow (list (hasheq 'role "analyst" 'task "Read the file")
                                    (hasheq 'role "reviewer" 'task "Review: {{result}}"))))
      (define result (execute-workflow wf (hasheq) exec-ctx))
      (check-false (tool-result-is-error? result) "workflow should succeed")
      (define content (tool-result-content result))
      (check-true (hash? content) "content should be a hash")
      (check-equal? (hash-ref content 'workflow) "test-wf")
      (define steps-hash (hash-ref content 'steps))
      (check-equal? (length steps-hash) 2))

    (test-case "execute workflow with template variable substitution"
      (define provider (make-static-text-provider "Analysis complete"))
      (define exec-ctx (make-test-exec-ctx provider))
      (define wf (make-simple-workflow (list (hasheq 'role "analyst" 'task "Analyze {{file}}"))))
      (define result (execute-workflow wf (hasheq 'file "src/main.rkt") exec-ctx))
      (check-false (tool-result-is-error? result)))

    (test-case "error in step stops pipeline"
      (define provider (make-failing-provider))
      (define exec-ctx (make-test-exec-ctx provider))
      (define wf
        (make-simple-workflow (list (hasheq 'role "step1" 'task "First task")
                                    (hasheq 'role "step2" 'task "Second task")
                                    (hasheq 'role "step3" 'task "Third task"))))
      (define result (execute-workflow wf (hasheq) exec-ctx))
      (check-true (tool-result-is-error? result) "should be error")
      (define content (tool-result-content result))
      ;; Error result content is a list of text parts, not a hash
      (define msg
        (if (list? content)
            (hash-ref (car content) 'text "")
            (format "~a" content)))
      (check-true (string-contains? msg "failed") "should mention failure"))

    (test-case "workflow capabilities passed to subagent-config"
      ;; This test verifies that capabilities from workflow-step are used
      ;; by creating a workflow with read-only capabilities
      (define provider (make-static-text-provider "Done"))
      (define exec-ctx (make-test-exec-ctx provider))
      (define wf
        (mas-workflow "caps-test"
                      "desc"
                      (list (workflow-step "analyst" "Do work" '(read-only) #f))
                      '()))
      (define result (execute-workflow wf (hasheq) exec-ctx))
      (check-false (tool-result-is-error? result) "read-only should not trigger HITL denial"))

    (test-case "dangerous sequential workflow without channel denies before sends or rate effects"
      (clear-approval-channel!)
      (define sends (box 0))
      (define timestamps (box '()))
      (define wf
        (mas-workflow "dangerous-seq"
                      "desc"
                      (list (workflow-step "runner" "rm -rf /" '(shell-exec) #f))
                      '()))
      (define result
        (parameterize ([current-spawn-timestamps timestamps])
          (execute-workflow wf (hasheq) (make-test-exec-ctx (make-counting-provider sends)))))
      (check-true (tool-result-is-error? result) "denied workflow should error")
      (check-equal? (unbox sends) 0)
      (check-equal? (unbox timestamps) '())
      (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text "") "failed")))

    (test-case "dangerous sequential workflow uses digest-bound interactive approval"
      (define wf
        (mas-workflow "dangerous-seq-approved"
                      "desc"
                      (list (workflow-step "runner" "echo ok" '(shell-exec) #f))
                      '()))
      (define result
        (call-with-approved-context (make-static-text-provider "Done")
                                    (lambda (exec-ctx) (execute-workflow wf (hasheq) exec-ctx))))
      (check-false (tool-result-is-error? result)))

    (test-case "dangerous parallel workflow without channel denies before sends or rate effects"
      (clear-approval-channel!)
      (define sends (box 0))
      (define timestamps (box '()))
      (define wf
        (mas-workflow "dangerous-par"
                      "desc"
                      (list (workflow-step "a" "Run A" '(shell-exec) #t)
                            (workflow-step "b" "Run B" '(shell-exec) #t)
                            (workflow-step "c" "Clean up" '(git-write) #f))
                      '()))
      (define result
        (parameterize ([current-spawn-timestamps timestamps])
          (execute-workflow wf (hasheq) (make-test-exec-ctx (make-counting-provider sends)))))
      (check-true (tool-result-is-error? result) "denied parallel workflow should error")
      (check-equal? (unbox sends) 0)
      (check-equal? (unbox timestamps) '())
      (define msg (hash-ref (car (tool-result-content result)) 'text ""))
      (check-true (string-contains? msg "failed"))
      (check-true (string-contains? msg "step 0")))

    (test-case "dangerous parallel workflow uses digest-bound interactive approval"
      (define wf
        (mas-workflow "dangerous-par-approved"
                      "desc"
                      (list (workflow-step "a" "Run A" '(shell-exec) #t)
                            (workflow-step "b" "Run B" '(shell-exec) #t))
                      '()))
      (define result
        (call-with-approved-context (make-static-text-provider "Done")
                                    (lambda (exec-ctx) (execute-workflow wf (hasheq) exec-ctx))))
      (check-false (tool-result-is-error? result)))

    (test-case "spawn APIs own exactly one interactive approval per workflow group"
      (for ([parallel? (in-list '(#f #t))])
        (dynamic-wind
         (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 1000)))
         (lambda ()
           (define requests 0)
           (define terminals 0)
           (define publisher
             (lambda (type payload)
               (cond
                 [(string=? type "mas.spawn-approval-requested")
                  (set! requests (add1 requests))
                  (check-true (approval-decide! (hash-ref payload 'request-id)
                                                (hash-ref payload 'commitment-digest)
                                                #t))]
                 [(string=? type "mas.spawn-approval-terminal") (set! terminals (add1 terminals))])))
           (define provider (make-static-text-provider "Done"))
           (define exec-ctx (make-test-exec-ctx provider #:publisher publisher))
           (define steps
             (if parallel?
                 (list (workflow-step "a" "Run A" '(shell-exec) #t)
                       (workflow-step "b" "Run B" '(read-only) #t))
                 (list (workflow-step "a" "Run A" '(shell-exec) #f))))
           (define result
             (execute-workflow (mas-workflow "owned" "desc" steps '()) (hasheq) exec-ctx))
           (check-false (tool-result-is-error? result))
           (check-equal? requests 1)
           (check-equal? terminals 1))
         clear-approval-channel!)))

    (test-case "role used in workflow steps"
      (define provider (make-static-text-provider "Role check"))
      (define exec-ctx (make-test-exec-ctx provider))
      (define wf (make-simple-workflow (list (hasheq 'role "researcher" 'task "Research topic"))))
      (define result (execute-workflow wf (hasheq) exec-ctx))
      (check-false (tool-result-is-error? result))
      (define content (tool-result-content result))
      (when (hash? content)
        (define steps-hash (hash-ref content 'steps))
        (check-equal? (hash-ref (car steps-hash) 'role) "researcher")))

    (test-case "execute parallel workflow steps"
      (define provider (make-static-text-provider "Parallel done"))
      (define exec-ctx (make-test-exec-ctx provider))
      (define wf
        (make-simple-workflow (list (hasheq 'role "a" 'task "Task A" 'parallel #t)
                                    (hasheq 'role "b" 'task "Task B" 'parallel #t))))
      (define result (execute-workflow wf (hasheq) exec-ctx))
      (check-false (tool-result-is-error? result) "parallel workflow should succeed")
      (define content (tool-result-content result))
      (check-true (hash? content) "content should be a hash")
      (check-equal? (hash-ref content 'workflow) "test-wf")
      (define steps-hash (hash-ref content 'steps))
      (check-equal? (length steps-hash) 2)
      (check-true (andmap (lambda (s) (hash-ref s 'success #f)) steps-hash)
                  "both parallel steps should succeed"))

    (test-case "execute mixed sequential and parallel workflow"
      (define provider (make-task-aware-provider))
      (define exec-ctx (make-test-exec-ctx provider))
      (define wf
        (make-simple-workflow (list (hasheq 'role "prep" 'task "Prepare")
                                    (hasheq 'role "a" 'task "Parallel A" 'parallel #t)
                                    (hasheq 'role "b" 'task "Parallel B" 'parallel #t)
                                    (hasheq 'role "finalize" 'task "Finalize: {{result}}"))))
      (define result (execute-workflow wf (hasheq) exec-ctx))
      (check-false (tool-result-is-error? result) "mixed workflow should succeed")
      (define content (tool-result-content result))
      (define steps-hash (hash-ref content 'steps))
      (check-equal? (length steps-hash) 4)
      (check-equal? (map (lambda (step) (hash-ref step 'role)) steps-hash)
                    '("prep" "a" "b" "finalize"))
      (check-true (andmap (lambda (s) (hash-ref s 'success #f)) steps-hash)
                  "all mixed steps should succeed")
      ;; Finalize step should see the last parallel step's result
      (define finalize-task (hash-ref (cadddr steps-hash) 'task ""))
      (check-true (string-contains? finalize-task "PARALLEL-B-SENTINEL")
                  "finalize should be rendered with previous result"))

    (test-case "parallel step failure stops pipeline"
      (define provider (make-failing-provider))
      (define exec-ctx (make-test-exec-ctx provider))
      (define wf
        (make-simple-workflow (list (hasheq 'role "a" 'task "Task A" 'parallel #t)
                                    (hasheq 'role "b" 'task "Task B" 'parallel #t)
                                    (hasheq 'role "c" 'task "Task C"))))
      (define result (execute-workflow wf (hasheq) exec-ctx))
      (check-true (tool-result-is-error? result) "should be error")
      (define content (tool-result-content result))
      (define msg
        (if (list? content)
            (hash-ref (car content) 'text "")
            (format "~a" content)))
      (check-true (string-contains? msg "failed") "should mention failure"))

    (test-case "workflow-step-result struct"
      (define r (workflow-step-result 0 "analyst" "Do thing" #t "Done"))
      (check-equal? (workflow-step-result-step r) 0)
      (check-equal? (workflow-step-result-role r) "analyst")
      (check-equal? (workflow-step-result-task r) "Do thing")
      (check-true (workflow-step-result-success? r))
      (check-equal? (workflow-step-result-result r) "Done"))

    ;; ── v0.99.28 W0: Partial workflow failure results ──

    (test-case "sequential failure preserves partial step results in details"
      ;; Step 1 succeeds, step 2 fails, step 3 is never reached.
      (define provider (make-succeed-then-fail-provider 1))
      (define exec-ctx (make-test-exec-ctx provider))
      (define wf
        (make-simple-workflow (list (hasheq 'role "step1" 'task "First task")
                                    (hasheq 'role "step2" 'task "Second task")
                                    (hasheq 'role "step3" 'task "Third task"))))
      (define result (execute-workflow wf (hasheq) exec-ctx))
      (check-true (tool-result-is-error? result) "should be error")
      (define details (tool-result-details result))
      (check-true (hash? details) "details should be a hash")
      (check-equal? (hash-ref details 'workflow) "test-wf")
      (check-equal? (hash-ref details 'status) "failed")
      (check-equal? (hash-ref details 'failed-step) 1)
      (define steps (hash-ref details 'steps))
      (check-equal? (length steps) 2 "should have 2 steps (step 0 + step 1), not 3")
      (check-true (hash-ref (car steps) 'success) "step 0 should be successful")
      (check-false (hash-ref (cadr steps) 'success) "step 1 should be failed")
      (for ([step (in-list steps)])
        (check-false (hash-has-key? step 'task))
        (check-false (hash-has-key? step 'result))
        (check-true (hash-has-key? step 'taskDigest))
        (check-true (hash-has-key? step 'resultDigest)))
      (check-false (string-contains? (format "~s" details) "Success 0"))
      ;; Step 2 (index 2) should be absent (skipped)
      (check-false (findf (lambda (s) (= (hash-ref s 'step) 2)) steps)
                   "step 2 should not be present (skipped)"))

    (test-case "HITL denied preserves denied step results in details"
      (clear-approval-channel!)
      (define provider (make-static-text-provider "Done"))
      (define exec-ctx (make-test-exec-ctx provider))
      (define wf
        (mas-workflow "hitl-denied-test"
                      "desc"
                      (list (workflow-step "runner" "rm -rf /" '(shell-exec) #f))
                      '()))
      (define result (execute-workflow wf (hasheq) exec-ctx))
      (check-true (tool-result-is-error? result) "should be error")
      (define details (tool-result-details result))
      (check-true (hash? details) "details should be a hash")
      (define steps (hash-ref details 'steps))
      (check-equal? (length steps) 1 "denied step should be present")
      (check-false (hash-ref (car steps) 'success) "denied step should be marked failed")
      (check-false (hash-has-key? (car steps) 'task))
      (check-false (hash-has-key? (car steps) 'result))
      (check-true (hash-has-key? (car steps) 'resultDigest)))

    (test-case "parallel failure preserves all parallel step results in details"
      (define provider (make-failing-provider))
      (define exec-ctx (make-test-exec-ctx provider))
      (define wf
        (make-simple-workflow (list (hasheq 'role "a" 'task "Task A" 'parallel #t)
                                    (hasheq 'role "b" 'task "Task B" 'parallel #t))))
      (define result (execute-workflow wf (hasheq) exec-ctx))
      (check-true (tool-result-is-error? result) "should be error")
      (define details (tool-result-details result))
      (check-true (hash? details) "details should be a hash")
      (define steps (hash-ref details 'steps))
      (check-equal? (length steps) 2 "both parallel steps should be present")
      (check-false (hash-ref (car steps) 'success) "step 0 should be failed")
      (check-false (hash-ref (cadr steps) 'success) "step 1 should be failed"))

    (test-case "successful workflow result shape unchanged"
      (define provider (make-static-text-provider "Done"))
      (define exec-ctx (make-test-exec-ctx provider))
      (define wf (make-simple-workflow (list (hasheq 'role "step1" 'task "First task"))))
      (define result (execute-workflow wf (hasheq) exec-ctx))
      (check-false (tool-result-is-error? result) "should succeed")
      (define content (tool-result-content result))
      (check-true (hash? content) "content should still be a hash")
      (check-equal? (hash-ref content 'workflow) "test-wf"))))

(run-tests suite)

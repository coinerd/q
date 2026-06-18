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
         "../tools/builtins/spawn-subagent.rkt"
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

(define (make-failing-provider)
  (make-provider (lambda () "failing-mock")
                 (lambda () (hasheq 'streaming #t))
                 (lambda (req) (error 'failing-provider "simulated failure"))
                 (lambda (req) (error 'failing-provider "streaming failed"))))

(define (make-test-exec-ctx provider)
  (make-exec-context #:working-directory (current-directory)
                     #:runtime-settings (hasheq 'provider provider 'model "mock-model")))

;; ── Test helpers ──

(define (make-simple-workflow steps)
  (define wf-steps
    (for/list ([s (in-list steps)])
      (workflow-step (hash-ref s 'role "assistant") (hash-ref s 'task "") #f #f)))
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

    (test-case "workflow-step-result struct"
      (define r (workflow-step-result 0 "analyst" "Do thing" #t "Done"))
      (check-equal? (workflow-step-result-step r) 0)
      (check-equal? (workflow-step-result-role r) "analyst")
      (check-equal? (workflow-step-result-task r) "Do thing")
      (check-true (workflow-step-result-success? r))
      (check-equal? (workflow-step-result-result r) "Done"))))

(run-tests suite)

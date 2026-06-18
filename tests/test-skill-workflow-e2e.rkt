#lang racket

;; @speed fast
;; @suite skills

;; tests/test-skill-workflow-e2e.rkt
;; v0.99.26 W4: E2E test for skill-router workflow action.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         json
         "../tools/builtins/skill-router.rkt"
         "../tools/tool.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt")

;; ── Test skill content ──

(define mas-workflow-skill-content
  (string-append "---\n"
                 "type: mas-workflow\n"
                 "agents:\n"
                 "  - role: analyst\n"
                 "    task: Analyze the input\n"
                 "    capabilities: [read-only]\n"
                 "  - role: summarizer\n"
                 "    task: Summarize: {{result}}\n"
                 "    capabilities: [read-only]\n"
                 "---\n"
                 "# Test Workflow\n\n"
                 "A simple 2-step workflow for testing."))

(define standard-skill-content
  (string-append "---\n"
                 "name: regular-skill\n"
                 "description: A standard skill\n"
                 "---\n"
                 "# Regular Skill\n\n"
                 "This is a non-workflow skill."))

;; ── Test helpers ──

(define temp-dir (make-temporary-file "q-workflow-e2e-~a" 'directory))

(define (setup-test-skills)
  (define skills-dir (build-path temp-dir ".q" "skills"))
  (define wf-dir (build-path skills-dir "test-workflow"))
  (define std-dir (build-path skills-dir "regular-skill"))
  (make-directory* wf-dir)
  (make-directory* std-dir)
  (call-with-output-file (build-path wf-dir "SKILL.md")
                         (lambda (out) (display mas-workflow-skill-content out)))
  (call-with-output-file (build-path std-dir "SKILL.md")
                         (lambda (out) (display standard-skill-content out))))

(define (cleanup-test-skills)
  (when (directory-exists? temp-dir)
    (delete-directory/files temp-dir)))

;; ── Suite ──

(define suite
  (test-suite "Skill Workflow E2E (v0.99.26 W4)"

    #:before setup-test-skills
    #:after cleanup-test-skills

    (test-case "list action shows skill types"
      (parameterize ([current-directory temp-dir])
        (define result (tool-skill-route (hasheq 'action "list")))
        (check-false (tool-result-is-error? result))
        (define content (tool-result-content result))
        (define text (hash-ref (car content) 'text ""))
        (define parsed (string->jsexpr text))
        ;; Should have both skills
        (check-true (list? parsed))
        (check-true (>= (length parsed) 2))
        ;; Check that types are included
        (define types (map (lambda (s) (hash-ref s 'type "standard")) parsed))
        (check-true (and (member "mas-workflow" types) #t) "should include mas-workflow type")
        (check-true (and (member "standard" types) #t) "should include standard type")))

    (test-case "workflow action rejects non-workflow skill"
      (parameterize ([current-directory temp-dir])
        (define result (tool-skill-route (hasheq 'action "workflow" 'name "regular-skill")))
        (check-true (tool-result-is-error? result))
        (define content (tool-result-content result))
        (define text (hash-ref (car content) 'text ""))
        (check-true (string-contains? text "not a mas-workflow"))))

    (test-case "workflow action rejects missing skill"
      (parameterize ([current-directory temp-dir])
        (define result (tool-skill-route (hasheq 'action "workflow" 'name "nonexistent")))
        (check-true (tool-result-is-error? result))
        (define content (tool-result-content result))
        (define text (hash-ref (car content) 'text ""))
        (check-true (string-contains? text "not found"))))

    (test-case "workflow action requires name"
      (parameterize ([current-directory temp-dir])
        (define result (tool-skill-route (hasheq 'action "workflow")))
        (check-true (tool-result-is-error? result))
        (define content (tool-result-content result))
        (define text (hash-ref (car content) 'text ""))
        (check-true (or (string-contains? text "requires") (string-contains? text "name")))))

    (test-case "workflow action executes mas-workflow from non-repo current directory"
      ;; Regression test for B-1: dynamic-require used runtime-relative paths,
      ;; so calling skill-route from a project directory failed. This test
      ;; parameterizes current-directory to a temp project dir and provides a
      ;; mock provider via exec-context.
      (parameterize ([current-directory temp-dir])
        (define mock-response
          (make-model-response (list (hasheq 'type "text" 'text "mocked step output"))
                               (hasheq 'prompt-tokens 0 'completion-tokens 0 'total-tokens 0)
                               "mock-model"
                               'stop))
        (define mock-provider (make-mock-provider mock-response #:name "workflow-test-mock"))
        (define exec-ctx
          (make-exec-context #:runtime-settings (hasheq 'provider mock-provider 'model "mock-model")))
        (define result
          (tool-skill-route
           (hasheq 'action "workflow" 'name "test-workflow" 'variables (hasheq 'file "src/main.rkt"))
           exec-ctx))
        (check-false (tool-result-is-error? result) "happy-path workflow should succeed")
        (define content (tool-result-content result))
        (check-true (list? content) "result content should be a list")
        (define text (hash-ref (car content) 'text ""))
        (check-true (string-contains? text "Workflow 'test-workflow' completed successfully")
                    "result should report successful completion")
        (check-true (string-contains? text "mocked step output")
                    "result should include mocked subagent output")
        ;; Verify the workflow executed 2 steps
        (define payload-match (regexp-match #rx"\n(.*)$" text))
        (check-true (pair? payload-match) "text should contain newline-delimited JSON payload")
        (define parsed (string->jsexpr (cadr payload-match)))
        (check-true (hash? parsed) "parsed payload should be a hash")
        (check-equal? (hash-ref parsed 'workflow #f) "test-workflow")
        (check-equal? (length (hash-ref parsed 'steps '())) 2)))))

(run-tests suite)

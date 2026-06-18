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
         "../tools/tool.rkt")

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
        (check-true (or (string-contains? text "requires") (string-contains? text "name")))))))

(run-tests suite)

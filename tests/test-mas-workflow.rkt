#lang racket

;; @speed fast
;; @suite skills

;; tests/test-mas-workflow.rkt
;; v0.99.26 W2: MAS Workflow Types + Parser tests.

(require rackunit
         rackunit/text-ui
         "../skills/mas-workflow.rkt"
         "../skills/frontmatter.rkt")

;; Helper: parse a SKILL.md frontmatter string and return the extended hash.
(define (parse-fm content)
  (parse-skill-frontmatter-extended content))

;; Helper: build a complete SKILL.md string with frontmatter.
(define (skill-with-frontmatter . fm-lines)
  (string-append "---\n" (string-join fm-lines "\n") "\n---\n"))

(define suite
  (test-suite "MAS Workflow Types + Parser (v0.99.26 W2)"

    ;; ── Struct construction ──

    (test-case "workflow-step struct construction"
      (define step (workflow-step "analyst" "Analyze {{file}}" '(read-only) #f))
      (check-equal? (workflow-step-role step) "analyst")
      (check-equal? (workflow-step-task step) "Analyze {{file}}")
      (check-equal? (workflow-step-capabilities step) '(read-only))
      (check-false (workflow-step-parallel? step)))

    (test-case "mas-workflow struct construction"
      (define wf (mas-workflow "test-wf" "A test workflow" '() '()))
      (check-equal? (mas-workflow-name wf) "test-wf")
      (check-equal? (mas-workflow-description wf) "A test workflow")
      (check-equal? (mas-workflow-steps wf) '())
      (check-equal? (mas-workflow-variables wf) '()))

    ;; ── parse-mas-workflow: valid workflows ──

    (test-case "parse valid workflow with 3 steps"
      (define fm
        (parse-fm (skill-with-frontmatter "type: mas-workflow"
                                          "agents:"
                                          "  - role: analyst"
                                          "    task: Read and summarize {{file}}"
                                          "  - role: reviewer"
                                          "    task: Review the summary for {{result}}"
                                          "  - role: writer"
                                          "    task: Write up the final report")))
      (define-values (wf err) (parse-mas-workflow "my-wf" "description" fm))
      (check-false err)
      (check-true (mas-workflow? wf))
      (check-equal? (mas-workflow-name wf) "my-wf")
      (check-equal? (length (mas-workflow-steps wf)) 3)
      (check-equal? (workflow-step-role (car (mas-workflow-steps wf))) "analyst")
      (check-equal? (workflow-step-role (cadr (mas-workflow-steps wf))) "reviewer")
      (check-equal? (workflow-step-role (caddr (mas-workflow-steps wf))) "writer"))

    (test-case "parse workflow with parallel steps"
      (define fm
        (parse-fm (skill-with-frontmatter "type: mas-workflow"
                                          "agents:"
                                          "  - role: researcher"
                                          "    task: Research {{topic}}"
                                          "    parallel: true"
                                          "  - role: checker"
                                          "    task: Check sources"
                                          "    parallel: true"
                                          "  - role: writer"
                                          "    task: Write summary")))
      (define-values (wf err) (parse-mas-workflow "parallel-wf" "desc" fm))
      (check-false err)
      (check-true (mas-workflow? wf))
      (define steps (mas-workflow-steps wf))
      (check-equal? (length steps) 3)
      (check-true (workflow-step-parallel? (car steps)) "first step should be parallel")
      (check-true (workflow-step-parallel? (cadr steps)) "second step should be parallel")
      (check-false (workflow-step-parallel? (caddr steps)) "third step should be sequential"))

    (test-case "parse workflow with inline array capabilities"
      (define fm
        (parse-fm (skill-with-frontmatter "type: mas-workflow"
                                          "agents:"
                                          "  - role: researcher"
                                          "    task: Research {{topic}}"
                                          "    capabilities: [read-only, network]")))
      (define-values (wf err) (parse-mas-workflow "research" "desc" fm))
      (check-false err)
      (define step (car (mas-workflow-steps wf)))
      (check-equal? (workflow-step-capabilities step) '(read-only network)))

    (test-case "parse workflow with list-form capabilities"
      (define fm
        (parse-fm (skill-with-frontmatter "type: mas-workflow"
                                          "agents:"
                                          "  - role: coder"
                                          "    task: Fix the bug"
                                          "    capabilities:"
                                          "      - read-only"
                                          "      - file-write")))
      (define-values (wf err) (parse-mas-workflow "fixer" "desc" fm))
      (check-false err)
      (define step (car (mas-workflow-steps wf)))
      (check-equal? (workflow-step-capabilities step) '(read-only file-write)))

    (test-case "default role when not specified"
      (define fm
        (parse-fm (skill-with-frontmatter "type: mas-workflow" "agents:" "  - task: Do something")))
      (define-values (wf err) (parse-mas-workflow "test" "desc" fm))
      (check-false err)
      (define step (car (mas-workflow-steps wf)))
      (check-equal? (workflow-step-role step) "assistant"))

    ;; ── parse-mas-workflow: validation failures ──

    (test-case "reject workflow without agents field"
      (define fm (hasheq 'type "mas-workflow"))
      (define-values (wf err) (parse-mas-workflow "test" "desc" fm))
      (check-false wf)
      (check-true (string-contains? err "agents")))

    (test-case "reject step without task"
      (define fm
        (parse-fm (skill-with-frontmatter "type: mas-workflow" "agents:" "  - role: analyst")))
      (define-values (wf err) (parse-mas-workflow "test" "desc" fm))
      (check-false wf)
      (check-true (string-contains? err "task")))

    (test-case "reject empty agents list"
      (define fm (hasheq 'agents '()))
      (define-values (wf err) (parse-mas-workflow "test" "desc" fm))
      (check-false wf)
      (check-true (string-contains? err "empty")))

    ;; ── extract-template-variables ──

    (test-case "extract template variables from tasks"
      (define steps
        (list (workflow-step "a" "Read {{file}}" #f #f)
              (workflow-step "b" "Review {{result}} for {{issue}}" #f #f)))
      (define vars (extract-template-variables steps))
      (check-equal? (sort vars string<?) '("file" "issue" "result")))

    (test-case "deduplicate repeated variables"
      (define steps
        (list (workflow-step "a" "Read {{file}}" #f #f) (workflow-step "b" "Process {{file}}" #f #f)))
      (define vars (extract-template-variables steps))
      (check-equal? vars '("file")))

    (test-case "no variables in task"
      (define steps (list (workflow-step "a" "Just a static task" #f #f)))
      (define vars (extract-template-variables steps))
      (check-equal? vars '()))

    ;; ── parse-capabilities ──

    (test-case "parse-capabilities from strings"
      (check-equal? (parse-capabilities '("read-only" "network")) '(read-only network)))

    (test-case "parse-capabilities #f returns #f"
      (check-false (parse-capabilities #f)))

    (test-case "parse-capabilities preserves explicit empty authority"
      (check-equal? (parse-capabilities '()) '()))

    (test-case "parse-capabilities rejects malformed or unknown explicit authority"
      (check-exn exn:fail:contract?
                 (lambda () (parse-capabilities '("read-only" 42 #f "file-write"))))
      (check-exn exn:fail:contract?
                 (lambda () (parse-capabilities '("read-only" "unknown-capability"))))
      (check-exn exn:fail:contract? (lambda () (parse-capabilities '("any")))))

    ;; ── workflow-has-result-chaining? ──

    (test-case "workflow-has-result-chaining? true"
      (define steps
        (list (workflow-step "a" "Step 1" #f #f) (workflow-step "b" "Review {{result}}" #f #f)))
      (define wf (mas-workflow "test" "desc" steps '("result")))
      (check-true (workflow-has-result-chaining? wf)))

    (test-case "workflow-has-result-chaining? false"
      (define steps (list (workflow-step "a" "Step 1" #f #f) (workflow-step "b" "Step 2" #f #f)))
      (define wf (mas-workflow "test" "desc" steps '()))
      (check-false (workflow-has-result-chaining? wf)))

    ;; ── Full end-to-end parse from SKILL.md content ──

    (test-case "full SKILL.md → mas-workflow parse"
      (define skill-content
        (string-append "---\n"
                       "type: mas-workflow\n"
                       "agents:\n"
                       "  - role: researcher\n"
                       "    task: Research {{topic}} and find sources\n"
                       "    capabilities: [read-only, network]\n"
                       "  - role: writer\n"
                       "    task: Write a summary based on {{result}}\n"
                       "    capabilities:\n"
                       "      - read-only\n"
                       "---\n"
                       "# Research and Write\n\n"
                       "This workflow does research then writes it up."))
      (define fm (parse-fm skill-content))
      (check-true (hash? fm))
      (define-values (wf err) (parse-mas-workflow "research-and-write" "desc" fm))
      (check-false err)
      (check-true (mas-workflow? wf))
      (check-equal? (length (mas-workflow-steps wf)) 2)
      (check-true (and (member "topic" (mas-workflow-variables wf)) #t) "should have topic var")
      (check-true (and (member "result" (mas-workflow-variables wf)) #t) "should have result var"))))

(run-tests suite)

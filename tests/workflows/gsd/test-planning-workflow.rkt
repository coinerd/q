#lang racket

;; tests/workflows/gsd/test-planning-workflow.rkt
;; v0.18.2 Wave 1: Planning workflow pipeline tests

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         json
         "../../../extensions/gsd-planning.rkt"
         "../../../extensions/context.rkt"
         "../../../extensions/dynamic-tools.rkt"
         "../../../extensions/ext-commands.rkt"
         "../../../extensions/api.rkt"
         (only-in "../../../extensions/loader.rkt" load-extension!)
         "../../../extensions/hooks.rkt"
         "../../../tools/tool.rkt"
         "../../../agent/event-bus.rkt"
         "../fixtures/mock-provider.rkt"
         "../fixtures/workflow-runner.rkt")

;; ── Helpers ──

(define (with-temp-dir proc)
  (define dir (make-temporary-file "plan-workflow-~a" 'directory))
  (with-handlers ([exn:fail? (lambda (e)
                               (when (directory-exists? dir)
                                 (delete-directory/files dir))
                               (raise e))])
    (begin0 (proc dir)
      (when (directory-exists? dir)
        (delete-directory/files dir)))))

(define gsd-ext-path
  (build-path (or (current-load-relative-directory) (current-directory))
              "../../../extensions/gsd-planning.rkt"))

(define (plan-file-exists? dir)
  (file-exists? (build-path dir ".planning" "PLAN.md")))

(define (read-plan dir)
  (call-with-input-file (build-path dir ".planning" "PLAN.md") (lambda (in) (port->string in))))

(define (result-text result)
  (string-join (for/list ([c (in-list (tool-result-content result))]
                          #:when (and (hash? c) (hash-ref c 'text #f)))
                 (hash-ref c 'text ""))
               ""))

;; ── Test Suite ──

(define test-planning-workflow
  (test-suite "Planning Workflow Tests (v0.18.2 Wave 1)"

    ;; Test 1: Create plan from user request
    (test-case "create plan via planning-write tool"
      (with-temp-dir (lambda (dir)
                       (parameterize ([current-directory dir])
                         (define result
                           (handle-planning-write
                            (hasheq 'artifact
                                    "PLAN"
                                    'content
                                    "# Plan\n## Goal\nTest goal\n## Waves\n- Wave 1: Do stuff")))
                         (check-true (plan-file-exists? dir) "PLAN.md should be created")
                         (define plan-text (read-plan dir))
                         (check-true (string-contains? plan-text "# Plan"))
                         (check-true (string-contains? plan-text "Wave 1"))))))

    ;; Test 2: Read back plan via planning-read tool
    (test-case "round-trip: write then read plan"
      (with-temp-dir (lambda (dir)
                       (parameterize ([current-directory dir])
                         ;; Write
                         (handle-planning-write
                          (hasheq 'artifact "PLAN" 'content "# Plan\n## Goal\nTest round-trip"))
                         ;; Read
                         (define result (handle-planning-read (hasheq 'artifact "PLAN")))
                         (check-true (tool-result? result) "Result should be tool-result")
                         (define text (result-text result))
                         (check-true (string-contains? text "Test round-trip")
                                     "Read should return written content")))))

    ;; Test 3: Resume from existing plan — append STATE.md
    (test-case "resume: plan exists, write STATE.md"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           ;; Create initial plan
           (write-planning-artifact! dir "PLAN" "# Plan\n## Goal\nTest resume")
           (check-true (plan-file-exists? dir))

           ;; Simulate resuming: write STATE.md
           (handle-planning-write (hasheq 'artifact
                                          "STATE"
                                          'content
                                          "# State\n## Status\nIn Progress\n## Current Wave\nWave 2"))

           ;; Read both
           (define plan-result (handle-planning-read (hasheq 'artifact "PLAN")))
           (define state-result (handle-planning-read (hasheq 'artifact "STATE")))

           (check-true (string-contains? (result-text plan-result) "Test resume"))
           (check-true (string-contains? (result-text state-result) "In Progress"))))))

    ;; Test 4: PLAN.md structure validation
    (test-case "valid plan structure has required sections"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           (handle-planning-write
            (hasheq 'artifact
                    "PLAN"
                    'content
                    (string-append "# Plan: Feature X\n\n"
                                   "## Goal\nImplement feature X\n\n"
                                   "## Scope\n- module-a.rkt\n- module-b.rkt\n\n"
                                   "## Waves\n"
                                   "### Wave 1: Foundation\n- Task 1\n- Verify: raco test\n\n"
                                   "### Wave 2: Implementation\n- Task 2\n\n"
                                   "## Dependencies\nNone\n\n"
                                   "## Risks\n- None identified")))

           (define plan-text (read-plan dir))
           (check-true (string-contains? plan-text "## Goal"))
           (check-true (string-contains? plan-text "## Scope"))
           (check-true (string-contains? plan-text "## Waves"))
           (check-true (string-contains? plan-text "## Dependencies"))
           (check-true (string-contains? plan-text "## Risks"))))))

    ;; Test 5: Archive stale plans
    (test-case "overwrite stale plan with new plan"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           ;; Write initial plan
           (handle-planning-write (hasheq 'artifact "PLAN" 'content "# Plan\n## Goal\nOld plan"))

           (define old-plan (read-plan dir))
           (check-true (string-contains? old-plan "Old plan"))

           ;; Write new plan
           (handle-planning-write (hasheq 'artifact "PLAN" 'content "# Plan\n## Goal\nNew plan"))

           (define new-plan (read-plan dir))
           (check-false (string-contains? new-plan "Old plan"))
           (check-true (string-contains? new-plan "New plan"))))))

    ;; Test 6: HANDOFF.json is valid JSON
    (test-case "HANDOFF.json round-trip"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           (write-planning-artifact!
            dir
            "HANDOFF"
            (hasheq 'machine "local" 'milestone "v0.18.2" 'last-wave 1 'next-step "Wave 2"))

           (define result (handle-planning-read (hasheq 'artifact "HANDOFF")))
           (check-true (tool-result? result) "HANDOFF read should return tool-result")
           (define text (result-text result))
           (check-true (string-contains? text "v0.18.2") "HANDOFF should contain milestone data")))))

    ;; Test 7: Valid artifact names
    (test-case "valid artifact names accepted"
      (check-true (valid-artifact-name? "PLAN"))
      (check-true (valid-artifact-name? "STATE"))
      (check-true (valid-artifact-name? "HANDOFF"))
      (check-true (valid-artifact-name? "VALIDATION"))
      (check-true (valid-artifact-name? "BUG_REPORT"))
      (check-true (valid-artifact-name? "BUG_PLAN"))
      (check-true (valid-artifact-name? "custom.md"))
      (check-true (valid-artifact-name? "data.json")))

    (test-case "invalid artifact names rejected"
      (check-false (valid-artifact-name? "../etc/passwd"))
      (check-false (valid-artifact-name? "foo/bar"))
      (check-false (valid-artifact-name? "")))

    ;; Test 8: Extension-loaded workflow produces session log
    (test-case "planning extension in full workflow produces log"
      (unless (file-exists? gsd-ext-path)
        (fail "gsd-planning extension not found"))

      (define ext-reg (make-extension-registry))
      (load-extension! ext-reg gsd-ext-path)

      (define result
        (run-workflow
         (make-scripted-provider (list (hash 'role 'assistant 'content "Done." 'tool_calls (list))))
         "Hello with planning"
         #:extension-registry ext-reg
         #:max-iterations 3))

      (check-not-false (workflow-result-output result))
      (check-true (file-exists? (workflow-result-session-log result)) "Session log should exist"))))

(run-tests test-planning-workflow)

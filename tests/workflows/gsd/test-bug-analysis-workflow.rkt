#lang racket

;; tests/workflows/gsd/test-bug-analysis-workflow.rkt
;; v0.18.2 Wave 2: Bug analysis and fix workflow tests
;;
;; Tests:
;;   - Bug report -> bug plan pipeline
;;   - Bug fix execution from BUG_PLAN
;;   - Verify BUG_REPORT.md and BUG_PLAN.md structure

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         json
         "../../../extensions/gsd-planning.rkt"
         "../../../extensions/context.rkt"
         "../../../extensions/api.rkt"
         "../../../extensions/dynamic-tools.rkt"
         "../../../extensions/ext-commands.rkt"
         "../../../extensions/hooks.rkt"
         "../../../tools/tool.rkt"
         "../../../agent/event-bus.rkt")

;; ── Helpers ──

(define (with-temp-dir proc)
  (define dir (make-temporary-file "bug-workflow-~a" 'directory))
  (with-handlers ([exn:fail? (lambda (e)
                               (when (directory-exists? dir)
                                 (delete-directory/files dir))
                               (raise e))])
    (begin0 (proc dir)
      (when (directory-exists? dir)
        (delete-directory/files dir)))))

(define (result-text result)
  (string-join (for/list ([c (in-list (tool-result-content result))]
                          #:when (and (hash? c) (hash-ref c 'text #f)))
                 (hash-ref c 'text ""))
               ""))

(define (artifact-exists? dir name)
  (define path (planning-artifact-path dir name))
  (and path (file-exists? path)))

(define (read-artifact-file dir name)
  (define path (planning-artifact-path dir name))
  (call-with-input-file path (lambda (in) (port->string in))))

;; ── Test Suite ──

(define test-bug-analysis-workflow
  (test-suite "Bug Analysis Workflow Tests (v0.18.2 Wave 2)"

    ;; Test 1: Create BUG_REPORT.md
    (test-case "create bug report via planning-write"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           (define bug-report
             (string-append "# Bug Report\n\n"
                            "## Summary\nAgent crashes on invalid JSON input\n\n"
                            "## Steps to Reproduce\n"
                            "1. Start agent with corrupted session file\n"
                            "2. Run any command\n"
                            "3. Observe crash in jsonl-read-all\n\n"
                            "## Expected Behavior\nAgent should handle corrupted JSONL gracefully\n\n"
                            "## Actual Behavior\nUnhandled exception in jsonl parsing\n\n"
                            "## Severity\nP1 — blocks normal operation"))

           (define result (handle-planning-write (hasheq 'artifact "BUG_REPORT" 'content bug-report)))

           (check-true (artifact-exists? dir "BUG_REPORT") "BUG_REPORT.md should exist")
           (define content (read-artifact-file dir "BUG_REPORT"))
           (check-true (string-contains? content "## Summary"))
           (check-true (string-contains? content "Steps to Reproduce"))
           (check-true (string-contains? content "P1"))))))

    ;; Test 2: Create BUG_PLAN.md from BUG_REPORT
    (test-case "create bug plan from bug report"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           ;; First write the bug report
           (handle-planning-write (hasheq 'artifact
                                          "BUG_REPORT"
                                          'content
                                          "# Bug Report\n## Summary\nTest bug\n## Severity\nP2"))

           ;; Then write the bug plan
           (define bug-plan
             (string-append "# Bug Plan\n\n"
                            "## Root Cause\nJSONL parser doesn't handle partial writes\n\n"
                            "## Fix Strategy\n"
                            "1. Add defensive parsing in jsonl-read-all\n"
                            "2. Skip corrupted lines instead of crashing\n"
                            "3. Log warnings for skipped entries\n\n"
                            "## Files Affected\n"
                            "- util/jsonl.rkt\n\n"
                            "## Verification\n"
                            "- raco test tests/test-jsonl.rkt\n"
                            "- Manual test with corrupted file"))

           (handle-planning-write (hasheq 'artifact "BUG_PLAN" 'content bug-plan))

           (check-true (artifact-exists? dir "BUG_PLAN") "BUG_PLAN.md should exist")
           (define plan-content (read-artifact-file dir "BUG_PLAN"))
           (check-true (string-contains? plan-content "## Root Cause"))
           (check-true (string-contains? plan-content "## Fix Strategy"))
           (check-true (string-contains? plan-content "## Verification"))))))

    ;; Test 3: Read BUG_PLAN back
    (test-case "round-trip: read bug plan back"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           (handle-planning-write
            (hasheq 'artifact "BUG_PLAN" 'content "# Bug Plan\n## Root Cause\nTest root cause"))

           (define result (handle-planning-read (hasheq 'artifact "BUG_PLAN")))

           (check-true (tool-result? result))
           (check-true (string-contains? (result-text result) "Test root cause"))))))

    ;; Test 4: BUG_STATE.md tracks fix progress
    (test-case "bug state tracks fix progress"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           ;; Write bug report and plan
           (handle-planning-write
            (hasheq 'artifact "BUG_REPORT" 'content "# Bug Report\n## Summary\nTest"))
           (handle-planning-write
            (hasheq 'artifact "BUG_PLAN" 'content "# Bug Plan\n## Root Cause\nTest"))

           ;; Write bug state
           (handle-planning-write
            (hasheq 'artifact
                    "BUG_STATE"
                    'content
                    "# Bug State\n## Status\nFix in progress\n## Current Step\nStep 2 of 3"))

           (define result (handle-planning-read (hasheq 'artifact "BUG_STATE")))

           (check-true (string-contains? (result-text result) "Fix in progress"))))))

    ;; Test 5: BUG_VALIDATION.md records verification
    (test-case "bug validation records test results"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           (handle-planning-write (hasheq 'artifact
                                          "BUG_VALIDATION"
                                          'content
                                          (string-append "# Bug Validation\n\n"
                                                         "## Tests Run\n"
                                                         "- raco test tests/test-jsonl.rkt\n\n"
                                                         "## Results\n"
                                                         "- 42/42 passed\n\n"
                                                         "## Manual Verification\n"
                                                         "- Corrupted file handled gracefully\n\n"
                                                         "## Verdict\n"
                                                         "FIX VERIFIED")))

           (check-true (artifact-exists? dir "BUG_VALIDATION"))
           (define result (handle-planning-read (hasheq 'artifact "BUG_VALIDATION")))
           (check-true (string-contains? (result-text result) "FIX VERIFIED"))))))

    ;; Test 6: All bug artifacts coexist
    (test-case "all bug artifacts coexist in .planning/"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           ;; Write all bug artifacts
           (handle-planning-write
            (hasheq 'artifact "BUG_REPORT" 'content "# Bug Report\n## Summary\nTest"))
           (handle-planning-write
            (hasheq 'artifact "BUG_PLAN" 'content "# Bug Plan\n## Root Cause\nTest"))
           (handle-planning-write
            (hasheq 'artifact "BUG_STATE" 'content "# Bug State\n## Status\nIn progress"))
           (handle-planning-write
            (hasheq 'artifact "BUG_VALIDATION" 'content "# Bug Validation\n## Verdict\nPending"))

           ;; All should exist
           (check-true (artifact-exists? dir "BUG_REPORT"))
           (check-true (artifact-exists? dir "BUG_PLAN"))
           (check-true (artifact-exists? dir "BUG_STATE"))
           (check-true (artifact-exists? dir "BUG_VALIDATION"))

           ;; All should be readable
           (for ([name '("BUG_REPORT" "BUG_PLAN" "BUG_STATE" "BUG_VALIDATION")])
             (define result (handle-planning-read (hasheq 'artifact name)))
             (check-true (tool-result? result) (format "~a should be readable" name)))))))

    ;; Test 7: Bug artifacts don't interfere with planning artifacts
    (test-case "bug and plan artifacts coexist"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           ;; Write plan artifacts
           (handle-planning-write (hasheq 'artifact "PLAN" 'content "# Plan\n## Goal\nTest"))
           (handle-planning-write (hasheq 'artifact "STATE" 'content "# State\n## Status\nActive"))

           ;; Write bug artifacts
           (handle-planning-write
            (hasheq 'artifact "BUG_REPORT" 'content "# Bug Report\n## Summary\nTest"))

           ;; Both should be readable independently
           (define plan-result (handle-planning-read (hasheq 'artifact "PLAN")))
           (define bug-result (handle-planning-read (hasheq 'artifact "BUG_REPORT")))

           (check-true (string-contains? (result-text plan-result) "Test"))
           (check-true (string-contains? (result-text bug-result) "Test"))))))))

(run-tests test-bug-analysis-workflow)

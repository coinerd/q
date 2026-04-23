#lang racket

;; tests/workflows/gsd/test-milestone-delivery-workflow.rkt
;; v0.18.2 Wave 3: Milestone/wave lifecycle workflow tests
;;
;; Tests:
;;   - Wave lifecycle simulation (plan → implement → verify)
;;   - Multi-wave milestone tracking
;;   - Review gate via HANDOFF.json

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         json
         "../../../extensions/gsd-planning.rkt"
         "../../../extensions/api.rkt"
         "../../../tools/tool.rkt"
         "../../../agent/event-bus.rkt")

;; ── Helpers ──

(define (with-temp-dir proc)
  (define dir (make-temporary-file "milestone-workflow-~a" 'directory))
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

;; ── Test Suite ──

(define test-milestone-delivery-workflow
  (test-suite "Milestone Delivery Workflow Tests (v0.18.2 Wave 3)"

    ;; Test 1: Full wave lifecycle: plan → implement → verify
    (test-case "wave lifecycle: plan → implement → verify"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           ;; Step 1: Create plan with multiple waves
           (handle-planning-write
            (hasheq 'artifact
                    "PLAN"
                    'content
                    (string-append "# Plan: Feature X\n\n"
                                   "## Waves\n"
                                   "### Wave 1: Foundation\n- Task 1\n- Verify: raco test\n\n"
                                   "### Wave 2: Implementation\n- Task 2\n- Verify: raco test")))

           ;; Step 2: Set state to Wave 1 in progress
           (handle-planning-write (hasheq 'artifact
                                          "STATE"
                                          'content
                                          "# State\n## Status\nIn Progress\n## Current Wave\nWave 1"))

           ;; Step 3: Verify Wave 1 is tracked
           (define state-after (result-text (handle-planning-read (hasheq 'artifact "STATE"))))
           (check-true (string-contains? state-after "Wave 1"))

           ;; Step 4: Advance to Wave 2
           (handle-planning-write
            (hasheq
             'artifact
             "STATE"
             'content
             "# State\n## Status\nIn Progress\n## Current Wave\nWave 2\n## Completed\n- Wave 1"))

           ;; Step 5: Verify advancement
           (define state-final (result-text (handle-planning-read (hasheq 'artifact "STATE"))))
           (check-true (string-contains? state-final "Wave 2"))
           (check-true (string-contains? state-final "Wave 1"))))))

    ;; Test 2: Multi-wave milestone tracking with VALIDATION.md
    (test-case "multi-wave milestone with validation"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           ;; Create plan
           (handle-planning-write
            (hasheq 'artifact "PLAN" 'content "# Plan\n## Waves\n### Wave 1\n### Wave 2"))

           ;; Complete Wave 1 — write validation
           (handle-planning-write (hasheq 'artifact
                                          "VALIDATION"
                                          'content
                                          (string-append "# Validation\n\n"
                                                         "## Wave 1\n"
                                                         "### Tests\n"
                                                         "- 100/100 passed\n"
                                                         "### Verdict\n"
                                                         "APPROVED\n\n"
                                                         "## Wave 2\n"
                                                         "### Tests\n"
                                                         "- Pending")))

           ;; Read validation back
           (define val-result (handle-planning-read (hasheq 'artifact "VALIDATION")))
           (check-true (string-contains? (result-text val-result) "APPROVED"))
           (check-true (string-contains? (result-text val-result) "100/100"))))))

    ;; Test 3: HANDOFF.json for machine switches
    (test-case "handoff.json for machine switch"
      (with-temp-dir (lambda (dir)
                       (parameterize ([current-directory dir])
                         ;; Write handoff data
                         (define handoff
                           (hasheq 'machine
                                   "vps"
                                   'milestone
                                   "v0.18.2"
                                   'last-wave
                                   2
                                   'next-step
                                   "Wave 3: trace analysis"
                                   'open-issues
                                   (list 1627)
                                   'branch
                                   "test/wave-1627"))
                         (write-planning-artifact! dir "HANDOFF" handoff)

                         ;; Read back
                         (define result (handle-planning-read (hasheq 'artifact "HANDOFF")))
                         (check-true (tool-result? result))
                         (define text (result-text result))
                         (check-true (string-contains? text "vps"))
                         (check-true (string-contains? text "v0.18.2"))
                         (check-true (string-contains? text "Wave 3"))))))

    ;; Test 4: SUMMARY.md for concise progress
    (test-case "summary.md tracks concise progress"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           (handle-planning-write (hasheq 'artifact
                                          "SUMMARY"
                                          'content
                                          (string-append "# Summary\n\n"
                                                         "## v0.18.2 Progress\n"
                                                         "- [x] Wave 1: Planning workflow tests\n"
                                                         "- [x] Wave 2: Bug analysis tests\n"
                                                         "- [ ] Wave 3: Milestone delivery tests\n\n"
                                                         "## Stats\n"
                                                         "- Tests: 16 new\n"
                                                         "- Files: 3 new")))

           (define result (handle-planning-read (hasheq 'artifact "SUMMARY")))
           (check-true (string-contains? (result-text result) "Wave 1"))
           (check-true (string-contains? (result-text result) "16 new"))))))

    ;; Test 5: Complete lifecycle — all artifacts present
    (test-case "complete lifecycle has all artifacts"
      (with-temp-dir
       (lambda (dir)
         (parameterize ([current-directory dir])
           ;; Write all artifacts as a complete milestone lifecycle would
           (handle-planning-write
            (hasheq 'artifact "PLAN" 'content "# Plan\n## Goal\nTest lifecycle"))
           (handle-planning-write (hasheq 'artifact "STATE" 'content "# State\n## Status\nComplete"))
           (handle-planning-write
            (hasheq 'artifact "VALIDATION" 'content "# Validation\n## Verdict\nAPPROVED"))
           (handle-planning-write (hasheq 'artifact
                                          "SUMMARY"
                                          'content
                                          (format "# Summary\n## Milestone\nCompleted at ~a"
                                                  (current-seconds))))

           ;; Write handoff as JSON
           (write-planning-artifact! dir "HANDOFF" (hasheq 'status "complete" 'version "v0.18.2"))

           ;; Verify all exist
           (for ([name '("PLAN" "STATE" "VALIDATION" "SUMMARY" "HANDOFF")])
             (check-true (file-exists? (planning-artifact-path dir name))
                         (format "~a should exist" name)))

           ;; Verify all readable
           (for ([name '("PLAN" "STATE" "VALIDATION" "SUMMARY" "HANDOFF")])
             (define result (handle-planning-read (hasheq 'artifact name)))
             (check-true (tool-result? result) (format "~a should be readable" name)))))))

    ;; Test 6: Artifact path resolution
    (test-case "artifact path resolution correctness"
      (define path-PLAN (planning-artifact-path "/tmp/test" "PLAN"))
      (check-equal? path-PLAN (build-path "/tmp/test" ".planning" "PLAN.md"))

      (define path-HANDOFF (planning-artifact-path "/tmp/test" "HANDOFF"))
      (check-equal? path-HANDOFF (build-path "/tmp/test" ".planning" "HANDOFF.json"))

      (define path-custom (planning-artifact-path "/tmp/test" "notes.md"))
      (check-equal? path-custom (build-path "/tmp/test" ".planning" "notes.md"))

      (define path-invalid (planning-artifact-path "/tmp/test" "noext"))
      (check-false path-invalid "Invalid extension should return #f"))))

(run-tests test-milestone-delivery-workflow)

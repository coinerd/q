#lang racket/base

;; @speed slow  ;; @suite workflows

;; BOUNDARY: integration

;; tests/workflows/extensions/test-extension-round-trip.rkt
;; v0.18.1 Wave 2: Full round-trip test with gsd-planning extension.
;; Loads extension, runs prompt, verifies planning-read/write tool calls work.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/runtime-path
         "../../workflows/fixtures/mock-provider.rkt"
         "../../workflows/fixtures/workflow-runner.rkt"
         "../../../agent/event-bus.rkt"
         "../../../util/message/protocol-types.rkt"
         (only-in "../../../extensions/api.rkt" make-extension-registry extension-registry?)
         (only-in "../../../extensions/loader.rkt" load-extension!))

;; ── Helpers ──

(define-runtime-path here-dir ".")
(define (extension-path name)
  (build-path here-dir ".." ".." ".." "extensions" (format "~a.rkt" name)))

(define gsd-planning-path (extension-path "gsd-planning"))

;; ── Test Suite ──

(define test-extension-round-trip
  (test-suite "Extension Round-Trip (v0.18.1 Wave 2)"

    (test-case "gsd-planning extension loads and planning-read works"
      ;; Skip if extension not found
      (unless (file-exists? gsd-planning-path)
        (fail "gsd-planning extension not found"))

      ;; Create extension registry with gsd-planning
      (define ext-reg (make-extension-registry))
      (define bus (make-event-bus))
      (load-extension! ext-reg gsd-planning-path #:event-bus bus)

      ;; Set up a temp project with .planning/PLAN.md
      (define result
        (run-workflow (make-scripted-provider
                       (list (tool-call-response "tc-1" "planning-read" (hash 'artifact "PLAN.md"))
                             (text-response "Plan reviewed.")))
                      "Check the plan"
                      #:extension-registry ext-reg
                      #:files (list (cons ".planning/PLAN.md" "# Plan\nWave 1: Do stuff"))
                      #:max-iterations 5))

      (check-not-false (workflow-result-output result))
      (check-equal? (loop-result-termination-reason (workflow-result-output result))
                    'completed
                    "Workflow should complete"))

    (test-case "extension-loaded workflow has session log"
      (unless (file-exists? gsd-planning-path)
        (fail "gsd-planning extension not found"))

      (define ext-reg (make-extension-registry))
      (load-extension! ext-reg gsd-planning-path)

      (define result
        (run-workflow (make-scripted-provider (list (text-response "Done.")))
                      "Hello"
                      #:extension-registry ext-reg
                      #:max-iterations 3))

      ;; Session log should exist and be valid
      (check-true (file-exists? (workflow-result-session-log result)) "Session log should exist")
      (check-not-false (workflow-session-entries result) "Session entries should be readable"))

    (test-case "skills-dir loads skills into system instructions"
      ;; Create a temp dir with a test skill
      (define tmp-dir (make-temporary-file "skill-test-~a" 'directory))
      (define skills-dir (build-path tmp-dir "skills"))
      (make-directory skills-dir)
      (define test-skill-dir (build-path skills-dir "test-skill"))
      (make-directory test-skill-dir)
      (call-with-output-file (build-path test-skill-dir "SKILL.md")
                             (lambda (out) (display "You are a test skill assistant.\n" out)))

      (define result
        (run-workflow (make-scripted-provider (list (text-response "Using test skill.")))
                      "Hello"
                      #:skills-dir tmp-dir
                      #:max-iterations 3))

      (check-not-false (workflow-result-output result))
      (check-equal? (loop-result-termination-reason (workflow-result-output result)) 'completed)

      ;; Cleanup
      (delete-directory/files tmp-dir))

    (test-case "multi-turn with extension and skills"
      (unless (file-exists? gsd-planning-path)
        (fail "gsd-planning extension not found"))

      (define result
        (run-workflow-multi-turn (make-scripted-provider (list (text-response "Turn 1")
                                                               (text-response "Turn 2")))
                                 '("First" "Second")
                                 #:extensions (list gsd-planning-path)
                                 #:max-iterations 3))

      (check-false (workflow-result-output result))
      (check-true (file-exists? (workflow-result-session-log result))))))

(run-tests test-extension-round-trip)

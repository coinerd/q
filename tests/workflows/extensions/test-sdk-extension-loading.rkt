#lang racket/base

;; BOUNDARY: integration

;; tests/workflows/extensions/test-sdk-extension-loading.rkt
;; v0.18.1 Wave 1: Verify extensions load through SDK runtime in workflow tests.

(require rackunit
         rackunit/text-ui
         racket/runtime-path
         "../../workflows/fixtures/mock-provider.rkt"
         "../../workflows/fixtures/workflow-runner.rkt"
         "../../../agent/event-bus.rkt"
         "../../../extensions/api.rkt"
         (only-in "../../../extensions/loader.rkt" load-extension!)
         "../../../tools/tool.rkt")

;; ── Helpers ──

(define-runtime-path here-dir ".")
(define (extension-path name)
  (build-path here-dir ".." ".." ".." "extensions" (format "~a.rkt" name)))

;; ── Test Suite ──

(define test-sdk-extension-loading
  (test-suite "SDK Extension Loading (v0.18.1)"

    (test-case "workflow-runner accepts #:extension-registry"
      ;; Create registry with gsd-planning extension loaded
      (define ext-reg (make-extension-registry))
      (define bus (make-event-bus))
      (define gsd-path (extension-path "gsd-planning"))
      (when (file-exists? gsd-path)
        (load-extension! ext-reg gsd-path #:event-bus bus))

      ;; Run workflow with the extension registry
      (define prov (make-scripted-provider (list (text-response "I'll check the planning state."))))
      (define result
        (run-workflow prov "Check planning state" #:extension-registry ext-reg #:max-iterations 3))

      ;; Verify workflow completed
      (check-not-false (workflow-result-output result) "Workflow should produce output"))

    (test-case "workflow-runner accepts #:extensions with path list"
      ;; Load extensions via path list
      (define gsd-path (extension-path "gsd-planning"))
      (unless (file-exists? gsd-path)
        (error 'test "Extension not found: ~a" gsd-path))

      (define prov (make-scripted-provider (list (text-response "Planning check done."))))
      (define result
        (run-workflow prov "Check planning" #:extensions (list gsd-path) #:max-iterations 3))

      (check-not-false (workflow-result-output result)))

    (test-case "no extensions: backward compatible"
      ;; Should work without any extensions (default behavior)
      (define prov (make-scripted-provider (list (text-response "Hello!"))))
      (define result (run-workflow prov "Hello" #:max-iterations 3))

      (check-not-false (workflow-result-output result)))

    (test-case "multi-turn accepts #:extensions"
      (define gsd-path (extension-path "gsd-planning"))
      (define prov (make-scripted-provider (list (text-response "Step 1") (text-response "Step 2"))))

      (define result
        (run-workflow-multi-turn prov
                                 '("First prompt" "Second prompt")
                                 #:extensions (if (file-exists? gsd-path)
                                                  (list gsd-path)
                                                  '())
                                 #:max-iterations 3))

      ;; Multi-turn returns #f for output (by design)
      (check-false (workflow-result-output result))
      (check-not-false (workflow-result-events result)))))

(run-tests test-sdk-extension-loading)

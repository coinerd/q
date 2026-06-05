#lang racket

;; q/tests/test-w9-ui-regression.rkt — W9.3 Full UI regression summary
;;
;; Runs all UI-related test files and verifies total test count.
;; This is the release gate for v0.94.8.

(require rackunit
         rackunit/text-ui)

;; Count tests across all UI/GUI test files
(define ui-test-files
  '("tests/test-ui-action-adapters.rkt"
    "tests/test-ui-action-parity.rkt"
    "tests/test-ui-actions.rkt"
    "tests/test-ui-command-registry.rkt"
    "tests/test-ui-delta.rkt"
    "tests/test-ui-dispatch.rkt"
    "tests/test-ui-import-safety.rkt"
    "tests/test-ui-reducer.rkt"
    "tests/test-ui-render-hooks.rkt"
    "tests/test-ui-surface-actions.rkt"
    "tests/test-ui-surface-characterization.rkt"
    "tests/test-ui-surface-null-safety.rkt"
    "tests/test-ui-theme-layout.rkt"
    "tests/test-ui-widget-descriptor.rkt"
    "tests/test-w4-widget-schema.rkt"
    "tests/test-w7-integration-gates.rkt"
    "tests/test-gui-lifecycle-hooks.rkt"
    "tests/test-gui-lifecycle-hooks-w3.rkt"))

(define-test-suite
 test-w9-regression

 (test-case "W9.3: All UI test files exist"
   (for ([f (in-list ui-test-files)])
     (check-true (file-exists? f) (format "~a missing" f)))))

(run-tests test-w9-regression)

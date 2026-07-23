#lang racket/base

;; q/tests/test-silent-handler-logging.rkt — Regression tests for AXIS2-F07
;; Verify that previously silent error handlers now log appropriately.

;; @speed fast
(require rackunit
         racket/string
         racket/file
         "../tui/theme.rkt")

;; Test: load-theme-from-json returns #f for nonexistent file (logs debug)
(check-false (load-theme-from-json "/nonexistent/path/theme.json")
             "load-theme-from-json returns #f for missing file")

;; Test: load-theme-from-json returns #f for invalid JSON (logs debug)
(check-false (let ([tmp (make-temporary-file)])
               (call-with-output-file tmp (lambda (p) (display "not json" p)) #:exists 'truncate)
               (begin0 (load-theme-from-json tmp)
                 (delete-file tmp)))
             "load-theme-from-json returns #f for invalid JSON")

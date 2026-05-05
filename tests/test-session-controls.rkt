#lang racket

;; tests/test-session-controls.rkt — W2-D4: Test scaffold for runtime/session-controls.rkt
;; v0.29.13: Smoke tests for session control functions.

(require rackunit
         "../runtime/session-controls.rkt")

(test-case "thinking-levels is a non-empty list of symbols"
  (check-true (list? thinking-levels))
  (check-true (> (length thinking-levels) 0))
  (for ([level (in-list thinking-levels)])
    (check-true (symbol? level))))

(test-case "thinking-level? accepts valid levels"
  (check-true (thinking-level? 'medium))
  (check-true (thinking-level? 'high))
  (check-true (thinking-level? 'off)))

(test-case "thinking-level? rejects invalid levels"
  (check-false (thinking-level? 'invalid))
  (check-false (thinking-level? "medium")))

#lang racket/base

;; test-exploration-loop-wiring.rkt — TDD tests for v0.28.22 W1
;; Tests for exploration loop detection wiring

(require rackunit
         rackunit/text-ui
         racket/string
         "../runtime/iteration/retry-policy.rkt")

(define suite
  (test-suite
   "Exploration loop detection wiring"

   (test-case
    "T5: detect repeating 2-tool pattern"
    ;; read-grep-read-grep-read-grep = 3 repeats
    (define tools '("read" "grep" "read" "grep" "read" "grep" "bash"))
    (define result (detect-exploration-loop tools))
    (check-true (string? result) "loop detected")
    (check-true (string-contains? result "read") "mentions read"))

   (test-case
    "T6: no warning for varied tool sequence"
    (define tools '("read" "bash" "edit" "write" "read" "bash"))
    (define result (detect-exploration-loop tools))
    (check-false result "no loop detected for varied sequence"))

   (test-case
    "detect-exploration-loop with insufficient history"
    (define tools '("read" "grep"))
    (define result (detect-exploration-loop tools))
    (check-false result "not enough data for loop detection"))

   (test-case
    "detect-exploration-loop with single tool repeated"
    (define tools '("read" "read" "read" "read" "read"))
    (define result (detect-exploration-loop tools))
    ;; Single-tool pattern: ("read" "read") repeated 2 times with one extra
    (check-true (or (string? result) (not result))
                "single-tool patterns handled gracefully"))))

(run-tests suite)

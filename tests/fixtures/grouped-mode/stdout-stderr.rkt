#lang racket/base

;; W7 characterization fixture: prints to stdout and stderr, then runs a
;; passing run-tests suite. Grouped execution must capture both ports.
(require rackunit
         rackunit/text-ui)

(module+ test
  (displayln "GMD-W7-STDOUT-MARKER")
  (displayln "GMD-W7-STDERR-MARKER" (current-error-port))
  (run-tests (test-suite "stdout-stderr"
               (test-case "trivial"
                 (check-true #t)))))

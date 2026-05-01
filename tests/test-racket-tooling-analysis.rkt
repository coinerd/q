#lang racket

;; tests/test-racket-tooling-analysis.rkt — tests for analysis sub-module

(require rackunit
         rackunit/text-ui
         (only-in "../extensions/racket-tooling/analysis.rkt" handle-racket-check))

(define analysis-tests
  (test-suite "Racket Tooling Analysis"

    (test-case "handle-racket-check: missing path raises error"
      (check-exn exn:fail?
                 (lambda () (handle-racket-check (hasheq 'path "" 'mode "syntax")))))

    (test-case "handle-racket-check: nonexistent file raises error"
      (check-exn exn:fail?
                 (lambda () (handle-racket-check (hasheq 'path "/nonexistent.rkt" 'mode "syntax")))))))

(module+ main
  (run-tests analysis-tests))
(module+ test
  (run-tests analysis-tests))

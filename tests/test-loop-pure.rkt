#lang racket

;; tests/test-loop-pure.rkt — tests for compute-next-loop-state

(require rackunit
         "../agent/loop.rkt")

(test-case "compute-next-loop-state exists"
  (check-not-exn (lambda () compute-next-loop-state)))

;; End of tests

#lang racket

;; tests/test-state-machine-pure.rkt — tests for compute-next-gsm-state

(require rackunit
         "../extensions/gsd/state-machine.rkt")

(test-case "compute-next-gsm-state exists"
  (check-not-exn (lambda () compute-next-gsm-state)))

;; End of tests

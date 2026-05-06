#lang racket/base

(require rackunit
         "../agent/loop.rkt"
         "../agent/state.rkt")

;; Test compute-next-loop-state
(test-case "compute-next-loop-state returns state unchanged for now"
  (define st (make-loop-state "s" "t"))
  (define result (compute-next-loop-state st 'some-event))
  (check eq? result st))

(printf "test-loop-state.rkt: all tests passed~n")

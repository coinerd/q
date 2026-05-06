#lang racket/base

(require rackunit
         "../../extensions/gsd/state-machine.rkt")

;; Test compute-next-gsm-state pure function
(define (test-transition current-mode target expected-ok?)
  (define state (make-initial-gsd-state))
  ;; set mode to current-mode
  (define state2 (struct-copy gsd-runtime-state state [mode current-mode]))
  (define result (compute-next-gsm-state state2 target))
  (check-eq? (ok? result) expected-ok?))

(test-case "idle -> exploring is valid"
  (test-transition 'idle 'exploring #t))

(test-case "idle -> executing is invalid"
  (test-transition 'idle 'executing #f))

(test-case "exploring -> plan-written is valid"
  (test-transition 'exploring 'plan-written #t))

(test-case "plan-written -> executing is valid"
  (test-transition 'plan-written 'executing #t))

(test-case "executing -> verifying is valid"
  (test-transition 'executing 'verifying #t))

(test-case "verifying -> idle is valid"
  (test-transition 'verifying 'idle #t))

(test-case "verifying -> executing is valid"
  (test-transition 'verifying 'executing #t))

(printf "test-gsd-state-machine.rkt: all tests passed~n")

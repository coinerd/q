#lang racket/base

;; tests/test-state-machine-pure.rkt — Pure FSM tests (RA-26)
;;
;; Tests compute-next-gsm-state without side effects.
;; All 13 transition cases from the audit plan.

(require rackunit
         racket/set
         "../extensions/gsd/state-machine.rkt"
         "../extensions/gsd/runtime-state-types.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-state mode #:executor [exec #f])
  (gsd-runtime-state mode 0 0 (set) exec #f #f 500 '()))

;; ============================================================
;; Valid transitions
;; ============================================================

(test-case "idle → exploring: valid"
  (define r (compute-next-gsm-state (make-state 'idle) 'exploring))
  (check-true (ok? r))
  (check-equal? (ok-from r) 'idle)
  (check-equal? (ok-to r) 'exploring))

(test-case "exploring → plan-written: valid"
  (define r (compute-next-gsm-state (make-state 'exploring) 'plan-written))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'plan-written))

(test-case "exploring → idle: valid (cancel)"
  (define r (compute-next-gsm-state (make-state 'exploring) 'idle))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'idle))

(test-case "plan-written → executing: valid"
  (define r (compute-next-gsm-state (make-state 'plan-written) 'executing))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'executing))

(test-case "plan-written → idle: valid (cancel)"
  (define r (compute-next-gsm-state (make-state 'plan-written) 'idle))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'idle))

(test-case "executing → verifying: valid"
  (define r (compute-next-gsm-state (make-state 'executing) 'verifying))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'verifying))

(test-case "executing → idle: valid (wave complete)"
  (define r (compute-next-gsm-state (make-state 'executing) 'idle))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'idle))

(test-case "verifying → idle: valid"
  (define r (compute-next-gsm-state (make-state 'verifying) 'idle))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'idle))

(test-case "verifying → executing: valid (re-plan)"
  (define r (compute-next-gsm-state (make-state 'verifying) 'executing))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'executing))

;; ============================================================
;; Invalid transitions
;; ============================================================

(test-case "idle → idle: valid via self-loop"
  (define r (compute-next-gsm-state (make-state 'idle) 'idle))
  (check-true (ok? r)))

(test-case "idle → executing: invalid"
  (define r (compute-next-gsm-state (make-state 'idle) 'executing))
  (check-true (err? r))
  (check-equal? (err-reason r) "invalid transition: idle → executing (valid: (exploring))"))

(test-case "executing → plan-written: invalid"
  (define r (compute-next-gsm-state (make-state 'executing) 'plan-written))
  (check-true (err? r)))

(test-case "verifying → plan-written: invalid"
  (define r (compute-next-gsm-state (make-state 'verifying) 'plan-written))
  (check-true (err? r)))

(test-case "plan-written → exploring: invalid"
  (define r (compute-next-gsm-state (make-state 'plan-written) 'exploring))
  (check-true (err? r)))

;; ============================================================
;; Executor clearing on transition
;; ============================================================

(test-case "executor cleared on executing → idle"
  (define state (make-state 'executing #:executor (lambda () 'work)))
  (define r (compute-next-gsm-state state 'idle))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'idle))

(test-case "executor preserved on executing → verifying"
  (define r (compute-next-gsm-state (make-state 'executing) 'verifying))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'verifying))

;; ============================================================
;; Invalid state values
;; ============================================================

(test-case "invalid state value returns err-result"
  (define r (compute-next-gsm-state (make-state 'idle) 'nonexistent))
  (check-true (err? r))
  (check-equal? (err-reason r) "invalid state: nonexistent"))

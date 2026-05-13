#lang racket/base

;; BOUNDARY: pure

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
  (define-values (r _) (compute-next-gsm-state (make-state 'idle) 'exploring))
  (check-true (ok? r))
  (check-equal? (ok-from r) 'idle)
  (check-equal? (ok-to r) 'exploring))

(test-case "exploring → plan-written: valid"
  (define-values (r _) (compute-next-gsm-state (make-state 'exploring) 'plan-written))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'plan-written))

(test-case "exploring → idle: valid (cancel)"
  (define-values (r _) (compute-next-gsm-state (make-state 'exploring) 'idle))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'idle))

(test-case "plan-written → executing: valid"
  (define-values (r _) (compute-next-gsm-state (make-state 'plan-written) 'executing))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'executing))

(test-case "plan-written → idle: valid (cancel)"
  (define-values (r _) (compute-next-gsm-state (make-state 'plan-written) 'idle))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'idle))

(test-case "executing → verifying: valid"
  (define-values (r _) (compute-next-gsm-state (make-state 'executing) 'verifying))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'verifying))

(test-case "executing → idle: valid (wave complete)"
  (define-values (r _) (compute-next-gsm-state (make-state 'executing) 'idle))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'idle))

(test-case "verifying → idle: valid"
  (define-values (r _) (compute-next-gsm-state (make-state 'verifying) 'idle))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'idle))

(test-case "verifying → executing: valid (re-plan)"
  (define-values (r _) (compute-next-gsm-state (make-state 'verifying) 'executing))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'executing))

;; ============================================================
;; Invalid transitions
;; ============================================================

(test-case "idle → idle: valid via self-loop"
  (define-values (r _) (compute-next-gsm-state (make-state 'idle) 'idle))
  (check-true (ok? r)))

(test-case "idle → executing: invalid"
  (define-values (r _) (compute-next-gsm-state (make-state 'idle) 'executing))
  (check-true (err? r))
  (check-equal? (err-reason r) "invalid transition: idle → executing (valid: (exploring))"))

(test-case "executing → plan-written: invalid"
  (define-values (r _) (compute-next-gsm-state (make-state 'executing) 'plan-written))
  (check-true (err? r)))

(test-case "verifying → plan-written: invalid"
  (define-values (r _) (compute-next-gsm-state (make-state 'verifying) 'plan-written))
  (check-true (err? r)))

(test-case "plan-written → exploring: invalid"
  (define-values (r _) (compute-next-gsm-state (make-state 'plan-written) 'exploring))
  (check-true (err? r)))

;; ============================================================
;; Executor clearing on transition
;; ============================================================

(test-case "executor cleared on executing → idle"
  (define state (make-state 'executing #:executor (lambda () 'work)))
  (define-values (r _) (compute-next-gsm-state state 'idle))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'idle))

(test-case "executor preserved on executing → verifying"
  (define-values (r _) (compute-next-gsm-state (make-state 'executing) 'verifying))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'verifying))

;; ============================================================
;; Returned state verification (RA-04)
;; ============================================================

(test-case "returned state has correct mode on valid transition"
  (define-values (r new-state) (compute-next-gsm-state (make-state 'idle) 'exploring))
  (check-true (ok? r))
  (check-equal? (gsd-runtime-state-mode new-state) 'exploring))

(test-case "returned state clears executor on executing → idle"
  (define-values (r new-state)
    (compute-next-gsm-state (make-state 'executing #:executor (lambda () 'work)) 'idle))
  (check-true (ok? r))
  (check-equal? (gsd-runtime-state-mode new-state) 'idle)
  (check-false (gsd-runtime-state-wave-executor new-state)))

(test-case "returned state clears executor on executing → verifying"
  (define exec (lambda () 'work))
  (define-values (r new-state)
    (compute-next-gsm-state (make-state 'executing #:executor exec) 'verifying))
  (check-true (ok? r))
  (check-equal? (gsd-runtime-state-mode new-state) 'verifying)
  (check-false (gsd-runtime-state-wave-executor new-state)))

(test-case "returned state is unchanged on invalid transition"
  (define state (make-state 'idle #:executor (lambda () 'work)))
  (define-values (r new-state) (compute-next-gsm-state state 'executing))
  (check-true (err? r))
  (check-equal? (gsd-runtime-state-mode new-state) 'idle)
  (check-equal? (gsd-runtime-state-wave-executor new-state) (gsd-runtime-state-wave-executor state)))

;; ============================================================
;; Invalid state values
;; ============================================================

(test-case "invalid state value returns err-result"
  (define-values (r _) (compute-next-gsm-state (make-state 'idle) 'nonexistent))
  (check-true (err? r))
  (check-equal? (err-reason r) "invalid state: nonexistent"))

;; ============================================================
;; FF-01: gsm-transition-to! auto-routing
;; ============================================================

(test-case "gsm-transition-to! idle from idle is no-op"
  (define-values (r _) (compute-next-gsm-state (make-state 'idle) 'idle))
  (check-true (ok? r))
  (check-equal? (ok-to r) 'idle))

(test-case "gsm-transition-to! finds multi-step path idle -> executing"
  ;; Path: idle -> exploring -> plan-written -> executing
  (define-values (r1 s1) (compute-next-gsm-state (make-state 'idle) 'exploring))
  (check-true (ok? r1))
  (define-values (r2 s2) (compute-next-gsm-state s1 'plan-written))
  (check-true (ok? r2))
  (define-values (r3 s3) (compute-next-gsm-state s2 'executing))
  (check-true (ok? r3))
  (check-equal? (gsd-runtime-state-mode s3) 'executing))

(test-case "gsm-transition-to! finds path plan-written -> exploring via idle"
  ;; plan-written -> idle -> exploring (no direct transition)
  (define-values (r1 s1) (compute-next-gsm-state (make-state 'plan-written) 'idle))
  (check-true (ok? r1))
  (define-values (r2 s2) (compute-next-gsm-state s1 'exploring))
  (check-true (ok? r2))
  (check-equal? (gsd-runtime-state-mode s2) 'exploring))

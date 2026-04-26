#lang racket

;; tests/test-gsd-state-machine.rkt — GSD State Machine tests
;;
;; Wave 0 of v0.21.0: State machine with explicit transitions and guards.
;; Covers: valid transitions, invalid transitions, reset, snapshot,
;; transition guards, thread safety.

(require rackunit
         "../extensions/gsd/state-machine.rkt")

;; ============================================================
;; Helper: navigate to arbitrary state
;; ============================================================

;; Navigate to a given state through valid transitions.
(define (navigate-to-state! target)
  (case target
    [(idle) (void)]
    [(exploring) (gsm-transition! 'exploring)]
    [(plan-written)
     (gsm-transition! 'exploring)
     (gsm-transition! 'plan-written)]
    [(executing)
     (gsm-transition! 'exploring)
     (gsm-transition! 'plan-written)
     (gsm-transition! 'executing)]
    [(verifying)
     (gsm-transition! 'exploring)
     (gsm-transition! 'plan-written)
     (gsm-transition! 'executing)
     (gsm-transition! 'verifying)]))

;; ============================================================
;; Valid state transitions
;; ============================================================

(test-case "initial state is idle"
  (reset-gsm!)
  (check-eq? (gsm-current) 'idle))

(test-case "idle → exploring via /plan"
  (reset-gsm!)
  (define r (gsm-transition! 'exploring))
  (check-eq? (gsm-current) 'exploring)
  (check-true (ok? r)))

(test-case "exploring → plan-written via planning-write PLAN"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (define r (gsm-transition! 'plan-written))
  (check-eq? (gsm-current) 'plan-written)
  (check-true (ok? r)))

(test-case "exploring → idle via /reset"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (define r (gsm-transition! 'idle))
  (check-eq? (gsm-current) 'idle)
  (check-true (ok? r)))

(test-case "plan-written → executing via /go"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (define r (gsm-transition! 'executing))
  (check-eq? (gsm-current) 'executing)
  (check-true (ok? r)))

(test-case "plan-written → exploring via /replan"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (define r (gsm-transition! 'exploring))
  (check-eq? (gsm-current) 'exploring)
  (check-true (ok? r)))

(test-case "executing → verifying after all waves attempted"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsm-transition! 'verifying))
  (check-eq? (gsm-current) 'verifying)
  (check-true (ok? r)))

(test-case "executing → exploring on catastrophic plan failure"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsm-transition! 'exploring))
  (check-eq? (gsm-current) 'exploring)
  (check-true (ok? r)))

(test-case "verifying → idle when all verifications pass"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (gsm-transition! 'verifying)
  (define r (gsm-transition! 'idle))
  (check-eq? (gsm-current) 'idle)
  (check-true (ok? r)))

(test-case "verifying → executing on verification failures"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (gsm-transition! 'verifying)
  (define r (gsm-transition! 'executing))
  (check-eq? (gsm-current) 'executing)
  (check-true (ok? r)))

;; ============================================================
;; Reset transition (any → idle)
;; ============================================================

(test-case "reset from any state goes to idle"
  (for ([from-state '(idle exploring plan-written executing verifying)])
    (reset-gsm!)
    (navigate-to-state! from-state)
    (define r (gsm-reset!))
    (check-eq? (gsm-current) 'idle)
    (check-true (ok? r) (format "reset from ~a should succeed" from-state))))

;; ============================================================
;; Invalid transitions
;; ============================================================

(test-case "idle → executing is invalid"
  (reset-gsm!)
  (define r (gsm-transition! 'executing))
  (check-eq? (gsm-current) 'idle)
  (check-false (ok? r)))

(test-case "idle → plan-written is invalid"
  (reset-gsm!)
  (define r (gsm-transition! 'plan-written))
  (check-eq? (gsm-current) 'idle)
  (check-false (ok? r)))

(test-case "idle → verifying is invalid"
  (reset-gsm!)
  (define r (gsm-transition! 'verifying))
  (check-eq? (gsm-current) 'idle)
  (check-false (ok? r)))

(test-case "executing → plan-written is invalid"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsm-transition! 'plan-written))
  (check-eq? (gsm-current) 'executing)
  (check-false (ok? r)))

(test-case "verifying → exploring is invalid"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (gsm-transition! 'verifying)
  (define r (gsm-transition! 'exploring))
  (check-eq? (gsm-current) 'verifying)
  (check-false (ok? r)))

;; ============================================================
;; Transition result inspection
;; ============================================================

(test-case "failed transition result has reason"
  (reset-gsm!)
  (define r (gsm-transition! 'executing))
  (check-false (ok? r))
  (check-true (err? r))
  (check-not-false (err-reason r)))

(test-case "successful transition result has from and to"
  (reset-gsm!)
  (define r (gsm-transition! 'exploring))
  (check-true (ok? r))
  (check-eq? (ok-from r) 'idle)
  (check-eq? (ok-to r) 'exploring))

;; ============================================================
;; Valid transitions query
;; ============================================================

(test-case "valid-next-states returns correct set for idle"
  (reset-gsm!)
  (check-equal? (gsm-valid-next-states) '(exploring)))

(test-case "valid-next-states returns correct set for exploring"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (check-equal? (sort (gsm-valid-next-states) symbol<?) '(idle plan-written)))

(test-case "valid-next-states returns correct set for plan-written"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (check-equal? (sort (gsm-valid-next-states) symbol<?) '(executing exploring)))

(test-case "valid-next-states returns correct set for executing"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (check-equal? (gsm-valid-next-states) '(verifying exploring)))

(test-case "valid-next-states returns correct set for verifying"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (gsm-transition! 'verifying)
  (check-equal? (gsm-valid-next-states) '(idle executing)))

;; ============================================================
;; Snapshot
;; ============================================================

(test-case "gsm-snapshot captures current state and history"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (define snap (gsm-snapshot))
  (check-eq? (hash-ref snap 'current) 'plan-written)
  (check-true (list? (hash-ref snap 'history))))

;; ============================================================
;; State predicates
;; ============================================================

(test-case "gsm-state? recognizes valid states"
  (check-true (gsm-state? 'idle))
  (check-true (gsm-state? 'exploring))
  (check-true (gsm-state? 'plan-written))
  (check-true (gsm-state? 'executing))
  (check-true (gsm-state? 'verifying)))

(test-case "gsm-state? rejects invalid states"
  (check-false (gsm-state? 'unknown))
  (check-false (gsm-state? 'planning))
  (check-false (gsm-state? #f)))

;; ============================================================
;; Allowed tools per state
;; ============================================================

(test-case "idle: all tools allowed"
  (reset-gsm!)
  (for ([tool '("read" "edit" "write" "bash" "planning-read" "planning-write")])
    (check-true (gsm-tool-allowed? tool)
                (format "~a should be allowed in idle" tool))))

(test-case "exploring: all tools allowed (free exploration)"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (for ([tool '("read" "edit" "write" "bash" "planning-read" "planning-write")])
    (check-true (gsm-tool-allowed? tool)
                (format "~a should be allowed in exploring" tool))))

(test-case "plan-written: edit/write/bash blocked, read allowed"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (check-true (gsm-tool-allowed? "read"))
  (check-true (gsm-tool-allowed? "planning-read"))
  (check-false (gsm-tool-allowed? "edit"))
  (check-false (gsm-tool-allowed? "write"))
  (check-false (gsm-tool-allowed? "bash")))

(test-case "executing: planning-write blocked, write/edit allowed"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (check-false (gsm-tool-allowed? "planning-write"))
  (check-true (gsm-tool-allowed? "read"))
  (check-true (gsm-tool-allowed? "edit"))
  (check-true (gsm-tool-allowed? "write"))
  (check-true (gsm-tool-allowed? "bash")))

(test-case "verifying: edit/write/bash blocked (read-only)"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (gsm-transition! 'verifying)
  (check-true (gsm-tool-allowed? "read"))
  (check-true (gsm-tool-allowed? "planning-read"))
  (check-false (gsm-tool-allowed? "edit"))
  (check-false (gsm-tool-allowed? "write"))
  (check-false (gsm-tool-allowed? "bash")))

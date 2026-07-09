#lang racket/base

;; @speed fast
;; @suite fast

;; W5 v0.99.35: Tests for transition-logic.rkt
;; Pure GSD state machine transition functions extracted from state-machine.rkt.
;; Tests cover: result types, state predicates, transition table,
;; BFS path finding, compute-next-gsm-state, invariants, wave computation.

(require rackunit
         rackunit/text-ui
         racket/set
         "../extensions/gsd/transition-logic.rkt"
         "../extensions/gsd/runtime-state-types.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-state mode
                    #:executor [exec #f]
                    #:total-waves [tw 0]
                    #:current-wave [cw 0]
                    #:completed-waves [comp (set)])
  (gsd-runtime-state mode tw cw comp exec #f #f 500 '()))

;; ============================================================
;; Result type tests
;; ============================================================

(define-test-suite result-type-tests
                   (test-case "ok-result struct is transparent"
                     (define r (ok-result 'idle 'exploring))
                     (check-equal? r (ok-result 'idle 'exploring)))
                   (test-case "ok? returns #t for ok-result"
                     (check-true (ok? (ok-result 'idle 'exploring))))
                   (test-case "ok? returns #f for err-result"
                     (check-false (ok? (err-result "bad" 'idle 'executing))))
                   (test-case "err? returns #t for err-result"
                     (check-true (err? (err-result "bad" 'idle 'executing))))
                   (test-case "err? returns #f for ok-result"
                     (check-false (err? (ok-result 'idle 'exploring))))
                   (test-case "ok-from/ok-to extractors"
                     (define r (ok-result 'idle 'exploring))
                     (check-equal? (ok-from r) 'idle)
                     (check-equal? (ok-to r) 'exploring))
                   (test-case "err-reason extractor"
                     (define r (err-result "bad transition" 'idle 'executing))
                     (check-equal? (err-reason r) "bad transition")))

;; ============================================================
;; State predicate tests
;; ============================================================

(define-test-suite state-predicate-tests
                   (test-case "gsm-state? recognizes all valid states"
                     (for ([s GSD-STATES])
                       (check-true (gsm-state? s) (format "~a should be valid" s))))
                   (test-case "gsm-state? rejects invalid states"
                     (check-false (gsm-state? 'unknown))
                     (check-false (gsm-state? 'idle2))
                     (check-false (gsm-state? 42))
                     (check-false (gsm-state? "idle")))
                   (test-case "GSD-STATES has exactly 5 states"
                     (check-equal? (length GSD-STATES) 5)))

;; ============================================================
;; Transition table tests
;; ============================================================

(define-test-suite transition-table-tests
                   (test-case "TRANSITIONS has 9 transitions"
                     (check-equal? (length TRANSITIONS) 9))
                   (test-case "TRANSITIONS-FLAT derived from enriched table"
                     (check-equal? (length TRANSITIONS-FLAT) (length TRANSITIONS))
                     (for ([t TRANSITIONS])
                       (define flat (cons (caar t) (cdr t)))
                       (check-true (for/or ([f TRANSITIONS-FLAT])
                                     (and (equal? (car f) (car flat)) (equal? (cdr f) (cdr flat))))
                                   (format "~a in TRANSITIONS-FLAT" t)))))

;; ============================================================
;; valid-transition? tests
;; ============================================================

(define-test-suite valid-transition-tests
                   (test-case "idle → exploring is valid"
                     (check-true (valid-transition? 'idle 'exploring)))
                   (test-case "exploring → plan-written is valid"
                     (check-true (valid-transition? 'exploring 'plan-written)))
                   (test-case "idle → executing is invalid"
                     (check-false (valid-transition? 'idle 'executing)))
                   (test-case "self-loop idle → idle is valid"
                     (check-true (valid-transition? 'idle 'idle)))
                   (test-case "self-loop exploring → exploring is invalid"
                     (check-false (valid-transition? 'exploring 'exploring)))
                   (test-case "valid transition with correct event"
                     (check-true (valid-transition? 'idle 'exploring 'explore)))
                   (test-case "valid transition with wrong event"
                     (check-false (valid-transition? 'idle 'exploring 'cancel))))

;; ============================================================
;; valid-targets tests
;; ============================================================

(define-test-suite valid-targets-tests
                   (test-case "valid targets from idle"
                     (check-equal? (valid-targets 'idle) '(exploring)))
                   (test-case "valid targets from exploring"
                     (check-equal? (valid-targets 'exploring) '(plan-written idle)))
                   (test-case "valid targets from plan-written"
                     (check-equal? (valid-targets 'plan-written) '(executing idle)))
                   (test-case "valid targets from executing"
                     (check-equal? (valid-targets 'executing) '(verifying idle)))
                   (test-case "valid targets from verifying"
                     (check-equal? (valid-targets 'verifying) '(idle executing))))

;; ============================================================
;; find-transition-path tests
;; ============================================================

(define-test-suite path-finding-tests
                   (test-case "direct path idle → exploring"
                     (define path (find-transition-path 'idle 'exploring))
                     (check-equal? path '(exploring)))
                   (test-case "multi-step path idle → executing"
                     (define path (find-transition-path 'idle 'executing))
                     (check-true (and path (pair? path) (eq? (car (reverse path)) 'executing))))
                   (test-case "no path exploring → plan-written (direct exists)"
                     ;; exploring → plan-written is a direct transition
                     (define path (find-transition-path 'exploring 'plan-written))
                     (check-equal? path '(plan-written)))
                   (test-case "path plan-written → exploring via idle"
                     ;; plan-written → idle → exploring
                     (define path (find-transition-path 'plan-written 'exploring))
                     (check-true (and path (member 'exploring path) #t)))
                   (test-case "path to self returns explicit empty path"
                     ;; Same-state path is a successful zero-hop path and must remain
                     ;; distinguishable from #f, which means no path exists.
                     (define path (find-transition-path 'verifying 'verifying))
                     (check-equal? path '())))

;; ============================================================
;; compute-next-gsm-state tests
;; ============================================================

(define-test-suite
 compute-next-tests
 (test-case "idle → exploring: valid transition"
   (define-values (r s) (compute-next-gsm-state (make-state 'idle) 'exploring))
   (check-true (ok? r))
   (check-equal? (ok-to r) 'exploring)
   (check-equal? (gsd-runtime-state-mode s) 'exploring))
 (test-case "executing → idle: executor cleared"
   (define-values (r s)
     (compute-next-gsm-state (make-state 'executing #:executor (lambda () 'work)) 'idle))
   (check-true (ok? r))
   (check-equal? (gsd-runtime-state-mode s) 'idle)
   (check-false (gsd-runtime-state-wave-executor s)))
 (test-case "executing → verifying: executor cleared"
   (define-values (r s)
     (compute-next-gsm-state (make-state 'executing #:executor (lambda () 'work)) 'verifying))
   (check-true (ok? r))
   (check-false (gsd-runtime-state-wave-executor s)))
 (test-case "idle → executing: invalid transition"
   (define-values (r s) (compute-next-gsm-state (make-state 'idle) 'executing))
   (check-true (err? r))
   (check-equal? (gsd-runtime-state-mode s) 'idle))
 (test-case "invalid target state returns err"
   (define-values (r _) (compute-next-gsm-state (make-state 'idle) 'nonexistent))
   (check-true (err? r))
   (check-equal? (err-reason r) "invalid state: nonexistent"))
 (test-case "transition with correct event succeeds"
   (define-values (r _) (compute-next-gsm-state (make-state 'idle) 'exploring #:event 'explore))
   (check-true (ok? r)))
 (test-case "transition with wrong event fails"
   (define-values (r _) (compute-next-gsm-state (make-state 'idle) 'exploring #:event 'cancel))
   (check-true (err? r)))
 (test-case "err-result preserves attempted target"
   (define-values (r _) (compute-next-gsm-state (make-state 'idle) 'executing))
   (check-true (err? r))
   (check-equal? (err-result-attempted r) 'executing)))

;; ============================================================
;; check-state-invariants tests
;; ============================================================

(define-test-suite
 invariant-tests
 (test-case "valid state passes invariants"
   (define-values (ok? msg) (check-state-invariants (make-state 'idle)))
   (check-true ok?)
   (check-false msg))
 (test-case "invalid mode fails invariants"
   (define-values (ok? msg)
     (check-state-invariants (gsd-runtime-state 'bad-mode 0 0 (set) #f #f #f 500 '())))
   (check-false ok?)
   (check-true (and (string? msg) #t)))
 (test-case "current-wave > total-waves fails"
   (define-values (ok? msg)
     (check-state-invariants (make-state 'idle #:total-waves 3 #:current-wave 5)))
   (check-false ok?))
 (test-case "executing without executor fails when waves exist"
   (define-values (ok? msg)
     (check-state-invariants (make-state 'executing #:total-waves 3 #:executor #f)))
   (check-false ok?)))

;; ============================================================
;; compute-next-pending-wave tests
;; ============================================================

(define-test-suite pending-wave-tests
                   (test-case "all pending when none completed"
                     (check-equal? (compute-next-pending-wave 5 (set)) 0))
                   (test-case "returns first non-completed index"
                     (check-equal? (compute-next-pending-wave 5 (set 0 1 2)) 3))
                   (test-case "returns #f when all completed"
                     (check-false (compute-next-pending-wave 3 (set 0 1 2))))
                   (test-case "returns #f for zero waves"
                     (check-false (compute-next-pending-wave 0 (set))))
                   (test-case "handles non-contiguous completed set"
                     (check-equal? (compute-next-pending-wave 5 (set 0 2 4)) 1)))

;; ============================================================
;; All tests
;; ============================================================

(define-test-suite all-transition-logic-tests
                   result-type-tests
                   state-predicate-tests
                   transition-table-tests
                   valid-transition-tests
                   valid-targets-tests
                   path-finding-tests
                   compute-next-tests
                   invariant-tests
                   pending-wave-tests)

(module+ test
  (run-tests all-transition-logic-tests))

(module+ main
  (run-tests all-transition-logic-tests))

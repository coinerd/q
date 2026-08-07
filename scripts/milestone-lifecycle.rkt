#lang racket

;; scripts/milestone-lifecycle.rkt — Milestone lifecycle state machine (W5).
;;
;; Extracted from scripts/milestone-gate.rkt / scripts/gsd-gates/gate-claims.rkt
;; (v0.99.42+). Explicit transition system per Manual §40 (State Machines).
;;
;; Linear chain (backward compatible, unchanged since W6):
;;   planned → in_progress → release_ready → release_published → ci_green → closed
;;
;; W5 branching transitions (require an explicit reason string):
;;   cancel    : in_progress | release_ready → cancelled    (abandoned milestone)
;;   rollback  : in_progress | release_ready → planned      (restart the cycle)
;;   reopen    : closed                                    → planned (needs more work)
;;   advance   : any state in the linear chain             → next state
;;
;; Backward-compat contract:
;;   - milestone-lifecycle-states stays the strict 6-state linear chain
;;     (milestone-lifecycle-next and the 2-arg milestone-valid-transition?
;;      depend on it, and tests pin its length).
;;   - The W5 branching transitions are expressed as transition *kinds*
;;     (advance/cancel/rollback/reopen) via valid-transition?, not as
;;     new from→to pairs on the 2-arg predicate.
;;
;; Pure module: no I/O. apply-transition! is functional — it returns a NEW
;; milestone-lifecycle-state with the transition appended to history.

(require racket/list
         racket/match)

;; ---------------------------------------------------------------------------
;; States
;; ---------------------------------------------------------------------------

;; Linear chain — DO NOT add branching states here: milestone-lifecycle-next
;; and milestone-valid-transition? (2-arg) rely on this being the strict
;; forward sequence, and existing tests pin its length.
(define milestone-lifecycle-states
  '(planned in_progress release_ready release_published ci_green closed))

;; All reachable states, including the W5 terminal `cancelled`.
(define milestone-lifecycle-states-all
  '(planned in_progress release_ready release_published ci_green closed cancelled))

;; Transition kinds understood by the state machine.
(define milestone-transitions
  '(advance cancel rollback reopen))

;; ---------------------------------------------------------------------------
;; Records
;; ---------------------------------------------------------------------------

(struct milestone-lifecycle-transition-result (from to ok? reason) #:transparent)

;; state:   current milestone lifecycle state
;; history: (listof milestone-lifecycle-transition-result), most recent first
(struct milestone-lifecycle-state (state history) #:transparent)

;; ---------------------------------------------------------------------------
;; Linear chain (W6, backward compatible)
;; ---------------------------------------------------------------------------

(define (milestone-lifecycle-next state)
  (define idx (index-of milestone-lifecycle-states state))
  (and idx
       (< (add1 idx) (length milestone-lifecycle-states))
       (list-ref milestone-lifecycle-states (add1 idx))))

;; 2-arg backward-compatible predicate: linear advance only.
(define (milestone-valid-transition? from-state to-state)
  (equal? (milestone-lifecycle-next from-state) to-state))

;; ---------------------------------------------------------------------------
;; W5 branching state machine
;; ---------------------------------------------------------------------------

;; target-state: state × transition → (or/c state #f)
;; Destination state implied by a transition from the given state, or #f
;; when the transition kind does not apply to that state.
(define (target-state from-state transition)
  (case transition
    [(advance) (milestone-lifecycle-next from-state)]
    [(cancel) (and (member from-state '(in_progress release_ready)) 'cancelled)]
    [(rollback) (and (member from-state '(in_progress release_ready)) 'planned)]
    [(reopen) (and (equal? from-state 'closed) 'planned)]
    [else #f]))

;; valid-transition?: state × transition × state → boolean
;; Branching-aware: allows cancel/rollback/reopen in addition to linear advance.
(define (valid-transition? from-state transition to-state)
  (define tgt (target-state from-state transition))
  (and tgt (equal? tgt to-state)))

;; allowed-transitions-from: state → (listof symbol)
;; Which transition kinds are legal from a given state (for error messages).
(define (allowed-transitions-from state)
  (for/list ([t (in-list milestone-transitions)]
             #:when (target-state state t))
    t))

;; transition-guard: state × transition × state → milestone-lifecycle-transition-result
;; Guard function that returns a descriptive error message on failure
;; (W5 risk control: "Guard functions return descriptive error messages").
(define (transition-guard from-state transition to-state)
  (cond
    [(valid-transition? from-state transition to-state)
     (milestone-lifecycle-transition-result from-state to-state #t "transition allowed")]
    [else
     (milestone-lifecycle-transition-result
      from-state
      to-state
      #f
      (format "invalid transition ~a: ~a → ~a; allowed from ~a: ~a"
              transition
              from-state
              to-state
              from-state
              (allowed-transitions-from from-state)))]))

;; apply-transition!: milestone-lifecycle-state × transition × reason
;;                   → milestone-lifecycle-state
;; Validates the transition against the state's current state, then returns a
;; NEW milestone-lifecycle-state with the transition recorded in history.
;; Raises exn:fail with a descriptive message on invalid transitions.
;; Every transition requires an explicit reason string (W5: no silent rollbacks).
(define (apply-transition! st transition reason)
  (define from-state (milestone-lifecycle-state-state st))
  (define to-state (target-state from-state transition))
  (cond
    [(not to-state)
     (error 'apply-transition!
            "invalid transition ~a from ~a (allowed: ~a); reason given: ~a"
            transition
            from-state
            (allowed-transitions-from from-state)
            reason)]
    [else
     (define guard (transition-guard from-state transition to-state))
     (if (milestone-lifecycle-transition-result-ok? guard)
         (milestone-lifecycle-state
          to-state
          (cons (milestone-lifecycle-transition-result from-state to-state #t reason)
                (milestone-lifecycle-state-history st)))
         (error 'apply-transition!
                "~a; reason given: ~a"
                (milestone-lifecycle-transition-result-reason guard)
                reason))]))

;; transition-reason: milestone-lifecycle-state → string
;; Reason string of the most recent recorded transition ("" if none).
(define (transition-reason st)
  (define hist (milestone-lifecycle-state-history st))
  (if (pair? hist)
      (milestone-lifecycle-transition-result-reason (car hist))
      ""))

;; ---------------------------------------------------------------------------
;; Provide
;; ---------------------------------------------------------------------------

(provide milestone-lifecycle-states
         milestone-lifecycle-states-all
         milestone-transitions
         (struct-out milestone-lifecycle-transition-result)
         (struct-out milestone-lifecycle-state)
         milestone-lifecycle-next
         milestone-valid-transition?
         valid-transition?
         target-state
         allowed-transitions-from
         transition-guard
         apply-transition!
         transition-reason
         target-state
         allowed-transitions-from)

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (test-case "linear chain: 6 states in order, unchanged"
    (check-equal? milestone-lifecycle-states
                  '(planned in_progress release_ready release_published ci_green closed))
    (check-equal? (length milestone-lifecycle-states) 6))

  (test-case "states-all includes cancelled terminal"
    (check-equal? (member 'cancelled milestone-lifecycle-states-all) '(cancelled))
    (check-equal? (length milestone-lifecycle-states-all) 7))

  (test-case "milestone-lifecycle-next: forward chain, stops at closed"
    (check-equal? (milestone-lifecycle-next 'planned) 'in_progress)
    (check-equal? (milestone-lifecycle-next 'in_progress) 'release_ready)
    (check-equal? (milestone-lifecycle-next 'release_ready) 'release_published)
    (check-equal? (milestone-lifecycle-next 'release_published) 'ci_green)
    (check-equal? (milestone-lifecycle-next 'ci_green) 'closed)
    (check-false (milestone-lifecycle-next 'closed)))

  (test-case "milestone-valid-transition? (2-arg) stays linear-only"
    (check-true (milestone-valid-transition? 'planned 'in_progress))
    (check-false (milestone-valid-transition? 'closed 'planned))
    (check-false (milestone-valid-transition? 'release_published 'release_ready)))

  (test-case "valid-transition? (3-arg): advance"
    (check-true (valid-transition? 'planned 'advance 'in_progress))
    (check-false (valid-transition? 'closed 'advance 'closed))
    (check-false (valid-transition? 'planned 'advance 'release_ready)))

  (test-case "valid-transition? (3-arg): cancel"
    (check-true (valid-transition? 'in_progress 'cancel 'cancelled))
    (check-true (valid-transition? 'release_ready 'cancel 'cancelled))
    (check-false (valid-transition? 'planned 'cancel 'cancelled))
    (check-false (valid-transition? 'closed 'cancel 'cancelled)))

  (test-case "valid-transition? (3-arg): rollback"
    (check-true (valid-transition? 'in_progress 'rollback 'planned))
    (check-true (valid-transition? 'release_ready 'rollback 'planned))
    (check-false (valid-transition? 'planned 'rollback 'planned))
    (check-false (valid-transition? 'ci_green 'rollback 'planned)))

  (test-case "valid-transition? (3-arg): reopen"
    (check-true (valid-transition? 'closed 'reopen 'planned))
    (check-false (valid-transition? 'in_progress 'reopen 'planned))
    (check-false (valid-transition? 'closed 'reopen 'in_progress)))

  (test-case "target-state: #f when transition kind not applicable"
    (check-false (target-state 'planned 'cancel))
    (check-false (target-state 'planned 'rollback))
    (check-false (target-state 'in_progress 'reopen))
    (check-equal? (target-state 'closed 'reopen) 'planned)
    (check-equal? (target-state 'release_ready 'cancel) 'cancelled))

  (test-case "allowed-transitions-from: descriptive guards"
    (check-equal? (allowed-transitions-from 'planned) '(advance))
    (check-equal? (allowed-transitions-from 'in_progress)
                  '(advance cancel rollback))
    (check-equal? (allowed-transitions-from 'closed) '(reopen)))

  (test-case "transition-guard: descriptive failure message"
    (define r (transition-guard 'planned 'cancel 'cancelled))
    (check-false (milestone-lifecycle-transition-result-ok? r))
    (check-true (string-contains? (milestone-lifecycle-transition-result-reason r)
                                  "invalid transition"))
    (check-true (string-contains? (milestone-lifecycle-transition-result-reason r)
                                  "cancel"))
    (check-true (string-contains? (milestone-lifecycle-transition-result-reason r)
                                  "advance")))

  (test-case "apply-transition!: advance records history and reason"
    (define st0 (milestone-lifecycle-state 'planned '()))
    (define st1 (apply-transition! st0 'advance "gate check passed"))
    (check-equal? (milestone-lifecycle-state-state st1) 'in_progress)
    (check-equal? (length (milestone-lifecycle-state-history st1)) 1)
    (check-equal? (transition-reason st1) "gate check passed")
    (check-equal? (milestone-lifecycle-transition-result-from
                   (car (milestone-lifecycle-state-history st1)))
                  'planned)
    (check-equal? (milestone-lifecycle-transition-result-to
                   (car (milestone-lifecycle-state-history st1)))
                  'in_progress)
    (check-true (milestone-lifecycle-transition-result-ok?
                 (car (milestone-lifecycle-state-history st1))))
    ;; st0 is unchanged (functional)
    (check-equal? (milestone-lifecycle-state-state st0) 'planned)
    (check-equal? (transition-reason st0) ""))

  (test-case "apply-transition!: rollback restarts the cycle"
    (define st (milestone-lifecycle-state 'release_ready '()))
    (define st1 (apply-transition! st 'rollback "release assets corrupted"))
    (check-equal? (milestone-lifecycle-state-state st1) 'planned)
    (check-equal? (transition-reason st1) "release assets corrupted"))

  (test-case "apply-transition!: cancel reaches terminal cancelled"
    (define st (milestone-lifecycle-state 'in_progress '()))
    (define st1 (apply-transition! st 'cancel "scope abandoned"))
    (check-equal? (milestone-lifecycle-state-state st1) 'cancelled)
    (check-false (target-state 'cancelled 'advance)))

  (test-case "apply-transition!: reopen a closed milestone"
    (define st (milestone-lifecycle-state 'closed '()))
    (define st1 (apply-transition! st 'reopen "follow-up work required"))
    (check-equal? (milestone-lifecycle-state-state st1) 'planned)
    (check-equal? (transition-reason st1) "follow-up work required"))

  (test-case "apply-transition!: history accumulates across transitions"
    (define st (milestone-lifecycle-state 'planned '()))
    (define st1 (apply-transition! st 'advance "gate check passed"))
    (define st2 (apply-transition! st1 'rollback "bad release assets"))
    (check-equal? (milestone-lifecycle-state-state st2) 'planned)
    (check-equal? (length (milestone-lifecycle-state-history st2)) 2)
    (check-equal? (milestone-lifecycle-transition-result-from
                   (cadr (milestone-lifecycle-state-history st2)))
                  'planned)
    (check-equal? (milestone-lifecycle-transition-result-to
                   (cadr (milestone-lifecycle-state-history st2)))
                  'in_progress))


  (test-case "apply-transition!: invalid transition raises descriptive error"
    (define st (milestone-lifecycle-state 'planned '()))
    (check-exn (regexp "invalid transition")
               (lambda ()
                 (apply-transition! st 'cancel "not cancellable from planned")))
    (check-exn (regexp "invalid transition")
               (lambda ()
                 (apply-transition! st 'rollback "not rollbackable from planned")))
    (check-exn (regexp "invalid transition")
               (lambda ()
                 (apply-transition! (milestone-lifecycle-state 'closed '())
                                    'advance "at end of chain")))
    (check-exn (regexp "invalid transition")
               (lambda ()
                 (apply-transition! st 'sideways "no such transition")))
    (check-exn (regexp "reason given")
               (lambda ()
                 (apply-transition! st 'rollback "not rollbackable from planned"))))

  (test-case "apply-transition!: reopen requires from closed"
    (define st (milestone-lifecycle-state 'in_progress '()))
    (check-exn exn:fail?
               (lambda ()
                 (apply-transition! st 'reopen "not reopenable from in_progress")))))

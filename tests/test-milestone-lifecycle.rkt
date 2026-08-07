#lang racket

;; @suite ci
;; @speed fast
;; tests/test-milestone-lifecycle.rkt
;; W5 (#8545): Tests for scripts/milestone-lifecycle.rkt — the branching
;; milestone lifecycle state machine (cancel / rollback / reopen).
;;
;; Covers the W5 deliverables:
;;   - explicit state machine (milestone-lifecycle-state, history)
;;   - branching transitions: cancelled, rolled_back (→ planned), reopened (→ planned)
;;   - guard functions with descriptive error messages
;;   - all transitions require an explicit reason
;;   - backward-compatible linear chain (planned → … → closed)

(require rackunit
         racket/string
         "../scripts/milestone-lifecycle.rkt")

;; ---------------------------------------------------------------------------
;; States: strict linear chain (backward compat) + cancelled terminal
;; ---------------------------------------------------------------------------

(test-case "states: strict 6-state linear chain unchanged, cancelled terminal added"
  (check-equal? milestone-lifecycle-states
                '(planned in_progress release_ready release_published ci_green closed))
  (check-equal? (length milestone-lifecycle-states) 6)
  (check-equal? (member 'cancelled milestone-lifecycle-states-all) '(cancelled)))

(test-case "linear chain: forward advance, stops at closed (backward compat)"
  (check-equal? (milestone-lifecycle-next 'planned) 'in_progress)
  (check-equal? (milestone-lifecycle-next 'in_progress) 'release_ready)
  (check-equal? (milestone-lifecycle-next 'release_ready) 'release_published)
  (check-equal? (milestone-lifecycle-next 'release_published) 'ci_green)
  (check-equal? (milestone-lifecycle-next 'ci_green) 'closed)
  (check-false (milestone-lifecycle-next 'closed))
  (check-true (milestone-valid-transition? 'planned 'in_progress))
  (check-false (milestone-valid-transition? 'closed 'planned)))

;; ---------------------------------------------------------------------------
;; Branching transitions (3-arg valid-transition?)
;; ---------------------------------------------------------------------------

(test-case "cancel: in_progress | release_ready → cancelled"
  (check-true (valid-transition? 'in_progress 'cancel 'cancelled))
  (check-true (valid-transition? 'release_ready 'cancel 'cancelled))
  (check-false (valid-transition? 'planned 'cancel 'cancelled))
  (check-false (valid-transition? 'closed 'cancel 'cancelled)))

(test-case "rollback: in_progress | release_ready → planned (restart cycle)"
  (check-true (valid-transition? 'in_progress 'rollback 'planned))
  (check-true (valid-transition? 'release_ready 'rollback 'planned))
  (check-false (valid-transition? 'planned 'rollback 'planned))
  (check-false (valid-transition? 'ci_green 'rollback 'planned)))

(test-case "reopen: closed → planned (needs more work)"
  (check-true (valid-transition? 'closed 'reopen 'planned))
  (check-false (valid-transition? 'in_progress 'reopen 'planned))
  (check-false (valid-transition? 'closed 'reopen 'in_progress)))

(test-case "target-state + allowed-transitions-from"
  (check-equal? (target-state 'release_ready 'cancel) 'cancelled)
  (check-equal? (target-state 'closed 'reopen) 'planned)
  (check-equal? (target-state 'in_progress 'rollback) 'planned)
  (check-false (target-state 'planned 'cancel))
  (check-false (target-state 'cancelled 'advance))
  (check-equal? (allowed-transitions-from 'planned) '(advance))
  (check-equal? (allowed-transitions-from 'in_progress) '(advance cancel rollback))
  (check-equal? (allowed-transitions-from 'release_ready) '(advance cancel rollback))
  (check-equal? (allowed-transitions-from 'closed) '(reopen)))

;; ---------------------------------------------------------------------------
;; Guards: descriptive error messages, no silent transitions
;; ---------------------------------------------------------------------------

(test-case "transition-guard: ok on allowed transition, descriptive error otherwise"
  (define ok (transition-guard 'release_ready 'rollback 'planned))
  (check-true (milestone-lifecycle-transition-result-ok? ok))
  (define bad (transition-guard 'planned 'cancel 'cancelled))
  (check-false (milestone-lifecycle-transition-result-ok? bad))
  (check-true (string-contains? (milestone-lifecycle-transition-result-reason bad)
                                "invalid transition"))
  (check-true (string-contains? (milestone-lifecycle-transition-result-reason bad)
                                "cancel"))
  ;; Descriptive: mentions what IS allowed from the state.
  (check-true (string-contains? (milestone-lifecycle-transition-result-reason bad)
                                "advance")))

(test-case "apply-transition!: functional, records history + explicit reason"
  (define st0 (milestone-lifecycle-state 'release_ready '()))
  (define st1 (apply-transition! st0 'rollback "release assets corrupted"))
  (check-equal? (milestone-lifecycle-state-state st1) 'planned)
  (check-equal? (transition-reason st1) "release assets corrupted")
  (check-equal? (length (milestone-lifecycle-state-history st1)) 1)
  (check-equal? (milestone-lifecycle-transition-result-from
                 (car (milestone-lifecycle-state-history st1)))
                'release_ready)
  (check-equal? (milestone-lifecycle-transition-result-to
                 (car (milestone-lifecycle-state-history st1)))
                'planned)
  (check-true (milestone-lifecycle-transition-result-ok?
               (car (milestone-lifecycle-state-history st1))))
  ;; Functional: original state unchanged, no reason recorded.
  (check-equal? (milestone-lifecycle-state-state st0) 'release_ready)
  (check-equal? (transition-reason st0) ""))

(test-case "apply-transition!: cancel and reopen reach expected states"
  (define cancelled
    (apply-transition! (milestone-lifecycle-state 'in_progress '())
                       'cancel
                       "scope abandoned"))
  (check-equal? (milestone-lifecycle-state-state cancelled) 'cancelled)
  (check-equal? (transition-reason cancelled) "scope abandoned")
  (define reopened
    (apply-transition! (milestone-lifecycle-state 'closed '())
                       'reopen
                       "follow-up work required"))
  (check-equal? (milestone-lifecycle-state-state reopened) 'planned)
  (check-equal? (transition-reason reopened) "follow-up work required"))

(test-case "apply-transition!: history accumulates most-recent-first"
  (define st (milestone-lifecycle-state 'planned '()))
  (define st1 (apply-transition! st 'advance "gate check passed"))
  (define st2 (apply-transition! st1 'rollback "bad release assets"))
  (check-equal? (milestone-lifecycle-state-state st2) 'planned)
  (check-equal? (length (milestone-lifecycle-state-history st2)) 2)
  ;; Most recent first: rollback in_progress → planned, then advance planned → in_progress.
  (check-equal? (milestone-lifecycle-transition-result-from
                 (car (milestone-lifecycle-state-history st2)))
                'in_progress)
  (check-equal? (milestone-lifecycle-transition-result-to
                 (car (milestone-lifecycle-state-history st2)))
                'planned)
  (check-equal? (milestone-lifecycle-transition-result-from
                 (cadr (milestone-lifecycle-state-history st2)))
                'planned)
  (check-equal? (milestone-lifecycle-transition-result-to
                 (cadr (milestone-lifecycle-state-history st2)))
                'in_progress)
  (check-equal? (milestone-lifecycle-transition-result-reason
                 (car (milestone-lifecycle-state-history st2)))
                "bad release assets")
  (check-equal? (milestone-lifecycle-transition-result-reason
                 (cadr (milestone-lifecycle-state-history st2)))
                "gate check passed"))

(test-case "apply-transition!: invalid transitions raise descriptive errors"
  (check-exn (regexp "invalid transition")
             (lambda ()
               (apply-transition! (milestone-lifecycle-state 'planned '())
                                  'cancel
                                  "not cancellable from planned")))
  (check-exn (regexp "invalid transition")
             (lambda ()
               (apply-transition! (milestone-lifecycle-state 'planned '())
                                  'rollback
                                  "not rollbackable from planned")))
  (check-exn (regexp "invalid transition")
             (lambda ()
               (apply-transition! (milestone-lifecycle-state 'closed '())
                                  'advance
                                  "at end of chain")))
  (check-exn (regexp "invalid transition")
             (lambda ()
               (apply-transition! (milestone-lifecycle-state 'in_progress '())
                                  'reopen
                                  "not reopenable from in_progress")))
  (check-exn (regexp "reason given")
             (lambda ()
               (apply-transition! (milestone-lifecycle-state 'planned '())
                                  'rollback
                                  "not rollbackable from planned")))
  (check-exn exn:fail?
             (lambda ()
               (apply-transition! (milestone-lifecycle-state 'planned '())
                                  'sideways
                                  "no such transition"))))

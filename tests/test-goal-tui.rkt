#lang racket/base

;; tests/test-goal-tui.rkt — TUI goal event reducer + display tests

(require rackunit
         racket/string
         "../tui/state-types.rkt"
         "../tui/state-events.rkt"
         "../util/protocol-types.rkt")

;; ============================================================
;; goal-display-info struct
;; ============================================================

(let ()
  (define gdi (goal-display-info "tests pass" 3 8 'active))
  (check-equal? (goal-display-info-goal-text gdi) "tests pass")
  (check-equal? (goal-display-info-turns-used gdi) 3)
  (check-equal? (goal-display-info-max-turns gdi) 8)
  (check-equal? (goal-display-info-status gdi) 'active))

;; ============================================================
;; initial-ui-state has no active goal
;; ============================================================

(let ()
  (define st (initial-ui-state))
  (check-false (ui-state-active-goal st)))

;; ============================================================
;; goal.started sets active-goal
;; ============================================================

(let ()
  (define st (initial-ui-state))
  (define evt (make-event "goal.started" 0 "s1" "t1"
                     (hasheq 'goal-text "make all tests pass"
                             'max-turns 8
                             'checks '())))
  (define st2 (apply-event-to-state st evt))
  (define goal (ui-state-active-goal st2))
  (check-not-false goal "active-goal set after goal.started")
  (check-equal? (goal-display-info-goal-text goal) "make all tests pass")
  (check-equal? (goal-display-info-turns-used goal) 0)
  (check-equal? (goal-display-info-max-turns goal) 8)
  (check-equal? (goal-display-info-status goal) 'active))

;; ============================================================
;; goal.turn.started increments turns
;; ============================================================

(let ()
  (define st (initial-ui-state))
  ;; First start a goal
  (define evt1 (make-event "goal.started" 0 "s1" "t1"
                      (hasheq 'goal-text "goal" 'max-turns 8 'checks '())))
  (define st1 (apply-event-to-state st evt1))
  ;; Then turn started
  (define evt2 (make-event "goal.turn.started" 0 "s1" "t2"
                      (hasheq 'turn-number 1 'goal-text "goal")))
  (define st2 (apply-event-to-state st1 evt2))
  (check-equal? (goal-display-info-turns-used (ui-state-active-goal st2)) 1))

;; ============================================================
;; goal.achieved sets status
;; ============================================================

(let ()
  (define st (initial-ui-state))
  (define evt1 (make-event "goal.started" 0 "s1" "t1"
                      (hasheq 'goal-text "goal" 'max-turns 8 'checks '())))
  (define st1 (apply-event-to-state st evt1))
  (define evt2 (make-event "goal.achieved" 0 "s1" "t2"
                      (hasheq 'goal-text "goal" 'turns-used 5 'total-token-cost 0)))
  (define st2 (apply-event-to-state st1 evt2))
  (check-equal? (goal-display-info-status (ui-state-active-goal st2)) 'achieved)
  (check-equal? (goal-display-info-turns-used (ui-state-active-goal st2)) 5))

;; ============================================================
;; goal.failed sets status
;; ============================================================

(let ()
  (define st (initial-ui-state))
  (define evt1 (make-event "goal.started" 0 "s1" "t1"
                      (hasheq 'goal-text "goal" 'max-turns 8 'checks '())))
  (define st1 (apply-event-to-state st evt1))
  (define evt2 (make-event "goal.failed" 0 "s1" "t2"
                      (hasheq 'goal-text "goal" 'reason "max turns" 'turns-used 8)))
  (define st2 (apply-event-to-state st1 evt2))
  (check-equal? (goal-display-info-status (ui-state-active-goal st2)) 'failed))

;; ============================================================
;; goal.check.completed is no-op
;; ============================================================

(let ()
  (define st (initial-ui-state))
  (define evt1 (make-event "goal.started" 0 "s1" "t1"
                      (hasheq 'goal-text "goal" 'max-turns 8 'checks '())))
  (define st1 (apply-event-to-state st evt1))
  (define evt2 (make-event "goal.check.completed" 0 "s1" "t2"
                      (hasheq 'label "test" 'exit-code 0 'timed-out? #f
                              'stdout "" 'stderr "")))
  (define st2 (apply-event-to-state st1 evt2))
  ;; State should be unchanged (same object)
  (check-eq? st2 st1))

;; ============================================================
;; goal.evaluated keeps active status
;; ============================================================

(let ()
  (define st (initial-ui-state))
  (define evt1 (make-event "goal.started" 0 "s1" "t1"
                      (hasheq 'goal-text "goal" 'max-turns 8 'checks '())))
  (define st1 (apply-event-to-state st evt1))
  (define evt2 (make-event "goal.evaluated" 0 "s1" "t2"
                      (hasheq 'achieved? #f 'reason "not yet"
                              'turn-number 1 'token-cost 10)))
  (define st2 (apply-event-to-state st1 evt2))
  (check-equal? (goal-display-info-status (ui-state-active-goal st2)) 'active))

;; ============================================================
;; Goal text truncated to 40 chars
;; ============================================================

(let ()
  (define long-text (make-string 60 #\x))
  (define st (initial-ui-state))
  (define evt (make-event "goal.started" 0 "s1" "t1"
                     (hasheq 'goal-text long-text 'max-turns 8 'checks '())))
  (define st2 (apply-event-to-state st evt))
  (check-equal? (string-length (goal-display-info-goal-text (ui-state-active-goal st2))) 40))

(displayln "All goal-TUI tests passed.")

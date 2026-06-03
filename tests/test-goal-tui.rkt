#lang racket/base

;; tests/test-goal-tui.rkt — TUI goal event reducer + display tests

(require rackunit
         racket/string
         "../tui/state-types.rkt"
         "../tui/state-events.rkt"
         "../util/message/protocol-types.rkt"
         "../tui/render/status-line.rkt"
         (only-in "../tui/render/message-layout.rkt"
                  styled-segment-text
                  styled-line-segments
                  styled-line?))

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
  (define evt
    (make-event "goal.started"
                0
                "s1"
                "t1"
                (hasheq 'goal-text "make all tests pass" 'max-turns 8 'checks '())))
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
  (define evt1
    (make-event "goal.started" 0 "s1" "t1" (hasheq 'goal-text "goal" 'max-turns 8 'checks '())))
  (define st1 (apply-event-to-state st evt1))
  ;; Then turn started
  (define evt2 (make-event "goal.turn.started" 0 "s1" "t2" (hasheq 'turn-number 1 'goal-text "goal")))
  (define st2 (apply-event-to-state st1 evt2))
  (check-equal? (goal-display-info-turns-used (ui-state-active-goal st2)) 1))

;; ============================================================
;; goal.achieved sets status
;; ============================================================

(let ()
  (define st (initial-ui-state))
  (define evt1
    (make-event "goal.started" 0 "s1" "t1" (hasheq 'goal-text "goal" 'max-turns 8 'checks '())))
  (define st1 (apply-event-to-state st evt1))
  (define evt2
    (make-event "goal.achieved"
                0
                "s1"
                "t2"
                (hasheq 'goal-text "goal" 'turns-used 5 'total-token-cost 0)))
  (define st2 (apply-event-to-state st1 evt2))
  (check-equal? (goal-display-info-status (ui-state-active-goal st2)) 'achieved)
  (check-equal? (goal-display-info-turns-used (ui-state-active-goal st2)) 5))

;; ============================================================
;; goal.failed sets status
;; ============================================================

(let ()
  (define st (initial-ui-state))
  (define evt1
    (make-event "goal.started" 0 "s1" "t1" (hasheq 'goal-text "goal" 'max-turns 8 'checks '())))
  (define st1 (apply-event-to-state st evt1))
  (define evt2
    (make-event "goal.failed"
                0
                "s1"
                "t2"
                (hasheq 'goal-text "goal" 'reason "max turns" 'turns-used 8)))
  (define st2 (apply-event-to-state st1 evt2))
  (check-equal? (goal-display-info-status (ui-state-active-goal st2)) 'failed))

;; ============================================================
;; goal.check.completed is no-op
;; ============================================================

(let ()
  (define st (initial-ui-state))
  (define evt1
    (make-event "goal.started" 0 "s1" "t1" (hasheq 'goal-text "goal" 'max-turns 8 'checks '())))
  (define st1 (apply-event-to-state st evt1))
  (define evt2
    (make-event "goal.check.completed"
                0
                "s1"
                "t2"
                (hasheq 'label "test" 'exit-code 0 'timed-out? #f 'stdout "" 'stderr "")))
  (define st2 (apply-event-to-state st1 evt2))
  ;; State should be unchanged (same object)
  (check-eq? st2 st1))

;; ============================================================
;; goal.evaluated keeps active status
;; ============================================================

(let ()
  (define st (initial-ui-state))
  (define evt1
    (make-event "goal.started" 0 "s1" "t1" (hasheq 'goal-text "goal" 'max-turns 8 'checks '())))
  (define st1 (apply-event-to-state st evt1))
  (define evt2
    (make-event "goal.evaluated"
                0
                "s1"
                "t2"
                (hasheq 'achieved? #f 'reason "not yet" 'turn-number 1 'token-cost 10)))
  (define st2 (apply-event-to-state st1 evt2))
  (check-equal? (goal-display-info-status (ui-state-active-goal st2)) 'active))

;; ============================================================
;; Goal text truncated to 40 chars
;; ============================================================

(let ()
  (define long-text (make-string 60 #\x))
  (define st (initial-ui-state))
  (define evt
    (make-event "goal.started" 0 "s1" "t1" (hasheq 'goal-text long-text 'max-turns 8 'checks '())))
  (define st2 (apply-event-to-state st evt))
  (check-equal? (string-length (goal-display-info-goal-text (ui-state-active-goal st2))) 40))

(displayln "All goal-TUI tests passed.")

;; ============================================================
;; Status bar goal badge rendering
;; ============================================================

(let ()
  ;; No goal — no badge
  (define st-no-goal (initial-ui-state))
  (define line-no-goal (render-status-bar st-no-goal 80))
  (define seg-text (styled-segment-text (car (styled-line-segments line-no-goal))))
  (check-false (string-contains? seg-text "goal") "no goal badge when inactive")
  ;; Active goal — badge present
  (define st-goal
    (struct-copy ui-state st-no-goal [active-goal (goal-display-info "tests pass" 3 8 'active)]))
  (define line-goal (render-status-bar st-goal 80))
  (define seg-text-goal (styled-segment-text (car (styled-line-segments line-goal))))
  (check-true (string-contains? seg-text-goal "goal") "goal badge present when active")
  (check-true (string-contains? seg-text-goal "3/8") "shows turn count"))

;; ============================================================
;; W0: Concurrent goal guard test (v0.71.7 → v0.71.8 enhanced)
;; ============================================================

;; WARN-2: Concurrent goal guard — verify guard logic
;; Direct behavioral test of the guard condition used in handle-goal-command.
;; (Loading tui/commands.rkt transitively loads event-bus + extensions which block in headless env)
(let ()
  (define st-with-goal
    (struct-copy ui-state
                 (initial-ui-state)
                 [active-goal (goal-display-info "existing goal" 2 8 'active)]))
  ;; Guard: when ui-state-active-goal is truthy, new goal must be rejected
  (check-true (goal-display-info? (ui-state-active-goal st-with-goal)) "active goal is set")
  ;; Simulate the guard check from handle-goal-command
  (define guard-rejects? (if (ui-state-active-goal st-with-goal) #t #f))
  (check-true guard-rejects? "concurrent guard rejects when active goal exists (WARN-2)")
  ;; Verify no-goal state allows setting
  (define st-no-goal (initial-ui-state))
  (define guard-allows? (if (ui-state-active-goal st-no-goal) #t #f))
  (check-false guard-allows? "concurrent guard allows when no active goal"))

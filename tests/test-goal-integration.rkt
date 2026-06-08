#lang racket/base

;; @speed slow
;; @suite default

;; tests/test-goal-integration.rkt — Integration tests for goal feature wiring
;; F-3: Verify agent-session-box is populated and goal-cancel-box works

(require rackunit
         racket/match
         "../tui/state.rkt"
         "../tui/commands/context.rkt"
         "../tui/context.rkt"
         "../agent/event-bus.rkt")

;; ============================================================
;; cmd-ctx has goal-cancel-box field
;; ============================================================

(test-case "cmd-ctx-goal-cancel-box accessor works"
  (define cancel-box (box #f))
  (define cctx (cmd-ctx (box (initial-ui-state))
                        (box #t)
                        #f #f
                        (box #f) #f (box #f) #f
                        (box "") #f #f
                        (box #f)
                        cancel-box))
  (check-eq? (cmd-ctx-goal-cancel-box cctx) cancel-box)
  ;; Set the cancel flag
  (set-box! cancel-box #t)
  (check-equal? (unbox (cmd-ctx-goal-cancel-box cctx)) #t))

;; ============================================================
;; cmd-ctx has agent-session-box field
;; ============================================================

(test-case "cmd-ctx-agent-session-box accessor works"
  (define sess-box (box 'mock-session))
  (define cctx (cmd-ctx (box (initial-ui-state))
                        (box #t)
                        #f #f
                        (box #f) #f (box #f) #f
                        (box "") #f #f
                        sess-box
                        (box #f)))
  (check-eq? (cmd-ctx-agent-session-box cctx) sess-box)
  (check-equal? (unbox (cmd-ctx-agent-session-box cctx)) 'mock-session))

;; ============================================================
;; tui-ctx has both agent-session-box and goal-cancel-box
;; ============================================================

(test-case "tui-ctx-agent-session-box and tui-ctx-goal-cancel-box work"
  (define sess-box (box 'session))
  (define cancel-box (box #f))
  (define ctx (tui-ctx (box (initial-ui-state))
                       (box 'input)
                       #f #f (box #t)
                       #f #f #f #f #f #f #f #f #f #f #f #f #f #f
                       sess-box
                       cancel-box))
  (check-eq? (tui-ctx-agent-session-box ctx) sess-box)
  (check-eq? (tui-ctx-goal-cancel-box ctx) cancel-box))

;; ============================================================
;; Cancel mechanism: setting cancel-box stops thread
;; ============================================================

(test-case "goal cancel-box signals thread to stop"
  (define cancel-box (box #f))
  (define running-box (box #t))
  ;; Simulate the shutdown-check logic from commands.rkt
  (define (shutdown-check)
    (or (unbox cancel-box)
        (not (unbox running-box))))
  (check-false (shutdown-check) "not cancelled, still running")
  (set-box! cancel-box #t)
  (check-true (shutdown-check) "cancel-box set => should stop")
  (set-box! cancel-box #f)
  (set-box! running-box #f)
  (check-true (shutdown-check) "running-box cleared => should stop"))

;; ============================================================
;; Thread write-back is skipped when cancelled
;; ============================================================

(test-case "cancelled thread skips write-back"
  (define state-box (box (initial-ui-state)))
  (define cancel-box (box #t))
  ;; Simulate the write-back logic from commands.rkt
  (define was-cancelled (unbox cancel-box))
  (unless was-cancelled
    (set-box! state-box
              (struct-copy ui-state (unbox state-box) [active-goal 'some-info])))
  ;; State should remain unchanged
  (check-false (ui-state-active-goal (unbox state-box)))
  ;; Reset for next goal
  (set-box! cancel-box #f)
  (check-false (unbox cancel-box)))



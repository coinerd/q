#lang racket/base

;; @speed slow
;; @suite tui-tmux
;; @boundary e2e

;; tests/test-tmux-tui-smoke.rkt — P0 tmux smoke tests for q --tui
;;
;; These are real end-to-end tests that launch q --tui inside a tmux
;; pseudo-terminal, interact with it, and verify behavior.
;;
;; Tests are SKIP by default. To run them:
;;   Q_TMUX_TUI_TESTS=1 raco test tests/test-tmux-tui-smoke.rkt
;;
;; Scenarios covered:
;;   T0: Launch welcome — q starts, q> prompt visible, status line visible
;;   T1: Quit command — /quit causes session to exit within timeout

(require rackunit
         racket/string
         "helpers/tmux-q-harness.rkt")

;; ============================================================
;; Skip guard
;; ============================================================

(unless (should-run-tmux-tests?)
  (printf "SKIP: tmux smoke tests require Q_TMUX_TUI_TESTS=1 and tmux~n")
  (exit 0))

(printf "Running tmux smoke tests...~n")

;; ============================================================
;; T0: Launch and verify prompt + status line
;; ============================================================

(define env-T0 #f)
(define sess-T0 #f)

(define (cleanup-T0)
  (when (and sess-T0 (session-alive? sess-T0))
    (stop-session! sess-T0)))

;; T0a: Session starts and stays alive
(check-true (dynamic-wind (lambda () (set! env-T0 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-T0 (start-q-tui-session! env-T0))
                            ;; Wait for q> prompt to appear
                            (wait-for-text sess-T0 "q>" #:timeout-ms 20000))
                          (lambda () (cleanup-T0)))
            "T0a: q --tui launches and shows q> prompt")

;; T0b: Status line is visible (ctx: indicator)
(check-true (dynamic-wind (lambda () (set! env-T0 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-T0 (start-q-tui-session! env-T0))
                            (wait-for-text sess-T0 "q>" #:timeout-ms 20000)
                            ;; Status line should show context indicator
                            (wait-for-text sess-T0 "ctx:" #:timeout-ms 5000))
                          (lambda () (cleanup-T0)))
            "T0b: status line with ctx: indicator is visible")

;; T0c: Mock provider activates (no config in temp HOME)
(check-true (dynamic-wind (lambda () (set! env-T0 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-T0 (start-q-tui-session! env-T0))
                            (wait-for-text sess-T0 "q>" #:timeout-ms 20000)
                            ;; Send a message and wait for mock response
                            (send-line! sess-T0 "hello")
                            (wait-for-text sess-T0 "Mock response" #:timeout-ms 15000))
                          (lambda () (cleanup-T0)))
            "T0c: mock provider responds to input")

;; ============================================================
;; T1: Quit command exits cleanly
;; ============================================================

(define env-T1 #f)
(define sess-T1 #f)

(define (cleanup-T1)
  (when (and sess-T1 (session-alive? sess-T1))
    (stop-session! sess-T1)))

;; T1a: /quit causes session to exit within timeout
(check-true (dynamic-wind (lambda () (set! env-T1 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-T1 (start-q-tui-session! env-T1))
                            (wait-for-text sess-T1 "q>" #:timeout-ms 20000)
                            ;; Send /quit
                            (send-line! sess-T1 "/quit")
                            ;; Session should exit within timeout
                            (wait-for-exit sess-T1 #:timeout-ms 10000))
                          (lambda () (cleanup-T1)))
            "T1a: /quit exits q within timeout")

;; T1b: No orphan tmux sessions after quit
(check-true (dynamic-wind (lambda () (set! env-T1 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-T1 (start-q-tui-session! env-T1))
                            (wait-for-text sess-T1 "q>" #:timeout-ms 20000)
                            (send-line! sess-T1 "/quit")
                            (wait-for-exit sess-T1 #:timeout-ms 10000)
                            ;; After quit, session should NOT be alive
                            (not (session-alive? sess-T1)))
                          (lambda () (cleanup-T1)))
            "T1b: no orphan tmux session after /quit")

(printf "tmux smoke tests complete.~n")

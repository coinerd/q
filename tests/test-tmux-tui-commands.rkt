#lang racket/base

;; @speed slow
;; @suite tui-tmux
;; @boundary e2e

;; tests/test-tmux-tui-commands.rkt — W4: slash-command usability and recovery
;;
;; Real end-to-end tests that launch q --tui inside tmux, send slash commands,
;; and verify that:
;;   C0: /help — help text visible, prompt returns
;;   C1: /status — status/session info visible, prompt returns
;;   C2: /clear — transcript cleared, prompt returns
;;   C3: /not-a-command — visible error, TUI still usable
;;   C4: /retry — graceful behavior, TUI still usable
;;
;; Tests are SKIP by default. To run:
;;   Q_TMUX_TUI_TESTS=1 raco test tests/test-tmux-tui-commands.rkt

(require rackunit
         racket/string
         "helpers/tmux-q-harness.rkt")

;; ============================================================
;; Skip guard
;; ============================================================

(unless (should-run-tmux-tests?)
  (printf "SKIP: tmux command tests require Q_TMUX_TUI_TESTS=1 and tmux~n")
  (exit 0))

(printf "Running tmux slash-command tests...~n")

;; ============================================================
;; Helper: verify prompt is usable after a command
;; Sends a regular message and checks mock response appears.
;; ============================================================

(define (prompt-still-usable? sess)
  (send-line! sess "are you there")
  (wait-for-text sess "Mock response" #:timeout-ms 15000))

;; ============================================================
;; C0: /help — help text visible, prompt returns
;; ============================================================

(define env-C0 #f)
(define sess-C0 #f)

(define (cleanup-C0)
  (when (and sess-C0 (session-alive? sess-C0))
    (stop-session! sess-C0)))

(check-true (dynamic-wind (lambda () (set! env-C0 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-C0 (start-q-tui-session! env-C0 #:rows 40))
                            (wait-for-text sess-C0 "q>" #:timeout-ms 20000)
                            (send-line! sess-C0 "/help")
                            ;; Help text should show "Commands:" header
                            (and (wait-for-text sess-C0 "Commands:" #:timeout-ms 10000)
                                 ;; Prompt should return
                                 (wait-for-text sess-C0 "q>" #:timeout-ms 5000)))
                          (lambda () (cleanup-C0)))
            "C0: /help shows commands and prompt returns")

;; ============================================================
;; C1: /status — session/status info visible, prompt returns
;; ============================================================

(define env-C1 #f)
(define sess-C1 #f)

(define (cleanup-C1)
  (when (and sess-C1 (session-alive? sess-C1))
    (stop-session! sess-C1)))

(check-true (dynamic-wind (lambda () (set! env-C1 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-C1 (start-q-tui-session! env-C1))
                            (wait-for-text sess-C1 "q>" #:timeout-ms 20000)
                            (send-line! sess-C1 "/status")
                            ;; Status should show "STATUS" or "Session:" label
                            (and (wait-for-text sess-C1 "STATUS" #:timeout-ms 10000)
                                 ;; Prompt should return
                                 (wait-for-text sess-C1 "q>" #:timeout-ms 5000)))
                          (lambda () (cleanup-C1)))
            "C1: /status shows session info and prompt returns")

;; ============================================================
;; C2: /clear — transcript cleared, prompt returns
;; ============================================================

(define env-C2 #f)
(define sess-C2 #f)

(define (cleanup-C2)
  (when (and sess-C2 (session-alive? sess-C2))
    (stop-session! sess-C2)))

(check-true (dynamic-wind (lambda () (set! env-C2 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-C2 (start-q-tui-session! env-C2))
                            (wait-for-text sess-C2 "q>" #:timeout-ms 20000)
                            ;; First, send a message so there's something to clear
                            (send-line! sess-C2 "hello")
                            (wait-for-text sess-C2 "Mock response" #:timeout-ms 15000)
                            ;; Now clear
                            (send-line! sess-C2 "/clear")
                            (sleep 2)
                            ;; After clear, "Mock response" should NOT be visible (cleared)
                            ;; and prompt should return
                            (define pane-after (capture-normalized sess-C2))
                            (and (not (text-found-in? "Mock response" pane-after))
                                 ;; Prompt should still be there
                                 (text-found-in? "q>" pane-after)))
                          (lambda () (cleanup-C2)))
            "C2: /clear removes transcript and prompt returns")

;; ============================================================
;; C3: /not-a-command — visible error, TUI still usable
;; ============================================================

(define env-C3 #f)
(define sess-C3 #f)

(define (cleanup-C3)
  (when (and sess-C3 (session-alive? sess-C3))
    (stop-session! sess-C3)))

(check-true (dynamic-wind (lambda () (set! env-C3 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-C3 (start-q-tui-session! env-C3))
                            (wait-for-text sess-C3 "q>" #:timeout-ms 20000)
                            ;; Send unknown command
                            (send-line! sess-C3 "/not-a-command")
                            ;; Error should be visible
                            (and (wait-for-text sess-C3 "Unknown command" #:timeout-ms 10000)
                                 ;; Prompt should return
                                 (wait-for-text sess-C3 "q>" #:timeout-ms 5000)
                                 ;; TUI should still be usable
                                 (prompt-still-usable? sess-C3)))
                          (lambda () (cleanup-C3)))
            "C3: unknown command shows error and TUI remains usable")

;; ============================================================
;; C4: /retry — graceful behavior, TUI still usable
;; ============================================================

(define env-C4 #f)
(define sess-C4 #f)

(define (cleanup-C4)
  (when (and sess-C4 (session-alive? sess-C4))
    (stop-session! sess-C4)))

(check-true (dynamic-wind (lambda () (set! env-C4 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-C4 (start-q-tui-session! env-C4))
                            (wait-for-text sess-C4 "q>" #:timeout-ms 20000)
                            ;; Send /retry (not a registered command, should be graceful)
                            (send-line! sess-C4 "/retry")
                            (sleep 3)
                            ;; TUI should still be usable after /retry
                            (prompt-still-usable? sess-C4))
                          (lambda () (cleanup-C4)))
            "C4: /retry is handled gracefully and TUI remains usable")

(printf "tmux slash-command tests complete.~n")

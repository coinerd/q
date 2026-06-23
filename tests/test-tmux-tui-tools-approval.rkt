#lang racket/base

;; @speed slow
;; @suite tui-tmux
;; @boundary e2e

;; tests/test-tmux-tui-tools-approval.rkt — W8: tools, approval, no-tools guardrails
;;
;; Real end-to-end tests that launch q --tui inside tmux and verify:
;;   A0: no-tools startup — prompt works
;;   A1: tool-disabled request — mock response, no real tool execution
;;   A3: /interrupt — no crash, prompt returns, TUI still usable
;;
;; Tests are SKIP by default. To run:
;;   Q_TMUX_TUI_TESTS=1 raco test tests/test-tmux-tui-tools-approval.rkt

(require rackunit
         racket/string
         "helpers/tmux-q-harness.rkt")

;; ============================================================
;; Skip guard
;; ============================================================

(unless (should-run-tmux-tests?)
  (printf "SKIP: tmux tools/approval tests require Q_TMUX_TUI_TESTS=1 and tmux~n")
  (exit 0))

(printf "Running tmux tools/approval tests...~n")

;; ============================================================
;; A0: no-tools startup — prompt works
;; ============================================================

(define env-A0 #f)
(define sess-A0 #f)

(define (cleanup-A0)
  (when (and sess-A0 (session-alive? sess-A0))
    (stop-session! sess-A0)))

(check-true (dynamic-wind (lambda () (set! env-A0 (make-tmux-test-env)))
                          (lambda ()
                            ;; Start with --no-tools flag
                            (set! sess-A0 (start-q-tui-session! env-A0 #:args '("--no-tools")))
                            (and (wait-for-text sess-A0 "q>" #:timeout-ms 20000)
                                 ;; Verify we can interact
                                 (begin
                                   (send-line! sess-A0 "hello no tools")
                                   (wait-for-text sess-A0 "Mock response" #:timeout-ms 15000))))
                          (lambda () (cleanup-A0)))
            "A0: no-tools startup — prompt works")

;; ============================================================
;; A1: tool-disabled request — mock response, no real tool
;; ============================================================

(define env-A1 #f)
(define sess-A1 #f)

(define (cleanup-A1)
  (when (and sess-A1 (session-alive? sess-A1))
    (stop-session! sess-A1)))

(check-true (dynamic-wind (lambda () (set! env-A1 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-A1 (start-q-tui-session! env-A1 #:args '("--no-tools")))
                            (wait-for-text sess-A1 "q>" #:timeout-ms 20000)
                            ;; Ask q to use a tool (which should not work with --no-tools)
                            (send-line! sess-A1 "please read the file /etc/passwd")
                            (sleep 5)
                            ;; Mock response should still appear
                            (define pane (capture-normalized sess-A1 #:lines 30))
                            ;; Mock response appears, and no tool execution evidence
                            (and (text-found-in? "Mock response" pane)
                                 ;; No tool-related output like "[tool]" or "bash" execution
                                 (not (text-found-in? "[tool]" pane))))
                          (lambda () (cleanup-A1)))
            "A1: tool-disabled request gets mock response, no tool execution")

;; ============================================================
;; A3: /interrupt — no crash, prompt returns, TUI usable
;; ============================================================

(define env-A3 #f)
(define sess-A3 #f)

(define (cleanup-A3)
  (when (and sess-A3 (session-alive? sess-A3))
    (stop-session! sess-A3)))

(check-true (dynamic-wind (lambda () (set! env-A3 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-A3 (start-q-tui-session! env-A3))
                            (wait-for-text sess-A3 "q>" #:timeout-ms 20000)
                            ;; Send a message
                            (send-line! sess-A3 "hello")
                            (sleep 1)
                            ;; Immediately interrupt
                            (send-line! sess-A3 "/interrupt")
                            (sleep 3)
                            ;; Check for interrupt confirmation
                            (define pane (capture-normalized sess-A3 #:lines 30))
                            (and (or (text-found-in? "interrupt" pane)
                                     (text-found-in? "Interrupt" pane))
                                 ;; Prompt should return
                                 (wait-for-text sess-A3 "q>" #:timeout-ms 5000)
                                 ;; TUI should still be usable after interrupt
                                 (begin
                                   (send-line! sess-A3 "still working after interrupt")
                                   (wait-for-text sess-A3 "Mock response" #:timeout-ms 15000))))
                          (lambda () (cleanup-A3)))
            "A3: /interrupt — no crash, prompt returns, TUI usable")

(printf "tmux tools/approval tests complete.~n")

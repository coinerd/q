#lang racket/base

;; @speed slow
;; @suite tui-tmux
;; @boundary e2e

;; tests/test-tmux-tui-resize-cleanup.rkt — W5: resize, unicode, cleanup
;;
;; Real end-to-end tests that launch q --tui inside tmux and verify:
;;   U1: resize smaller (60x20) — no crash, prompt still usable
;;   U2: resize larger (120x40) — no crash, prompt still usable
;;   U3: unicode prompt — no mojibake, text renders correctly
;;   U5: terminal cleanup — no orphan tmux session after quit
;;
;; Tests are SKIP by default. To run:
;;   Q_TMUX_TUI_TESTS=1 raco test tests/test-tmux-tui-resize-cleanup.rkt

(require rackunit
         racket/string
         "helpers/tmux-q-harness.rkt")

;; ============================================================
;; Skip guard
;; ============================================================

(unless (should-run-tmux-tests?)
  (printf "SKIP: tmux resize/cleanup tests require Q_TMUX_TUI_TESTS=1 and tmux~n")
  (exit 0))

(printf "Running tmux resize/unicode/cleanup tests...~n")

;; ============================================================
;; U1: resize smaller (60x20) — no crash, prompt usable
;; ============================================================

(define env-U1 #f)
(define sess-U1 #f)

(define (cleanup-U1)
  (when (and sess-U1 (session-alive? sess-U1))
    (stop-session! sess-U1)))

(check-true (dynamic-wind (lambda () (set! env-U1 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-U1 (start-q-tui-session! env-U1 #:cols 100 #:rows 30))
                            (wait-for-text sess-U1 "q>" #:timeout-ms 20000)
                            ;; Resize smaller
                            (resize-session! sess-U1 60 20)
                            (sleep 2)
                            ;; Session should still be alive
                            (and (session-alive? sess-U1)
                                 ;; Prompt should still be visible
                                 (wait-for-text sess-U1 "q>" #:timeout-ms 5000)
                                 ;; Should be able to send a message and get response
                                 (begin
                                   (send-line! sess-U1 "after resize small")
                                   (wait-for-text sess-U1 "Mock response" #:timeout-ms 15000))))
                          (lambda () (cleanup-U1)))
            "U1: resize smaller (60x20) — no crash, prompt usable")

;; ============================================================
;; U2: resize larger (120x40) — no crash, prompt usable
;; ============================================================

(define env-U2 #f)
(define sess-U2 #f)

(define (cleanup-U2)
  (when (and sess-U2 (session-alive? sess-U2))
    (stop-session! sess-U2)))

(check-true (dynamic-wind (lambda () (set! env-U2 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-U2 (start-q-tui-session! env-U2 #:cols 80 #:rows 24))
                            (wait-for-text sess-U2 "q>" #:timeout-ms 20000)
                            ;; Resize larger
                            (resize-session! sess-U2 120 40)
                            (sleep 2)
                            ;; Session should still be alive
                            (and (session-alive? sess-U2)
                                 ;; Prompt should still be visible
                                 (wait-for-text sess-U2 "q>" #:timeout-ms 5000)
                                 ;; Should be able to send a message and get response
                                 (begin
                                   (send-line! sess-U2 "after resize large")
                                   (wait-for-text sess-U2 "Mock response" #:timeout-ms 15000))))
                          (lambda () (cleanup-U2)))
            "U2: resize larger (120x40) — no crash, prompt usable")

;; ============================================================
;; U3: unicode prompt — no mojibake
;; ============================================================

(define env-U3 #f)
(define sess-U3 #f)

(define (cleanup-U3)
  (when (and sess-U3 (session-alive? sess-U3))
    (stop-session! sess-U3)))

(check-true (dynamic-wind (lambda () (set! env-U3 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-U3 (start-q-tui-session! env-U3 #:cols 100 #:rows 30))
                            (wait-for-text sess-U3 "q>" #:timeout-ms 20000)
                            ;; Send text with various unicode ranges: accented, CJK, emoji
                            (send-line! sess-U3 "café résumé 日本語 🎉")
                            (sleep 3)
                            ;; Capture the pane and check for unicode rendering
                            (define pane (capture-normalized sess-U3 #:lines 30))
                            ;; The echoed input should contain the unicode text without mojibake.
                            ;; We check for key substrings that would be corrupted by encoding issues.
                            (and (text-found-in? "café" pane)
                                 (text-found-in? "日本語" pane)
                                 ;; Session still alive and functional
                                 (wait-for-text sess-U3 "Mock response" #:timeout-ms 15000)))
                          (lambda () (cleanup-U3)))
            "U3: unicode text renders without mojibake")

;; ============================================================
;; U5: terminal cleanup — no orphan session after quit
;; ============================================================

(define env-U5 #f)
(define sess-U5 #f)

(check-true (dynamic-wind (lambda () (set! env-U5 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-U5 (start-q-tui-session! env-U5))
                            (wait-for-text sess-U5 "q>" #:timeout-ms 20000)
                            (send-line! sess-U5 "test message")
                            (wait-for-text sess-U5 "Mock response" #:timeout-ms 15000)
                            ;; Quit cleanly
                            (send-line! sess-U5 "/quit")
                            (wait-for-exit sess-U5 #:timeout-ms 10000)
                            ;; After quit, the session should NOT be alive
                            (not (session-alive? sess-U5)))
                          ;; No cleanup needed — session should already be dead.
                          ;; If it's somehow still alive, clean it up so we don't leave orphans.
                          (lambda ()
                            (when (and sess-U5 (session-alive? sess-U5))
                              (stop-session! sess-U5))))
            "U5: no orphan session after quit")

(printf "tmux resize/unicode/cleanup tests complete.~n")

#lang racket/base

;; @speed slow
;; @suite tui-tmux
;; @boundary e2e

;; tests/test-tmux-tui-context-memory.rkt — W6: context and memory real-life scenarios
;;
;; Real end-to-end tests that launch q --tui inside tmux and verify:
;;   X0: context off — startup/prompt works (default, no config)
;;   X1: bounded/default context — status line has ctx indicator
;;   M0: memory off — prompt works, no memory artifacts
;;   M1: file-jsonl memory configured — q starts without crash, no memory
;;       corruption of real user data
;;
;; Tests are SKIP by default. To run:
;;   Q_TMUX_TUI_TESTS=1 raco test tests/test-tmux-tui-context-memory.rkt

(require rackunit
         racket/file
         racket/string
         json
         "helpers/tmux-q-harness.rkt")

;; ============================================================
;; Skip guard
;; ============================================================

(unless (should-run-tmux-tests?)
  (printf "SKIP: tmux context/memory tests require Q_TMUX_TUI_TESTS=1 and tmux~n")
  (exit 0))

(printf "Running tmux context/memory tests...~n")

;; ============================================================
;; X0: context off — startup/prompt works (default, no config)
;; ============================================================

(define env-X0 #f)
(define sess-X0 #f)

(define (cleanup-X0)
  (when (and sess-X0 (session-alive? sess-X0))
    (stop-session! sess-X0)))

(check-true (dynamic-wind (lambda () (set! env-X0 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-X0 (start-q-tui-session! env-X0))
                            ;; Default startup: no config, context is off/empty
                            ;; Prompt should appear
                            (and (wait-for-text sess-X0 "q>" #:timeout-ms 20000)
                                 ;; Should be able to interact
                                 (begin
                                   (send-line! sess-X0 "hello")
                                   (wait-for-text sess-X0 "Mock response" #:timeout-ms 15000))))
                          (lambda () (cleanup-X0)))
            "X0: default startup (no config) — prompt works")

;; ============================================================
;; X1: bounded/default context — status line has ctx indicator
;; ============================================================

(define env-X1 #f)
(define sess-X1 #f)

(define (cleanup-X1)
  (when (and sess-X1 (session-alive? sess-X1))
    (stop-session! sess-X1)))

(check-true (dynamic-wind (lambda () (set! env-X1 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-X1 (start-q-tui-session! env-X1))
                            (wait-for-text sess-X1 "q>" #:timeout-ms 20000)
                            ;; The status line should contain "ctx:" indicator
                            ;; With mock provider, context tokens will be 0 or very low
                            (wait-for-text sess-X1 "ctx:" #:timeout-ms 10000))
                          (lambda () (cleanup-X1)))
            "X1: status line shows ctx: indicator")

;; ============================================================
;; M0: memory off — prompt works, no memory files
;; ============================================================

(define env-M0 #f)
(define sess-M0 #f)

(define (cleanup-M0)
  (when (and sess-M0 (session-alive? sess-M0))
    (stop-session! sess-M0)))

(check-true (dynamic-wind (lambda () (set! env-M0 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-M0 (start-q-tui-session! env-M0))
                            (wait-for-text sess-M0 "q>" #:timeout-ms 20000)
                            (send-line! sess-M0 "remember this fact")
                            (wait-for-text sess-M0 "Mock response" #:timeout-ms 15000)
                            (send-line! sess-M0 "/quit")
                            (wait-for-exit sess-M0 #:timeout-ms 10000)
                            (sleep 1)
                            ;; No memory files should exist in temp HOME
                            ;; Session artifacts are fine, but memory.jsonl should NOT appear
                            (define home-dir (tmux-env-home env-M0))
                            (null? (with-handlers ([exn:fail? (lambda (e) '())])
                                     (find-files (lambda (p)
                                                   (regexp-match? #rx"memory" (path->string p)))
                                                 home-dir))))
                          (lambda () (cleanup-M0)))
            "M0: memory off — no memory files created")

;; ============================================================
;; M1: file-jsonl memory configured — q starts without crash
;; ============================================================

(define env-M1 #f)
(define sess-M1 #f)

(define (cleanup-M1)
  (when (and sess-M1 (session-alive? sess-M1))
    (stop-session! sess-M1)))

;; Helper: write a q config file with memory backend enabled
(define (write-memory-config! home-dir)
  (define q-dir (build-path home-dir ".q"))
  (make-directory* q-dir)
  (define config-path (build-path q-dir "config.rkt"))
  (call-with-output-file config-path
                         (lambda (out) (displayln "((memory . ((backend . file-jsonl))))" out))
                         #:exists 'replace))

(check-true
 (dynamic-wind (lambda ()
                 (set! env-M1 (make-tmux-test-env))
                 ;; Write config with file-jsonl memory backend before launching q
                 (write-memory-config! (tmux-env-home env-M1)))
               (lambda ()
                 ;; q should start fine even with memory backend configured
                 ;; Mock provider won't trigger memory extraction, but q must not crash
                 (set! sess-M1 (start-q-tui-session! env-M1))
                 (and (wait-for-text sess-M1 "q>" #:timeout-ms 20000)
                      ;; Should be able to interact normally
                      (begin
                        (send-line! sess-M1 "hello with memory")
                        (wait-for-text sess-M1 "Mock response" #:timeout-ms 15000))
                      ;; Verify config file was not corrupted
                      (let ([config-path (build-path (tmux-env-home env-M1) ".q" "config.rkt")])
                        (file-exists? config-path))))
               (lambda () (cleanup-M1)))
 "M1: file-jsonl memory configured — q starts without crash, config intact")

(printf "tmux context/memory tests complete.~n")

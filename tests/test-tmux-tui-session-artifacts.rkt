#lang racket/base

;; @speed slow
;; @suite tui-tmux
;; @boundary e2e

;; tests/test-tmux-tui-session-artifacts.rkt — W3: prompt-response and session artifacts
;;
;; Real end-to-end tests that launch q --tui inside tmux, interact with it,
;; and verify that:
;;   T2: User input is echoed and Mock response appears
;;   T3: Session artifacts (session.jsonl, trace.jsonl, scrollback.jsonl) are
;;       created on disk after the session ends.
;;
;; Tests are SKIP by default. To run:
;;   Q_TMUX_TUI_TESTS=1 raco test tests/test-tmux-tui-session-artifacts.rkt

(require rackunit
         racket/file
         racket/string
         racket/format
         json
         "helpers/tmux-q-harness.rkt")

;; ============================================================
;; Skip guard
;; ============================================================

(unless (should-run-tmux-tests?)
  (printf "SKIP: tmux artifact tests require Q_TMUX_TUI_TESTS=1 and tmux~n")
  (exit 0))

(printf "Running tmux session artifact tests...~n")

;; ============================================================
;; T2: Prompt-response — input echo + Mock response visible
;; ============================================================

(define env-T2 #f)
(define sess-T2 #f)

(define (cleanup-T2)
  (when (and sess-T2 (session-alive? sess-T2))
    (stop-session! sess-T2)))

;; T2a: User input is echoed with "> " prefix
(check-true (dynamic-wind (lambda () (set! env-T2 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-T2 (start-q-tui-session! env-T2))
                            (wait-for-text sess-T2 "q>" #:timeout-ms 20000)
                            (send-line! sess-T2 "hello world")
                            ;; Wait for the echo line to appear
                            (wait-for-text sess-T2 "> hello world" #:timeout-ms 10000))
                          (lambda () (cleanup-T2)))
            "T2a: user input is echoed with '> ' prefix")

;; T2b: Mock provider response appears
(check-true (dynamic-wind (lambda () (set! env-T2 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-T2 (start-q-tui-session! env-T2))
                            (wait-for-text sess-T2 "q>" #:timeout-ms 20000)
                            (send-line! sess-T2 "tell me a joke")
                            (wait-for-text sess-T2 "Mock response" #:timeout-ms 15000))
                          (lambda () (cleanup-T2)))
            "T2b: Mock provider response is visible")

;; T2c: Multi-turn — second message response also visible
(check-true
 (dynamic-wind (lambda () (set! env-T2 (make-tmux-test-env)))
               (lambda ()
                 (set! sess-T2 (start-q-tui-session! env-T2 #:rows 50))
                 (wait-for-text sess-T2 "q>" #:timeout-ms 20000)
                 ;; First turn
                 (send-line! sess-T2 "first message")
                 (wait-for-text sess-T2 "Mock response" #:timeout-ms 15000)
                 ;; Wait for prompt to return
                 (sleep 1)
                 ;; Second turn
                 (send-line! sess-T2 "second message")
                 ;; Capture enough lines to see both responses
                 (wait-for-predicate sess-T2
                                     (lambda ()
                                       (define pane (capture-normalized sess-T2 #:lines 100))
                                       ;; Count occurrences of "Mock response"
                                       (define matches (regexp-match* #rx"Mock response" pane))
                                       (>= (length matches) 2))
                                     #:timeout-ms 15000))
               (lambda () (cleanup-T2)))
 "T2c: multi-turn interaction shows multiple responses")

;; ============================================================
;; T3: Session artifacts created after quit
;; ============================================================

(define env-T3 #f)
(define sess-T3 #f)

(define (cleanup-T3)
  (when (and sess-T3 (session-alive? sess-T3))
    (stop-session! sess-T3)))

;; T3a: session.jsonl exists in session subdir after quit
(check-true (dynamic-wind (lambda () (set! env-T3 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-T3 (start-q-tui-session! env-T3))
                            (wait-for-text sess-T3 "q>" #:timeout-ms 20000)
                            (send-line! sess-T3 "hello")
                            (wait-for-text sess-T3 "Mock response" #:timeout-ms 15000)
                            ;; Quit and wait for exit
                            (send-line! sess-T3 "/quit")
                            (wait-for-exit sess-T3 #:timeout-ms 10000)
                            ;; Give filesystem a moment to flush
                            (sleep 1)
                            ;; Find the session subdir
                            (define session-dir (tmux-q-session-session-dir sess-T3))
                            (define subdir (find-first-session-subdir session-dir))
                            (and subdir (file-exists? (build-path subdir "session.jsonl"))))
                          (lambda () (cleanup-T3)))
            "T3a: session.jsonl exists in session subdir after quit")

;; T3b: trace.jsonl exists in session subdir after quit
(check-true (dynamic-wind (lambda () (set! env-T3 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-T3 (start-q-tui-session! env-T3))
                            (wait-for-text sess-T3 "q>" #:timeout-ms 20000)
                            (send-line! sess-T3 "hello")
                            (wait-for-text sess-T3 "Mock response" #:timeout-ms 15000)
                            (send-line! sess-T3 "/quit")
                            (wait-for-exit sess-T3 #:timeout-ms 10000)
                            (sleep 1)
                            (define session-dir (tmux-q-session-session-dir sess-T3))
                            (define subdir (find-first-session-subdir session-dir))
                            (and subdir (file-exists? (build-path subdir "trace.jsonl"))))
                          (lambda () (cleanup-T3)))
            "T3b: trace.jsonl exists in session subdir after quit")

;; T3c: scrollback.jsonl exists under sessions dir after quit
(check-true (dynamic-wind (lambda () (set! env-T3 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-T3 (start-q-tui-session! env-T3))
                            (wait-for-text sess-T3 "q>" #:timeout-ms 20000)
                            (send-line! sess-T3 "hello")
                            (wait-for-text sess-T3 "Mock response" #:timeout-ms 15000)
                            (send-line! sess-T3 "/quit")
                            (wait-for-exit sess-T3 #:timeout-ms 10000)
                            (sleep 1)
                            (define session-dir (tmux-q-session-session-dir sess-T3))
                            (file-exists? (build-path session-dir "scrollback.jsonl")))
                          (lambda () (cleanup-T3)))
            "T3c: scrollback.jsonl exists under sessions dir after quit")

;; T3d: session.jsonl contains valid JSONL (first line parses as JSON)
(check-true (dynamic-wind (lambda () (set! env-T3 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-T3 (start-q-tui-session! env-T3))
                            (wait-for-text sess-T3 "q>" #:timeout-ms 20000)
                            (send-line! sess-T3 "test message")
                            (wait-for-text sess-T3 "Mock response" #:timeout-ms 15000)
                            (send-line! sess-T3 "/quit")
                            (wait-for-exit sess-T3 #:timeout-ms 10000)
                            (sleep 1)
                            (define session-dir (tmux-q-session-session-dir sess-T3))
                            (define subdir (find-first-session-subdir session-dir))
                            (and subdir
                                 (let ([jsonl-path (build-path subdir "session.jsonl")])
                                   (and (file-exists? jsonl-path)
                                        ;; Parse the first non-empty line as JSON
                                        (let* ([content (file->string jsonl-path)]
                                               [lines (filter (lambda (l)
                                                                (> (string-length (string-trim l)) 0))
                                                              (string-split content "\n"))]
                                               [first-line (string-trim (car lines))])
                                          (with-handlers ([exn:fail? (lambda (e) #f)])
                                            (string->jsexpr first-line)
                                            #t))))))
                          (lambda () (cleanup-T3)))
            "T3d: session.jsonl first line is valid JSON")

(printf "tmux session artifact tests complete.~n")

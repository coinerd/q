#lang racket/base

;; @speed slow
;; @suite tui-tmux
;; @boundary e2e

;; tests/test-tmux-tui-gsd.rkt — W7: GSD planning real-life scenarios
;;
;; Real end-to-end tests that launch q --tui inside tmux and verify:
;;   G0: GSD planning prompt — mock response visible
;;   G1: GSD temp project — artifacts confined to temp project (no leak)
;;   G2: /goal command — visible result (goal set, runs, graceful failure)
;;
;; Tests are SKIP by default. To run:
;;   Q_TMUX_TUI_TESTS=1 raco test tests/test-tmux-tui-gsd.rkt

(require rackunit
         racket/file
         racket/string
         "helpers/tmux-q-harness.rkt")

;; ============================================================
;; Skip guard
;; ============================================================

(unless (should-run-tmux-tests?)
  (printf "SKIP: tmux GSD tests require Q_TMUX_TUI_TESTS=1 and tmux~n")
  (exit 0))

(printf "Running tmux GSD planning tests...~n")

;; ============================================================
;; G0: GSD planning prompt — mock response visible
;; ============================================================

(define env-G0 #f)
(define sess-G0 #f)

(define (cleanup-G0)
  (when (and sess-G0 (session-alive? sess-G0))
    (stop-session! sess-G0)))

(check-true (dynamic-wind (lambda () (set! env-G0 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-G0 (start-q-tui-session! env-G0))
                            (wait-for-text sess-G0 "q>" #:timeout-ms 20000)
                            ;; Send a GSD-like planning prompt
                            (send-line! sess-G0
                                        "help me plan a milestone for adding user authentication")
                            ;; Mock response should appear
                            (wait-for-text sess-G0 "Mock response" #:timeout-ms 15000))
                          (lambda () (cleanup-G0)))
            "G0: GSD planning prompt gets mock response")

;; ============================================================
;; G1: GSD temp project — no artifacts leak outside temp HOME
;; ============================================================

(define env-G1 #f)
(define sess-G1 #f)

(define (cleanup-G1)
  (when (and sess-G1 (session-alive? sess-G1))
    (stop-session! sess-G1)))

(check-true (dynamic-wind (lambda () (set! env-G1 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-G1 (start-q-tui-session! env-G1))
                            (wait-for-text sess-G1 "q>" #:timeout-ms 20000)
                            ;; Send a planning-like message
                            (send-line! sess-G1 "create a plan with 3 waves")
                            (wait-for-text sess-G1 "Mock response" #:timeout-ms 15000)
                            ;; Quit and wait
                            (send-line! sess-G1 "/quit")
                            (wait-for-exit sess-G1 #:timeout-ms 10000)
                            (sleep 1)
                            ;; Verify no .planning/ or GSD artifacts created in project dir
                            (define proj-dir (tmux-env-project-dir env-G1))
                            ;; Check project dir has no .planning directory
                            (define planning-in-proj
                              (with-handlers ([exn:fail? (lambda (e) #f)])
                                (directory-exists? (build-path proj-dir ".planning"))))
                            ;; Check project dir has no files at all (temp project, clean)
                            (define proj-files
                              (with-handlers ([exn:fail? (lambda (e) '())])
                                (directory-list proj-dir)))
                            ;; Both conditions: no .planning, no stray files
                            (and (not planning-in-proj) (null? proj-files)))
                          (lambda () (cleanup-G1)))
            "G1: temp project has no GSD artifacts (isolation verified)")

;; ============================================================
;; G2: /goal command — visible result (set, run, graceful failure)
;; ============================================================

(define env-G2 #f)
(define sess-G2 #f)

(define (cleanup-G2)
  (when (and sess-G2 (session-alive? sess-G2))
    (stop-session! sess-G2)))

(check-true (dynamic-wind (lambda () (set! env-G2 (make-tmux-test-env)))
                          (lambda ()
                            (set! sess-G2 (start-q-tui-session! env-G2 #:rows 50))
                            (wait-for-text sess-G2 "q>" #:timeout-ms 20000)
                            ;; Set a goal — use /goal with description
                            (send-line! sess-G2 "/goal test goal for planning")
                            ;; Goal should start running and produce some [SYS] [goal] output
                            ;; With mock provider, it will fail gracefully
                            (sleep 10)
                            ;; Check for goal-related output in pane
                            (define pane (capture-normalized sess-G2 #:lines 80))
                            ;; Should show either "goal" or "Goal" somewhere
                            (or (text-found-in? "[goal]" pane) (text-found-in? "Goal" pane)))
                          (lambda () (cleanup-G2)))
            "G2: /goal command shows visible result")

(printf "tmux GSD planning tests complete.~n")

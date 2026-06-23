#!/usr/bin/env racket
#lang racket/base

;; scripts/tmux-tui-smoke.rkt — Developer command to run all tmux TUI scenarios
;;
;; Usage:
;;   racket scripts/tmux-tui-smoke.rkt           # run all (requires Q_TMUX_TUI_TESTS=1)
;;   Q_TMUX_TUI_TESTS=1 racket scripts/tmux-tui-smoke.rkt
;;   racket scripts/tmux-tui-smoke.rkt --list    # list all scenarios without running
;;   racket scripts/tmux-tui-smoke.rkt --filter smoke   # run only smoke-tagged tests
;;
;; This script wraps `raco test` for all tmux TUI test files, providing:
;;   - Unified entry point for all tmux scenarios
;;   - Skip/fail/pass counts
;;   --list to enumerate available scenarios
;;   --filter to run subsets

(require racket/cmdline
         racket/string
         racket/system
         racket/port)

;; ============================================================
;; Scenario registry
;; ============================================================

(define scenario-files
  '(("smoke" "tests/test-tmux-tui-smoke.rkt" "T0/T1: Basic startup, prompt, quit, orphan check")
    ("artifacts" "tests/test-tmux-tui-session-artifacts.rkt"
                 "T2/T3: Prompt-response, session artifacts")
    ("commands" "tests/test-tmux-tui-commands.rkt"
                "C0-C4: Slash commands (/help, /status, /clear, etc.)")
    ("resize" "tests/test-tmux-tui-resize-cleanup.rkt" "U1-U5: Resize, unicode, cleanup")
    ("context" "tests/test-tmux-tui-context-memory.rkt" "X0-X1/M0-M1: Context and memory scenarios")
    ("gsd" "tests/test-tmux-tui-gsd.rkt" "G0-G2: GSD planning scenarios")
    ("tools" "tests/test-tmux-tui-tools-approval.rkt" "A0-A3: Tools, approval, no-tools guardrails")))

;; ============================================================
;; Main
;; ============================================================

(define list-only? (make-parameter #f))
(define filter-tag (make-parameter #f))

(command-line #:program "tmux-tui-smoke"
              #:once-each [("-l" "--list") "List all tmux scenarios without running" (list-only? #t)]
              [("-f" "--filter") tag "Filter scenarios by tag" (filter-tag tag)])

(printf "=== q tmux TUI smoke runner ===~n")

(when (list-only?)
  (printf "~nAvailable scenarios:~n")
  (for ([s (in-list scenario-files)])
    (define tag (car s))
    (define file (cadr s))
    (define desc (caddr s))
    (printf "  ~a~a~a~n" tag (make-string (- 12 (string-length tag)) #\space) desc))
  (exit 0))

;; Check env
(define env-set? (equal? (getenv "Q_TMUX_TUI_TESTS") "1"))
(unless env-set?
  (printf "WARNING: Q_TMUX_TUI_TESTS is not set to 1. Tests will SKIP.~n")
  (printf "Set Q_TMUX_TUI_TESTS=1 to enable tmux tests.~n~n"))

;; Determine which files to run
(define files-to-run
  (if (filter-tag)
      (filter (lambda (s) (equal? (car s) (filter-tag))) scenario-files)
      scenario-files))

(when (null? files-to-run)
  (printf "No scenarios match filter '~a'~n" (filter-tag))
  (exit 1))

(printf "Running ~a scenario file(s)...~n~n" (length files-to-run))

;; Run each file
(define results '())
(for ([s (in-list files-to-run)])
  (define tag (car s))
  (define file (cadr s))
  (printf "--- [~a] ~a ---~n" tag file)
  (define full-path (build-path (current-directory) file))
  (if (file-exists? full-path)
      (let* ([cmd (format "raco test ~a 2>&1" file)]
             [output (with-output-to-string (lambda () (system cmd)))])
        (display output)
        (define passed?
          (and (string-contains? output "tests passed") (not (string-contains? output "failure"))))
        (set! results (cons (list tag file passed? output) results)))
      (begin
        (printf "  SKIP: file not found~n")
        (set! results (cons (list tag file 'missing "") results))))
  (printf "~n"))

;; Summary
(printf "=== SUMMARY ===~n")
(define pass-count (length (filter (lambda (r) (eq? (caddr r) #t)) results)))
(define fail-count (length (filter (lambda (r) (eq? (caddr r) #f)) results)))
(define skip-count (length (filter (lambda (r) (eq? (caddr r) 'missing)) results)))
(printf "  PASS: ~a  FAIL: ~a  SKIP: ~a  TOTAL: ~a~n"
        pass-count
        fail-count
        skip-count
        (length results))

(for ([r (in-list (reverse results))])
  (define tag (car r))
  (define status (caddr r))
  (define marker
    (cond
      [(eq? status #t) "✅ PASS"]
      [(eq? status #f) "❌ FAIL"]
      [else "⏭️ SKIP"]))
  (printf "  ~a ~a~n" marker tag))

(if (> fail-count 0)
    (exit 1)
    (exit 0))

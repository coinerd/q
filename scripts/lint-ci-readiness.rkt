#!/usr/bin/env racket
#lang racket/base

;; scripts/lint-ci-readiness.rkt — CI readiness lint.
;;
;; Checks for local-only stray files, symlinks, and gitignore hygiene
;; that could cause local-vs-CI metric differences.
;;
;; Usage:
;;   cd q/ && racket scripts/lint-ci-readiness.rkt
;;
;; Exit 0 if clean, 1 if issues found.

(require racket/file
         racket/list
         racket/string
         racket/port
         racket/system)

(define errors '())
(define warnings '())

(define (add-error! msg)
  (set! errors (cons msg errors)))
(define (add-warning! msg)
  (set! warnings (cons msg warnings)))

;; --- Check 1: Symlinks in tracked directories ---

(define (check-symlinks)
  (define tracked-dirs
    '("agent" "cli"
              "extensions"
              "interfaces"
              "llm"
              "runtime"
              "sandbox"
              "tools"
              "tui"
              "util"
              "scripts"
              "tests"))
  (for ([dir (in-list tracked-dirs)]
        #:when (directory-exists? dir))
    (for ([f (in-directory dir)])
      (when (and (file-exists? f) (link-exists? f))
        (add-error! (format "ERROR: symlink in tracked dir: ~a" f))))))

;; --- Check 2: Untracked files that look like they should be ignored ---

(define (check-untracked)
  (define status-out
    (with-output-to-string
     (lambda () (void (system "git status --porcelain --ignore-submodules 2>/dev/null")))))
  (define lines
    (filter (lambda (l) (and (string-prefix? l "??") (not (string=? l ""))))
            (string-split status-out "\n")))
  (for ([line (in-list lines)])
    (define file (string-trim (substring line 3)))
    (when (string-suffix? file ".rkt")
      (add-warning! (format "WARNING: untracked .rkt file: ~a" file)))))

;; --- Check 3: .q/extensions/ symlinks (local-only) ---

(define (check-dot-q-extensions)
  (define ext-dir ".q/extensions")
  (when (directory-exists? ext-dir)
    (for ([f (directory-list ext-dir)])
      (define full (build-path ext-dir f))
      (when (link-exists? full)
        (add-warning! (format "WARNING: symlink in .q/extensions/ (local-only): ~a" full))))))

;; --- Check 4: gitignore covers common local-only paths ---

(define (check-gitignore)
  (unless (file-exists? ".gitignore")
    (add-error! "ERROR: .gitignore not found")
    (void))
  (define content (file->string ".gitignore"))
  (define required-entries
    '((".q/" "user config directory") ("compiled/" "compiled bytecode")
                                      (".planning/" "planning artifacts")
                                      ("*.zo" "compiled modules")))
  (for ([entry (in-list required-entries)])
    (unless (string-contains? content (car entry))
      (add-error! (format "ERROR: .gitignore missing entry for ~a (~a)" (car entry) (cadr entry))))))

;; --- Main ---

(define (main)
  (unless (file-exists? "main.rkt")
    (printf "ERROR: Run from the q/ directory (main.rkt not found).~n")
    (exit 1))

  (printf "=== CI Readiness Lint ===~n~n")

  (check-symlinks)
  (check-untracked)
  (check-dot-q-extensions)
  (check-gitignore)

  ;; Print results
  (for ([e (reverse errors)])
    (displayln e))
  (for ([w (reverse warnings)])
    (displayln w))

  (printf "~n---~n")
  (printf "~a errors, ~a warnings~n" (length errors) (length warnings))

  (if (null? errors)
      (begin
        (displayln "CI readiness: OK")
        (exit 0))
      (begin
        (displayln "CI readiness: FAIL")
        (exit 1))))

(main)

#lang racket/base

;; extensions/gsd/bash-detect.rkt — Bash file-reading detection
;;
;; Wave 3b of v0.21.0: Detects when bash is used to read files,
;; bypassing the read tool. DD-2: No budgets, detect bash bypass.
;;
;; This catches indirect file reads (sed, cat, head, tail, awk, etc.)
;; that circumvent the read tool's tracking.

(require racket/string)

(provide detect-file-read-bash
         FILE-READ-PATTERNS)

;; ============================================================
;; Patterns
;; ============================================================

;; Regex patterns for bash commands that read file contents.
;; Note: Racket #rx does not support \s — use literal spaces.
(define FILE-READ-PATTERNS
  ;; sed print commands
  (list #rx"sed +-n"
        ;; cat with file argument
        #rx"cat +"
        ;; head/tail
        #rx"head +"
        #rx"tail +"
        ;; pagers (less common in scripts but still reads)
        #rx"less +"
        #rx"more +"
        ;; python open() — catches python -c 'open(...'
        #rx"python[0-9.]* +-c.*open"
        #rx"python[0-9.]* +-c.*\\.read"
        ;; awk reads files
        #rx"awk +"
        ;; grep with $ (reading file content, not just filename matching)
        #rx"grep +.*\\$"
        ;; perl reading files
        #rx"perl +-ne"
        #rx"perl +-e.*open"))

;; Commands that should NOT be flagged even if they contain keywords.
(define SAFE-PREFIXES
  (list "git "
        "ls "
        "find "
        "mkdir "
        "echo "
        "test "
        "[ "
        "rm "
        "cp "
        "mv "
        "chmod "
        "touch "
        "which "
        "env "
        "raco "
        "racket "
        "dune "
        "make "
        "cargo "
        "npm "))

;; ============================================================
;; Detection
;; ============================================================

;; Detect if a bash command is being used to read file contents.
;; Returns (values is-file-read? detail-string)
;; detail is #f if not detected, or a description of what was matched.
(define (detect-file-read-bash cmd-string)
  (cond
    [(not (string? cmd-string)) (values #f #f)]
    [(string=? (string-trim cmd-string) "") (values #f #f)]
    ;; Check safe prefixes first — allow git, ls, find, etc.
    [(safe-command? cmd-string) (values #f #f)]
    ;; Check against read patterns
    [else
     (define matched (find-matching-pattern cmd-string))
     (if matched
         (values #t matched)
         (values #f #f))]))

;; ============================================================
;; Internal
;; ============================================================

(define (safe-command? cmd)
  (for/or ([prefix SAFE-PREFIXES])
    (string-prefix? (string-trim cmd) prefix)))

(define (find-matching-pattern cmd)
  (for/or ([pat FILE-READ-PATTERNS])
    (define m (regexp-match pat cmd))
    (if m
        (format "Matched pattern: ~a" (object-name pat))
        #f)))

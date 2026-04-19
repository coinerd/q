#lang racket

;;; lint-release-notes.rkt — CI tool that validates release notes follow
;;; the required template for a given version entry in a changelog.

(require racket/string
         racket/file
         racket/port)

;; ---------------------------------------------------------------------------
;; Required section patterns (case-insensitive)
;; ---------------------------------------------------------------------------

(define required-section-patterns
  '("### User-Visible Changes" "### Features"
                               "### Bug Fixes"
                               "### Breaking / Behavior Changes"
                               "### Migration Notes"
                               "### Testing"
                               "### Operational / Release"))

;; Sections that are *mandatory* for every version entry.
;; The first group is an OR: at least one of them must appear.
(define user-visible-group '("### User-Visible Changes" "### Features" "### Bug Fixes"))

(define mandatory-solo-sections
  '("### Breaking / Behavior Changes" "### Migration Notes"
                                      "### Testing"
                                      "### Operational / Release"))

;; ---------------------------------------------------------------------------
;; Parsing helpers
;; ---------------------------------------------------------------------------

;; Extract the version block for `ver` from the full changelog text.
;; A version block starts with a heading like "## 0.4.0" and ends just
;; before the next "## " heading (or EOF).
(define (extract-version-block text ver)
  (define lines (string-split text "\n"))
  (define target (string-append "## " ver))
  (define target-alt (string-append "## v" ver)) ; allow "v0.4.0" too
  (let loop ([lst lines]
             [in-block? #f]
             [acc '()])
    (cond
      [(null? lst)
       (if in-block?
           (string-join (reverse acc) "\n")
           #f)]
      [else
       (define line (car lst))
       (define next-heading?
         (and (string-prefix? (string-trim line) "## ")
              (not (string-prefix? (string-trim line) "### "))))
       (cond
         [(and (not in-block?) next-heading?)
          (define trimmed (string-trim line))
          (if (or (string=? trimmed target) (string=? trimmed target-alt))
              (loop (cdr lst) #t '())
              (loop (cdr lst) #f '()))]
         ;; hit the next version heading — stop
         [(and in-block? next-heading?) (string-join (reverse acc) "\n")]
         [in-block? (loop (cdr lst) #t (cons line acc))]
         [else (loop (cdr lst) #f '())])])))

;; Collect all ### headings present in a block (normalized to lowercase,
;; stripped of trailing whitespace).
(define (extract-section-headers block)
  (for/list ([line (in-list (string-split block "\n"))]
             #:when (string-prefix? (string-trim line) "### "))
    (string-downcase (string-trim line))))

;; ---------------------------------------------------------------------------
;; Validation
;; ---------------------------------------------------------------------------

(define (validate-release-notes block)
  (define headers (extract-section-headers block))
  (define (has? pattern)
    (member (string-downcase pattern) headers))

  (define errors '())

  ;; At least one user-visible-group section must exist
  (unless (ormap has? user-visible-group)
    (set! errors (cons (format "Missing one of: ~a" (string-join user-visible-group ", ")) errors)))

  ;; Each mandatory solo section must exist
  (for ([section (in-list mandatory-solo-sections)])
    (unless (has? section)
      (set! errors (cons (format "Missing required section: ~a" section) errors))))

  (reverse errors))

;; ---------------------------------------------------------------------------
;; Main entry points (for programmatic use and CLI)
;; ---------------------------------------------------------------------------

(define (lint-changelog changelog-path version)
  (define text (file->string changelog-path))
  (define block (extract-version-block text version))
  (cond
    [(not block) (list (format "Version '~a' not found in ~a" version changelog-path))]
    [else (validate-release-notes block)]))

;; CLI -----------------------------------------------------------------------

(define cli-file (make-parameter "CHANGELOG.md"))
(define cli-version (make-parameter #f))
(define cli-check (make-parameter #f))

(define (parse-args args)
  (let loop ([rest args])
    (cond
      [(null? rest) (void)]
      [(and (>= (length rest) 2) (equal? (car rest) "--file"))
       (cli-file (cadr rest))
       (loop (cddr rest))]
      [(and (>= (length rest) 2) (equal? (car rest) "--version"))
       (cli-version (cadr rest))
       (loop (cddr rest))]
      [(equal? (car rest) "--check")
       (cli-check #t)
       (loop (cdr rest))]
      [else
       (eprintf "Unknown option: ~a\n" (car rest))
       (exit 2)])))

(module+ main
  (parse-args (vector->list (current-command-line-arguments)))
  (unless (cli-version)
    (eprintf "Error: --version is required\n")
    (exit 2))
  (unless (file-exists? (cli-file))
    (eprintf "Error: file not found: ~a\n" (cli-file))
    (exit 2))
  (define errors (lint-changelog (cli-file) (cli-version)))
  (cond
    [(null? errors) (printf "PASSED: ~a version ~a\n" (cli-file) (cli-version))]
    [else
     (for ([e (in-list errors)])
       (printf "ERROR: ~a\n" e))
     (when (cli-check)
       (exit 1))]))

;; Provide public API for testing
(provide lint-changelog
         extract-version-block
         validate-release-notes
         required-section-patterns)

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

;; Extract exactly one version block. Canonical release metadata may follow
;; the version, but partial and duplicate version headings fail closed.
(define (extract-version-block text ver)
  (define lines (string-split text "\n"))
  (define heading-rx #px"^## v?([0-9]+\\.[0-9]+\\.[0-9]+(?:-[0-9A-Za-z][0-9A-Za-z.-]*)?)(?:\\s|$)")
  (define target-indexes
    (for/list ([line (in-list lines)]
               [index (in-naturals)]
               #:when (let ([m (regexp-match heading-rx (string-trim line))])
                        (and m (string=? (cadr m) ver))))
      index))
  (and (= (length target-indexes) 1)
       (let* ([start (add1 (car target-indexes))]
              [tail (drop lines start)]
              [block-lines
               (takef tail (lambda (line) (not (regexp-match? heading-rx (string-trim line)))))])
         (string-join block-lines "\n"))))

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

  ;; Reject the exact false wording that escaped earlier release checks. If a
  ;; release discusses the disputed counts, require their distinct populations
  ;; and retained evidence to be named rather than presenting one denominator.
  (when (regexp-match? #px"(?i:working directory is always canonical)" block)
    (set! errors (cons "False cwd contract: 'working directory is always canonical'" errors)))
  (define discusses-disputed-counts?
    (regexp-match?
     #px"(?i:(?:B53|T45|P44|(?:44|45|53)(?:[- ]file|\\s+passing|\\s+of\\s+(?:44|45|53)|/(?:44|45|53))))"
     block))
  (when (and discusses-disputed-counts?
             (not (and (regexp-match? #px"(?i:authoritative.*53[- ]file)" block)
                       (regexp-match? #px"(?i:release-tracked.*45[- ]file)" block)
                       (regexp-match? #px"(?i:TS7)" block)
                       (regexp-match? #px"v0\\.99\\.75-W0-EVIDENCE-FREEZE\\.md" block))))
    (set! errors (cons "Unreconciled test-count terminology" errors)))

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
  ;; F-15 (#8753): Auto-detect version from util/version.rkt if not provided.
  (unless (cli-version)
    (define version-text
      (with-handlers ([exn:fail? (lambda (_) #f)])
        (file->string "util/version.rkt")))
    (when version-text
      (define m (regexp-match #rx"define q-version \"([^\"]+)\"" version-text))
      (when m
        (cli-version (cadr m)))))
  (unless (cli-version)
    (eprintf "Error: --version is required (or util/version.rkt must exist for auto-detection)\n")
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

#lang racket

;;; lint-release-notes.rkt — CI tool that validates release notes follow
;;; the required template for a given version entry in a changelog.
;;; Additionally (BUG-0049): every BUG-NNNN token in the entry is
;;; cross-checked against the bug registry (.planning/bugs/) — unknown
;;; ids, status contradictions (cited as fixed while the registry still
;;; says open/planned/partial), and severity mismatches are errors.

(require racket/string
         racket/file
         racket/path
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
;; Bug registry cross-check (BUG-0049)
;; ---------------------------------------------------------------------------

;; The authoritative bug registry lives at .planning/bugs/INDEX.md
;; (normally one level above the git root, i.e. at the project base).
;; Auto-discovery covers both layouts (registry inside the repo root or at
;; the project base); tests override the location via the
;; bug-registry-path parameter. When no registry can be located the
;; cross-check is skipped: it requires the planning tree, and CI checkouts
;; without it must not fail on its absence.

(define bug-registry-path (make-parameter #f))

(define script-dir
  (simplify-path
   (path-only
    (resolved-module-path-name
     (variable-reference->resolved-module-path (#%variable-reference))))))

(define (resolve-bug-registry)
  (cond
    [(bug-registry-path)
     => (lambda (p) (and (file-exists? p) (simplify-path p)))]
    [else
     (define candidates
       (list (build-path script-dir 'up ".planning" "bugs" "INDEX.md")
             (build-path script-dir 'up 'up ".planning" "bugs" "INDEX.md")))
     (for/or ([c (in-list candidates)]
              #:when (file-exists? c))
       (simplify-path c))]))

(struct bug-entry (id severity status-head) #:transparent)

(define severity-levels '("critical" "high" "medium" "low"))

(define status-head-rx
  #px"^(reported|triaged|in-progress|fixed|validated|closed|wontfix|duplicate|deferred|partial|open|planned)(?![a-z])")

;; Recognize a table cell that *is* a status (its first word is a status
;; head): "reported", "planned v1.00.22 W5", "fixed v1.00.15 (#9506)",
;; "partial: half landed", ... Returns the canonical head word or #f.
(define (parse-status-cell cell)
  (define m (regexp-match status-head-rx (string-downcase (string-trim cell))))
  (and m (cadr m)))

;; Recognize a table cell that is exactly a severity level.
(define (parse-severity-cell cell)
  (define s (string-downcase (string-trim cell)))
  (and (member s severity-levels) s))

;; Rows look like:
;;   | BUG-0049 | 2026-08-27 | Title | component | low | planned v1.00.22 W5 | — | [file] |
;; Column alignment is NOT guaranteed across the registry history (status
;; and "Fixed in" sometimes share one cell, file links repeat, ...), so
;; severity/status are recognized by cell shape rather than by position.
(define (parse-bug-registry text)
  (define registry (make-hash))
  (for ([line (in-list (string-split text "\n"))]
        #:when (regexp-match? #px"^\\s*\\|\\s*BUG-\\d{4}\\s*\\|" line))
    (define cells (map string-trim (string-split line "|")))
    ;; string-split drops empty fields, so cells = (id reported title component severity status fixed-in file ...)
    (define id (and (pair? cells) (car cells)))
    (when (and id (regexp-match? #px"^BUG-\\d{4}$" id))
      (hash-set! registry id
                 (bug-entry id
                            (ormap parse-severity-cell (cdr cells))
                            (ormap parse-status-cell (cdr cells))))))
  registry)

(define bug-token-rx #px"BUG-\\d{4}")

;; Registry statuses that contradict a changelog claim that a bug is fixed.
(define not-fixed-heads
  '("reported" "triaged" "in-progress" "partial" "open" "planned"
    "deferred" "duplicate" "wontfix"))

;; A changelog line claims the bug is fixed/resolved/closed.
(define fix-claim-rx
  #px"(?i:\\b(?:fix|fixes|fixed|fixing|resolve|resolves|resolved|resolving|close|closes|closed|closing)\\b)")

;; Severity the changelog attributes to a token: "critical BUG-0102" or
;; "BUG-0102 (severity: critical)". Returns the level or #f.
(define (claimed-severity-for tok line)
  (define before
    (regexp-match (regexp (format "(?i:(critical|high|medium|low)[ \\t]+~a)"
                                  (regexp-quote tok)))
                  line))
  (define after
    (regexp-match (regexp (format "(?i:~a[ \\t]*\\([ \\t]*(severity[ \\t]*:?[ \\t]*)?(critical|high|medium|low)\\))"
                                  (regexp-quote tok)))
                  line))
  (cond [before (string-downcase (cadr before))]
        [(and after (caddr after)) (string-downcase (caddr after))]
        [else #f]))

(define (check-bug-token tok line entry registry-path)
  (cond
    [(not entry)
     (format "bug-registry: unknown bug reference — ~a is not in ~a. Register the bug or correct the id."
             tok registry-path)]
    [else
     (define contradicted-status
       (and (regexp-match? fix-claim-rx line)
            (bug-entry-status-head entry)
            (member (bug-entry-status-head entry) not-fixed-heads)
            (bug-entry-status-head entry)))
     (define claimed-sev (claimed-severity-for tok line))
     (define sev-mismatch?
       (and claimed-sev
            (bug-entry-severity entry)
            (not (string=? claimed-sev (bug-entry-severity entry)))))
     (cond
       [contradicted-status
        (format "bug-registry: status contradiction — changelog claims ~a is fixed but the registry status is '~a' (~a). Close the bug in the registry or correct the entry."
                tok contradicted-status registry-path)]
       [sev-mismatch?
        (format "bug-registry: severity mismatch — changelog calls ~a '~a' but the registry says '~a' (~a). Align changelog and registry."
                tok claimed-sev (bug-entry-severity entry) registry-path)]
       [else #f])]))

;; Cross-check every BUG-NNNN token in a version block against the bug
;; registry: unknown ids, status contradictions (claimed fixed while the
;; registry says open/planned/partial/...), and severity mismatches are
;; named errors. Returns '() when no registry is locatable.
(define (validate-bug-refs block)
  (define registry-path (resolve-bug-registry))
  (cond
    [(not registry-path) '()]
    [else
     (define registry (parse-bug-registry (file->string registry-path)))
     (define errors
       (for*/list ([line (in-list (string-split block "\n"))]
                   [tok (in-list (remove-duplicates (regexp-match* bug-token-rx line) string=?))])
         (check-bug-token tok line (hash-ref registry tok #f) registry-path)))
     (remove-duplicates (filter values errors))]))

;; ---------------------------------------------------------------------------
;; Main entry points (for programmatic use and CLI)
;; ---------------------------------------------------------------------------

(define (lint-changelog changelog-path version)
  (define text (file->string changelog-path))
  (define block (extract-version-block text version))
  (cond
    [(not block) (list (format "Version '~a' not found in ~a" version changelog-path))]
    [else (append (validate-release-notes block)
                  (validate-bug-refs block))]))

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
          required-section-patterns
          validate-bug-refs
          bug-registry-path
          parse-bug-registry)

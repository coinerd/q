#lang racket/base

;; scripts/status-result.rkt — Result-type boundary for README status checks
;;
;; W5 (#8418): Expected-failure/result-boundary pilot.
;;
;; This module demonstrates the result-type pattern: instead of communicating
;; failures via printf + exit codes (ad-hoc, unparseable), the check function
;; returns a structured result that callers can dispatch on programmatically.
;;
;; Key design decisions:
;;   1. Each failure mode is a distinct struct variant — no magic strings
;;   2. The check function is pure — takes pre-read data, no I/O
;;   3. The CLI layer formats results for human consumption
;;   4. #:transparent structs enable equal?-based test assertions
;;
;; Pattern applicability:
;;   - Lint scripts with multiple distinct failure modes
;;   - Build verification steps with structured outcomes
;;   - Any "printf + exit 1" error path that callers need to handle

(require racket/match
         racket/string)

;; ---------------------------------------------------------------------------
;; Result types
;; ---------------------------------------------------------------------------

(provide (struct-out status-ok)
         (struct-out status-version-mismatch)
         (struct-out status-description-mismatch)
         (struct-out status-missing-section)
         (struct-out status-file-not-found)
         status-check-result?
         status-result-kind
         status-ok?
         format-status-result
         status-result-exit-code
         check-readme-status)

;; Everything matches: version and description agree.
;; Fields: version (string), description (string)
(struct status-ok (version description) #:transparent)

;; README Status version differs from q-version.
;; Fields: found (string), expected (string), path (string)
(struct status-version-mismatch (found expected path) #:transparent)

;; Version matches but description differs from CHANGELOG.
;; Fields: found (string), expected (string), version (string), path (string)
(struct status-description-mismatch (found expected version path) #:transparent)

;; No ## Status section found in README.
;; Fields: path (string)
(struct status-missing-section (path) #:transparent)

;; README file does not exist.
;; Fields: path (string)
(struct status-file-not-found (path) #:transparent)

;; ---------------------------------------------------------------------------
;; Predicates and discriminator
;; ---------------------------------------------------------------------------

;; Union predicate: true for any status-check result variant.
(define (status-check-result? v)
  (or (status-ok? v)
      (status-version-mismatch? v)
      (status-description-mismatch? v)
      (status-missing-section? v)
      (status-file-not-found? v)))

;; Discriminator: returns a symbol identifying the result variant.
(define (status-result-kind r)
  (cond
    [(status-ok? r) 'ok]
    [(status-version-mismatch? r) 'version-mismatch]
    [(status-description-mismatch? r) 'description-mismatch]
    [(status-missing-section? r) 'missing-section]
    [(status-file-not-found? r) 'file-not-found]
    [else (error 'status-result-kind "not a status-check-result: ~a" r)]))

;; Exit code for a result: 0 for ok, 1 for any failure.
(define (status-result-exit-code r)
  (if (status-ok? r) 0 1))

;; ---------------------------------------------------------------------------
;; Formatting
;; ---------------------------------------------------------------------------

;; Format a result for human-readable CLI output.
(define (format-status-result r)
  (match r
    [(status-ok version _)
     (format "OK: README Status block version (~a) and description match CHANGELOG" version)]
    [(status-version-mismatch found expected _)
     (format "MISMATCH: README Status says v~a, q-version is v~a" found expected)]
    [(status-description-mismatch found expected _ _)
     (format "MISMATCH: Status description differs from CHANGELOG~n  Status: ~a~n  Expected: ~a"
             (truncate-string found 80)
             (truncate-string expected 80))]
    [(status-missing-section path) (format "MISSING: No version found in Status section of ~a" path)]
    [(status-file-not-found path) (format "ERROR: ~a not found" path)]))

(define (truncate-string s max-len)
  (if (> (string-length s) max-len)
      (substring s 0 max-len)
      s))

;; ---------------------------------------------------------------------------
;; Pure check function
;; ---------------------------------------------------------------------------

;; Internal parsing helpers (self-contained, no I/O)

(define status-heading-rx #rx"^## Status")
(define next-heading-rx #rx"^## ")
(define status-version-rx #rx"\\*\\*v([0-9]+\\.[0-9]+\\.[0-9]+)\\*\\*")

;; Find the version string in the Status section.
;; Returns (cons version-string line-index) or #f.
(define (find-status-version lines)
  (define in-section? #f)
  (for/or ([line (in-list lines)])
    (cond
      [(regexp-match? status-heading-rx line)
       (set! in-section? #t)
       #f]
      [(and in-section? (regexp-match? next-heading-rx line)) #f]
      [(and in-section? (regexp-match status-version-rx line))
       (define m (regexp-match status-version-rx line))
       (and m (cadr m))]
      [else #f])))

;; Find the first status entry line (the line containing **vX.Y.Z**).
(define (find-status-entry-line lines)
  (define in-section? #f)
  (for/or ([line (in-list lines)])
    (cond
      [(regexp-match? status-heading-rx line)
       (set! in-section? #t)
       #f]
      [(and in-section? (regexp-match? next-heading-rx line)) #f]
      [(and in-section? (regexp-match? status-version-rx line)) line]
      [else #f])))

;; Normalize an entry for comparison: collapse whitespace, strip version prefix.
(define (normalize-entry s)
  (define no-ws (regexp-replace* #rx"[\\s]+" (string-trim s) " "))
  (define stripped (regexp-replace #rx"^\\*\\*v[0-9.]+\\*\\* — " no-ws ""))
  (string-trim stripped))

;; Main check: given README lines, q-version, CHANGELOG version/summary, and path,
;; returns a status-check-result.
;; Pure function — no I/O, no side effects.
(define (check-readme-status lines q-version cl-version cl-summary path)
  (define found-version (find-status-version lines))
  (cond
    ;; No version found in Status section
    [(not found-version) (status-missing-section path)]
    ;; Version mismatch
    [(not (equal? found-version q-version)) (status-version-mismatch found-version q-version path)]
    ;; Version matches — check description if CHANGELOG data available
    [(and cl-version cl-summary)
     (define status-line (find-status-entry-line lines))
     (define expected-entry (format "**v~a** — ~a" cl-version cl-summary))
     (define norm-status (and status-line (normalize-entry status-line)))
     (define norm-expected (normalize-entry expected-entry))
     (if (and norm-status (string=? norm-status norm-expected))
         (status-ok q-version (or cl-summary ""))
         (status-description-mismatch (or status-line "") expected-entry q-version path))]
    ;; No CHANGELOG data — version match is sufficient
    [else (status-ok q-version "")]))

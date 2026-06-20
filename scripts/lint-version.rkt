#!/usr/bin/env racket
#lang racket/base

;; lint-version.rkt — Version-consistency linter.
;;
;; Canonical source: util/version.rkt  (define q-version "X.Y.Z")
;;
;; Checks:
;;   1. info.rkt version matches canonical
;;   2. README.md version badge matches canonical
;;   3. README.md verify snippet matches canonical
;;   4. All .md files have consistent version strings
;;
;; Exit 0 if clean, 1 if any mismatch found.
;; Run from the q/ directory.

(require racket/file
         racket/list
         racket/path
         racket/string
         racket/port
         "version-guard.rkt"
         "version-surface.rkt"
         "lint-version-io.rkt")

;; W6 (#8419): Pure check functions are exported for unit testing.
;; I/O wrappers use lint-version-io.rkt parameters for injectable file access.
(provide check-info-content
         check-readme-content
         check-changelog-content
         find-mismatched-versions
         check-info-rkt
         check-readme-badge
         check-md-files
         check-changelog-integrity)

;; ---------------------------------------------------------------------------
;; Parsing helpers
;; ---------------------------------------------------------------------------

;; Version parsing centralized in version-surface.rkt (W2 #8415).
;; Locally re-exported under original names for backward compatibility.
(define parse-q-version parse-q-version-from-content)
(define parse-info-version parse-info-version-from-content)

(define VERSION-PAT #rx"[0-9]+\\.[0-9]+\\.[0-9]+")

;; ---------------------------------------------------------------------------
;; File scanning
;; ---------------------------------------------------------------------------

(define (skip-path? p)
  (define s (path->string p))
  (or (string-contains? s "/compiled/")
      (string-contains? s "/.git/")
      (string-contains? s "/.planning/")
      (string-contains? s "/.pi/")
      (and (filename-extension p) (equal? (bytes->string/utf-8 (filename-extension p)) "zo"))))

(define (collect-md-files base-dir)
  (sort (for/list ([f (in-directory base-dir)]
                   #:when (and (not (skip-path? f))
                               (let ([ext (filename-extension f)])
                                 (and ext (equal? (bytes->string/utf-8 ext) "md")))))
          f)
        path<?))

;; Return list of (list line-num version-string) for each non-matching version.
(define (find-mismatched-versions lines canonical-version filename)
  (define fname
    (if (path? filename)
        (path->string (file-name-from-path filename))
        filename))
  (define skip-file?
    (or (equal? fname "README.md")
        (equal? fname "CHANGELOG.md")
        (equal? fname "CHANGELOG-ARCHIVE.md")
        (equal? fname "releasing.md")
        (equal? fname "why-q.md")
        (equal? fname "api-stability.md")
        (equal? fname "compatibility-matrix.md")
        (equal? fname "package-registry-spec.md")
        (equal? fname "publish-verify-workflow.md")
        (equal? fname "sdk-rpc-catalog.md")
        (string-contains? (if (path? filename)
                              (path->string filename)
                              filename)
                          "examples/README.md")
        (string-contains? (if (path? filename)
                              (path->string filename)
                              filename)
                          "docs/tutorials/")
        (string-contains? (if (path? filename)
                              (path->string filename)
                              filename)
                          "docs/ecosystem/")
        (string-contains? (if (path? filename)
                              (path->string filename)
                              filename)
                          "docs/demos/")
        (equal? fname "EXTENSIONS_INVENTORY.md")
        (equal? fname "browser-guide.md")
        (equal? fname "security.md")
        (string-contains? (if (path? filename)
                              (path->string filename)
                              filename)
                          "docs/reports/")
        (string-contains? (if (path? filename)
                              (path->string filename)
                              filename)
                          "docs/adr/")))
  ;; historical-line? is now provided by version-guard.rkt
  ;; Skip lines inside fenced code blocks (``` ... ```)
  (define in-code-block #f)
  (define (code-fence-line? line)
    (define trimmed (string-trim line))
    (string-prefix? trimmed "```"))
  (append* (for/list ([line (in-list lines)]
                      [lineno (in-naturals 1)])
             (when (code-fence-line? line)
               (set! in-code-block (not in-code-block)))
             (define matches (regexp-match* VERSION-PAT line))
             (for/list ([v (in-list matches)]
                        #:when (and (not (equal? v canonical-version))
                                    (not skip-file?)
                                    (not (historical-line? line))
                                    (not in-code-block)))
               (list lineno v)))))

;; ---------------------------------------------------------------------------
;; CHANGELOG integrity threshold
;; ---------------------------------------------------------------------------

(define MIN-UNIQUE-VERSIONS 50)

;; ---------------------------------------------------------------------------
;; Pure check functions (no I/O, no side effects)
;; W6 (#8419): Extracted from I/O wrappers for testability.
;; Each returns a list of errors: (list path lineno found expected) or '()
;; ---------------------------------------------------------------------------

;; Pure: check info.rkt content for version match.
(define (check-info-content content canonical)
  (define info-v (parse-info-version content))
  (cond
    [(not info-v) (list (list 'info.rkt 0 "<unparseable>" canonical))]
    [(equal? info-v canonical) '()]
    [else (list (list 'info.rkt 0 info-v canonical))]))

;; Pure: check README content for badge + verify snippet version match.
(define (check-readme-content content canonical)
  (define badge-m (regexp-match #rx"badge/version-([0-9]+\\.[0-9]+\\.[0-9]+)" content))
  (define badge-v (and badge-m (cadr badge-m)))
  (define verify-m (regexp-match #rx"q version ([0-9]+\\.[0-9]+\\.[0-9]+)" content))
  (define verify-v (and verify-m (cadr verify-m)))
  (append (if (and badge-v (not (equal? badge-v canonical)))
              (list (list 'README.md 0 badge-v canonical))
              '())
          (if (and verify-v (not (equal? verify-v canonical)))
              (list (list 'README.md 0 verify-v canonical))
              '())))

;; Pure: check CHANGELOG content for version header count integrity.
(define (check-changelog-content content)
  (define lines (string-split content "\n"))
  (define versions
    (for/list ([line (in-list lines)])
      (define m (regexp-match #rx"^## v([0-9]+\\.[0-9]+\\.[0-9]+)" line))
      (and m (cadr m))))
  (define unique (remove-duplicates (filter (lambda (x) x) versions)))
  (if (< (length unique) MIN-UNIQUE-VERSIONS)
      (list (list 'CHANGELOG.md
                  0
                  (format "~a unique versions (< ~a)" (length unique) MIN-UNIQUE-VERSIONS)
                  (format ">= ~a unique versions" MIN-UNIQUE-VERSIONS)))
      '()))

;; ---------------------------------------------------------------------------
;; I/O wrappers (use parameters from lint-version-io.rkt)
;; ---------------------------------------------------------------------------

(define (check-info-rkt canonical)
  (define exists? (current-lint-file-exists?))
  (define read-string (current-lint-file->string))
  (define info-path (build-path (current-directory) "info.rkt"))
  (cond
    [(not (exists? info-path))
     (displayln "  WARN: info.rkt not found")
     '()]
    [else
     (define info-content (read-string info-path))
     (define info-v (parse-info-version info-content))
     (cond
       [(not info-v)
        (displayln "  ERROR: info.rkt — could not parse version")
        (list (list info-path 0 "<unparseable>" canonical))]
       [(equal? info-v canonical)
        (printf "  info.rkt: OK (~a)~n" info-v)
        '()]
       [else
        (printf "  ERROR: info.rkt: ~a ≠ canonical ~a~n" info-v canonical)
        (list (list info-path 0 info-v canonical))])]))

(define (check-readme-badge canonical)
  (define exists? (current-lint-file-exists?))
  (define read-string (current-lint-file->string))
  (define readme-path (build-path (current-directory) "README.md"))
  (cond
    [(not (exists? readme-path))
     (displayln "  WARN: README.md not found")
     '()]
    [else
     (define content (read-string readme-path))
     (define badge-m (regexp-match #rx"badge/version-([0-9]+\\.[0-9]+\\.[0-9]+)" content))
     (define badge-v (and badge-m (cadr badge-m)))
     (define verify-m (regexp-match #rx"q version ([0-9]+\\.[0-9]+\\.[0-9]+)" content))
     (define verify-v (and verify-m (cadr verify-m)))
     (append (if (and badge-v (not (equal? badge-v canonical)))
                 (begin
                   (printf "  ERROR: README badge: ~a ≠ canonical ~a~n" badge-v canonical)
                   (list (list readme-path 0 badge-v canonical)))
                 (begin
                   (when badge-v
                     (printf "  README badge: OK (~a)~n" badge-v))
                   '()))
             (if (and verify-v (not (equal? verify-v canonical)))
                 (begin
                   (printf "  ERROR: README verify: ~a ≠ canonical ~a~n" verify-v canonical)
                   (list (list readme-path 0 verify-v canonical)))
                 (begin
                   (when verify-v
                     (printf "  README verify: OK (~a)~n" verify-v))
                   '())))]))

(define (check-md-files canonical)
  (define read-string (current-lint-file->string))
  (define custom-paths (current-lint-read-md-paths))
  (define md-files
    (if custom-paths
        (custom-paths (current-directory))
        (collect-md-files (current-directory))))
  (append* (for/list ([f (in-list md-files)])
             (define lines (string-split (read-string f) "\n"))
             (define mismatches (find-mismatched-versions lines canonical f))
             (for/list ([m (in-list mismatches)])
               (printf "  ERROR: ~a:~a: \"~a\" ≠ canonical ~a~n"
                       (if (path? f)
                           (path->string f)
                           f)
                       (first m)
                       (second m)
                       canonical)
               (list f (first m) (second m))))))

(define (check-changelog-integrity)
  ;; Validate CHANGELOG.md has sufficient unique version headers.
  ;; Returns list of errors (empty = OK).
  (define exists? (current-lint-file-exists?))
  (define read-string (current-lint-file->string))
  (define changelog-path (build-path (current-directory) "CHANGELOG.md"))
  (cond
    [(not (exists? changelog-path))
     (displayln "  WARN: CHANGELOG.md not found")
     '()]
    [else
     (define content (read-string changelog-path))
     (define errors (check-changelog-content content))
     (define lines (string-split content "\n"))
     (define versions
       (for/list ([line (in-list lines)])
         (define m (regexp-match #rx"^## v([0-9]+\\.[0-9]+\\.[0-9]+)" line))
         (and m (cadr m))))
     (define unique (remove-duplicates (filter (lambda (x) x) versions)))
     (printf "  CHANGELOG.md: ~a unique version headers~n" (length unique))
     (if (null? errors)
         (begin
           (displayln "  CHANGELOG integrity: OK")
           '())
         (begin
           (printf
            "  ERROR: CHANGELOG.md appears corrupted (< ~a unique versions). ~
                    Historical entries may have been overwritten.~n"
            MIN-UNIQUE-VERSIONS)
           errors))]))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (main)
  ;; --- Read canonical version from util/version.rkt ---
  (define exists? (current-lint-file-exists?))
  (define read-string (current-lint-file->string))
  (define util-path (build-path (current-directory) "util" "version.rkt"))
  (unless (exists? util-path)
    (displayln "ERROR: util/version.rkt not found in current directory")
    (exit 1))

  (define util-content (read-string util-path))
  (define canonical (parse-q-version util-content))
  (unless canonical
    (displayln "ERROR: could not parse q-version from util/version.rkt")
    (exit 1))

  (printf "Canonical version: ~a (from util/version.rkt)~n~n" canonical)

  ;; --- Run all checks ---
  (displayln "Checking info.rkt ...")
  (define info-errors (check-info-rkt canonical))

  (displayln "Checking README.md ...")
  (define readme-errors (check-readme-badge canonical))

  (displayln "Checking CHANGELOG integrity ...")
  (define changelog-errors (check-changelog-integrity))

  (displayln "Checking .md files ...")
  (define md-errors (check-md-files canonical))

  (define errors (append info-errors readme-errors changelog-errors md-errors))

  ;; --- Report ---
  (displayln "---")
  (printf "Version: ~a (from util/version.rkt)~n" canonical)
  (printf "Errors: ~a~n" (length errors))
  (if (null? errors)
      (begin
        (displayln "Version lint PASSED")
        (exit 0))
      (begin
        (displayln "Version lint FAILED")
        (exit 1))))

(module+ main
  (main))

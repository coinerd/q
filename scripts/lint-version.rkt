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
         racket/port)

;; ---------------------------------------------------------------------------
;; Parsing helpers
;; ---------------------------------------------------------------------------

(define VERSION-PAT #rx"[0-9]+\\.[0-9]+\\.[0-9]+")

;; Parse `(define q-version "X.Y.Z")` from util/version.rkt content.
(define (parse-q-version content)
  (define m (regexp-match #rx"\\(define q-version \"([0-9]+\\.[0-9]+\\.[0-9]+)\"" content))
  (and m (cadr m)))

;; Parse `(define version "X.Y.Z")` from info.rkt content.
(define (parse-info-version content)
  (define lines (string-split content "\n"))
  (for/or ([line (in-list lines)])
    (define trimmed (string-trim line))
    (cond
      [(and (string-prefix? trimmed "(define version") (string-suffix? trimmed ")"))
       (define m (regexp-match #rx"\"([0-9]+\\.[0-9]+\\.[0-9]+)\"" trimmed))
       (and m (cadr m))]
      [else #f])))

;; ---------------------------------------------------------------------------
;; File scanning
;; ---------------------------------------------------------------------------

(define (skip-path? p)
  (define s (path->string p))
  (or (string-contains? s "/compiled/")
      (string-contains? s "/.git/")
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
    (or (equal? fname "CHANGELOG.md")
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
                          "docs/demos/")))
  ;; Skip historical release lines like "**v0.6.3** — ..."
  (define historical-line? (lambda (line) (regexp-match? #rx"^\\*\\*v[0-9]" (string-trim line))))
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
;; Checks
;; ---------------------------------------------------------------------------

(define (check-info-rkt canonical)
  (define info-path (build-path (current-directory) "info.rkt"))
  (cond
    [(not (file-exists? info-path))
     (displayln "  WARN: info.rkt not found")
     '()]
    [else
     (define info-content (file->string info-path))
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
  (define readme-path (build-path (current-directory) "README.md"))
  (cond
    [(not (file-exists? readme-path))
     (displayln "  WARN: README.md not found")
     '()]
    [else
     (define content (file->string readme-path))
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
  (define md-files (collect-md-files (current-directory)))
  (append* (for/list ([f (in-list md-files)])
             (define lines (string-split (file->string f) "\n"))
             (define mismatches (find-mismatched-versions lines canonical f))
             (for/list ([m (in-list mismatches)])
               (printf "  ERROR: ~a:~a: \"~a\" ≠ canonical ~a~n"
                       (path->string f)
                       (first m)
                       (second m)
                       canonical)
               (list f (first m) (second m))))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (main)
  ;; --- Read canonical version from util/version.rkt ---
  (define util-path (build-path (current-directory) "util" "version.rkt"))
  (unless (file-exists? util-path)
    (displayln "ERROR: util/version.rkt not found in current directory")
    (exit 1))

  (define util-content (file->string util-path))
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

  (displayln "Checking .md files ...")
  (define md-errors (check-md-files canonical))

  (define errors (append info-errors readme-errors md-errors))

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

(main)

#!/usr/bin/env racket
#lang racket/base

(require racket/file
         racket/list
         racket/path
         racket/string
         racket/port)

;; ---------------------------------------------------------------------------
;; Version-consistency linter — cross-checks info.rkt version against all .md files.
;; Exit 0 if clean, 1 if any version mismatch found.
;; ---------------------------------------------------------------------------

;;; --- helpers ---

(define VERSION-PAT #rx"[0-9]+\\.[0-9]+\\.[0-9]+")

;; Parse `(define version "X.Y.Z")` from info.rkt content.
(define (parse-canonical-version content)
  (define lines (string-split content "\n"))
  (for/or ([line (in-list lines)])
    (define trimmed (string-trim line))
    (cond
      [(and (string-prefix? trimmed "(define version")
            (string-suffix? trimmed ")"))
       (define m (regexp-match #rx"\"([0-9]+\\.[0-9]+\\.[0-9]+)\"" trimmed))
       (and m (cadr m))]
      [else #f])))

;; Predicate: skip compiled/ directories and .zo files.
(define (skip-path? p)
  (define s (path->string p))
  (or (string-contains? s "/compiled/")
      (string-contains? s "/.git/")
      (and (filename-extension p)
           (equal? (bytes->string/utf-8 (filename-extension p)) "zo"))))

;; Collect all .md files under base-dir, excluding compiled/ and .zo.
(define (collect-md-files base-dir)
  (sort
   (for/list ([f (in-directory base-dir)]
              #:when (and (not (skip-path? f))
                         (let ([ext (filename-extension f)])
                           (and ext (equal? (bytes->string/utf-8 ext) "md")))))
     f)
   path<?))

;; Return list of (list line-num version-string) for each non-matching version.
(define (find-mismatched-versions lines canonical-version filename)
  (define fname (path->string (file-name-from-path filename)))
  (define skip-file?
    (or (equal? fname "CHANGELOG.md")
        (equal? fname "releasing.md")))
  (append*
   (for/list ([line (in-list lines)]
              [lineno (in-naturals 1)])
     (define matches (regexp-match* VERSION-PAT line))
     (for/list ([v (in-list matches)]
                #:when (and (not (equal? v canonical-version))
                            (not skip-file?)))
       (list lineno v)))))

;;; --- main ---

(define (main)
  (define info-path (build-path (current-directory) "info.rkt"))
  (unless (file-exists? info-path)
    (displayln "ERROR: info.rkt not found in current directory")
    (exit 1))

  (define info-content (file->string info-path))
  (define canonical (parse-canonical-version info-content))
  (unless canonical
    (displayln "ERROR: could not parse version from info.rkt")
    (exit 1))

  (define md-files (collect-md-files (current-directory)))
  (define errors
    (append*
     (for/list ([f (in-list md-files)])
       (define lines (string-split (file->string f) "\n"))
       (define mismatches (find-mismatched-versions lines canonical f))
       (for/list ([m (in-list mismatches)])
         (list f (first m) (second m))))))

  ;; Report errors
  (for ([e (in-list errors)])
    (printf "ERROR: ~a:~a: version \"~a\" does not match info.rkt version \"~a\"~n"
            (path->string (first e))
            (second e)
            (third e)
            canonical))

  ;; Summary
  (displayln "---")
  (printf "Version: ~a (from info.rkt)~n" canonical)
  (printf "Scanned: ~a .md files~n" (length md-files))
  (printf "Errors: ~a~n" (length errors))
  (if (null? errors)
      (displayln "Version lint PASSED")
      (begin (displayln "Version lint FAILED") (exit 1))))

(main)

#!/usr/bin/env racket
#lang racket/base

(require racket/file
         racket/list
         racket/path
         racket/string)

;; ---------------------------------------------------------------------------
;; Format/style linter — checks .rkt files for tabs, trailing whitespace,
;; line length, and consistent #lang header.
;; Exit 0 if no errors, 1 if any errors found (warnings are reported only).
;; ---------------------------------------------------------------------------

(define MAX-LINE-LENGTH 150)

;;; --- state ---

(define errors '())
(define warnings '())

(define (add-error! msg)
  (set! errors (cons msg errors)))
(define (add-warning! msg)
  (set! warnings (cons msg warnings)))

;;; --- helpers ---

(define (skip-path? p)
  (define s (path->string p))
  (or (string-contains? s "/compiled/")
      (string-contains? s "/.git/")
      (string-contains? s "/benchmarks/")))

(define (collect-rkt-files base-dir)
  (sort (for/list ([f (in-directory base-dir)]
                   #:when (and (not (skip-path? f))
                               (let ([ext (filename-extension f)])
                                 (and ext (equal? (bytes->string/utf-8 ext) "rkt")))))
          f)
        path<?))

(define (comment-line? line)
  (string-prefix? (string-trim line) ";;"))

;;; --- checks per file ---

(define (check-tabs filepath lines)
  (for ([line (in-list lines)]
        [lineno (in-naturals 1)])
    (when (string-contains? line "\t")
      (add-error! (format "ERROR: ~a:~a: tab character found (use spaces only)" filepath lineno)))))

(define (check-trailing-whitespace filepath lines)
  (for ([line (in-list lines)]
        [lineno (in-naturals 1)])
    (define len (string-length line))
    (when (and (> len 0)
               (let ([c (string-ref line (sub1 len))]) (or (char=? c #\space) (char=? c #\tab))))
      (add-error! (format "ERROR: ~a:~a: trailing whitespace" filepath lineno)))))

(define (check-line-length filepath lines)
  (for ([line (in-list lines)]
        [lineno (in-naturals 1)])
    (when (> (string-length line) MAX-LINE-LENGTH)
      (add-error! (format "ERROR: ~a:~a: line too long (~a chars, max ~a)"
                          filepath
                          lineno
                          (string-length line)
                          MAX-LINE-LENGTH)))))

(define (check-lang-header filepath lines)
  ;; First non-empty, non-shebang line should start with #lang
  (define canonical-lang "racket/base")
  (for/or ([line (in-list lines)]
           [lineno (in-naturals 1)])
    (define trimmed (string-trim line))
    (cond
      [(string=? trimmed "") #f] ; skip blank lines
      [(string-prefix? trimmed "#!") #f] ; skip shebang
      [(string-prefix? trimmed "#lang")
       (define lang (string-trim (substring trimmed 5)))
       (unless (or (string=? lang canonical-lang)
                   ;; These are acceptable alternatives
                   (string=? lang "racket")
                   (string-prefix? lang "at-exp "))
         (add-warning! (format "WARNING: ~a:~a: non-standard #lang ~a (expected #lang ~a)"
                               filepath
                               lineno
                               lang
                               canonical-lang)))
       #t]
      ;; Non-#lang first meaningful line — not a Racket source concern
      [else #t])))

;;; --- per-file driver ---

(define (lint-file filepath)
  (define lines (file->lines filepath))
  (define rel (path->string (find-relative-path (current-directory) filepath)))
  (check-tabs rel lines)
  (check-trailing-whitespace rel lines)
  (check-line-length rel lines)
  (check-lang-header rel lines))

;;; --- main ---

(define (main)
  (define rkt-files (collect-rkt-files (current-directory)))

  (when (null? rkt-files)
    (displayln "No .rkt files found. Run from q/ root.")
    (exit 2))

  (for ([f (in-list rkt-files)])
    (lint-file f))

  ;; Print results: errors first, then warnings
  (for ([e (reverse errors)])
    (displayln e))
  (for ([w (reverse warnings)])
    (displayln w))

  (displayln "---")
  (printf "~a errors, ~a warnings~n" (length errors) (length warnings))
  (printf "Scanned: ~a .rkt files~n" (length rkt-files))

  (if (null? errors)
      (begin
        (displayln "Format lint PASSED")
        (exit 0))
      (begin
        (displayln "Format lint FAILED")
        (exit 1))))

(main)

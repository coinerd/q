#lang racket/base

;; extensions/gsd/shared.rkt — Shared GSD utility functions
;;
;; v0.32.1 Wave 1: DRY extraction — single canonical definition for
;; functions previously duplicated across gsd modules.

(require racket/string
         racket/list)

(provide extract-plan-title
         slugify)

;; extract-plan-title : string? -> string?
;; Extracts the plan title from a PLAN.md-style text.
;; Looks for a line matching "# Plan: <title>".
;; Returns the title string, or "archived-plan" if no match found.
;; Previously duplicated in wave-executor.rkt and archive.rkt.
(define (extract-plan-title text)
  (define lines (string-split text "\n"))
  (define title-line
    (for/first ([line lines]
                #:when (regexp-match? #rx"^# +Plan:" line))
      line))
  (cond
    [(not title-line) "archived-plan"]
    [else
     (define m (regexp-match #rx"^# +Plan: +(.+)$" title-line))
     (if m
         (string-trim (cadr m))
         "archived-plan")]))

;; slugify : string? -> string?
;; Converts a title string to a URL-safe slug.
;; Lowercases, replaces non-alphanumeric chars with hyphens,
;; collapses consecutive hyphens, truncates to 40 chars.
;; Returns "wave" for empty/whitespace-only input.
;; Previously duplicated in wave-docs.rkt and plan-types.rkt.
(define (slugify title)
  (define s (string-trim title))
  (define slug-chars
    (for/list ([c (in-string s)])
      (cond
        [(char-alphabetic? c) (char-downcase c)]
        [(char-numeric? c) c]
        [(char=? c #\-) #\-]
        [(char=? c #\space) #\-]
        [else #f])))
  (define cleaned (collapse-hyphens (filter values slug-chars)))
  (define result (list->string cleaned))
  (define truncated
    (if (> (string-length result) 40)
        (string-trim (substring result 0 40) "-")
        result))
  (if (string=? truncated "") "wave" truncated))

;; collapse-hyphens : (listof char?) -> (listof char?)
;; Collapses consecutive hyphens into a single hyphen.
(define (collapse-hyphens chars)
  (define-values (rev acc)
    (for/fold ([rev '()]
               [prev-hyphen? #f])
              ([c (in-list chars)])
      (cond
        [(and prev-hyphen? (char=? c #\-)) (values rev #t)]
        [else (values (cons c rev) (char=? c #\-))])))
  (reverse rev))

#lang racket/base

;; util/string-helpers.rkt — string utility functions (with deliberate bugs)

(require racket/string
         racket/format)

(provide slugify
         truncate-with-ellipsis
         count-words
         indent-lines
         strip-ansi-codes)

;; BUG: doesn't handle multiple consecutive dashes, e.g. "a---b" → "a---b" instead of "a-b"
(define (slugify s)
  (define lower (string-downcase s))
  (define dashed (regexp-replace* #rx"[^a-z0-9]+" lower "-"))
  (string-trim dashed "-"))

;; BUG: off-by-one — truncates at len-1 instead of len, losing one extra character
(define (truncate-with-ellipsis s len)
  (if (<= (string-length s) len)
      s
      (string-append (substring s 0 (- len 1)) "…")))

;; BUG: empty string returns 1 instead of 0
(define (count-words s)
  (define trimmed (string-trim s))
  (if (string=? trimmed "")
      1
      (add1 (length (regexp-match* #rx" " trimmed)))))

;; Works correctly
(define (indent-lines s prefix)
  (string-join
   (for/list ([line (in-list (string-split s "\n"))])
     (string-append prefix line))
   "\n"))

;; BUG: doesn't handle \x1b sequences (only \033)
(define (strip-ansi-codes s)
  (regexp-replace* #rx"\033\\[[0-9;]*m" s ""))

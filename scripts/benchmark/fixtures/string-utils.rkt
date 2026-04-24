#lang racket

(provide trim-whitespace
         slugify
         camel->kebab)

;; Trim leading and trailing whitespace from a string.
;; BUG: only handles spaces, not tabs or other whitespace.
(define (trim-whitespace s)
  (define space-char #\space)
  (let loop-front ([chars (string->list s)])
    (cond
      [(null? chars) '()]
      [(char=? (car chars) space-char) (loop-front (cdr chars))]
      [else
       (let loop-back ([chars (reverse chars)])
         (cond
           [(null? chars) '()]
           [(char=? (car chars) space-char) (loop-back (cdr chars))]
           [else (reverse chars)]))]))
    ;; oops, the result of the outer let is the list, not a string
  (list->string
   (let loop-front ([chars (string->list s)])
     (cond
       [(null? chars) '()]
       [(char=? (car chars) space-char) (loop-front (cdr chars))]
       [else
        (let loop-back ([rev (reverse chars)])
          (cond
            [(null? chars) '()]
            [(char=? (car rev) space-char) (loop-back (cdr rev))]
            [else (reverse rev)]))]))))

;; Convert a string to a URL-friendly slug.
(define (slugify s)
  (let* ([s (string-downcase s)]
         [s (regexp-replace* #rx"[^a-z0-9]+" s "-")]
         [s (string-trim s "-")])
    s))

;; Convert camelCase to kebab-case.
(define (camel->kebab s)
  (let* ([s (regexp-replace* #rx"([a-z])([A-Z])" s "\\1-\\2")]
         [s (string-downcase s)])
    s))

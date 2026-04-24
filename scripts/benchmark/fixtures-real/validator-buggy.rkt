#lang racket/base

;; tools/validator.rkt — input validation helpers (with deliberate bugs)

(require racket/string
         racket/list
         racket/match)

(provide valid-module-name?
         valid-export-name?
         sanitize-path
         validate-config
         config-missing-fields)

;; BUG: allows names starting with numbers, which Racket doesn't allow
(define (valid-module-name? s)
  (and (string? s)
       (regexp-match? #rx"^[a-zA-Z0-9_-]+$" s)))

;; Works correctly
(define (valid-export-name? s)
  (and (string? s)
       (regexp-match? #rx"^[a-zA-Z_][a-zA-Z0-9_?!*-]*$" s)))

;; BUG: doesn't prevent path traversal — ".." is allowed
(define (sanitize-path s)
  (define cleaned (regexp-replace* #rx"[#<>|*?\"\\]" s ""))
  (define no-double-slash (regexp-replace* #rx"//" cleaned "/"))
  (string-trim no-double-slash))

;; BUG: returns #t for empty hash — should require at least 'providers key
(define (validate-config cfg)
  (and (hash? cfg)
       #t))

;; BUG: doesn't check for 'providers key
(define (config-missing-fields cfg)
  (define required '(providers default-model))
  (for/list ([key (in-list required)]
             #:when (not (hash-has-key? cfg key)))
    key))

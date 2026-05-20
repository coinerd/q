#lang racket/base

;; util/command-helpers.rkt -- shared command argument extraction
;;
;; R6: Deduplicated extract-cmd-args from gsd/command-parser.rkt
;; and gsd-planning/command-normalization.rkt into a single source.

(require racket/contract
         racket/string)

(define (extract-cmd-args input-text)
  (define trimmed (string-trim input-text))
  (if (and (> (string-length trimmed) 0) (char=? (string-ref trimmed 0) #\/))
      (let ([parts (string-split trimmed)])
        (if (>= (length parts) 2)
            (string-trim (string-join (cdr parts) " "))
            ""))
      ""))

(provide (contract-out [extract-cmd-args (-> string? string?)]))

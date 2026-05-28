#lang racket/base

;; q/gui/components/input-helpers.rkt — Pure multiline input helpers
;;
;; Headless-testable key handling and input validation for the GUI.

(require racket/contract
         racket/string
         "markdown-parser.rkt")

(provide (contract-out
          [input-key-should-submit? (-> any/c boolean? boolean? boolean?)]
          [prepare-input-for-submit (-> string? string?)]
          [input-line-count (-> string? exact-nonnegative-integer?)]
          [input-looks-like-code? (-> string? boolean?)]))

(define (input-key-should-submit? key-code shift? control?)
  (and (equal? key-code 'return)
       (not shift?)
       (not control?)))

(define (prepare-input-for-submit text)
  (string-trim text #:left? #f))

(define (input-line-count text)
  (length (string-split text "\n")))

(define (input-looks-like-code? text)
  (or (contains-code-blocks? text)
      (ormap (lambda (pat) (string-contains? text pat))
             (list "(define " "(let " "(lambda " "(if " "(cond " "(for " "(when " "(set! "))))

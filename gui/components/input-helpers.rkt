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

;; Should this key event trigger submit?
;; Enter without Shift/Control → submit
;; Shift+Enter or Control+Enter → insert newline
(define (input-key-should-submit? key-code shift? control?)
  (and (equal? key-code 'return)
       (not shift?)
       (not control?)))

;; Process input text: trim trailing whitespace for submission
(define (prepare-input-for-submit text)
  (string-trim text #:left? #f))

;; Split input into lines for validation
(define (input-line-count text)
  (length (string-split text "\n")))

;; Check if input appears to be a code block (for auto-detection)
(define (input-looks-like-code? text)
  (or (contains-code-blocks? text)
      (ormap (lambda (pat) (string-contains? text pat))
             (list "(define " "(let " "(lambda " "(if " "(cond " "(for " "(when " "(set! "))))

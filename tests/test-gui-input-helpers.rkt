#lang racket/base

(require rackunit
         rackunit/text-ui
         "../gui/components/input-helpers.rkt")

(define-test-suite test-multiline-input
  (test-case "input-key-should-submit? Enter without modifiers"
    (check-true (input-key-should-submit? 'return #f #f)))

  (test-case "input-key-should-submit? Shift+Enter does not submit"
    (check-false (input-key-should-submit? 'return #t #f)))

  (test-case "input-key-should-submit? Control+Enter does not submit"
    (check-false (input-key-should-submit? 'return #f #t)))

  (test-case "input-key-should-submit? non-return key does not submit"
    (check-false (input-key-should-submit? 'space #f #f)))

  (test-case "prepare-input-for-submit trims trailing whitespace"
    (check-equal? (prepare-input-for-submit "hello   ") "hello")
    (check-equal? (prepare-input-for-submit "hello") "hello"))

  (test-case "input-line-count single line"
    (check-equal? (input-line-count "hello") 1))

  (test-case "input-line-count multiple lines"
    (check-equal? (input-line-count "line1\nline2\nline3") 3))

  (test-case "input-looks-like-code? detects racket"
    (check-true (input-looks-like-code? "(define x 1)"))
    (check-true (input-looks-like-code? "(let ([x 1]) x)")))

  (test-case "input-looks-like-code? plain text"
    (check-false (input-looks-like-code? "Hello, how are you?"))))

(run-tests test-multiline-input)

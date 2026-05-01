#lang racket

;; tests/tui/test-render-message-layout.rkt — tests for render/message-layout

(require rackunit
         rackunit/text-ui
         "../../tui/render/message-layout.rkt")

(define layout-tests
  (test-suite "Render Message Layout"

    (test-case "plain-line creates styled-line"
      (define sl (plain-line "hello"))
      (check-equal? (styled-line->text sl) "hello"))

    (test-case "styled-line->text concatenates segments"
      (define sl (styled-line (list (styled-segment "hello " '(bold))
                                     (styled-segment "world" '()))))
      (check-equal? (styled-line->text sl) "hello world"))

    (test-case "styles->sgr: bold"
      (check-not-false (regexp-match #rx"1" (styles->sgr '(bold)))))

    (test-case "styles->sgr: empty style"
      (check-equal? (styles->sgr '()) ""))

    (test-case "wrap-text: short text unchanged"
      (define result (wrap-text "hello" 100))
      (check-equal? result '("hello")))

    (test-case "wrap-single-line: short line unchanged"
      (define result (wrap-single-line "abc" 100))
      (check-equal? result '("abc")))

    (test-case "theme->style returns list"
      (define style (theme->style 'user))
      (check-true (list? style)))))

(module+ main
  (run-tests layout-tests))
(module+ test
  (run-tests layout-tests))

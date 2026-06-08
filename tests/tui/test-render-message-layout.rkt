#lang racket

;; @speed fast  ;; @suite tui

;; BOUNDARY: io

;; tests/tui/test-render-message-layout.rkt — tests for render/message-layout

(require rackunit
         rackunit/text-ui
         racket/string
         "../../tui/render/message-layout.rkt"
         "../../tui/state-types.rkt")

(define layout-tests
  (test-suite "Render Message Layout"

    (test-case "plain-line creates styled-line"
      (define sl (plain-line "hello"))
      (check-equal? (styled-line->text sl) "hello"))

    (test-case "render-message-layout: styled-line->text concatenates segments"
      (define sl (styled-line (list (styled-segment "hello " '(bold)) (styled-segment "world" '()))))
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
      (check-true (list? style)))

    ;; v0.28.21 W1: Enhanced thinking visual styling
    (test-case "format-entry thinking uses dim italic cyan style"
      (define entry (transcript-entry 'thinking "test thinking text" 0 (hasheq) #f))
      (define result (format-entry entry))
      (check-true (andmap styled-line? result))
      (define all-segments (append-map styled-line-segments result))
      (define first-style (styled-segment-style (car all-segments)))
      (check-not-false (member 'dim first-style) "thinking should be dim")
      (check-not-false (member 'italic first-style) "thinking should be italic")
      (check-not-false (member 'cyan first-style) "thinking should be cyan"))

    (test-case "format-entry thinking truncates long text to 3 lines"
      (define long-text
        (string-join (for/list ([i (in-range 10)])
                       (format "line ~a of thinking" i))
                     "\n"))
      (define entry (transcript-entry 'thinking long-text 0 (hasheq) #f))
      (define result (format-entry entry))
      ;; Should have at most 4 lines: 3 content + 1 truncation indicator
      (check-true (<= (length result) 4) (format "expected ≤4 lines, got ~a" (length result))))

    (test-case "format-entry thinking has visible separator prefix"
      (define entry (transcript-entry 'thinking "some thought" 0 (hasheq) #f))
      (define result (format-entry entry))
      (define first-text (styled-line->text (car result)))
      (check-not-false (regexp-match #rx"──" first-text)
                       (format "expected separator prefix in '~a'" first-text)))))

(module+ main
  (run-tests layout-tests))

#lang racket

;; tests/tui/ansi-wrap.rkt — Tests for ANSI-aware word wrapping

(require rackunit
         rackunit/text-ui
         "../../../q/tui/ansi-wrap.rkt")

(define ESC (integer->char 27))

(define ansi-wrap-tests
  (test-suite
   "ANSI-aware word wrapping"

   (test-case "plain text wrapping"
     (define result (wrap-ansi-line "hello world" 6))
     (check-equal? (length result) 2)
     (check-equal? (ansi-visible-width (car result)) 6)   ; "hello "
     (check-equal? (ansi-visible-width (cadr result)) 5))  ; "world"

   (test-case "no wrap needed"
     (define result (wrap-ansi-line "short" 80))
     (check-equal? result '("short")))

   (test-case "ansi-visible-width ignores escape sequences"
     (define s (format "~a[1;36mhello~a[0m" ESC ESC))
     (check-equal? (ansi-visible-width s) 5))

   (test-case "wrap preserves ANSI codes"
     (define s (format "~a[31mhello world~a[0m" ESC ESC))
     (define result (wrap-ansi-line s 6))
     (check-equal? (length result) 2)
     (check-true (string-contains? (car result) "[31m"))
     (check-equal? (ansi-visible-width (car result)) 6)
     (check-equal? (ansi-visible-width (cadr result)) 5))

   (test-case "wrap with multiple ANSI segments"
     (define s (format "~a[1m~a[31mhello ~a[32mworld test~a[0m" ESC ESC ESC ESC))
     (define result (wrap-ansi-line s 8))
     (check >= (length result) 2))

   (test-case "find-ansi-break-pos skips escape sequences"
     (define s (format "~a[1mhello~a[0m world" ESC ESC))
     (define pos (find-ansi-break-pos s 5))
     (check-true (<= (ansi-visible-width (substring s 0 pos)) 5)))

   (test-case "wrap-ansi-text splits on newlines"
     (define s "first line\nsecond line")
     (define result (wrap-ansi-text s 80))
     (check-equal? (length result) 2))

   (test-case "wrap-ansi-text wraps long lines after newline split"
     (define s "a very long line that needs to be wrapped\nshort")
     (define result (wrap-ansi-text s 10))
     (check >= (length result) 3))

   (test-case "zero-width returns input"
     (check-equal? (wrap-ansi-text "hello" 0) '("hello")))

   (test-case "long word breaks at width"
     (define result (wrap-ansi-line "abcdefghij" 5))
     (check-equal? (length result) 2)
     (check-equal? (ansi-visible-width (car result)) 5))

   (test-case "break at word boundary preserves space position"
     (define result (wrap-ansi-line "aaa bbb ccc" 4))
     (check >= (length result) 2))
   ))

(run-tests ansi-wrap-tests)

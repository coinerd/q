#lang racket

;; @speed fast  ;; @suite tui

;; BOUNDARY: io

;; tests/tui/test-input-history-ops.rkt — unit tests for tui/input/history-ops

(require rackunit
         rackunit/text-ui
         "../../tui/input.rkt")

(define history-tests
  (test-suite "Input History Ops"

    (test-case "history-push adds to history"
      (define st0 (initial-input-state))
      (define st1 (input-history-push st0 "first"))
      (check-equal? (input-state-history st1) '("first"))
      (define st2 (input-history-push st1 "second"))
      (check-equal? (input-state-history st2) '("first" "second")))

    (test-case "history-push ignores empty"
      (define st0 (initial-input-state))
      (define st1 (input-history-push st0 ""))
      (check-equal? (input-state-history st1) '()))

    (test-case "history-push deduplicates consecutive"
      (define st0 (initial-input-state))
      (define st1 (input-history-push st0 "hello"))
      (define st2 (input-history-push st1 "hello"))
      (check-equal? (input-state-history st2) '("hello")))

    (test-case "history-up/down navigation"
      (define st0 (initial-input-state))
      (define st1 (input-history-push st0 "first"))
      (define st2 (input-history-push st1 "second"))
      (define st3 (input-history-push st2 "third"))
      ;; Go up: should show "third" (newest)
      (define st4 (input-history-up st3))
      (check-equal? (input-current-text st4) "third")
      ;; Go up again: should show "second"
      (define st5 (input-history-up st4))
      (check-equal? (input-current-text st5) "second")
      ;; Go up again: should show "first"
      (define st6 (input-history-up st5))
      (check-equal? (input-current-text st6) "first")
      ;; Go up again: at oldest, no change
      (define st7 (input-history-up st6))
      (check-equal? (input-current-text st7) "first")
      ;; Go down: should show "second"
      (define st8 (input-history-down st7))
      (check-equal? (input-current-text st8) "second")
      ;; Go down to end: should restore saved text
      (define st9 (input-history-down st8))
      (check-equal? (input-current-text st9) "third")
      (define st10 (input-history-down st9))
      ;; After leaving history, cursor text is restored
      (check-false (input-state-history-idx st10)))

    (test-case "history-up on empty history is no-op"
      (define st0 (initial-input-state))
      (define st1 (input-history-up st0))
      (check-true (input-empty? st1)))

    (test-case "history-down without browsing is no-op"
      (define st0 (initial-input-state))
      (define st1 (input-history-down st0))
      (check-true (input-empty? st1)))

    (test-case "submit clears buffer and returns text"
      (define st (input-insert-string (initial-input-state) "  hello  "))
      (define-values (text st2) (input-submit st))
      (check-equal? text "hello")
      (check-true (input-empty? st2))
      (check-equal? (input-state-history st2) '("hello")))

    (test-case "submit empty returns #f"
      (define-values (text st) (input-submit (initial-input-state)))
      (check-false text)
      (check-true (input-empty? st)))))

(module+ main
  (run-tests history-tests))

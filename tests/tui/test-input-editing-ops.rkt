#lang racket

;; BOUNDARY: io

;; tests/tui/test-input-editing-ops.rkt — unit tests for tui/input/editing-ops

(require rackunit
         rackunit/text-ui
         "../../tui/input.rkt")

(define editing-tests
  (test-suite "Input Editing Ops"

    (test-case "initial state is empty"
      (define st (initial-input-state))
      (check-true (input-empty? st))
      (check-true (input-at-beginning? st))
      (check-true (input-at-end? st)))

    (test-case "insert char"
      (define st (input-insert-char (initial-input-state) #\h))
      (check-equal? (input-current-text st) "h")
      (check-true (input-at-end? st))
      (check-false (input-at-beginning? st)))

    (test-case "insert multiple chars"
      (define st0 (initial-input-state))
      (define st1 (input-insert-char st0 #\a))
      (define st2 (input-insert-char st1 #\b))
      (define st3 (input-insert-char st2 #\c))
      (check-equal? (input-current-text st3) "abc")
      (check-equal? (input-state-cursor st3) 3))

    (test-case "backspace"
      (define st (input-insert-char (initial-input-state) #\x))
      (define st2 (input-backspace st))
      (check-true (input-empty? st2)))

    (test-case "backspace at beginning is no-op"
      (define st (initial-input-state))
      (define st2 (input-backspace st))
      (check-true (input-empty? st2)))

    (test-case "delete"
      (define st (input-insert-char (initial-input-state) #\a))
      (define st2 (input-home st))
      (define st3 (input-delete st2))
      (check-true (input-empty? st3)))

    (test-case "delete at end is no-op"
      (define st (input-insert-char (initial-input-state) #\a))
      (define st2 (input-delete st))
      (check-equal? (input-current-text st2) "a"))

    (test-case "cursor movement"
      (define st (input-insert-string (initial-input-state) "hello"))
      (define st2 (input-cursor-left st))
      (check-equal? (input-state-cursor st2) 4)
      (define st3 (input-cursor-right st2))
      (check-equal? (input-state-cursor st3) 5)
      (define st4 (input-home st))
      (check-true (input-at-beginning? st4))
      (define st5 (input-end st4))
      (check-true (input-at-end? st5)))

    (test-case "clear"
      (define st (input-insert-string (initial-input-state) "hello"))
      (define st2 (input-clear st))
      (check-true (input-empty? st2)))

    (test-case "insert-string (paste)"
      (define st (input-insert-string (initial-input-state) "hello world"))
      (check-equal? (input-current-text st) "hello world")
      (check-equal? (input-state-cursor st) 11))

    (test-case "undo/redo"
      (define st0 (initial-input-state))
      (define st1 (input-insert-char st0 #\a))
      (define st2 (input-insert-char st1 #\b))
      (check-equal? (input-current-text st2) "ab")
      (define st3 (input-undo st2))
      (check-equal? (input-current-text st3) "a")
      (define st4 (input-redo st3))
      (check-equal? (input-current-text st4) "ab"))

    (test-case "word navigation"
      (define st (input-insert-string (initial-input-state) "hello world"))
      (define st2 (input-cursor-word-left st))
      (check-equal? (input-state-cursor st2) 6)
      (define st3 (input-cursor-word-left st2))
      (check-equal? (input-state-cursor st3) 0)
      (define st4 (input-cursor-word-right st3))
      (check-equal? (input-state-cursor st4) 5))

    (test-case "kill ring: kill-to-end + yank"
      (define st (input-insert-string (initial-input-state) "hello world"))
      (define st2 (input-home st))
      (define st3 (input-kill-to-end st2))
      (check-true (input-empty? st3))
      (define st4 (input-yank st3))
      (check-equal? (input-current-text st4) "hello world"))

    (test-case "kill ring: kill-to-beginning"
      (define st (input-insert-string (initial-input-state) "hello world"))
      (define st2 (input-kill-to-beginning st))
      (check-true (input-empty? st2)))

    (test-case "insert newline (multiline)"
      (define st (input-insert-string (initial-input-state) "abc"))
      (define st2 (input-home st))
      (define st3 (input-insert-newline st2))
      (check-equal? (input-current-text st3) "\nabc"))))

(module+ main
  (run-tests editing-tests))
(module+ test
  (run-tests editing-tests))

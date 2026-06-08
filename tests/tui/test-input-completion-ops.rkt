#lang racket

;; BOUNDARY: io

;; tests/tui/test-input-completion-ops.rkt — unit tests for tui/input/completion-ops

(require rackunit
         rackunit/text-ui
         "../../tui/input.rkt")

(define completion-tests
  (test-suite "Input Completion Ops"

    (test-case "expand-inline-bash with !!"
      (check-equal? (expand-inline-bash "!!" "ls -la") "ls -la")
      (check-equal? (expand-inline-bash "!! extra" "ls") "ls extra")
      (check-equal? (expand-inline-bash "!!" #f) "!!"))

    (test-case "expand-inline-bash passes non-bang through"
      (check-equal? (expand-inline-bash "hello" "ls") "hello")
      (check-equal? (expand-inline-bash "!cmd" "ls") "!cmd"))

    (test-case "input-expand-file-ref: no @ returns unchanged"
      (define st (input-insert-string (initial-input-state) "hello"))
      (define st2 (input-expand-file-ref st))
      (check-equal? (input-current-text st2) "hello"))

    (test-case "input-expand-file-ref: @ at start with no match"
      ;; Can't easily test glob behavior in unit tests, but verify no crash
      (define st (input-insert-string (initial-input-state) "@nonexistent-file-xyz"))
      (define st2 (input-expand-file-ref st))
      ;; Should return unchanged state (no match)
      (check-equal? (input-current-text st2) "@nonexistent-file-xyz"))))

(module+ main
  (run-tests completion-tests))

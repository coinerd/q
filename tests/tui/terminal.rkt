#lang racket

;; tests/tui/terminal.rkt — Tests for tui/terminal module
;;
;; Most terminal I/O cannot be tested in a headless environment.
;; These tests verify that:
;;   1. The module loads and provides the expected bindings
;;   2. Key helper functions work correctly
;;   3. The adapter functions have correct signatures

(require rackunit
         rackunit/text-ui
         "../../../q/tui/terminal.rkt")

(define terminal-tests
  (test-suite
   "TUI Terminal"

   ;; ============================================================
   ;; Module loads
   ;; ============================================================

   (test-case "tui-term-open is a procedure"
     (check-true (procedure? tui-term-open)))

   (test-case "tui-term-close is a procedure"
     (check-true (procedure? tui-term-close)))

   (test-case "tui-screen-size is a procedure"
     (check-true (procedure? tui-screen-size)))

   (test-case "tui-clear-screen is a procedure"
     (check-true (procedure? tui-clear-screen)))

   (test-case "tui-cursor is a procedure"
     (check-true (procedure? tui-cursor)))

   (test-case "tui-display is a procedure"
     (check-true (procedure? tui-display)))

   (test-case "tui-newline is a procedure"
     (check-true (procedure? tui-newline)))

   (test-case "tui-clear-line-right is a procedure"
     (check-true (procedure? tui-clear-line-right)))

   (test-case "tui-flush is a procedure"
     (check-true (procedure? tui-flush)))

   (test-case "tui-normal is a procedure"
     (check-true (procedure? tui-normal)))

   (test-case "tui-bold is a procedure"
     (check-true (procedure? tui-bold)))

   (test-case "tui-inverse is a procedure"
     (check-true (procedure? tui-inverse)))

   (test-case "tui-underline is a procedure"
     (check-true (procedure? tui-underline)))

   (test-case "tui-dim is a procedure"
     (check-true (procedure? tui-dim)))

   (test-case "tui-fg is a procedure"
     (check-true (procedure? tui-fg)))

   (test-case "tui-read-key is a procedure"
     (check-true (procedure? tui-read-key)))

   (test-case "tui-byte-ready? is a procedure"
     (check-true (procedure? tui-byte-ready?)))

   ;; ============================================================
   ;; Key helpers — pure functions, fully testable
   ;; ============================================================

   (test-case "tui-key-char? returns true for char input"
     (check-true (tui-key-char? #\a))
     (check-true (tui-key-char? #\return)))

   (test-case "tui-key-char? returns false for symbol input"
     (check-false (tui-key-char? 'left))
     (check-false (tui-key-char? 'backspace)))

   (test-case "tui-key-char returns the character"
     (check-equal? (tui-key-char #\a) #\a)
     (check-equal? (tui-key-char #\Z) #\Z))

   (test-case "tui-key-symbol returns the symbol"
     (check-equal? (tui-key-symbol 'left) 'left)
     (check-equal? (tui-key-symbol 'up) 'up))
   ))

(run-tests terminal-tests)

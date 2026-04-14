#lang racket

;; tests/test-readline.rkt — Tests for util/readline.rkt
;;
;; Tests readline utility functions. Since readline requires a real terminal,
;; we test the fallback path (non-stdin ports) and the predicate.

(require rackunit
         racket/port
         "../util/readline.rkt")

;; ============================================================
;; readline-available? predicate
;; ============================================================

(test-case "readline-available? is a boolean"
  (check-true (boolean? readline-available?)))

;; ============================================================
;; read-line-with-history fallback path (non-stdin port)
;; ============================================================

(test-case "read-line-with-history reads from string port (fallback)"
  ;; When `in` is not (current-input-port), the fallback path is used.
  (define in (open-input-string "hello world\n"))
  (define out (open-output-string))
  (define result (read-line-with-history "> " in out))
  (check-equal? result "hello world")
  ;; The prompt should have been written to out
  (check-equal? (get-output-string out) "> "))

(test-case "read-line-with-history returns EOF for empty input"
  (define in (open-input-string ""))
  (define out (open-output-string))
  (define result (read-line-with-history "> " in out))
  (check-equal? result eof))

(test-case "read-line-with-history reads multiple lines sequentially"
  (define in (open-input-string "first\nsecond\n"))
  (define out (open-output-string))
  (check-equal? (read-line-with-history "> " in out) "first")
  (check-equal? (read-line-with-history "> " in out) "second"))

(test-case "read-line-with-history displays prompt before reading"
  (define in (open-input-string "test\n"))
  (define out (open-output-string))
  (read-line-with-history "prompt> " in out)
  (define output (get-output-string out))
  (check-equal? output "prompt> "))

(test-case "read-line-with-history handles empty line"
  (define in (open-input-string "\n"))
  (define out (open-output-string))
  (define result (read-line-with-history "> " in out))
  (check-equal? result ""))

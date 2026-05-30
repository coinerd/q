#lang racket/base

;; tests/test-shell-quote.rkt — T-7 tests for util/shell-quote.rkt

(require rackunit
         rackunit/text-ui
         "../util/shell-quote.rkt")

(define shell-quote-suite
  (test-suite
   "Shell-quote tests"
   (test-case "simple string is quoted"
     (check-equal? (shell-quote "hello") "'hello'"))

   (test-case "string with spaces"
     (check-equal? (shell-quote "hello world") "'hello world'"))

   (test-case "string with single quotes is escaped"
     (check-equal? (shell-quote "it's") "'it'\\''s'"))

   (test-case "string with double quotes"
     (check-equal? (shell-quote "say \"hi\"") "'say \"hi\"'"))

   (test-case "string with backticks"
     (check-equal? (shell-quote "`id`") "'`id`'"))

   (test-case "string with dollar sign"
     (check-equal? (shell-quote "$HOME") "'$HOME'"))

   (test-case "string with semicolon"
     (check-equal? (shell-quote "a;b") "'a;b'"))

   (test-case "string with pipe"
     (check-equal? (shell-quote "a|b") "'a|b'"))

   (test-case "string with &&"
     (check-equal? (shell-quote "a&&b") "'a&&b'"))

   (test-case "string with newlines"
     (check-equal? (shell-quote "line1\nline2") "'line1\nline2'"))

   (test-case "empty string"
     (check-equal? (shell-quote "") "''"))

   (test-case "unicode string"
     (check-equal? (shell-quote "héllo wörld") "'héllo wörld'"))

   (test-case "injection: rm -rf / is safely quoted"
     (define result (shell-quote "; rm -rf /"))
     (check-equal? result "'; rm -rf /'"))

   (test-case "injection: command substitution"
     (define result (shell-quote "$(cat /etc/passwd)"))
     (check-equal? result "'$(cat /etc/passwd)'"))

   (test-case "injection: backtick execution"
     (define result (shell-quote "`id`"))
     (check-equal? result "'`id`'"))

   (test-case "non-string input is converted"
     (check-equal? (shell-quote 42) "'42'")
     (check-equal? (shell-quote 'sym) "'sym'"))
   ))

(run-tests shell-quote-suite)

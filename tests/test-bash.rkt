#lang racket

;; test-bash.rkt — tests for tools/builtins/bash.rkt

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "../tools/tool.rkt"
                  tool-result? tool-result-is-error? tool-result-content)
         "../tools/builtins/bash.rkt")

(define bash-tests
  (test-suite
   "bash-tool"

   (test-case "echo command returns tool-result"
     (define r (tool-bash (hasheq 'command "echo hello world")))
     (check-pred tool-result? r)
     (check-false (tool-result-is-error? r)))

   (test-case "echo output contains expected text"
     (define r (tool-bash (hasheq 'command "echo hello")))
     (check-pred tool-result? r)
     (check-false (tool-result-is-error? r))
     (define text (hash-ref (car (tool-result-content r)) 'text))
     (check-regexp-match "hello" text
                         (format "expected 'hello' in output, got: ~a" text)))

   (test-case "shell pipe works via /bin/sh -c"
     (define r (tool-bash (hasheq 'command "echo hello | grep h")))
     (check-pred tool-result? r)
     (check-false (tool-result-is-error? r))
     (define text (hash-ref (car (tool-result-content r)) 'text))
     (check-regexp-match "hello" text
                         (format "expected 'hello' in piped output, got: ~a" text)))

   (test-case "stderr is included in output"
     (define r (tool-bash (hasheq 'command "echo err-msg >&2")))
     (check-pred tool-result? r)
     (check-false (tool-result-is-error? r))
     (define text (hash-ref (car (tool-result-content r)) 'text))
     (check-regexp-match "err-msg" text
                         (format "expected 'err-msg' in output, got: ~a" text)))

   (test-case "exit 1 returns success result (non-zero exit is not a tool error)"
     (define r (tool-bash (hasheq 'command "exit 1")))
     (check-pred tool-result? r)
     (check-false (tool-result-is-error? r)))

   (test-case "missing command returns error"
     (define r (tool-bash (hasheq)))
     (check-pred tool-result? r)
     (check-true (tool-result-is-error? r)))

   (test-case "empty command returns error"
     (define r (tool-bash (hasheq 'command "")))
     (check-pred tool-result? r)
     (check-true (tool-result-is-error? r)))

   (test-case "BUG-20: empty output produces diagnostic message"
     (define r (tool-bash (hasheq 'command "true")))  ; true produces no output
     (check-pred tool-result? r)
     (check-false (tool-result-is-error? r))
     (define text (hash-ref (car (tool-result-content r)) 'text))
     (check-regexp-match "Command produced no output" text
                         (format "Expected diagnostic message, got: ~a" text))
     (check-regexp-match "try a different approach" text
                         (format "Expected 'try a different approach', got: ~a" text)))
   ))

(run-tests bash-tests)

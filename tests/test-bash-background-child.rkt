#lang racket

;; Regression test for BUG_REPORT-v0.96.x-BASH-TOOL-HANG-BACKGROUND-CHILD
;; Verifies that a backgrounded child process inheriting the stdout pipe
;; does not cause the bash tool to hang forever.

(require rackunit
         rackunit/text-ui
         (only-in "../tools/tool.rkt" tool-result? tool-result-is-error? tool-result-content)
         "../tools/builtins/bash.rkt")

(define bg-child-tests
  (test-suite
   "bash-backgrounded-child-regression"

   (test-case
    "backgrounded child does not hang tool"
    (define r
      (tool-bash
       (hasheq 'command
               (string-append
                "echo bg_test_output; "
                "python3 -m http.server 19999 --directory /tmp &"))))
    (check-pred tool-result? r)
    (check-false (tool-result-is-error? r))
    (define text (hash-ref (car (tool-result-content r)) 'text))
    (check-regexp-match
     "bg_test_output"
     text
     (format "expected 'bg_test_output' in output, got: ~a" text))
    (system "pkill -f 'http.server 19999' 2>/dev/null"))

   (test-case
    "backgrounded sleep does not hang tool"
    (define r
      (tool-bash
       (hasheq 'command "echo before_bg; sleep 300 & echo after_bg")))
    (check-pred tool-result? r)
    (check-false (tool-result-is-error? r))
    (define text (hash-ref (car (tool-result-content r)) 'text))
    (check-regexp-match "before_bg" text)
    (check-regexp-match "after_bg" text)
    (system "pkill -f 'sleep 300' 2>/dev/null"))))

(run-tests bg-child-tests)

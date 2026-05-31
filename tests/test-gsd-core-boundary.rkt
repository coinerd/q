#lang racket/base

;; tests/test-gsd-core-boundary.rkt -- Boundary tests for gsd/core.rkt API
;; v0.74.5: Verify contract-out on GSD core exports.

(require rackunit
         rackunit/text-ui
         (only-in "../extensions/gsd/core.rkt"
                  gsd-command-dispatch
                  gsd-write-guard
                  gsd-show-status
                  cmd-reset
                  reset-all-gsd-state!
                  gsd-commands
                  gsd-command-result?
                  gsd-command-result-success
                  gsd-command-result-message))

(define-test-suite gsd-core-boundary-tests

  (test-case "gsd-commands is a non-empty list"
    (check-not-false (list? gsd-commands))
    (check-true (> (length gsd-commands) 0)))

  (test-case "gsd-command-dispatch with unknown command returns #f"
    (check-false (gsd-command-dispatch 'nonexistent #f)))

  (test-case "gsd-command-dispatch rejects non-symbol/string"
    (check-exn exn:fail:contract?
               (lambda () (gsd-command-dispatch 42 #f))))

  (test-case "gsd-write-guard returns policy-decision"
    (define result (gsd-write-guard "/tmp/test.rkt" "/tmp"))
    (check-not-false result))

  (test-case "reset-all-gsd-state! returns void"
    (check-equal? (reset-all-gsd-state!) (void)))

  (test-case "gsd-show-status returns gsd-command-result"
    (define result (gsd-show-status))
    (check-true (gsd-command-result? result)))

  (test-case "cmd-reset returns gsd-command-result"
    (define result (cmd-reset))
    (check-true (gsd-command-result? result)))

  (test-case "gsd-command-result accessors work"
    (define result (gsd-show-status))
    (check-not-false (gsd-command-result-success result))
    (check-not-false (string? (gsd-command-result-message result)))))

(module+ main
  (run-tests gsd-core-boundary-tests))

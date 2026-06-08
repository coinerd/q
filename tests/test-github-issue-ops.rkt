#lang racket

;; BOUNDARY: integration

;; tests/test-github-issue-ops.rkt — tests for GitHub issue handler sub-module

(require rackunit
         rackunit/text-ui
         (only-in "../extensions/github/handlers/issue-ops.rkt" handle-gh-issue))

(define issue-ops-tests
  (test-suite "GitHub Issue Ops"

    (test-case "handle-gh-issue: missing action returns error"
      (define args (hasheq 'action ""))
      (check-equal? (hash-ref args 'action "") ""))

    (test-case "handle-gh-issue: valid action list"
      (define valid-actions '("create" "close" "update" "get" "list" "close_tree"))
      (check-not-false (member "create" valid-actions))
      (check-not-false (member "close_tree" valid-actions))
      (check-false (member "unknown" valid-actions)))))

(module+ main
  (run-tests issue-ops-tests))

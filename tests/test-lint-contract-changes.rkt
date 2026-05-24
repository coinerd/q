#lang racket

(require rackunit
         racket/runtime-path)

(define-runtime-path script-path "../scripts/lint-contract-changes.rkt")

(test-case "count-any/c-in-diff: detects additions and removals"
  (define count-fn (dynamic-require script-path 'count-any/c-in-diff))
  (define diff-text
    "+(provide (contract-out [foo (-> any/c any/c)]))\n-(provide (contract-out [foo (-> string? number?)]))")
  (define-values (added removed) (count-fn diff-text))
  (check-equal? added 2)
  (check-equal? removed 0))

(test-case "count-any/c-in-diff: no changes"
  (define count-fn (dynamic-require script-path 'count-any/c-in-diff))
  (define-values (added removed) (count-fn "no any/c here"))
  (check-equal? added 0)
  (check-equal? removed 0))

(test-case "find-contract-out-changes: detects additions"
  (define find-fn (dynamic-require script-path 'find-contract-out-changes))
  (define diff-text
    "+(provide (contract-out [foo (-> string? any/c)]))\n-some other line\n+(define foo identity)")
  (define-values (adds rems) (find-fn diff-text))
  (check-equal? (length adds) 1)
  (check-equal? (length rems) 0))

(test-case "find-contract-out-changes: ignores diff headers"
  (define find-fn (dynamic-require script-path 'find-contract-out-changes))
  (define diff-text "--- a/foo.rkt\n+++ b/foo.rkt\n+(contract-out [bar any/c])")
  (define-values (adds rems) (find-fn diff-text))
  (check-equal? (length adds) 1)
  (check-equal? (length rems) 0))

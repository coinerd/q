#lang racket

(require rackunit
         rackunit/text-ui
         "../util/hook-types.rkt")

(define hook-types-suite
  (test-suite
   "hook-types tests"

   (test-case "hook-pass returns hook-result with action 'pass"
     (define r (hook-pass))
     (check-equal? (hook-result-action r) 'pass)
     (check-false (hook-result-payload r)))

   (test-case "hook-pass accepts payload"
     (define r (hook-pass 'some-data))
     (check-equal? (hook-result-action r) 'pass)
     (check-equal? (hook-result-payload r) 'some-data))

   (test-case "hook-amend returns hook-result with action 'amend"
     (define r (hook-amend "new content"))
     (check-equal? (hook-result-action r) 'amend)
     (check-equal? (hook-result-payload r) "new content"))

   (test-case "hook-block returns hook-result with action 'block"
     (define r (hook-block))
     (check-equal? (hook-result-action r) 'block)
     (check-false (hook-result-payload r)))

   (test-case "hook-block accepts reason string"
     (define r (hook-block "not allowed"))
     (check-equal? (hook-result-action r) 'block)
     (check-equal? (hook-result-payload r) "not allowed"))

   (test-case "hook-result struct is transparent"
     (define r (hook-pass 42))
     (check-true (hook-result? r))
     (check-equal? r (hook-result 'pass 42)))))

(run-tests hook-types-suite)

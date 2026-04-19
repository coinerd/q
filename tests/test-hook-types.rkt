#lang racket

(require rackunit
         rackunit/text-ui
         "../util/hook-types.rkt")

(define hook-types-suite
  (test-suite "hook-types tests"

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
      (check-equal? r (hook-result 'pass 42)))

    ;; #1310: New hook points
    (test-case "new hook points are registered"
      (define names (hook-point-names))
      (check-not-false (member 'agent.end names) "agent.end registered")
      (check-not-false (member 'session-shutdown names) "session-shutdown registered")
      (check-not-false (member 'user-bash names) "user-bash registered")
      (check-not-false (member 'session-before-tree names) "session-before-tree registered")
      (check-not-false (member 'session-tree names) "session-tree registered"))

    (test-case "new hook points are valid hook names"
      (check-true (valid-hook-name? 'agent.end))
      (check-true (valid-hook-name? 'session-shutdown))
      (check-true (valid-hook-name? 'user-bash))
      (check-true (valid-hook-name? 'session-before-tree))
      (check-true (valid-hook-name? 'session-tree)))

    (test-case "new hook points have correct actions"
      (check-equal? (valid-hook-actions-for 'agent.end) '(pass))
      (check-equal? (valid-hook-actions-for 'session-shutdown) '(pass))
      (check-equal? (valid-hook-actions-for 'user-bash) '(pass amend block))
      (check-equal? (valid-hook-actions-for 'session-before-tree) '(pass block))
      (check-equal? (valid-hook-actions-for 'session-tree) '(pass)))

    (test-case "hook-point-names returns 47+ points"
      (define names (hook-point-names))
      (check-true (>= (length names) 47)
                  (format "expected >= 47 hook points, got ~a" (length names))))))

(run-tests hook-types-suite)

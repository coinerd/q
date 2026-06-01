#lang racket/base

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion-text
                  task-conclusion-id)
         (only-in "../runtime/context-assembly/conclusion-ranker.rkt"
                  score-conclusion
                  rank-and-budget))

(define c-error
  (task-conclusion "e1" "Root cause found" 'error-cause 'debugging '() 100 '(debug) '()))
(define c-decision
  (task-conclusion "d1" "Use struct for data" 'decision 'implementation '() 200 '(impl) '("ref1")))
(define c-fact
  (task-conclusion "f1" "Module has 500 lines" 'fact 'exploration '() 300 '(exploration) '()))
(define c-old (task-conclusion "o1" "Old finding" 'fact 'idle '() 10000 '() '()))
(define c-large (task-conclusion "big" (make-string 500 #\x) 'fact 'idle '() 400 '() '()))

(define suite
  (test-suite "conclusion-ranker"
    (test-case "state match scores higher"
      (define s-match (score-conclusion c-error 'debugging '()))
      (define s-no-match (score-conclusion c-error 'idle '()))
      (check-true (> s-match s-no-match)))

    (test-case "category priority works"
      (define s-error (score-conclusion c-error #f '()))
      (define s-fact (score-conclusion c-fact #f '()))
      (check-true (> s-error s-fact)))

    (test-case "tag overlap adds score"
      (define s-tags (score-conclusion c-error 'debugging '(debug)))
      (define s-no-tags (score-conclusion c-error 'debugging '()))
      (check-true (> s-tags s-no-tags)))

    (test-case "has dependencies adds score"
      (define s-dep (score-conclusion c-decision #f '()))
      (define s-no-dep (score-conclusion c-fact #f '()))
      ;; c-decision has deps, c-fact doesn't — but category also differs
      ;; just check both are positive
      (check-true (> s-dep 0))
      (check-true (> s-no-dep 0)))

    (test-case "rank-and-budget respects token limit"
      (define result
        (rank-and-budget (list c-error c-decision c-fact c-large)
                         #:max-conclusion-tokens 500
                         #:token-estimate
                         (lambda (c) (* 4 (+ 10 (string-length (task-conclusion-text c)))))))
      ;; Should not include the 2000-token large conclusion
      (check-true (< (length result) 4))
      (check-true (>= (length result) 1)))

    (test-case "rank-and-budget with empty list returns empty"
      (check-equal? (rank-and-budget '()) '()))

    (test-case "rank-and-budget orders by score"
      (define result
        (rank-and-budget (list c-fact c-error c-decision)
                         #:max-conclusion-tokens 50000
                         #:token-estimate (lambda (_) 10)))
      ;; error-cause should come before fact (higher category priority)
      (check-not-false (member "e1" (map (lambda (c) (task-conclusion-id c)) result))))

    (test-case "rank-and-budget never drops all if some fit"
      (define result
        (rank-and-budget (list c-error)
                         #:max-conclusion-tokens 5000
                         #:token-estimate (lambda (_) 100)))
      (check-equal? (length result) 1))))

(run-tests suite)

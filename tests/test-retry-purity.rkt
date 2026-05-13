#lang racket/base

;; BOUNDARY: integration

;; tests/test-retry-purity.rkt -- Retry policy purity test (I-06)

(require rackunit
         "../runtime/iteration/retry-policy.rkt")

(test-case "count-occurrences is pure (same input, same output)"
  (define items '("read" "edit" "read" "bash" "read" "edit"))
  (define result1 (count-occurrences items))
  (define result2 (count-occurrences items))
  (check-equal? result1 result2)
  (check-equal? (hash-ref result1 "read") 3)
  (check-equal? (hash-ref result1 "edit") 2)
  (check-equal? (hash-ref result1 "bash") 1))

(test-case "count-occurrences returns immutable hash"
  (define result (count-occurrences '("a" "b" "a")))
  (check-exn exn:fail? (lambda () (hash-set! result "c" 1))))

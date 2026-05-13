#lang racket

;; BOUNDARY: integration

;; tests/test-session-index-schema.rkt — tests for session-index/schema

(require rackunit
         rackunit/text-ui
         "../runtime/session-index/schema.rkt")

(define schema-tests
  (test-suite "Session Index Schema"

    (test-case "make-empty-index creates valid index"
      (define idx (make-empty-index))
      (check-true (session-index? idx))
      (check-equal? (hash-count (session-index-by-id idx)) 0)
      (check-equal? (vector-length (session-index-entry-order idx)) 0))

    (test-case "bookmark construction"
      (define bm (make-bookmark "bm-1" "entry-1" "my-label" 1000))
      (check-true (bookmark? bm))
      (check-equal? (bookmark-id bm) "bm-1")
      (check-equal? (bookmark-entry-id bm) "entry-1")
      (check-equal? (bookmark-label bm) "my-label")
      (check-equal? (bookmark-timestamp bm) 1000))

    (test-case "navigate-result construction"
      (define nr (navigate-result 'entry 'branch 'children #t))
      (check-true (navigate-result? nr))
      (check-true (navigate-result-leaf? nr)))))

(module+ main
  (run-tests schema-tests))
(module+ test
  (run-tests schema-tests))

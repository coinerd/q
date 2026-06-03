#lang racket

;; BOUNDARY: integration

;; tests/test-session-index-query.rkt — tests for session-index/query

(require rackunit
         rackunit/text-ui
         "../runtime/session-index/schema.rkt"
         "../runtime/session-index/query.rkt"
         (only-in "../util/message/protocol-types.rkt"
                  make-message
                  make-text-part
                  message-id
                  message-parent-id))

(define query-tests
  (test-suite "Session Index Query"

    (test-case "lookup-entry: empty index"
      (define idx (make-empty-index))
      (check-false (lookup-entry idx "nonexistent")))

    (test-case "children-of: empty index"
      (define idx (make-empty-index))
      (check-equal? (children-of idx "nonexistent") '()))

    (test-case "leaf-nodes: empty index"
      (define idx (make-empty-index))
      (check-equal? (leaf-nodes idx) '()))

    (test-case "resolve-active-leaf: empty index"
      (define idx (make-empty-index))
      (check-false (resolve-active-leaf idx)))

    (test-case "leaf-depth: non-existent entry"
      (define idx (make-empty-index))
      (check-false (leaf-depth idx "nonexistent")))

    (test-case "get-branch: non-existent entry"
      (define idx (make-empty-index))
      (check-false (get-branch idx "nonexistent")))))

(module+ main
  (run-tests query-tests))
(module+ test
  (run-tests query-tests))

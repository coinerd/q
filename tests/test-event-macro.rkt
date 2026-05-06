#lang racket/base

;; tests/test-event-macro.rkt — tests for define-event macro

(require rackunit
         "../util/event-macro.rkt")

;; Test: define-event creates a struct and predicate
(define-event test-event (field1 field2))

(test-case
 "define-event creates struct and predicate"
 (define ev (test-event "value1" "value2"))
 (check-true (test-event? ev))
 (check-equal? (test-event-field1 ev) "value1")
 (check-equal? (test-event-field2 ev) "value2"))

;; Test: predicate works correctly
(test-case
 "predicate correctly identifies events"
 (define ev1 (test-event "a" "b"))
 (check-true (test-event? ev1))
 (check-false (test-event? "not an event"))
 (check-false (test-event? 123)))

;; Tests are automatically run by `raco test`

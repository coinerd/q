#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-step-directive.rkt -- Step directive type tests (W-02)

(require rackunit
         "../runtime/iteration/directive.rkt")

(test-case "directive-recurse construction and accessors"
  (define d (directive-recurse 'ctx 'counters 'ws))
  (check-true (directive-recurse? d))
  (check-eq? (directive-recurse-new-ctx d) 'ctx)
  (check-eq? (directive-recurse-new-counters d) 'counters)
  (check-eq? (directive-recurse-ws d) 'ws))

(test-case "directive-stop construction and accessors"
  (define d (directive-stop 'final-result))
  (check-true (directive-stop? d))
  (check-eq? (directive-stop-result d) 'final-result))

(test-case "directive-yield construction and accessors"
  (define d (directive-yield '(ev1 ev2) 'ctx 'counters 'ws))
  (check-true (directive-yield? d))
  (check-equal? (directive-yield-events d) '(ev1 ev2)))

(test-case "step-directive? predicate accepts all variants"
  (check-true (step-directive? (directive-recurse 1 2 3)))
  (check-true (step-directive? (directive-stop 'done)))
  (check-true (step-directive? (directive-yield '() 1 2 3))))

(test-case "step-directive? rejects non-directives"
  (check-false (step-directive? 'recurse))
  (check-false (step-directive? 42))
  (check-false (step-directive? #f))
  (check-false (step-directive? (hash 'status 'ready))))

(test-case "directive-recurse accessor works"
  (define d (directive-recurse '(a b) '(c d) 'ws))
  (check-equal? (directive-recurse-new-ctx d) '(a b)))

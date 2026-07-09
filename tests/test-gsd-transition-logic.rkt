#lang racket

;; @speed fast
;; @suite default

(require rackunit
         "../extensions/gsd/transition-logic.rkt")

(test-case "find-transition-path same-state returns explicit empty path"
  (check-equal? (find-transition-path 'idle 'idle) '())
  (check-equal? (find-transition-path 'verifying 'verifying) '()))

(test-case "find-transition-path no-path remains distinguishable from same-state"
  (check-false (find-transition-path 'idle 'not-a-gsd-state)))

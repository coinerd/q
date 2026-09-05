#lang racket/base
;; W7 grouped-mode fixture: eligible; explicit (exit 0) after passing check.
(require rackunit)

(module+ test
  (require rackunit)
  (check-equal? (+ 1 1) 2 "f04 before explicit exit")
  (exit 0))

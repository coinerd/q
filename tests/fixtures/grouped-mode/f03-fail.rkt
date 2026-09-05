#lang racket/base
;; W7 grouped-mode fixture: eligible; one failing + one passing check.
(require rackunit)

(module+ test
  (require rackunit)
  (check-equal? 1 2 "f03 intentional failure")
  (check-true #t "f03 passing companion"))

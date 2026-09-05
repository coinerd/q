#lang racket/base
;; @isolation process
;; W7 grouped-mode fixture: declares process isolation -> grouped gate refuses.
(require rackunit)

(module+ test
  (require rackunit)
  (check-true #t "f08 process isolation declared"))

#lang racket/base
;; @mutates env cwd
;; W7 grouped-mode fixture: declares mutation -> grouped gate refuses.
(require rackunit)

(module+ test
  (require rackunit)
  (putenv "W7_F07_SEEN" "1")
  (check-true #t "f07 declared mutation"))

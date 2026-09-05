#lang racket/base
;; W7 grouped-mode fixture: eligible; emits distinguishable stdout/stderr lines.
(require rackunit)

(module+ test
  (require rackunit)
  (displayln "F02-OUT-LINE-1")
  (displayln "F02-OUT-LINE-2")
  (eprintf "F02-ERR-LINE-1\n")
  (check-true #t "f02 output capture"))

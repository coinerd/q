#lang racket/base
;; W7 grouped-mode fixture: eligible; sleeps long enough to trip a short timeout.
(require rackunit)

(module+ test
  (require rackunit)
  (displayln "GMD-W7-TIMEOUT-STARTED")
  (flush-output)
  (check-true (begin
                (sleep 60)
                #t)
              "f09 sleeps past short timeout"))

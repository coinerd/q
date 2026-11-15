#lang racket/base

;; W7 characterization fixture: module+ test hangs. Exercised only with a
;; short runner timeout so both modes record a timeout verdict.
(require rackunit)

(module+ test
  (displayln "GMD-W7-TIMEOUT-STARTED")
  (flush-output)
  (sleep 600))

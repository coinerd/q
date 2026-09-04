#lang racket/base

;; W7 characterization fixture: module+ test hangs. Exercised only with a
;; short runner timeout so both modes record a timeout verdict.
(require rackunit)

(module+ test
  (sleep 600))

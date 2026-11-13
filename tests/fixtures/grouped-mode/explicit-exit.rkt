#lang racket/base

;; W7 characterization fixture: module+ test body requests (exit 0).
;; Grouped execution must redirect the early exit, record failure, and survive.
(require rackunit)

(module+ test
  (exit 0))

#lang racket/base

;; W7 characterization fixture: module+ test body requests (exit 0).
;; Grouped execution must redirect exit, record code 0, and survive.
(require rackunit)

(module+ test
  (exit 0))

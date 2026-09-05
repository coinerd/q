#lang racket/base

;; W7 characterization fixture: module+ test body raises an exception.
;; Grouped execution must record exit 1 with the marker on stderr.
(require rackunit)

(module+ test
  (error "GMD-W7-EXCEPTION-MARKER"))

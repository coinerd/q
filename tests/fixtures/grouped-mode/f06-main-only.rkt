#lang racket/base
;; W7 grouped-mode fixture: module+ main only, no module+ test.
;; Grouped gate: missing-module-plus-test-form -> subprocess fallback.
(require rackunit)

(module+ main
  (require rackunit)
  (check-true #t "f06 main submodule only"))

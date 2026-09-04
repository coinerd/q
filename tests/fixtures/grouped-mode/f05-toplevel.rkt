#lang racket/base
;; W7 grouped-mode fixture: top-level test body only, no module+ test.
;; Grouped gate: missing-module-plus-test-form -> subprocess fallback.
(require rackunit)

(check-true #t "f05 top-level only")

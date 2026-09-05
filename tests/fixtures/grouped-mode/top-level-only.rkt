#lang racket/base

;; W7 characterization fixture: top-level test body, no module+ wrapper.
;; Grouped requires the submodule form, so this falls back with reason
;; missing-module-plus-test-form; subprocess keeps raco discovery parity.
(require rackunit)
(check-true #t)
(displayln "top-level-only ran")

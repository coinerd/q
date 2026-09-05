#lang racket/base

;; W7 characterization fixture: HAS (module+ test ...) but only bare check
;; forms (no run-tests self-report). Grouped dynamic-require runs the checks
;; silently (exit 0, zero parsed tests) and must fall back with the stable
;; named reason zero-parsed-output.
(require rackunit)

(module+ test
  (check-true #t)
  (check-equal? 1 1))

#lang racket/base
;; W7 grouped-mode fixture: eligible BY THE GATES (no declarations), but it
;; mutates the host environment. Undeclared mutation cannot be detected by
;; static gates; grouped execution therefore leaks it into the host process.
;; This fixture documents WHY mutation declaration headers are mandatory and why
;; grouped stays opt-in (v1.00.27 boundary).
(require rackunit)

(module+ test
  (require rackunit)
  (putenv "W7_F10_UNDECLARED" "leaked")
  (check-true #t "f10 undeclared mutation"))

#lang racket/base
;; W7 grouped-mode fixture: eligible (no declaration header tokens, real
;; module+ test submodule) AND it SELF-REPORTS via rackunit/text-ui.
;; Self-reporting is the W6 contract for a file to keep grouped execution:
;; bare check-* forms produce no parseable output in-process and fall back
;; to subprocess with the named reason zero-parsed-output.
(module+ test
  (require rackunit
           rackunit/text-ui)
  (run-tests (test-suite "f01 eligible pass"
               (test-case "always passes"
                 (check-true #t)))))

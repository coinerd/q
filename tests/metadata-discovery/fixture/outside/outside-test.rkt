;; @suite all
;; @speed fast
;; @boundary unit
;; @isolation temp-dir
;; @mutates none
;; @not-test #t

#lang racket/base

;; Fixture: a test-shaped file that lives OUTSIDE the discovery root
;; (fixture/tests/). It must never be discovered, because collect-test-files
;; only walks <base-dir>/tests. Marked @not-test so that if a future bug
;; ever pulls it into a discovery walk, it is still excluded by metadata.

(module outside-fixture racket/base
  (provide outside-thing)
  (define (outside-thing) 'outside))

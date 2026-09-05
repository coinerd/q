#lang racket/base

;; @suite testing
;; @speed fast
;; @boundary e2e
;; @mutates temp
;; @isolation process
;;
;; W5 frozen discovery fixture: declared process isolation makes it a
;; mutating-family file via metadata (name carries no mutating pattern).
;; FIXTURE DATA — never executed by collection; copied to a temp root by
;; tests/test-run-tests-shard.rkt.

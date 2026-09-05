#lang racket/base

;; @suite tui
;; @speed fast
;; @boundary integration
;;
;; W5 frozen discovery fixture: suite tui metadata with a basename that
;; does NOT match the test-tui- heuristic prefix and a path without /tui/,
;; so only metadata selects it into the tui suite.
;; FIXTURE DATA — never executed by collection; copied to a temp root by
;; tests/test-run-tests-shard.rkt.

#lang racket/base

;; @suite default
;; @speed ultra
;; @not-test true
;; @flarb wobble
;;
;; W5 frozen discovery fixture: MALFORMED/edge metadata on purpose —
;; unknown tag (flarb) and invalid speed value (ultra). Declared not-test.
;; so it is excluded from every gate and from canonical metadata
;; discovery; the malformed-tag findings are asserted directly through
;; validate-file in tests/test-run-tests-shard.rkt.
;; FIXTURE DATA — never executed by collection; copied to a temp root by
;; tests/test-run-tests-shard.rkt.

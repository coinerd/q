#lang racket/base

;; scripts/run-tests/hotspot-submit.rkt — RESERVED for the W1 baseline submit
;; leg (gate ownership for test-runtime/hotspot-baseline/v1 evidence).
;;
;; Status: no consumer exists yet (zero references across scripts/ and
;; tests/ as of the W0 hotspot session), so no contract is implemented here.
;; Deliberately NOT speculatively coded: the submit gate must be defined by
;; the wave that consumes it. See hotspot-benchmark.rkt for the collector and
;; hotspot-baseline.rkt for its canonical entrypoint.

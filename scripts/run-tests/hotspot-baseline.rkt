#lang racket/base

;; scripts/run-tests/hotspot-baseline.rkt — canonical CLI entrypoint for the
;; v1.00.24 W0 hotspot baseline collector.
;;
;; The baseline manifest schema is `test-runtime/hotspot-baseline/v1`; the
;; `command` field recorded inside every baseline manifest names THIS script
;; (see hotspot-benchmark.rkt and tests/test-hotspot-benchmark.rkt). All
;; collection, canonicalization, validation, and `--check` semantics live in
;; hotspot-benchmark.rkt; this file exists so the recorded command is stable
;; and replayable even if engine internals move.
;;
;; Typical verify command:
;;   racket scripts/run-tests/hotspot-baseline.rkt \
;;     --manifest test-runtime/baseline.json --samples 10 \
;;     --allowlist scripts/run-tests/hotspot-allowlist.txt

(require "hotspot-benchmark.rkt")

(module+ main
  (run-hotspot-benchmark-main
   (vector->list (current-command-line-arguments))
   #:program "hotspot-baseline"))

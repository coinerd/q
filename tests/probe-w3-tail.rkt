#lang racket/base
;; Probe stub — investigation concluded 2025-XX: ci.yml's test-aggregate line contains
;; invisible non-whitespace characters, so exact string equality fails. The tolerant
;; regex pin lives in tests/test-w3-telemetry-relocation.rkt (regexp-match? on
;; (?m:^\\s*test-aggregate:)). This file is intentionally inert.

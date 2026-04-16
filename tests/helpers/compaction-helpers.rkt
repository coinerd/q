#lang racket/base

;; q/tests/helpers/compaction-helpers.rkt
;; Centralized helpers for compaction tests.
;;
;; Provides a standard low-token configuration so compaction actually
;; triggers with test-sized messages (~4 tokens each).

(require "../../runtime/token-compaction.rkt")

(provide LOW-TOKEN-CONFIG
         low-token-config)

;; Standard low token config for compaction tests:
;;   max-context = 10 tokens, recent-window = 5, token-budget = 20
;; With ~4 tokens per test message, this ensures compaction triggers
;; after just a few messages.
(define LOW-TOKEN-CONFIG (token-compaction-config 10 5 20))

;; Alias for clarity
(define low-token-config LOW-TOKEN-CONFIG)

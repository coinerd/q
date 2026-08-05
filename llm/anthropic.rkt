#lang racket/base

;; llm/anthropic.rkt — Anthropic provider adapter (facade)
;;
;; v0.99.43 W0: Monolithic module (571 lines) decomposed into sub-modules:
;;   llm/anthropic/format.rkt — pure request/response/SSE formatting
;;   llm/anthropic/sse.rkt    — HTTP transport + provider constructor
;; This file is now a thin facade re-exporting the public API. No callers
;; change; the public surface is byte-for-byte backward compatible.

(require racket/contract
         (only-in "provider.rkt" provider?)
         "anthropic/sse.rkt"
         "anthropic/format.rkt"
         ;; W8 v0.99.35: Pure helpers extracted from this module
         "anthropic-helpers.rkt")

;; Provider constructor
(provide (contract-out [make-anthropic-provider (-> hash? provider?)])
         ;; Request/response helpers (exported for testing)
         anthropic-build-request-body
         anthropic-parse-response
         anthropic-parse-stream-chunks
         anthropic-parse-single-event
         ;; Internal helpers for testing
         anthropic-translate-tool)

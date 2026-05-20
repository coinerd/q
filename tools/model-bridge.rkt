#lang racket/base

;; tools/model-bridge.rkt — Thin facade for LLM provider access from tools
;;
;; Re-exports a minimal set of LLM functions needed by tool implementations
;; (specifically spawn-subagent). This avoids direct llm/ imports from tools/,
;; maintaining the architectural layer boundary.
;;
;; Layer: tools → model-bridge → llm (facade pattern)

(require racket/contract
         "../llm/provider.rkt"
         "../llm/model.rkt")

(provide (contract-out [make-model-request (-> any/c any/c any/c any/c)]
                       [provider-send (-> any/c any/c any/c)]
                       [model-response-content (-> any/c any/c)]
                       [model-response-stop-reason (-> any/c any/c)]
                       [make-model-response (-> any/c any/c any/c any/c any/c)]
                       [make-mock-provider
                        (->* (any/c) (#:name string? #:stream-chunks any/c) any/c)]))

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

(provide (contract-out [make-model-request (-> list? list? hash? model-request?)])
         model-request?
         (contract-out
          [provider-send (-> provider? model-request? model-response?)]
          [model-response-content (-> model-response? list?)]
          [model-response-stop-reason (-> model-response? (or/c symbol? #f))]
          [make-model-response (-> list? hash? string? (or/c symbol? #f) model-response?)]
          [make-mock-provider
           (->* (model-response?) (#:name string? #:stream-chunks (or/c list? #f)) provider?)]))

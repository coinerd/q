#lang racket/base

;; tools/model-bridge.rkt — Thin facade for LLM provider access from tools
;;
;; Re-exports a minimal set of LLM functions needed by tool implementations
;; (specifically spawn-subagent). This avoids direct llm/ imports from tools/,
;; maintaining the architectural layer boundary.
;;
;; Layer: tools → model-bridge → llm (facade pattern)

(require "../llm/provider.rkt"
         "../llm/model.rkt")

(provide make-model-request
         provider-send
         model-response-content
         model-response-stop-reason
         make-model-response
         make-mock-provider)

#lang racket/base

;; extensions/mcp-adapter.rkt — Model Context Protocol (MCP) adapter FACADE
;; STABILITY: evolving
;;
;; Thin facade that re-exports everything from sub-modules:
;;   extensions/mcp/protocol.rkt  — protocol dispatch, stdio server, event emission
;;   extensions/mcp/tool-bridge.rkt — tool serialization, tools/call handling
;;   extensions/mcp/client.rkt   — client message construction and parsing
;;
;; MAS Schritt 6: Verteilte Härtung & MCP-Integration
;;
;; MCP is an open protocol (JSON-RPC 2.0) for communication between
;; LLM applications and tool providers.
;;
;; This module implements the protocol layer only — it handles JSON-RPC
;; message construction, parsing, and routing. The transport layer
;; (stdio or TCP) is handled by the caller.
;;
;; Feature-gated via mas.mcp.enabled (default false).
;;
;; NOTE: This is a thin facade. Consumers should continue to require
;; from extensions/mcp-adapter.rkt for backward compatibility.

(require racket/contract
         (only-in "../tools/registry.rkt" tool-registry?)
         (only-in "../tools/tool.rkt" tool?)
         "mcp/protocol.rkt"
         "mcp/tool-bridge.rkt"
         "mcp/client.rkt")

;; ============================================================
;; Re-exports from extensions/mcp/protocol.rkt
;; ============================================================

(provide MCP-PROTOCOL-VERSION
         MCP-SERVER-NAME
         MCP-SERVER-VERSION
         current-mcp-execute-fn
         current-mcp-event-sink)

(provide (contract-out [handle-mcp-request (-> hash? tool-registry? procedure? hash?)]
                       [handle-mcp-raw-input (-> string? tool-registry? procedure? hash?)]
                       [run-mcp-stdio-server! (-> tool-registry? void?)]
                       [mcp-notification? (-> any/c boolean?)]))

;; ============================================================
;; Re-exports from extensions/mcp/tool-bridge.rkt
;; ============================================================

(provide (contract-out [tool->mcp-jsexpr (-> tool? hash?)]
                       [tools->mcp-list (-> tool-registry? (listof hash?))]))

;; ============================================================
;; Re-exports from extensions/mcp/client.rkt
;; ============================================================

(provide (contract-out [build-mcp-initialize (-> (or/c string? number?) hash?)]
                       [build-mcp-tools-list (-> (or/c string? number?) hash?)]
                       [build-mcp-tools-call (-> (or/c string? number?) string? hash? hash?)]
                       [build-mcp-initialized-notification (-> hash?)]
                       [build-mcp-ping (-> (or/c string? number?) hash?)]
                       [parse-mcp-initialize-response (-> hash? (or/c hash? #f))]
                       [parse-mcp-tools-list-response (-> hash? (listof hash?))]
                       [parse-mcp-tools-call-response (-> hash? (listof hash?))]
                       [mcp-error-response? (-> any/c boolean?)]
                       [mcp-error-code (-> hash? (or/c number? #f))]
                       [mcp-error-message (-> hash? (or/c string? #f))]))

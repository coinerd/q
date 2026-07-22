#lang racket/base

;; extensions/mcp/client.rkt — MCP client message construction and parsing
;; STABILITY: evolving
;;
;; Sub-module of the MCP adapter split. Handles JSON-RPC 2.0 message
;; construction for MCP client operations (initialize, tools/list,
;; tools/call, notifications, ping) and response parsing.
;;
;; Part of the mcp-adapter module split (T-4).

(require racket/contract
         json
         (only-in "../../util/version.rkt" q-version)
         (only-in "protocol.rkt" MCP-PROTOCOL-VERSION MCP-SERVER-NAME MCP-SERVER-VERSION))

;; ============================================================
;; MCP Client: Message Construction
;; ============================================================

;; Build an MCP initialize request.
(define (build-mcp-initialize id)
  (hasheq 'jsonrpc
          "2.0"
          'id
          id
          'method
          "initialize"
          'params
          (hasheq 'protocolVersion
                  MCP-PROTOCOL-VERSION
                  'clientInfo
                  (hasheq 'name MCP-SERVER-NAME 'version MCP-SERVER-VERSION)
                  'capabilities
                  (hasheq))))

;; Build an MCP tools/list request.
(define (build-mcp-tools-list id)
  (hasheq 'jsonrpc "2.0" 'id id 'method "tools/list" 'params (hasheq)))

;; Build an MCP tools/call request.
(define (build-mcp-tools-call id tool-name args)
  (hasheq 'jsonrpc
          "2.0"
          'id
          id
          'method
          "tools/call"
          'params
          (hasheq 'name tool-name 'arguments args)))

;; Build an MCP notifications/initialized message.
(define (build-mcp-initialized-notification)
  (hasheq 'jsonrpc "2.0" 'method "notifications/initialized"))

;; Build an MCP ping request.
(define (build-mcp-ping id)
  (hasheq 'jsonrpc "2.0" 'id id 'method "ping"))

;; ============================================================
;; MCP Client: Response Parsing
;; ============================================================

;; Parse an MCP initialize response.
;; Returns server capabilities hash or #f on error.
(define (parse-mcp-initialize-response resp)
  (define result (hash-ref resp 'result #f))
  (and result (hash-ref result 'capabilities #f)))

;; Parse an MCP tools/list response.
;; Returns list of tool hashes or '() on error.
(define (parse-mcp-tools-list-response resp)
  (define result (hash-ref resp 'result #f))
  (if result
      (hash-ref result 'tools '())
      '()))

;; Parse an MCP tools/call response.
;; Returns the content list or '() if missing.
(define (parse-mcp-tools-call-response resp)
  (define result (hash-ref resp 'result #f))
  (if result
      (hash-ref result 'content '())
      '()))

;; Check if a response is an error response.
(define (mcp-error-response? resp)
  (and (hash? resp) (hash-has-key? resp 'error)))

;; Extract error details from an error response.
;; Returns (cons code message) or #f.
(define (mcp-error-code resp)
  (define err (hash-ref resp 'error #f))
  (and err (hash-ref err 'code #f)))

(define (mcp-error-message resp)
  (define err (hash-ref resp 'error #f))
  (and err (hash-ref err 'message #f)))

;; ============================================================
;; Provides
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

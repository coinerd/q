#lang racket/base

;; @speed fast
;; @suite arch
;; BOUNDARY: architecture

(require rackunit
         racket/runtime-path
         racket/string
         racket/file)

(define-runtime-path repo-root "..")

(define (source relative)
  (file->string (build-path repo-root relative)))

(test-case "provider transport ownership stays below agent and tools"
  (define bridge (source "tools/builtins/provider-hash-bridge.rkt"))
  (define loop (source "agent/loop-messages.rkt"))
  (define spawn (source "tools/builtins/spawn-subagent.rkt"))
  (check-false (string-contains? bridge "agent/loop-messages.rkt"))
  (check-true (string-contains? bridge "util/message/provider-transport.rkt"))
  (check-true (string-contains? loop "util/message/provider-transport.rkt"))
  (check-true (string-contains? loop "serialize-provider-messages"))
  ;; v0.99.65: spawn-subagent no longer directly imports provider-transport.
  ;; Provider transport ownership moved to the message layer.
  (check-false (string-contains? spawn "util/message/provider-transport.rkt"))
  (check-false (string-contains? spawn "../../agent/")))

(test-case "compatibility bridge contains no provider wire constructors"
  (define bridge (source "tools/builtins/provider-hash-bridge.rkt"))
  (for ([legacy (in-list '("tool_call" "tool_result" "tool_calls" "tool_call_id"))])
    (check-false (string-contains? bridge legacy))))

;; v0.99.81 W1 PN-2: Transport boundary — no unsupported socket/FD extraction.
;; The net/http-client response-port boundary exposes no supported public
;; @boundary unit
;; socket/FD extraction seam after HTTP/TLS wrapping. Production code must
;; not guess at FFI signatures, platform constants, or SO_KEEPALIVE options.
(test-case "PN-2: provider code has no unsupported FD/socket extraction"
  (define transport-files
    (list "llm/stream.rkt"
          "llm/http-helpers.rkt"
          "llm/openai-compatible.rkt"
          "llm/anthropic.rkt"
          "llm/gemini.rkt"
          "llm/azure-openai.rkt"))
  (for ([path (in-list transport-files)])
    (define code (source path))
    (check-false (string-contains? code "SO_KEEPALIVE")
                 (format "~a must not claim TCP SO_KEEPALIVE" path))
    (check-false (string-contains? code "file-stream->fd")
                 (format "~a must not extract FD from response port" path))
    (check-false (string-contains? code "port-provides-progress-ev?")
                 (format "~a must not use unsupported port progress predicate" path))
    (check-false (string-contains? code "tcp-set-socket-option")
                 (format "~a must not guess tcp FFI" path))
    (check-false (string-contains? code "ffi/tcp") (format "~a must not import tcp FFI" path))))

(test-case "PN-2: liveness metadata is application-level only"
  (define stream-code (source "llm/stream.rkt"))
  (check-true (string-contains? stream-code "received-heartbeats?"))
  (check-true (string-contains? stream-code "received-any-data?"))
  (check-true (string-contains? stream-code "phase-from-state"))
  (check-false (string-contains? stream-code "SO_KEEPALIVE")))

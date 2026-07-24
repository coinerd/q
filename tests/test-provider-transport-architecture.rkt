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

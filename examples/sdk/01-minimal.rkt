#lang racket/base

;; examples/sdk/01-minimal.rkt — Simplest session with defaults
;;
;; Usage: racket examples/sdk/01-minimal.rkt
;; This example creates a minimal agent session. Replace the provider
;; with a real LLM provider for actual usage.

(require "../../interfaces/sdk.rkt")

(define rt (create-agent-session #:provider (hasheq 'type 'test)))

(printf "Session created: ~a~n" (hash-ref (session-info rt) 'session-id))
(printf "Active: ~a~n" (hash-ref (session-info rt) 'active?))

#lang racket/base

;; util/extension-types.rkt — Pure type definitions for extension context (M-06)
;; A-3: Also provides extension-registry? predicate for Typed Racket boundary. — Pure type definitions for extension context (M-06)
;;
;; Contains only the struct definition with NO runtime/ or llm/ imports.
;; Runtime-dependent convenience methods stay in extensions/context.rkt.

(require racket/contract)

;; A-3: Standalone extension-registry? predicate.
;; Typed Racket #:opaque creates its own predicate; this is for untyped consumers.
;; Matches the struct definition in extensions/api.rkt.
(define extension-registry? procedure?)

(provide (contract-out [extension-registry? (-> any/c boolean?)])
         extension-ctx
         extension-ctx?
         extension-ctx-session-id
         extension-ctx-session-dir
         extension-ctx-event-bus
         extension-ctx-extension-registry
         extension-ctx-model-name
         extension-ctx-cancellation-token
         extension-ctx-working-directory
         extension-ctx-session-store
         extension-ctx-tool-registry
         extension-ctx-command-registry
         extension-ctx-ui-channel
         extension-ctx-provider-registry
         extension-ctx-session-messages
         extension-ctx-session-token-usage
         extension-ctx-gsd-ctx)

;; Extension context struct — bundles all session/runtime state that
;; extension handlers may need. All fields are read-only after construction.
;; Transparent struct for testability and debugging.
(struct extension-ctx
        (session-id ; string?
         session-dir ; (or/c path-string? #f)
         event-bus ; event-bus?
         extension-registry ; extension-registry?
         model-name ; (or/c string? #f)
         cancellation-token ; (or/c cancellation-token? #f)
         working-directory ; (or/c path-string? #f)
         ;; FEAT-58: rich extension context fields
         session-store ; (or/c any/c #f) — read-only session access
         tool-registry ; (or/c any/c #f) — dynamic tool registration
         command-registry ; (or/c any/c #f) — slash command registration
         ui-channel ; (or/c channel? #f) — user interaction requests
         provider-registry ; (or/c any/c #f) — LLM provider access (#1114)
         ;; #1223: session state fields for query API
         session-messages ; (or/c (listof hash?) #f) — read-only message history
         session-token-usage ; (or/c hash? #f) — token usage stats
         gsd-ctx ; (or/c any/c #f) — per-session GSD state (C-01 v0.35.1)
         )
  #:transparent)

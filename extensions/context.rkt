#lang racket/base

;; extensions/context.rkt — rich extension context
;;
;; Provides:
;;   - extension-ctx struct bundling all session/runtime state
;;     that extension handlers may need
;;   - Accessor functions: ctx-session-id, ctx-session-dir,
;;     ctx-event-bus, ctx-extension-registry, ctx-model,
;;     ctx-signal, ctx-cwd
;;   - Constructor: make-extension-ctx with keyword args
;;   - #1220: ctx-register-provider! convenience wrapper
;;   - #1223: ctx-session-messages, ctx-session-token-count query methods
;;
;; Design notes:
;;   - Fields are read-only after construction
;;   - Optional fields default to #f (model-name, cancellation-token, working-directory)
;;   - Transparent struct for testability and debugging

(require racket/contract
         (only-in "../runtime/provider-registry.rkt"
                  register-provider!
                  unregister-provider!
                  list-providers
                  lookup-provider
                  provider-info?
                  provider-info-provider)
         (only-in "../llm/provider.rkt" provider?)
         ;; M-06: Import struct from pure types module
         "../util/extension-types.rkt")

;; Struct and predicate
(provide extension-ctx
         extension-ctx?
         ;; Constructor
         make-extension-ctx
         ;; Accessors (original)
         ctx-session-id
         ctx-session-dir
         ctx-event-bus
         ctx-extension-registry
         ctx-model
         ctx-signal
         ctx-cwd
         ;; Accessors (FEAT-58: rich extension context)
         ctx-session-store
         ctx-tool-registry
         ctx-command-registry
         ctx-ui-channel
         ;; Accessor (#1114: provider registry access for extensions)
         ctx-provider-registry
         ctx-gsd-ctx
         ;; #1220: Provider convenience methods
         ctx-register-provider!
         ctx-unregister-provider!
         ctx-list-providers
         ctx-lookup-provider
         ;; #1223: Session state query methods
         ctx-session-messages
         ctx-session-token-count
         ctx-session-model)

;; M-06: Struct definition moved to util/extension-types.rkt (no runtime imports).
;; Re-exported here for backward compatibility.
;; Struct fields: session-id, session-dir, event-bus, extension-registry,
;;   model-name, cancellation-token, working-directory, session-store,
;;   tool-registry, command-registry, ui-channel, provider-registry,
;;   session-messages, session-token-usage, gsd-ctx

;; ============================================================
;; Constructor — keyword args with optional fields defaulting to #f
;; ============================================================

(define (make-extension-ctx #:session-id session-id
                            #:session-dir session-dir
                            #:event-bus event-bus
                            #:extension-registry extension-registry
                            #:model-name [model-name #f]
                            #:cancellation-token [cancellation-token #f]
                            #:working-directory [working-directory #f]
                            ;; FEAT-58: optional rich context fields
                            #:session-store [session-store #f]
                            #:tool-registry [tool-registry #f]
                            #:command-registry [command-registry #f]
                            #:ui-channel [ui-channel #f]
                            #:provider-registry [provider-registry #f]
                            ;; #1223: session state query fields
                            #:session-messages [session-messages #f]
                            #:session-token-usage [session-token-usage #f]
                            #:gsd-ctx [gsd-ctx #f])
  (extension-ctx session-id
                 session-dir
                 event-bus
                 extension-registry
                 model-name
                 cancellation-token
                 working-directory
                 session-store
                 tool-registry
                 command-registry
                 ui-channel
                 provider-registry
                 session-messages
                 session-token-usage
                 gsd-ctx))

;; ============================================================
;; Accessor aliases — short, ergonomic names
;; ============================================================

(define ctx-session-id extension-ctx-session-id)
(define ctx-session-dir extension-ctx-session-dir)
(define ctx-event-bus extension-ctx-event-bus)
(define ctx-extension-registry extension-ctx-extension-registry)
(define ctx-model extension-ctx-model-name)
(define ctx-signal extension-ctx-cancellation-token)
(define ctx-cwd extension-ctx-working-directory)
;; FEAT-58: rich context accessor aliases
(define ctx-session-store extension-ctx-session-store)
(define ctx-tool-registry extension-ctx-tool-registry)
(define ctx-command-registry extension-ctx-command-registry)
(define ctx-ui-channel extension-ctx-ui-channel)
;; #1114: provider registry accessor alias
(define ctx-provider-registry extension-ctx-provider-registry)
;; C-01 v0.35.1: GSD context accessor
(define ctx-gsd-ctx extension-ctx-gsd-ctx)

;; ============================================================
;; #1220: Provider convenience methods
;; ============================================================

;; Register a provider via the context's provider-registry.
;; Returns 'registered or 'updated on success.
;; Returns (hasheq 'error #t 'message "...") when no registry on context.
(define (ctx-register-provider! ctx name provider-instance #:config [config (hasheq)])
  (define reg (extension-ctx-provider-registry ctx))
  (if reg
      (register-provider! reg name provider-instance #:config config)
      (hasheq 'error #t 'message "No provider-registry on context")))

;; Unregister a provider via the context's provider-registry.
(define (ctx-unregister-provider! ctx name)
  (define reg (extension-ctx-provider-registry ctx))
  (when reg
    (unregister-provider! reg name)))

;; List all providers from the context's provider-registry.
;; Returns empty list if no registry.
(define (ctx-list-providers ctx)
  (define reg (extension-ctx-provider-registry ctx))
  (if reg
      (list-providers reg)
      '()))

;; Look up a provider by name from the context's provider-registry.
(define (ctx-lookup-provider ctx name)
  (define reg (extension-ctx-provider-registry ctx))
  (if reg
      (lookup-provider reg name)
      #f))

;; ============================================================
;; #1223: Session state query methods
;; ============================================================

;; Get read-only conversation history from context.
;; Returns (listof hash) or empty list if not available.
(define (ctx-session-messages ctx)
  (or (extension-ctx-session-messages ctx) '()))

;; Get token usage stats from context.
;; Returns hash with 'input-tokens, 'output-tokens, or empty hash.
(define (ctx-session-token-count ctx)
  (or (extension-ctx-session-token-usage ctx) (hasheq)))

;; Get current model name from context.
;; Returns string or #f.
(define (ctx-session-model ctx)
  (extension-ctx-model-name ctx))

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
;;
;; Design notes:
;;   - Fields are read-only after construction
;;   - Optional fields default to #f (model-name, cancellation-token, working-directory)
;;   - Transparent struct for testability and debugging

(require racket/contract)

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
         ctx-ui-channel)

;; ============================================================
;; extension-ctx struct
;; ============================================================

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
         ui-channel) ; (or/c channel? #f) — user interaction requests
  #:transparent)

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
                            #:ui-channel [ui-channel #f])
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
                 ui-channel))

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

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

(provide
 ;; Struct and predicate
 extension-ctx
 extension-ctx?
 ;; Constructor
 make-extension-ctx
 ;; Accessors
 ctx-session-id
 ctx-session-dir
 ctx-event-bus
 ctx-extension-registry
 ctx-model
 ctx-signal
 ctx-cwd)

;; ============================================================
;; extension-ctx struct
;; ============================================================

(struct extension-ctx
  (session-id          ; string?
   session-dir         ; (or/c path-string? #f)
   event-bus           ; event-bus?
   extension-registry  ; extension-registry?
   model-name          ; (or/c string? #f)
   cancellation-token  ; (or/c cancellation-token? #f)
   working-directory)  ; (or/c path-string? #f)
  #:transparent)

;; ============================================================
;; Constructor — keyword args with optional fields defaulting to #f
;; ============================================================

(define (make-extension-ctx
         #:session-id session-id
         #:session-dir session-dir
         #:event-bus event-bus
         #:extension-registry extension-registry
         #:model-name [model-name #f]
         #:cancellation-token [cancellation-token #f]
         #:working-directory [working-directory #f])
  (extension-ctx session-id
                 session-dir
                 event-bus
                 extension-registry
                 model-name
                 cancellation-token
                 working-directory))

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

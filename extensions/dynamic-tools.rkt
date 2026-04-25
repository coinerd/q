#lang racket/base

;; extensions/dynamic-tools.rkt — dynamic tool registration from extensions
;;
;; FEAT-62: Provides the extension-facing API for dynamically registering
;; and deregistering tools at runtime via the extension context.
;;
;; Extensions call:
;;   (ext-register-tool! ctx name description schema handler)
;;   (ext-unregister-tool! ctx name)
;;
;; This delegates to the tool-registry stored in the extension-ctx.

(require racket/contract
         "context.rkt"
         "../tools/tool.rkt")

(provide (contract-out [ext-register-tool!
                        (->* (extension-ctx? string? ; name
                                             string? ; description
                                             hash? ; schema (JSON Schema)
                                             procedure?) ; handler (hash? -> tool-result?)
                             (#:prompt-guidelines (or/c string? #f))
                             void?)]
                       [ext-unregister-tool! (-> extension-ctx? string? void?)]
                       [ext-list-dynamic-tools (-> extension-ctx? (listof string?))]
                       [ext-set-active-tools! (-> extension-ctx? (listof string?) void?)]))

;; ============================================================
;; ext-register-tool! : extension-ctx? string? string? hash? procedure? -> void?
;; ============================================================

;; Register a tool dynamically from an extension.
;; The tool-registry in the extension context must be set (not #f).
;;
;; FIX: The tool scheduler always calls handlers with (args exec-ctx), but
;; extension handlers typically take only (args). We wrap the handler to
;; accept both arguments, making all extension tools work without requiring
;; every handler to add [exec-ctx #f].
(define (ext-register-tool! ctx
                            name
                            description
                            schema
                            handler
                            #:prompt-guidelines [prompt-guidelines #f])
  (define reg (ctx-tool-registry ctx))
  (unless reg
    (error
     'ext-register-tool!
     "no tool-registry available in extension context. \
            Ensure #:tool-registry is set when creating the context."))
  ;; Wrap handler to always accept (args exec-ctx) — defense against arity mismatch
  ;; when the scheduler calls ((tool-execute t) args exec-ctx).
  (define wrapped-handler (lambda (args exec-ctx) (handler args)))
  (define t (make-tool name description schema wrapped-handler #:prompt-guidelines prompt-guidelines))
  (register-tool! reg t))

;; ============================================================
;; ext-unregister-tool! : extension-ctx? string? -> void?
;; ============================================================

;; Deregister a dynamically registered tool.
(define (ext-unregister-tool! ctx name)
  (define reg (ctx-tool-registry ctx))
  (unless reg
    (error 'ext-unregister-tool! "no tool-registry available in extension context"))
  (unregister-tool! reg name))

;; ============================================================
;; ext-list-dynamic-tools : extension-ctx? -> (listof string?)
;; ============================================================

;; List names of all dynamically registered tools.
(define (ext-list-dynamic-tools ctx)
  (define reg (ctx-tool-registry ctx))
  (if reg
      (tool-names reg)
      '()))

;; ============================================================
;; ext-set-active-tools! : extension-ctx? (listof string?) -> void?
;; ============================================================

;; Restrict active tools to the named set.
(define (ext-set-active-tools! ctx active-names)
  (define reg (ctx-tool-registry ctx))
  (unless reg
    (error 'ext-set-active-tools! "no tool-registry available in extension context"))
  (set-active-tools! reg active-names))

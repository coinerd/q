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
                        (-> extension-ctx?
                            string? ; name
                            string? ; description
                            hash? ; schema (JSON Schema)
                            procedure? ; handler (hash? -> tool-result?)
                            void?)]
                       [ext-unregister-tool! (-> extension-ctx? string? void?)]
                       [ext-list-dynamic-tools (-> extension-ctx? (listof string?))]))

;; ============================================================
;; ext-register-tool! : extension-ctx? string? string? hash? procedure? -> void?
;; ============================================================

;; Register a tool dynamically from an extension.
;; The tool-registry in the extension context must be set (not #f).
(define (ext-register-tool! ctx name description schema handler)
  (define reg (ctx-tool-registry ctx))
  (unless reg
    (error
     'ext-register-tool!
     "no tool-registry available in extension context. \
            Ensure #:tool-registry is set when creating the context."))
  (define t (make-tool name description schema handler))
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

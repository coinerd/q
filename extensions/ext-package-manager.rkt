#lang racket/base

;; extensions/ext-package-manager.rkt — Extension Package Manager
;;
;; Provides a tool interface for discovering, installing, removing, and querying
;; extension packages.
;;
;; v0.99.88 W3 (MA-04): package lifecycle operations are consumed through the
;; INJECTED neutral package-host-service (util/extension/host-services.rkt).
;; The concrete ../runtime/package.rkt import was REMOVED — the Runtime adapter
;; (runtime/extension-host-adapter.rkt) owns the concrete implementation and
;; converts qpm-package values to pure package-summary data. Direct calls
;; without an injected service degrade to the safe null service (empty list,
;; not-installed, error results).
;; Actions: list, install, remove, info

(require racket/contract
         racket/string
         "define-extension.rkt"
         "dynamic-tools.rkt"
         "context.rkt"
         "hooks.rkt"
         "tool-api.rkt"
         (only-in "../util/extension/host-services.rkt"
                  package-host-service
                  package-host-service?
                  package-host-service-package-list
                  package-host-service-package-installed?
                  package-host-service-package-install
                  package-host-service-package-remove
                  package-summary?
                  package-summary-name
                  package-summary-version))

(provide ext-package-manager-extension
         handle-ext-pkg
         register-ext-pkg-tools)

;; ============================================================
;; Null service — historical no-injection behavior (safe defaults)
;; ============================================================

;; Service used when no package-host-service is injected. Mirrors the
;; historical absent-registry degradation of the ctx-* provider wrappers:
;; empty list, not-installed, error string, #f.
(define (null-package-service)
  (package-host-service (lambda () '())
                        (lambda (name) #f)
                        (lambda (source-dir) "error: no package service on context")
                        (lambda (name) #f)))

;; Format a list of package summaries into a human-readable string.
(define (format-pkg-list summaries)
  (string-join (for/list ([s summaries])
                 (format "- ~a (~a)" (package-summary-name s) (package-summary-version s)))
               "\n"))

;; Handle an ext-package tool invocation against a given service.
;; Supported actions: list, info, install, remove.
(define (handle-pkg-with svc args)
  (define action (hash-ref args 'action "list"))

  (cond
    [(string=? action "list")
     (define summaries ((package-host-service-package-list svc)))
     (define text
       (if (null? summaries)
           "No packages installed."
           (format-pkg-list summaries)))
     (make-success-result (list (hasheq 'type "text" 'text text)))]
    [(string=? action "info")
     (define name (hash-ref args 'name ""))
     (if ((package-host-service-package-installed? svc) name)
         (make-success-result
          (list (hasheq 'type "text" 'text (format "Package ~a is installed." name))))
         (make-error-result (format "Package ~a not found." name)))]
    [(string=? action "install")
     (define path (hash-ref args 'path ""))
     (define result ((package-host-service-package-install svc) path))
     (if (package-summary? result)
         (make-success-result (list (hasheq 'type
                                            "text"
                                            'text
                                            (format "Installed ~a (~a)"
                                                    (package-summary-name result)
                                                    (package-summary-version result)))))
         (make-error-result (format "Install failed: ~a" result)))]
    [(string=? action "remove")
     (define name (hash-ref args 'name ""))
     (if ((package-host-service-package-remove svc) name)
         (make-success-result (list (hasheq 'type "text" 'text (format "Removed ~a." name))))
         (make-error-result (format "Failed to remove ~a." name)))]
    [else (make-error-result (format "Unknown action: ~a" action))]))

;; Backward-compatible entry point. Direct calls carry no service, so they
;; degrade to the safe null service; the tool path closes over the service
;; injected at registration time.
(define (handle-ext-pkg args [exec-ctx #f])
  (handle-pkg-with (if (package-host-service? exec-ctx)
                       exec-ctx
                       (null-package-service))
                   args))

;; Register the ext-package tool with the extension context.
;; v0.99.88 W3: the handler closes over the ctx's injected
;; package-host-service (or the null service when absent).
(define (register-ext-pkg-tools ctx _payload)
  (define svc
    (if (package-host-service? (ctx-package-service ctx))
        (ctx-package-service ctx)
        (null-package-service)))
  (ext-register-tool!
   ctx
   "ext-package"
   (string-append "Extension package manager. " "Actions: list, install (from path), remove, info.")
   (hasheq 'type
           "object"
           'required
           '("action")
           'properties
           (hasheq 'action
                   (hasheq 'type "string" 'description "list|install|remove|info")
                   'name
                   (hasheq 'type "string" 'description "Package name (for info/remove)")
                   'path
                   (hasheq 'type "string" 'description "Local path (for install)")))
   (lambda (args [exec-ctx #f]) (handle-pkg-with svc args)))
  (hook-pass ctx))

(define-q-extension ext-package-manager-extension
                    #:version "1.0.0"
                    #:api-version "1"
                    #:on register-tools
                    register-ext-pkg-tools)

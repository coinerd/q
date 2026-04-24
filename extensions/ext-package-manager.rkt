#lang racket/base

;; extensions/ext-package-manager.rkt — Extension Package Manager
;;
;; Phase E: Wraps runtime/package.rkt as a tool.
;; Actions: list, install, remove, info

(require racket/contract
         racket/string
         "define-extension.rkt"
         "dynamic-tools.rkt"
         "context.rkt"
         "hooks.rkt"
         "../tools/tool.rkt"
         "../runtime/package.rkt"
         "../extensions/manifest.rkt")

(provide ext-package-manager-extension
         handle-ext-pkg)

(define (format-pkg-list pkgs)
  (string-join (for/list ([p pkgs])
                 (define m (qpm-package-manifest p))
                 (format "- ~a (~a)" (qpm-manifest-name m) (qpm-manifest-version m)))
               "\n"))

(define (handle-ext-pkg args [exec-ctx #f])
  (define action (hash-ref args 'action "list"))

  (cond
    [(string=? action "list")
     (define pkgs (list-packages))
     (define text
       (if (null? pkgs)
           "No packages installed."
           (format-pkg-list pkgs)))
     (make-success-result (list (hasheq 'type "text" 'text text)))]
    [(string=? action "info")
     (define name (hash-ref args 'name ""))
     (if (package-installed? name)
         (make-success-result
          (list (hasheq 'type "text" 'text (format "Package ~a is installed." name))))
         (make-error-result (format "Package ~a not found." name)))]
    [(string=? action "install")
     (define path (hash-ref args 'path ""))
     (define result (install-package-from-dir path))
     (if (qpm-package? result)
         (make-success-result (list (hasheq 'type
                                            "text"
                                            'text
                                            (format "Installed ~a (~a)"
                                                    (qpm-manifest-name (qpm-package-manifest result))
                                                    (qpm-manifest-version
                                                     (qpm-package-manifest result))))))
         (make-error-result (format "Install failed: ~a" result)))]
    [(string=? action "remove")
     (define name (hash-ref args 'name ""))
     (if (remove-package name)
         (make-success-result (list (hasheq 'type "text" 'text (format "Removed ~a." name))))
         (make-error-result (format "Failed to remove ~a." name)))]
    [else (make-error-result (format "Unknown action: ~a" action))]))

(define (register-ext-pkg-tools ctx _payload)
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
   handle-ext-pkg)
  (hook-pass ctx))

(define-q-extension ext-package-manager-extension
                    #:version "1.0.0"
                    #:api-version "1"
                    #:on register-tools
                    register-ext-pkg-tools)

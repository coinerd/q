#lang racket

;; extensions/loader.rkt — discover and load extension modules
;;
;; Provides:
;;   - discover-extensions: find extension modules in a directory tree
;;   - load-extension!: dynamic-load a module and register its extension
;;
;; Extension modules must:
;;   - Be in a subdirectory named "extensions/" (or direct .rkt files)
;;   - Provide `the-extension` bound to an extension? struct

(require racket/contract
         racket/file
         "api.rkt")

(provide
 discover-extensions
 load-extension!)

;; ============================================================
;; discover-extensions : path-string? -> (listof extension?)
;; ============================================================

;; Looks for .rkt files in <dir>/extensions/ and loads each one,
;; extracting the `the-extension` binding.
(define (discover-extensions dir)
  (define ext-dir (build-path dir "extensions"))
  (cond
    [(not (directory-exists? ext-dir))
     '()]
    [else
     (define files (filter (λ (f) (regexp-match? #rx"\\.rkt$" f))
                           (directory-list ext-dir #:build? #t)))
     (for/list ([f files]
                #:when (try-load-extension f))
       (try-load-extension f))]))

;; ============================================================
;; load-extension! : extension-registry? path-string? -> void?
;; ============================================================

;; Dynamically loads a module, extracts `the-extension`, and
;; registers it. Silently skips if the module doesn't provide
;; a valid extension.
(define (load-extension! registry path)
  (define ext (try-load-extension path))
  (when (and ext (extension? ext))
    (register-extension! registry ext)))

;; ============================================================
;; Internal helper: try to load a module and extract the-extension
;; ============================================================

(define (try-load-extension path)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (define mod-path (path->complete-path path))
    ;; Dynamic require: the module must provide `the-extension`
    (dynamic-require mod-path 'the-extension)))

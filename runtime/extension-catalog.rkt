#lang racket/base

;; runtime/extension-catalog.rkt — Extension discovery and management
;;
;; Wave 2 of v0.17.6: Provides functions to discover, list, activate,
;; and deactivate q extensions from the source tree.
;;
;; - known-extensions-dir: find q/extensions/ (config override or binary-relative)
;; - list-known-extensions: scan source tree for available extensions
;; - list-active-extensions: query loaded extensions from registry
;; - activate-extension!: create symlink in target directory
;; - deactivate-extension!: remove symlink

(require racket/file
         racket/path
         json
         "../extensions/api.rkt"
         (only-in "../extensions/loader.rkt" get-extension-name-from-path))

(provide known-extensions-dir
         list-known-extensions
         list-active-extensions
         activate-extension!
         deactivate-extension!
         ext-info
         ext-info?
         ext-info-name
         ext-info-source-path
         ext-info-version
         ext-info-description)

;; ============================================================
;; Extension source directory discovery
;; ============================================================

;; known-extensions-dir : -> path?
;; Find the q/extensions/ source directory.
;; Priority:
;;   1. ~/.q/config.json → extensions.source_dir (if set and exists)
;;   2. Binary-relative: <dir-of-q-binary>/extensions/
(define (known-extensions-dir)
  (or (config-source-dir)
      (binary-relative-extensions-dir)
      ;; Should never reach here
      (error 'known-extensions-dir "cannot find q/extensions/ directory")))

;; config-source-dir : -> (or/c path? #f)
;; Read ~/.q/config.json for extensions.source_dir override.
(define (config-source-dir)
  (define home-dir (find-system-path 'home-dir))
  (define cfg-path (build-path home-dir ".q" "config.json"))
  (and (file-exists? cfg-path)
       (with-handlers ([exn:fail? (λ (_) #f)])
         (define cfg (call-with-input-file cfg-path (λ (in) (read-json in))))
         (define dir-str (hash-ref cfg 'extensions (hash)))
         (define source-dir
           (if (hash? dir-str)
               (hash-ref dir-str 'source_dir #f)
               #f))
         (and (string? source-dir)
              (let ([p (string->path source-dir)]) (and (directory-exists? p) p))))))

;; binary-relative-extensions-dir : -> (or/c path? #f)
;; Find extensions/ relative to this module's source directory.
;; In dev: q/runtime/extension-catalog.rkt -> parent -> q/extensions/
;; Uses variable-reference to reliably find the module's own path
;; even when current-load-relative-directory is #f (e.g., racket main.rkt).
(define (binary-relative-extensions-dir)
  (define this-dir (or (current-load-relative-directory) (module-source-dir) (current-directory)))
  (define ext-dir (build-path this-dir ".." "extensions"))
  (define cleaned (simple-form-path ext-dir))
  (and (directory-exists? cleaned) cleaned))

;; module-source-dir : -> (or/c path? #f)
;; Resolve the directory containing this module's source file.
;; Works even when current-load-relative-directory is #f.
(define (module-source-dir)
  (define raw (#%variable-reference))
  (define rmp (variable-reference->resolved-module-path raw))
  (and rmp
       (resolved-module-path? rmp)
       (let ([vr (resolved-module-path-name rmp)])
         (and (path? vr)
              (let-values ([(base name must-be-dir) (split-path vr)])
                (and (path? base) base))))))

;; ============================================================
;; List known extensions
;; ============================================================

;; Extension info struct for catalog entries
(struct ext-info (name source-path version description) #:transparent)

;; list-known-extensions : -> (listof ext-info?)
;; Scan the known-extensions-dir for all loadable .rkt extension files.
;; Returns a list of ext-info structs.
;; Skips infrastructure modules (api.rkt, loader.rkt, etc.) that are
;; part of the extension framework itself.
(define (list-known-extensions)
  (define ext-dir (known-extensions-dir))
  (if (not (directory-exists? ext-dir))
      '()
      (let* ([infra-names '("api.rkt" "loader.rkt"
                                      "context.rkt"
                                      "hooks.rkt"
                                      "dynamic-tools.rkt"
                                      "define-extension.rkt"
                                      "events.rkt"
                                      "ext-commands.rkt"
                                      "manifest.rkt"
                                      "manifest-audit.rkt"
                                      "quarantine.rkt"
                                      "ui-channel.rkt"
                                      "dialog-api.rkt"
                                      "custom-ui-api.rkt"
                                      "custom-renderer-registry.rkt"
                                      "message-inject.rkt"
                                      "message-renderer.rkt")]
             [all-files (directory-list ext-dir #:build? #t)]
             [rkt-files
              (filter (λ (f) (and (file-exists? f) (regexp-match? #rx"\\.rkt$" (path->string f))))
                      all-files)]
             ;; Filter out infrastructure modules
             [ext-files (filter (λ (f)
                                  (define name (path->string (file-name-from-path f)))
                                  (not (member name infra-names)))
                                rkt-files)]
             ;; Also check subdirectories for extension modules
             ;; Convention: name/name.rkt is an extension; helpers are not
             [subdir-files (for*/list ([f (in-list all-files)]
                                       #:when (directory-exists? f)
                                       [sub (in-list (directory-list f #:build? #t))]
                                       #:when (and (file-exists? sub)
                                                   (regexp-match? #rx"\\.rkt$" (path->string sub)))
                                       ;; Only pick up files matching dir-name/dir-name.rkt
                                       #:when
                                       (equal? (path->string (file-name-from-path sub))
                                               (string-append (path->string (file-name-from-path f))
                                                              ".rkt")))
                             sub)]
             [all-ext-files (append ext-files subdir-files)])
        (for/list ([f (in-list all-ext-files)])
          (define name (get-extension-name-from-path f))
          (ext-info name f "unknown" "")))))

;; ============================================================
;; List active extensions
;; ============================================================

;; list-active-extensions : extension-registry? -> (listof ext-info?)
;; Query the registry for currently loaded extensions.
(define (list-active-extensions registry)
  (for/list ([ext (in-list (list-extensions registry))])
    (ext-info (extension-name ext) 'active (extension-version ext) "")))

;; ============================================================
;; Activate / deactivate extensions
;; ============================================================

;; activate-extension! : string? path? -> void?
;; Create a symlink in target-dir pointing to the extension's source file.
;; Handles both flat files (name.rkt) and subdirectory extensions (name/name.rkt).
;; Idempotent: no error if already activated.
(define (activate-extension! name target-dir)
  (define source-dir (known-extensions-dir))
  (define flat-source (build-path source-dir (format "~a.rkt" name)))
  (define subdir-source (build-path source-dir name (format "~a.rkt" name)))
  (define actual-source
    (cond
      [(file-exists? flat-source) flat-source]
      [(file-exists? subdir-source) subdir-source]
      [else (error 'activate "extension '~a' not found in ~a" name source-dir)]))
  (define link-name (file-name-from-path actual-source))
  (define link-path (build-path target-dir link-name))
  (make-directory* target-dir)
  ;; Remove existing link or file if present
  (when (or (link-exists? link-path) (file-exists? link-path))
    (delete-file link-path))
  (make-file-or-directory-link actual-source link-path))

;; deactivate-extension! : string? path? -> void?
;; Remove the symlink for the named extension from target-dir.
;; No error if not present.
(define (deactivate-extension! name target-dir)
  (define link-path (build-path target-dir (format "~a.rkt" name)))
  (when (or (link-exists? link-path) (file-exists? link-path))
    (delete-file link-path)))

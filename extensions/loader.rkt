#lang racket/base

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
         racket/path
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "api.rkt"
         "quarantine.rkt")

(provide extension-load-error
         extension-load-error?
         extension-load-error-path
         extension-load-error-message
         extension-load-error-category
         discover-extensions
         load-extension!
         try-load-extension
         get-extension-name-from-path)

;; ============================================================
;; extension-load-error struct
;; ============================================================

;; Struct for load errors — returned instead of #f by try-load-extension.
;; category is one of: 'not-found 'syntax-error 'api-mismatch 'unknown
(struct extension-load-error (path message category) #:transparent)

;; ============================================================
;; discover-extensions : path-string? -> (listof extension?)
;; ============================================================

;; Looks for .rkt files in <dir>/extensions/ and loads each one,
;; extracting the `the-extension` binding.
(define (discover-extensions dir)
  (define ext-dir (build-path dir "extensions"))
  (cond
    [(not (directory-exists? ext-dir)) '()]
    [else
     (define files
       (filter (λ (f) (regexp-match? #rx"\\.rkt$" f)) (directory-list ext-dir #:build? #t)))
     (filter (λ (r) (and r (not (extension-load-error? r))))
             (for/list ([f files])
               (try-load-extension f)))]))

;; ============================================================
;; load-extension! : extension-registry? path-string? #:event-bus -> void?
;; ============================================================

;; Dynamically loads a module, extracts `the-extension`, and
;; registers it. Returns structured error info if loading fails.
;; When event-bus is provided, publishes extension.load.failed on error.
(define (load-extension! registry path #:event-bus [event-bus #f])
  (define ext-name (get-extension-name-from-path path))
  (define state (extension-state ext-name))
  (when (and (not (eq? state 'disabled)) (not (eq? state 'quarantined)))
    (define result (try-load-extension path))
    (cond
      [(extension-load-error? result)
       (log-warning (format "extension load failed [~a]: ~a — ~a"
                            (extension-load-error-category result)
                            path
                            (extension-load-error-message result)))
       (when event-bus
         (publish! event-bus
                   (make-event "extension.load.failed"
                               (current-seconds)
                               ""
                               #f
                               (hash 'path
                                     (if (path? path)
                                         (path->string path)
                                         path)
                                     'error
                                     (extension-load-error-message result)
                                     'category
                                     (extension-load-error-category result)))))]
      [(and result (extension? result)) (register-extension! registry result)]))
  (void))

;; Cache for try-load-extension results
(define load-cache (make-hash))
(define load-cache-sem (make-semaphore 1))

(define (cached-try-load path)
  (call-with-semaphore load-cache-sem
                       (lambda ()
                         (cond
                           [(hash-has-key? load-cache path) (hash-ref load-cache path)]
                           [else
                            (define result (try-load-extension path))
                            (hash-set! load-cache path result)
                            result]))))

;; ============================================================
;; Internal helper: try to load a module and extract the-extension
;; ============================================================

;; Classifies exceptions into error categories.
;; Returns either the extension object or an extension-load-error struct.
(define (try-load-extension path)
  (with-handlers ([exn:fail? (λ (e)
                               (extension-load-error (if (path? path)
                                                         (path->string path)
                                                         path)
                                                     (exn-message e)
                                                     (classify-exception e)))])
    (define mod-path (path->complete-path path))
    (unless (file-exists? mod-path)
      (raise (make-not-found-error path)))
    ;; Dynamic require: the module must provide `the-extension`
    (dynamic-require mod-path 'the-extension)))

;; Classify an exception into a category symbol.
(define (classify-exception e)
  (cond
    [(exn:fail:syntax? e) 'syntax-error]
    [(exn:fail:read? e) 'syntax-error]
    [(not-found-error? e) 'not-found]
    [else 'unknown]))

;; Internal exception type for missing files.
(struct not-found-error exn:fail () #:transparent)

(define (make-not-found-error path)
  (not-found-error (format "file not found: ~a" path) (current-continuation-marks)))

;; ============================================================
;; get-extension-name-from-path : path-string? -> string?
;; ============================================================

;; Extracts the extension name from a path:
;;   - For "foo.rkt" -> "foo"
;;   - For "dir/foo.rkt" -> "foo"
;;   - For "dir/foo/" -> "foo"
(define (get-extension-name-from-path path)
  (define filename (file-name-from-path path))
  (define name-str (path->string filename))
  ;; If it looks like a file with extension, strip the extension
  (define base (regexp-replace #rx"\\.[^.]+$" name-str ""))
  ;; If base is empty (path ended in separator), use the directory name
  (if (string=? base "")
      (let-values ([(parent dir _) (split-path (path->complete-path path))])
        (if dir
            (path->string dir)
            "unknown"))
      base))

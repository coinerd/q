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
         "api.rkt"
         "quarantine.rkt")

(provide
 discover-extensions
 load-extension!
 get-extension-name-from-path)

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
     (filter values
       (for/list ([f files])
         (try-load-extension f)))]))

;; ============================================================
;; load-extension! : extension-registry? path-string? -> void?
;; ============================================================

;; Dynamically loads a module, extracts `the-extension`, and
;; registers it. Silently skips if the module doesn't provide
;; a valid extension.
(define (load-extension! registry path)
  (define ext-name (get-extension-name-from-path path))
  (define state (extension-state ext-name))
  (when (and (not (eq? state 'disabled))
             (not (eq? state 'quarantined)))
    (define ext (try-load-extension path))
    (when (and ext (extension? ext))
      (register-extension! registry ext)))
  (void))

;; Cache for try-load-extension results
(define load-cache (make-hash))
(define load-cache-sem (make-semaphore 1))

(define (cached-try-load path)
  (call-with-semaphore load-cache-sem
    (lambda ()
      (cond
        [(hash-has-key? load-cache path)
         (hash-ref load-cache path)]
        [else
         (define result (try-load-extension path))
         (hash-set! load-cache path result)
         result]))))

;; ============================================================
;; Internal helper: try to load a module and extract the-extension
;; ============================================================

(define (try-load-extension path)
  (with-handlers ([exn:fail? (λ (e)
                               (log-warning (format "try-load-extension failed for ~a: ~a"
                                                    path (exn-message e)))
                               #f)])
    (define mod-path (path->complete-path path))
    ;; Dynamic require: the module must provide `the-extension`
    (dynamic-require mod-path 'the-extension)))

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
        (if dir (path->string dir) "unknown"))
      base))

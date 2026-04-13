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
         (only-in racket/path file-name-from-path)
         (only-in "../util/path-helpers.rkt" path-only)
         racket/list
         racket/port
         racket/string
         json
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../util/checksum.rkt"
         "api.rkt"
         "manifest.rkt"
         "quarantine.rkt")

(provide ;; Extension loading errors
         extension-load-error
         extension-load-error?
         extension-load-error-path
         extension-load-error-message
         extension-load-error-category
         ;; Discovery and loading
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
                               (hasheq 'path
                                     (if (path? path)
                                         (path->string path)
                                         path)
                                     'error
                                     (extension-load-error-message result)
                                     'category
                                     (extension-load-error-category result)))))]
      [(and result (extension? result)) (register-extension! registry result)]))
  (void))

;; Cache for try-load-extension results with LRU eviction (Issue #201)
;; Max 64 entries, TTL 30 minutes
(define max-cache-entries 64)
(define cache-ttl-seconds (* 30 60))

;; Cache is a list of (key . (value . timestamp)) pairs, most-recently-used first
(define load-cache (box '()))
(define load-cache-sem (make-semaphore 1))

(define (cache-entry-expired? entry)
  (> (- (current-seconds) (cddr entry)) cache-ttl-seconds))

(define (evict-cache!)
  (define now (current-seconds))
  (define entries (unbox load-cache))
  ;; Remove expired entries
  (define live (filter (lambda (e) (not (cache-entry-expired? e))) entries))
  ;; If still over limit, remove oldest (last in list = least recently used)
  (define trimmed
    (if (> (length live) max-cache-entries)
        (take live max-cache-entries)
        live))
  (set-box! load-cache trimmed))

(define (cached-try-load path)
  (call-with-semaphore load-cache-sem
                       (lambda ()
                         (evict-cache!)
                         (define entries (unbox load-cache))
                         (define found (assoc path entries))
                         (cond
                           [(and found (not (cache-entry-expired? found)))
                            ;; Move to front (most recently used)
                            (define rest (filter (lambda (e) (not (equal? (car e) path))) entries))
                            (set-box! load-cache (cons found rest))
                            (cdr found)]
                           [else
                            ;; Remove stale entry if present
                            (define clean (filter (lambda (e) (not (equal? (car e) path))) entries))
                            (define result (try-load-extension path))
                            (define entry (cons path (cons result (current-seconds))))
                            (set-box! load-cache (cons entry clean))
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
    ;; Validate manifest if present (SEC-04)
    (define manifest-path (build-path (path-only-with-default mod-path) "qpm.json"))
    (when (file-exists? manifest-path)
      (define raw (with-input-from-file manifest-path read-json))
      (when (hash? raw)
        (define-values (valid? errors)
          (validate-manifest (qpm-manifest-from-hash raw)))
        (unless valid?
          (raise (extension-load-error (path->string path)
                                       (format "manifest validation failed: ~a" (string-join errors ", "))
                                       'api-mismatch)))
        ;; SEC-05: Integrity hash verification
        (define ext-dir (path-only-with-default mod-path))
        (define current-hash (compute-extension-directory-hash ext-dir raw))
        (define stored-hash (hash-ref raw 'integrity #f))
        (cond
          [(not stored-hash)
           ;; First load: store the integrity hash
           (hash-set! raw 'integrity current-hash)
           (call-with-output-file manifest-path
             (lambda (out) (write-json raw out) (newline out))
             #:exists 'replace)]
          [(not (equal? stored-hash current-hash))
           (raise (extension-load-error (path->string path)
                                        (format "integrity hash mismatch: expected ~a, got ~a"
                                                stored-hash current-hash)
                                        'api-mismatch))])))
    ;; Dynamic require: the module must provide `the-extension`
    (dynamic-require mod-path 'the-extension)))

;; path-only imported from util/path-helpers.rkt
;; (local wrapper preserves (current-directory) fallback for relative paths)
(define (path-only-with-default p)
  (or (path-only p) (current-directory)))

;; Helper: construct qpm-manifest from a raw JSON hash
(define (qpm-manifest-from-hash h)
  (make-qpm-manifest
   #:name (hash-ref h 'name "unknown")
   #:version (hash-ref h 'version "0.0.0")
   #:api-version (hash-ref h 'api-version "1")
   #:type (string->symbol (hash-ref h 'type "extension"))
   #:description (hash-ref h 'description "")
   #:author (hash-ref h 'author "unknown")))

;; SEC-05: Compute SHA256 hash of all files listed in the manifest
(define (compute-extension-directory-hash ext-dir manifest-hash)
  (define files (hash-ref manifest-hash 'files '()))
  (if (null? files)
      ""
      (let ([sorted (sort files string<?)])
        (sha256-string
         (call-with-output-string
          (lambda (out)
            (for ([f (in-list sorted)])
              (define full-path (build-path ext-dir f))
              (when (file-exists? full-path)
                (call-with-input-file full-path
                  (lambda (in) (display (port->string in) out)))))))))))

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

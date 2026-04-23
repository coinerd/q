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
         "../util/version.rkt"
         "api.rkt"
         "manifest.rkt"
         "quarantine.rkt")

;; Extension loading errors
(provide extension-load-error
         extension-load-error?
         extension-load-error-path
         extension-load-error-message
         extension-load-error-category
         ;; Discovery and loading
         discover-extensions
         load-extension!
         try-load-extension
         get-extension-name-from-path
         ;; Hot reload (#1146)
         reload-extensions!
         discover-extension-files)

;; ============================================================
;; extension-load-error struct
;; ============================================================

;; Struct for load errors — returned instead of #f by try-load-extension.
;; category is one of: 'not-found 'syntax-error 'api-mismatch 'unknown
(struct extension-load-error (path message category) #:transparent)

;; Per-extension startup timeout in seconds (default: 30s).
;; Set to #f to disable timeout.
(define current-extension-startup-timeout (make-parameter 30))

;; ============================================================
;; discover-extensions : path-string? -> (listof extension?)
;; ============================================================

;; Looks for .rkt files in <dir>/extensions/ and loads each one,
;; extracting the `the-extension` binding.
;; Now also supports subdirectory extensions: extensions/<name>/<name>.rkt
;; or extensions/<name>/main.rkt.
(define (discover-extensions dir)
  (define ext-dir (build-path dir "extensions"))
  (cond
    [(not (directory-exists? ext-dir)) '()]
    [else
     ;; Collect flat .rkt files
     (define flat-files
       (filter (λ (f) (regexp-match? #rx"\\.rkt$" (path->string f)))
               (directory-list ext-dir #:build? #t)))
     ;; Collect subdirectory extension entry points
     (define subdir-files
       (filter-map (λ (d)
                     (and (directory-exists? d)
                          (let* ([dir-name (path->string (file-name-from-path d))]
                                 [name-rkt (build-path d (string-append dir-name ".rkt"))]
                                 [main-rkt (build-path d "main.rkt")])
                            (cond
                              [(file-exists? name-rkt) name-rkt]
                              [(file-exists? main-rkt) main-rkt]
                              [else #f]))))
                   (directory-list ext-dir #:build? #t)))
     (define all-files (append flat-files subdir-files))
     (filter (λ (r) (and r (not (extension-load-error? r))))
             (for/list ([f all-files])
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
    (define timeout-secs (current-extension-startup-timeout))
    (define result
      (if timeout-secs
          ;; Run with timeout to prevent hanging on slow extensions
          (let ([chan (make-channel)])
            (define thd (thread (lambda () (channel-put chan (try-load-extension path)))))
            (define maybe-result (sync/timeout timeout-secs chan))
            (unless maybe-result
              (kill-thread thd)) ; #447: prevent thread leak
            (if maybe-result
                maybe-result
                (extension-load-error (if (path? path)
                                          (path->string path)
                                          path)
                                      (format "extension startup timed out after ~as" timeout-secs)
                                      'timeout)))
          ;; No timeout — direct call
          (try-load-extension path)))
    (cond
      [(extension-load-error? result)
       (log-warning "extension load failed [~a]: ~a \u2014 ~a"
                    (extension-load-error-category result)
                    path
                    (extension-load-error-message result))
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

;; Cache infrastructure removed (#448): was never called in production
;; code paths (discover-extensions calls try-load-extension directly).
;; If caching is needed in the future, re-introduce with a clear call site.

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
        (define-values (valid? errors) (validate-manifest (qpm-manifest-from-hash raw)))
        (unless valid?
          (raise (extension-load-error (path->string path)
                                       (format "manifest validation failed: ~a"
                                               (string-join errors ", "))
                                       'api-mismatch)))
        ;; SEC-05: Integrity hash verification
        (define ext-dir (path-only-with-default mod-path))
        (define current-hash (compute-extension-directory-hash ext-dir raw))
        (define stored-hash (hash-ref raw 'integrity #f))
        (cond
          [(not stored-hash)
           ;; First load: store the integrity hash (skip on read-only dirs)
           (hash-set! raw 'integrity current-hash)
           (with-handlers ([exn:fail:filesystem?
                            (λ (e)
                              (log-warning
                               (format
                                "extension '~a': could not write integrity hash (read-only dir): ~a"
                                (hash-ref raw 'name "unknown")
                                (exn-message e))))])
             (call-with-output-file manifest-path
                                    (lambda (out)
                                      (write-json raw out)
                                      (newline out))
                                    #:exists 'replace))]
          [(not (equal? stored-hash current-hash))
           (raise (extension-load-error
                   (path->string path)
                   (format "integrity hash mismatch: expected ~a, got ~a" stored-hash current-hash)
                   'api-mismatch))])
        ;; Compatibility check: warn if extension declares incompatible range
        (when (hash-has-key? raw 'compatibility)
          (define compat-hash (hash-ref raw 'compatibility))
          (when (hash? compat-hash)
            (define ext-name (hash-ref raw 'name "unknown"))
            (define min-v (hash-ref compat-hash 'min-q-version #f))
            (define max-v (hash-ref compat-hash 'max-q-version #f))
            (when (and min-v (version<? q-version min-v))
              (log-warning "extension '~a' requires q >= ~a, current version is ~a"
                           ext-name
                           min-v
                           q-version))
            (when (and max-v (string? max-v) (version<=? max-v q-version))
              (log-warning
               (format
                "extension '~a' declares compatibility up to ~a (exclusive), current version is ~a"
                ext-name
                max-v
                q-version)))))))
    ;; Dynamic require: the module must provide `the-extension`
    (dynamic-require mod-path 'the-extension)))

;; path-only imported from util/path-helpers.rkt
;; (local wrapper preserves (current-directory) fallback for relative paths)
(define (path-only-with-default p)
  (or (path-only p) (current-directory)))

;; Helper: construct qpm-manifest from a raw JSON hash
(define (qpm-manifest-from-hash h)
  (make-qpm-manifest #:name (hash-ref h 'name "unknown")
                     #:version (hash-ref h 'version "0.0.0")
                     #:api-version (hash-ref h 'api-version "1")
                     #:type (string->symbol (hash-ref h 'type "extension"))
                     #:description (hash-ref h 'description "")
                     #:author (hash-ref h 'author "unknown")))

;; SEC-05: Compute SHA256 hash of all files listed in the manifest.
;; Warns and includes a special marker for missing files (instead of
;; silently skipping them, which would weaken the integrity guarantee).
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
              (cond
                [(file-exists? full-path)
                 (call-with-input-file full-path (lambda (in) (display (port->string in) out)))]
                [else
                 (log-warning "extension manifest: declared file missing: ~a (in ~a)" f ext-dir)
                 ;; Include a special marker so the hash changes when files
                 ;; are missing — prevents a "missing files = no change" exploit
                 (display (format "[MISSING:~a]" f) out)]))))))))

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

;; ============================================================
;; Hot reload (#1146)
;; ============================================================

;; discover-extension-files : (listof path-string?) -> (listof (cons/c string? path?))
;; Returns (extension-name . file-path) pairs for all .rkt files found in
;; the given directory paths.
(define (discover-extension-files paths)
  (for*/list ([dir (in-list paths)]
              #:when (directory-exists? dir)
              [f (in-directory dir)]
              #:when (file-exists? f)
              #:when (regexp-match? #rx"\\.rkt$" (path->string f)))
    (cons (path->string (path-replace-suffix (file-name-from-path f) #"")) f)))

;; reload-extensions! : extension-registry? (listof path-string?) -> (listof string?)
;; Reload extensions from configured paths.
;; 1. Discover all .rkt files in extension-paths
;; 2. For each file, try dynamic-require of 'the-extension
;; 3. If new extension (not in registry), register it
;; 4. If extension file removed, unregister it
;; 5. Return list of names loaded
(define (reload-extensions! registry extension-paths)
  (define discovered (discover-extension-files extension-paths))
  (define existing-names (map extension-name (list-extensions registry)))
  (define discovered-names (map car discovered))
  ;; Unregister removed extensions
  (for ([name (in-list existing-names)]
        #:unless (member name discovered-names))
    (with-handlers ([exn:fail? (lambda (e)
                                 (log-warning
                                  (format "Failed to unregister ~a: ~a" name (exn-message e))))])
      (unregister-extension! registry name)))
  ;; Register new extensions
  (define loaded '())
  (for ([pair (in-list discovered)])
    (define name (car pair))
    (define path (cdr pair))
    (unless (member name existing-names)
      (with-handlers ([exn:fail? (lambda (e)
                                   (log-warning
                                    (format "Failed to load ~a: ~a" name (exn-message e))))])
        (define ext (dynamic-require (path->complete-path path) 'the-extension))
        (when (extension? ext)
          (register-extension! registry ext)
          (set! loaded (cons name loaded))))))
  (reverse loaded))

;; ═══════════════════════════════════════════════════════════════════
;; Version comparison helpers
;; ═══════════════════════════════════════════════════════════════════

(define (version-parts v)
  (map (λ (s) (or (string->number s) 0)) (take (append (string-split v ".") '("0" "0" "0")) 3)))

(define (version<=? a b)
  (define pa (version-parts a))
  (define pb (version-parts b))
  (or (< (car pa) (car pb))
      (and (= (car pa) (car pb))
           (or (< (cadr pa) (cadr pb)) (and (= (cadr pa) (cadr pb)) (<= (caddr pa) (caddr pb)))))))

(define (version<? a b)
  (and (version<=? a b) (not (equal? a b))))

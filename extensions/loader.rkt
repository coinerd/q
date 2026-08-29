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
         racket/match
         racket/file
         (only-in racket/path file-name-from-path)
         (only-in "../util/path/path-helpers.rkt" path-only)
         racket/list
         racket/port
         racket/string
         json
         (only-in "../util/event/event.rkt" make-event)
         "../util/event/event-bus.rkt"
         "api.rkt"
         "../util/json/checksum.rkt"
         "../util/version.rkt"
         "api.rkt"
         "manifest.rkt"
         "quarantine.rkt"
         "tiers.rkt")

;; Extension loading errors
(provide extension-load-error
         extension-load-error?
         extension-load-error-path
         extension-load-error-message
         extension-load-error-category
         classify-exception
         (contract-out
          [discover-extensions (-> path-string? (listof extension?))]
          [load-extension!
           (->* (extension-registry? path-string?) (#:event-bus (or/c event-bus? #f)) void?)]
          [try-load-extension (-> path-string? (or/c any/c extension-load-error?))]
          [get-extension-name-from-path (-> path-string? string?)]
          [reload-extensions! (-> extension-registry? (listof path-string?) (listof string?))]
          [reload-extensions!/report
           (-> extension-registry? (listof path-string?) hash?)]
          [purge-compiled-dirs! (-> path-string? (listof path?))]
          [discover-extension-files
           (-> (listof path-string?) (listof (cons/c string? path-string?)))]))

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
;; load-extension! : extension-registry? path-string? #:event-bus -> boolean?
;; ============================================================

;; Dynamically loads a module, extracts `the-extension`, and
;; registers it. Returns structured error info if loading fails.
;; When event-bus is provided, publishes extension.load.failed on error.
;; Returns #t if extension was loaded and registered, #f otherwise.
;; Phase 1: Validate extension state (disabled/quarantined/ok)
(define (load-extension-validate path)
  (define ext-name (get-extension-name-from-path path))
  (define state (extension-state ext-name))
  (match state
    [(or 'disabled 'quarantined) state]
    [_ 'ok]))

;; Phase 2: Attempt to load extension with optional timeout
(define (load-extension-attempt path)
  (define timeout-secs (current-extension-startup-timeout))
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

;; Phase 3: Register loaded extension or report error
(define (load-extension-register! registry result path event-bus)
  (cond
    [(extension-load-error? result)
     (log-warning "extension load failed [~a]: ~a — ~a"
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
                                     (extension-load-error-category result)))))
     (void)]
    [(and result (extension? result))
     ;; Tier validation: default to 'hooks (lowest) if no manifest tier declared
     (define declared-tier 'hooks)
     (define tier-result (extension-tier-valid? result declared-tier))
     (when (list? tier-result)
       (for ([msg (in-list tier-result)])
         (log-warning "extension tier violation [~a]: ~a" (extension-name result) msg)))
     (register-extension! registry result)
     (void)]
    [else (void)]))

;; Orchestrator: validate → attempt → register
(define (load-extension! registry path #:event-bus [event-bus #f])
  (define validation (load-extension-validate path))
  (when (eq? validation 'ok)
    (define result (load-extension-attempt path))
    (load-extension-register! registry result path event-bus)))

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
    (define raw-path (path->complete-path path))
    ;; Resolve symlinks so relative requires inside the extension module
    ;; are resolved relative to the REAL file's directory, not the symlink's.
    ;; E.g. .q/extensions/gsd-planning.rkt (symlink) → extensions/gsd-planning.rkt (real)
    (define mod-path (simplify-path (resolve-path raw-path)))
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
          (define compat-hash (hash-ref raw 'compatibility #f))
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
  (match e
    [(? exn:fail:syntax?) 'syntax-error]
    [(? exn:fail:read?) 'syntax-error]
    [(? exn:fail:filesystem?) 'filesystem-error]
    [(? exn:fail:contract?) 'contract-error]
    [(? not-found-error?) 'not-found]
    [_ 'unknown]))

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
;; Hot reload (#1146) + stale-bytecode recovery (BUG-0047, W3)
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

;; Root of the q source tree this loader lives in
;; (q/extensions/loader.rkt -> two levels up). The wide bytecode purge on
;; the recovery path is scoped from here.
(define q-source-root
  (simplify-path
   (build-path (resolved-module-path-name
                (variable-reference->resolved-module-path
                 (#%variable-reference)))
               'up 'up)))

;; Roots whose `compiled/` bytecode caches are purged when a load fails
;; with a stale-linklet error. Parameterized so tests can point recovery
;; at a sandbox instead of the live q tree. #f -> (list q-source-root).
(define current-reload-bytecode-roots (make-parameter #f))

(define (compiled-dir? p)
  (and (directory-exists? p)
       (equal? "compiled" (path->string (file-name-from-path p)))))

;; purge-compiled-dirs! : path-string? -> (listof path?)
;; Delete every `compiled/` bytecode cache directory under `root` so the
;; next load recompiles from source (BUG-0047). Never descends into a
;; compiled/ dir itself; deletion failures are logged and skipped
;; (read-only trees simply keep their stale bytecode and the reload
;; reports the failure honestly).
(define (purge-compiled-dirs! root)
  (define purged '())
  (define (walk! dir)
    (for ([p (in-list (directory-list dir #:build? #t))])
      (cond
        [(compiled-dir? p)
         (with-handlers
             ([exn:fail?
               (lambda (e)
                 (log-warning "purge-compiled-dirs!: cannot delete ~a: ~a"
                              (path->string p) (exn-message e)))])
           (delete-directory/files p)
           (set! purged (cons p purged))
           (log-info "purge-compiled-dirs!: purged stale bytecode cache ~a"
                     (path->string p)))]
        [(directory-exists? p) (walk! p)])))
  (when (directory-exists? root) (walk! root))
  (reverse purged))

;; Messages raised by the module manager when compiled bytecode (.zo
;; linklets) is stale or incompatible — e.g. after merging source
;; changes while the process (and its compiled/ caches) keeps running:
;;   instantiate-linklet: mismatch; reference to a variable that is not
;;   exported ... / bad bytecode / corrupt .zo / cannot re-declare ...
(define stale-bytecode-rx
  #rx"(?i:linklet|bytecode|\\.zo|not exported|cannot re-declare|corrupt|module mismatch)")

(define (stale-bytecode-error? e)
  (and (exn:fail? e)
       (regexp-match? stale-bytecode-rx (exn-message e))))

;; Path of THIS loader module's directory (q/extensions/), used to
;; derive the shared-module attach list below.
(define this-file-dir
  (simplify-path
   (path-only (resolved-module-path-name
               (variable-reference->resolved-module-path
                (#%variable-reference))))))

;; shared-file-modules : -> (listof path?)
;; File modules that must be ATTACHED into a fresh namespace (instead of
;; re-instantiated from source) so their structs keep identity with the
;; running namespace: everything under q/extensions/ plus the util
;; modules extensions depend on. Computed at call time — new extension
;; tree files are covered without editing a hardcoded list. Collected
;; modules (racket, rackunit, ...) are NOT attached: the fresh namespace
;; re-instantiates them from installed bytecode (no identity contract
;; crosses those).
(define (shared-file-modules)
  (define util-rels
    '("../util/event/event.rkt" "../util/event/event-bus.rkt"
      "../util/json/checksum.rkt" "../util/version.rkt"
      "../util/hook-types.rkt"))
  (append
   (for/list ([p (in-directory this-file-dir)]
              #:when (regexp-match? #rx"\\.rkt$" (path->string p)))
     (simplify-path p))
   (for/list ([rel (in-list util-rels)])
     (simplify-path (build-path this-file-dir rel)))))

;; load-extension-fresh : path-string? -> any/c
;; Load `the-extension` from `path` in a FRESH namespace with every
;; shared file module (see shared-file-modules) attached, so the
;; extension module is recompiled/re-instantiated from the CURRENT
;; source instead of the namespace-cached instance (BUG-0047: the old
;; reload path returned the cached linklet and reported a false success).
;; Attaching preserves struct identity for shared modules (e.g. api.rkt),
;; so the freshly loaded `the-extension` satisfies the running
;; namespace's `extension?`.
(define (load-extension-fresh path)
  (define mod-path
    (simplify-path (resolve-path (path->complete-path path))))
  (define src-ns (current-namespace))
  (define fresh-ns (make-base-namespace))
  (for ([abs (in-list (shared-file-modules))])
    ;; Skip modules not instantiated in the running namespace (test
    ;; contexts): dynamic-require in the fresh namespace then loads them
    ;; from source — same behavior as the old attach-with-handler pattern
    ;; in agent/registry.rkt.
    (with-handlers ([exn:fail? void])
      (namespace-attach-module src-ns (list 'file (path->string abs)) fresh-ns)))
  (parameterize ([current-namespace fresh-ns])
    (dynamic-require (list 'file (path->string mod-path)) 'the-extension)))

;; reload-extensions!/report : extension-registry? (listof path-string?) -> hash?
;; Full hot-reload with stale-bytecode recovery and HONEST reporting
;; (BUG-0047 W3). Returns a hash:
;;   'loaded    — names successfully loaded (and registered)
;;   'failed    — (cons name message) for every extension that could NOT
;;                be loaded (nothing is silently dropped)
;;   'recovered — names that loaded only after a stale-bytecode purge +
;;                recompile retry
;;   'purged    — compiled/ cache dirs deleted along the way
(define (reload-extensions!/report registry extension-paths)
  ;; 1. Unload all current extensions.
  (define existing-names (map extension-name (list-extensions registry)))
  (for ([name (in-list existing-names)])
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (log-warning "reload: failed to unregister ~a: ~a"
                                    name (exn-message e)))])
      (unregister-extension! registry name)))
  ;; 2. Purge stale bytecode caches under the extension roots so every
  ;; extension module is recompiled from CURRENT source. Extension trees
  ;; are small; deterministic freshness beats timestamp guessing after
  ;; merges.
  (define purged (append-map purge-compiled-dirs! extension-paths))
  ;; 3. Discover and load all extensions - one fresh namespace per
  ;; module, with stale-linklet recovery (purge shared-tree caches once,
  ;; retry from source) and named failures otherwise.
  (define discovered (discover-extension-files extension-paths))
  (define loaded '())
  (define failed '())
  (define recovered '())
  (define wide-purged? #f)
  (define (try-load path)
    ;; -> (list result message recovered?)
    (define first-result
      (with-handlers ([exn:fail? (lambda (e) e)])
        (load-extension-fresh path)))
    (cond
      [(not (exn:fail? first-result)) (list first-result #f #f)]
      [(not (stale-bytecode-error? first-result))
       (list #f (exn-message first-result) #f)]
      [else
       ;; Stale/incompatible bytecode: purge the shared-tree caches once,
       ;; then retry from source. If the retry also fails we report the
       ;; failure by name (never a false success).
       (let* ([roots (or (current-reload-bytecode-roots)
                         (list q-source-root))]
              [_ (unless wide-purged?
                   (set! wide-purged? #t)
                   (set! purged
                         (append purged (append-map purge-compiled-dirs! roots)))
                   (log-info
                    "reload: stale-linklet error, purged bytecode caches under ~a, retrying from source"
                    (string-join (map path->string roots) ", ")))]
              [retry (with-handlers ([exn:fail? (lambda (e) e)])
                       (load-extension-fresh path))])
         (if (exn:fail? retry)
             (list #f (exn-message retry) #f)
             (list retry #f #t)))]))
  (for ([pair (in-list discovered)])
    (define name (car pair))
    (define path (cdr pair))
    (define outcome (try-load path))
    (define result (list-ref outcome 0))
    (define message (list-ref outcome 1))
    (define recovered? (list-ref outcome 2))
    (cond
      [(and result (extension? result))
       (register-extension! registry result)
       (set! loaded (cons name loaded))
       (when recovered? (set! recovered (cons name recovered)))]
      [else
       (define msg
         (or message
             (and result
                  (format "loaded value is not an extension?: ~v" result))
             "load failed"))
       (log-warning "reload: FAILED to load ~a: ~a" name msg)
       (set! failed (cons (cons name msg) failed))]))
  (hasheq 'loaded (reverse loaded)
          'failed (reverse failed)
          'recovered (reverse recovered)
          'purged purged))

;; reload-extensions! : extension-registry? (listof path-string?) -> (listof string?)
;; Backward-compatible wrapper: returns just the names successfully
;; loaded. Callers that need honest failure reporting (e.g. the /reload
;; TUI command) use reload-extensions!/report.
(define (reload-extensions! registry extension-paths)
  (hash-ref (reload-extensions!/report registry extension-paths) 'loaded))


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

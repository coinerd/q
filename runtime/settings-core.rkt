#lang racket/base
;; STABILITY: public

;; runtime/settings.rkt — global and local runtime settings
;;
;; Responsibility: load, merge, and query configuration from
;; global (~/.q/config.json) and project-local (.q/config.json)
;; config files.  This is the foundation that auth-store and
;; model-registry depend on for reading provider configuration.
;;
;; Key behaviors:
;;   - Pure loading: load-global-settings / load-project-settings → hashes
;;   - Graceful: missing files → empty hash, malformed JSON → empty hash
;;   - Deep merge: project overrides global, nested hashes are merged
;;   - Query: setting-ref (flat), setting-ref* (nested path)
;;   - Provider access: convenience functions for providers section

(require "../util/json/json-helpers.rkt")
(require racket/contract
         racket/match
         racket/file
         racket/port
         racket/hash
         racket/list
         json
         "../util/config-paths.rkt")

;; Structs + Loading contracts (F6a extraction)
(provide q-settings
         q-settings?
         q-settings-merged
         q-settings-global
         q-settings-project

         (contract-out
          [load-settings
           (->* ()
                (path-string? #:home-dir path-string? #:config-path (or/c path-string? #f))
                q-settings?)]
          [load-global-settings (->* () (path-string?) hash?)]
          [load-project-settings (->* () (path-string?) hash?)]
          [make-minimal-settings
           (->* ()
                (#:provider (or/c string? #f) #:model (or/c string? #f) #:overrides (or/c hash? #f))
                q-settings?)]
          [merge-settings (-> hash? hash? hash?)]
          [deep-merge-hash (-> hash? hash? hash?)]
          [config-parse-error (-> path-string? (or/c string? #f))])

         ;; Internal helpers (needed by settings.rkt query functions)
         hash-nested-ref
         not-found?)

;; ============================================================
;; Struct
;; ============================================================

(struct q-settings
        (global ; #:INTERNAL — hash from ~/.q/config.json; use `merged' for access
         project ; #:INTERNAL — hash from .q/config.json; use `merged' for access
         merged ; PUBLIC — deep-merged settings (project overrides global)
         )
  #:transparent)

;; ============================================================
;; Internal helpers
;; ============================================================

;; v0.14.4 Wave 0: Detect config file that exists but has invalid JSON.
;; Returns #f if file is missing or valid, or an error message string if broken.
(define (config-parse-error path)
  (if (file-exists? path)
      (with-handlers ([exn:fail? (λ (e) (exn-message e))])
        (call-with-input-file path
                              (λ (in)
                                (define result (read-json in))
                                (if (hash? result) #f "top-level value is not a JSON object"))))
      #f))

;; Safely parse a JSON file, returning #f on any failure
;; or if the top-level value is not a JSON object (hash).
;; Logs a warning to stderr if the file exists but has invalid JSON.
(define (try-read-json-file path)
  (with-handlers ([exn:fail? (λ (e)
                               (when (file-exists? path)
                                 (fprintf (current-error-port)
                                          "WARNING: Config file ~a has invalid JSON: ~a\n"
                                          path
                                          (exn-message e)))
                               #f)])
    (if (file-exists? path)
        (call-with-input-file path
                              (λ (in)
                                (define result (read-json in))
                                (if (hash? result) result #f)))
        #f)))

;; Deep merge two hashes; right side (project) wins on conflicts.
;; Only hashes are merged recursively — scalars, lists, etc. are
;; replaced outright by the right-hand value.
(define (deep-merge-hash left right)
  (cond
    [(and (hash? left) (not (hash? right))) left]
    [(and (not (hash? left)) (hash? right)) right]
    [(not (and (hash? left) (hash? right))) (hash)]
    [else
     (for/fold ([acc left]) ([(k v) (in-hash right)])
       (define left-v (hash-ref acc k #f))
       (match* (left-v v)
         [((? hash?) (? hash?)) (hash-set acc k (deep-merge-hash left-v v))]
         [(_ _) (hash-set acc k v)]))]))

;; Sentinel for distinguishing "not found" from "found #f"
(define NOT-FOUND (gensym 'not-found))

(define (not-found? v)
  (eq? v NOT-FOUND))

;; Walk a nested hash using a list of keys.
;; Returns NOT-FOUND if any intermediate step fails.
(define (hash-nested-ref h key-path)
  (cond
    [(null? key-path) NOT-FOUND]
    ;; last key — look up directly
    [(null? (cdr key-path)) (hash-ref h (car key-path) NOT-FOUND)]
    [else
     (define next (hash-ref h (car key-path) NOT-FOUND))
     (if (hash? next)
         (hash-nested-ref next (cdr key-path))
         NOT-FOUND)]))

;; ============================================================
;; Loading
;; ============================================================

;; Load global settings from <home-dir>/.q/config.json.
;; Returns a hash on success, (hash) on missing / invalid.
(define (load-global-settings [home-dir (find-system-path 'home-dir)])
  (define cfg-path (build-path (car (project-config-dirs home-dir)) "config.json"))
  (or (try-read-json-file cfg-path) (hash)))

;; Load project settings from <project-dir>/.q/config.json.
;; Falls back to <project-dir>/.pi/config.json if .q/ is absent.
;; Returns a hash on success, (hash) on missing / invalid.
(define (load-project-settings [project-dir (current-directory)])
  (define dirs (project-config-dirs project-dir))
  (define (try-load-from-dir dir)
    (define cfg-path (build-path dir "config.json"))
    (and (file-exists? cfg-path) (try-read-json-file cfg-path)))
  (or (try-load-from-dir (first dirs)) (try-load-from-dir (second dirs)) (hash)))

;; Settings memoization cache (T3-5)
;; Key: (list project-dir home-dir), Value: (cons mtime q-settings)
;; W15 (v0.72.7): Mutable hash — accessed from main thread only during init.
;; Thread safety: Accessed from main thread only. If concurrent access needed,
;; add semaphore guard or swap to atomic box of immutable hash.
(define settings-cache (make-hash))

;; Internal: load settings from disk (no caching).
(define (load-settings-from-disk project-dir home-dir config-path)
  (define global-hash
    (if config-path
        (if (file-exists? config-path)
            (with-handlers ([exn:fail? (lambda (_) (hash))])
              (let ([result (read-json-file config-path)])
                (if (hash? result)
                    result
                    (hash))))
            (hash))
        (load-global-settings home-dir)))
  (define project-hash (load-project-settings project-dir))
  (define merged-hash (merge-settings global-hash project-hash))
  (q-settings global-hash project-hash merged-hash))

;; Load and merge both global and project settings.
;; Returns a q-settings struct. Memoized by (project-dir, home-dir).
(define (load-settings [project-dir (current-directory)]
                       #:home-dir [home-dir (find-system-path 'home-dir)]
                       #:config-path [config-path #f])
  (cond
    [config-path (load-settings-from-disk project-dir home-dir config-path)]
    [else
     (define key (list project-dir home-dir))
     (define cfg-path (build-path (car (project-config-dirs project-dir)) "config.json"))
     (define current-mtime (and (file-exists? cfg-path) (file-or-directory-modify-seconds cfg-path)))
     (define cached (hash-ref settings-cache key #f))
     (if (and cached (equal? (car cached) current-mtime))
         (cdr cached)
         (let ([result (load-settings-from-disk project-dir home-dir #f)])
           (hash-set! settings-cache key (cons current-mtime result))
           result))]))

;; Create a minimal q-settings with defaults.
;; Used when no config files exist (e.g., SDK path, tests).
;; Returns a q-settings with empty global/project and merged hash
;; containing only the provided overrides (or empty hash).
(define (make-minimal-settings #:provider [provider #f]
                               #:model [model #f]
                               #:overrides [overrides (hash)])
  (define effective-overrides (or overrides (hash)))
  (define merged
    (for/fold ([acc (hash)]) ([(k v) (in-hash effective-overrides)])
      (if v
          (hash-set acc k v)
          acc)))
  (define with-provider
    (if provider
        (hash-set merged 'default-provider provider)
        merged))
  (define with-model
    (if model
        (hash-set with-provider 'default-model model)
        with-provider))
  (q-settings (hash) (hash) with-model))

;; ============================================================
;; Merging
;; ============================================================

;; Merge global and project hashes with project winning on conflicts.
(define (merge-settings global-hash project-hash)
  (deep-merge-hash global-hash project-hash))

;; ============================================================

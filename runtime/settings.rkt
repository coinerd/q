#lang racket/base

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

(require racket/file
         racket/port
         racket/hash
         racket/list
         json
         "../util/config-paths.rkt")

(provide
 ;; Structs
 (struct-out q-settings)

 ;; Loading
 load-settings
 load-global-settings
 load-project-settings

 ;; Merging
 merge-settings

 ;; Query
 setting-ref
 setting-ref*
 provider-config
 provider-names

 ;; Parallel execution
 parallel-tools-enabled?

 ;; Defaults and derived paths
 default-session-dir
 default-project-dir
 session-dir-from-settings
 project-dir-from-settings)

;; ============================================================
;; Struct
;; ============================================================

(struct q-settings
  (global    ; hash — parsed from ~/.q/config.json (or (hash) if missing)
   project   ; hash — parsed from .q/config.json (or (hash) if missing)
   merged    ; hash — deep-merged with project overriding global
   )
  #:transparent)

;; ============================================================
;; Internal helpers
;; ============================================================

;; Safely parse a JSON file, returning #f on any failure
;; or if the top-level value is not a JSON object (hash).
(define (try-read-json-file path)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (call-with-input-file path
      (λ (in)
        (define result (read-json in))
        (if (hash? result) result #f)))))

;; Deep merge two hashes; right side (project) wins on conflicts.
;; Only hashes are merged recursively — scalars, lists, etc. are
;; replaced outright by the right-hand value.
(define (deep-merge-hash left right)
  (for/fold ([acc left])
            ([(k v) (in-hash right)])
    (define left-v (hash-ref acc k #f))
    (cond
      [(and (hash? left-v) (hash? v))
       (hash-set acc k (deep-merge-hash left-v v))]
      [else
       (hash-set acc k v)])))

;; Walk a nested hash using a list of keys.
;; Returns #f if any intermediate step fails.
(define (hash-nested-ref h key-path)
  (cond
    [(null? key-path) #f]
    [(null? (cdr key-path))
     ;; last key — look up directly
     (hash-ref h (car key-path) #f)]
    [else
     (define next (hash-ref h (car key-path) #f))
     (if (hash? next)
         (hash-nested-ref next (cdr key-path))
         #f)]))

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
    (and (file-exists? cfg-path)
         (try-read-json-file cfg-path)))
  (or (try-load-from-dir (first dirs))
      (try-load-from-dir (second dirs))
      (hash)))

;; Load and merge both global and project settings.
;; Returns a q-settings struct.
(define (load-settings [project-dir (current-directory)]
                       #:home-dir [home-dir (find-system-path 'home-dir)]
                       #:config-path [config-path #f])
  (define global-hash
    (if config-path
        (if (file-exists? config-path)
            (with-handlers ([exn:fail? (λ (_) (hash))])
              (call-with-input-file config-path
                (λ (in) (read-json in))))
            (hash))
        (load-global-settings home-dir)))
  (define project-hash (load-project-settings project-dir))
  (define merged-hash (merge-settings global-hash project-hash))
  (q-settings global-hash project-hash merged-hash))

;; ============================================================
;; Merging
;; ============================================================

;; Merge global and project hashes with project winning on conflicts.
(define (merge-settings global-hash project-hash)
  (deep-merge-hash global-hash project-hash))

;; ============================================================
;; Query
;; ============================================================

;; Get a setting by key from merged settings.
;; Returns default if key not found.
(define (setting-ref settings key [default #f])
  (hash-ref (q-settings-merged settings) key default))

;; Get a nested setting by key path (list of keys).
;; e.g., (setting-ref* settings '(providers openai base-url))
;; Accepts both symbol and string keys.
;; Returns default if any step in the path is missing.
(define (setting-ref* settings key-path [default #f])
  (cond
    [(null? key-path) default]
    [else
     (define result (hash-nested-ref (q-settings-merged settings) key-path))
     (if result result default)]))

;; Get a specific provider's config hash.
;; Returns #f if provider not configured.
(define (provider-config settings provider-name)
  (setting-ref* settings (list 'providers provider-name)))

;; List configured provider names.
(define (provider-names settings)
  (define providers-hash (setting-ref settings 'providers (hash)))
  (map (lambda (k) (if (symbol? k) (symbol->string k) k)) (hash-keys providers-hash)))

;; ============================================================
;; Parallel execution setting
;; ============================================================

;; Whether tools may be executed in parallel within a batch.
;; Reads 'parallel-tools from merged settings, defaults to #f.
(define (parallel-tools-enabled? settings)
  (setting-ref settings 'parallel-tools #f))

;; ============================================================
;; Defaults and derived paths
;; ============================================================

;; Default session directory: ~/.q/sessions
(define (default-session-dir)
  (build-path (global-config-dir) "sessions"))

;; Default project directory: current working directory
(define (default-project-dir)
  (current-directory))

;; Get session-dir from merged settings, falling back to default
(define (session-dir-from-settings settings)
  (or (setting-ref settings 'session-dir #f)
      (default-session-dir)))

;; Get project-dir from merged settings, falling back to default
(define (project-dir-from-settings settings)
  (or (setting-ref settings 'project-dir #f)
      (default-project-dir)))

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

;; Structs
(provide (struct-out q-settings)

         ;; Loading
         load-settings
         load-global-settings
         load-project-settings

         ;; Constructor
         make-minimal-settings

         ;; Merging
         merge-settings

         ;; Query
         setting-ref
         setting-ref*
         provider-config
         provider-names
         config-parse-error

         ;; Parallel execution
         parallel-tools-enabled?

         ;; HTTP request timeout
         http-request-timeout

         ;; Per-model timeout profiles (v0.14.2)
         get-model-timeout
         effective-request-timeout

         ;; Destructive command warning
         warn-on-destructive?

         ;; Sandbox settings
         sandbox-enabled?
         sandbox-timeout
         sandbox-memory-limit
         sandbox-max-output
         sandbox-max-processes

         ;; Defaults and derived paths
         default-session-dir
         default-project-dir
         session-dir-from-settings
         project-dir-from-settings)

;; ============================================================
;; Struct
;; ============================================================

(struct q-settings
        (global ; hash — parsed from ~/.q/config.json (or (hash) if missing)
         project ; hash — parsed from .q/config.json (or (hash) if missing)
         merged ; hash — deep-merged with project overriding global
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
  (for/fold ([acc left]) ([(k v) (in-hash right)])
    (define left-v (hash-ref acc k #f))
    (cond
      [(and (hash? left-v) (hash? v)) (hash-set acc k (deep-merge-hash left-v v))]
      [else (hash-set acc k v)])))

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

;; Load and merge both global and project settings.
;; Returns a q-settings struct.
(define (load-settings [project-dir (current-directory)]
                       #:home-dir [home-dir (find-system-path 'home-dir)]
                       #:config-path [config-path #f])
  (define global-hash
    (if config-path
        (if (file-exists? config-path)
            (with-handlers ([exn:fail? (λ (_) (hash))])
              (call-with-input-file config-path (λ (in) (read-json in))))
            (hash))
        (load-global-settings home-dir)))
  (define project-hash (load-project-settings project-dir))
  (define merged-hash (merge-settings global-hash project-hash))
  (q-settings global-hash project-hash merged-hash))

;; Create a minimal q-settings with defaults.
;; Used when no config files exist (e.g., SDK path, tests).
;; Returns a q-settings with empty global/project and merged hash
;; containing only the provided overrides (or empty hash).
(define (make-minimal-settings #:provider [provider #f]
                               #:model [model #f]
                               #:overrides [overrides (hash)])
  (define merged
    (for/fold ([acc (hash)]) ([(k v) (in-hash overrides)])
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
     (if (not-found? result) default result)]))

;; Get a specific provider's config hash.
;; Returns #f if provider not configured.
(define (provider-config settings provider-name)
  (setting-ref* settings (list 'providers provider-name)))

;; List configured provider names.
(define (provider-names settings)
  (define providers-hash (setting-ref settings 'providers (hash)))
  (hash-keys providers-hash))

;; ============================================================
;; Parallel execution setting
;; ============================================================

;; Whether tools may be executed in parallel within a batch.
;; Reads 'parallel-tools from merged settings, defaults to #f.
(define (parallel-tools-enabled? settings)
  (setting-ref settings 'parallel-tools #f))

;; ============================================================
;; HTTP request timeout setting
;; ============================================================

;; Overall HTTP request timeout in seconds (covers connection + response reading).
;; Defaults to 300 seconds (5 minutes).
(define (http-request-timeout settings)
  (setting-ref settings 'http-request-timeout 300))

;; ============================================================
;; Destructive command warning setting
;; ============================================================

;; Whether to emit a warning to stderr when destructive commands are detected.
;; Defaults to #f (no warning).
(define (warn-on-destructive? settings)
  (setting-ref settings 'warn-on-destructive #f))

;; ============================================================
;; Sandbox settings
;; ============================================================

;; Whether sandboxing is enabled for bash tool execution.
;; Reads 'tools.use-sandbox' from merged settings, defaults to #t.
(define (sandbox-enabled? settings)
  (setting-ref* settings '(tools use-sandbox) #t))

;; Sandbox timeout in seconds. Reads 'tools.sandbox-timeout'.
;; Defaults to 120 seconds.
(define (sandbox-timeout settings)
  (setting-ref* settings '(tools sandbox-timeout) 120))

;; Sandbox memory limit in bytes. Reads 'tools.sandbox-memory'.
;; Defaults to 536870912 (512 MB).
(define (sandbox-memory-limit settings)
  (setting-ref* settings '(tools sandbox-memory) 536870912))

;; Sandbox max output in bytes. Reads 'tools.sandbox-max-output'.
;; Defaults to 1048576 (1 MB).
(define (sandbox-max-output settings)
  (setting-ref* settings '(tools sandbox-max-output) 1048576))

;; Sandbox max concurrent processes. Reads 'tools.sandbox-max-processes'.
;; Defaults to 10.
(define (sandbox-max-processes settings)
  (setting-ref* settings '(tools sandbox-max-processes) 10))

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
  (or (setting-ref settings 'session-dir #f) (default-session-dir)))

;; Get project-dir from merged settings, falling back to default
(define (project-dir-from-settings settings)
  (or (setting-ref settings 'project-dir #f) (default-project-dir)))

;; ============================================================
;; Per-model timeout profiles (v0.14.2 Wave 3)
;; ============================================================

;; Get per-model timeout override for a specific timeout kind.
;; Config schema: { "timeouts": { "models": { "glm-5.1": { "request": 900 } } } }
;; Returns #f if no per-model override configured.
(define (get-model-timeout settings model-name timeout-key)
  (define model-overrides (setting-ref* settings `(timeouts models ,(string->symbol model-name)) #f))
  (and model-overrides (hash? model-overrides) (hash-ref model-overrides timeout-key #f)))

;; Get effective request timeout: per-model override or global default.
(define (effective-request-timeout settings model-name)
  (or (get-model-timeout settings model-name 'request) (http-request-timeout settings)))

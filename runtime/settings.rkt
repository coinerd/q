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

(require "../util/json-helpers.rkt")
(require racket/contract
         racket/match
         racket/file
         racket/port
         racket/hash
         racket/list
         json
         "../util/config-paths.rkt"
         (only-in "../util/sandbox-config.rkt"
                  sandbox-enabled?
                  sandbox-timeout
                  sandbox-memory-limit
                  sandbox-max-output
                  sandbox-max-processes))

;; Structs
(provide q-settings
         q-settings?
         q-settings-global
         q-settings-project
         q-settings-merged

         ;; Loading — contracted
         (contract-out [load-settings
                        (->* ()
                             (path-string? #:home-dir path-string?
                                           #:config-path (or/c path-string? #f))
                             q-settings?)])

         ;; Loading — all contracted
         (contract-out [load-global-settings (->* () (path-string?) hash?)]
                       [load-project-settings (->* () (path-string?) hash?)]
                       [make-minimal-settings
                        (->* ()
                             (#:provider any/c #:model (or/c string? #f) #:overrides (or/c hash? #f))
                             q-settings?)]
                       [merge-settings (-> hash? hash? hash?)]
                       [deep-merge-hash (-> any/c any/c hash?)]
                       [setting-ref (->* (q-settings? (or/c symbol? string?)) (any/c) any/c)]
                       [setting-ref* (->* (q-settings? (listof any/c)) (any/c) any/c)]
                       [provider-config (-> q-settings? (or/c symbol? string?) (or/c hash? #f))]
                       [provider-names (-> q-settings? (listof symbol?))]
                       [config-parse-error (-> path-string? (or/c string? #f))]
                       [parallel-tools-enabled? (-> q-settings? boolean?)]
                       [http-request-timeout (-> q-settings? number?)]
                       [get-model-timeout (-> q-settings? string? symbol? (or/c number? #f))]
                       [effective-request-timeout (-> q-settings? string? number?)]
                       [warn-on-destructive? (-> q-settings? boolean?)]
                       [security-config-from-settings (-> q-settings? hash?)]
                       [default-session-dir (-> path-string?)]
                       [default-project-dir (-> path?)]
                       [session-dir-from-settings (-> q-settings? (or/c path-string? #f))]
                       [project-dir-from-settings (-> q-settings? (or/c path-string? #f))]
                       [trace-enabled? (-> q-settings? boolean?)]
                       [trace-max-files (-> q-settings? exact-positive-integer?)]
                       [steering-gentle-threshold (-> q-settings? number?)]
                       [steering-strong-threshold (-> q-settings? number?)]
                       [steering-hard-cap (-> q-settings? number?)]
                       [steering-same-file-dedup? (-> q-settings? boolean?)]
                       [credential-policy
                        (-> q-settings?
                            (or/c 'auto 'keychain-preferred 'keychain-required 'env-only))]
                       [shell-risk-classifier
                        (-> q-settings?
                            (or/c 'regex 'structured 'both))])

         ;; Sandbox settings (re-exported, not locally defined)
         sandbox-enabled?
         sandbox-timeout
         sandbox-memory-limit
         sandbox-max-output
         sandbox-max-processes)

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
;; Security config loader (v0.25.2 — F3)
;; ============================================================

(define (security-config-from-settings settings)
  (define merged (q-settings-merged settings))
  (hasheq 'execution-policy-mode
          (hash-ref merged 'execution-policy (hash-ref merged 'execution-policy.mode #f))
          'execution-policy-allowed
          (hash-ref merged 'execution-policy.allowed '())
          'secret-scrub-extra-denylist
          (hash-ref merged 'secret-scrub.extra-denylist '())
          'secret-scrub-allowlist
          (hash-ref merged 'secret-scrub.allowlist '())))

;; ============================================================
;; Sandbox settings — re-exported from util/sandbox-config.rkt
;; ============================================================

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

;; ============================================================
;; Trace logging config (v0.15.0)
;; ============================================================

;; Is trace logging enabled? Reads logging.trace.enabled from config.
;; Default: #f (disabled — zero overhead when off)
(define (trace-enabled? settings)
  (define trace-cfg (setting-ref* settings '(logging trace) #f))
  (and (hash? trace-cfg) (hash-ref trace-cfg 'enabled #f)))

;; Maximum trace files to keep (rotation). Default: 10.
(define (trace-max-files settings)
  (define trace-cfg (setting-ref* settings '(logging trace) #f))
  (if (hash? trace-cfg)
      (hash-ref trace-cfg 'max-files 10)
      10))

;; ============================================================
;; Steering config (v0.18.0)
;; ============================================================

;; Gentle steering threshold. Default: 8 (raised from 5 in v0.18.0).
(define (steering-gentle-threshold settings)
  (setting-ref* settings '(steering gentle_threshold) 8))

;; Strong steering threshold. Default: 12 (raised from 7 in v0.18.0).
(define (steering-strong-threshold settings)
  (setting-ref* settings '(steering strong_threshold) 12))

;; Hard cap for steering. Default: 20 (raised from 12 in v0.18.0).
(define (steering-hard-cap settings)
  (setting-ref* settings '(steering hard_cap) 20))

;; Whether same-file dedup is enabled. Default: #t.
(define (steering-same-file-dedup? settings)
  (setting-ref* settings '(steering same_file_dedup) #t))

;; ============================================================
;; Credential policy (v0.70.1)
;; ============================================================

;; Credential policy controls how credential backends behave when
;; keychain is unavailable and file/env fallback would be used.
;;
;; Modes:
;;   'auto               — current chain, no policy enforcement (default, backward-compatible)
;;   'keychain-preferred — file fallback allowed with warning
;;   'keychain-required  — file fallback forbidden for store/load
;;   'env-only           — no local credential file writes
;;
;; Config key: credentials.policy

(define (credential-policy settings)
  (setting-ref* settings '(credentials policy) 'auto))

;; ============================================================
;; Shell risk classifier mode (v0.70.3)
;; ============================================================

;; Controls which shell risk detection method to use.
;; Modes:
;;   'regex     — use regex-based patterns only (default, backward-compatible)
;;   'structured — use structured classifier only
;;   'both       — use both; classifier wins on disagreement
;;
;; Config key: security.shell-risk-classifier
(define (shell-risk-classifier settings)
  (setting-ref* settings '(security shell-risk-classifier) 'regex))

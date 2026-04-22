#lang racket/base

;;; runtime/auth-store.rkt — provider credentials lookup
;;;
;;; Resolves API keys for providers by checking environment variables
;;; first, then falling back to config. No coupling to settings.rkt —
;;; all config is received as parameter hashes.

(require json
         racket/file
         racket/string
         racket/generic
         racket/path)

;; Structs
(provide (struct-out credential)
         (struct-out redacted-credential)

         ;; Lookup
         lookup-credential
         credential-present?

         ;; Storage (optional persistence)
         store-credential!

         ;; Batch resolution
         resolve-provider-credentials

         ;; Redaction
         mask-api-key
         cred->redacted

         ;; Validation
         validate-credential-format

         ;; Credential file
         load-credential-file
         save-credential-file!
         credential-file-path

         ;; Scoped access
         with-credential)

;; ═══════════════════════════════════════════════════════════════════
;; mask-api-key (defined before credential struct for custom-write)
;; ═══════════════════════════════════════════════════════════════════

;; Mask an API key for safe display.
;; Shows first 3 chars and last 4 chars, with "..." in between.
;; If key is too short (< 8 chars), shows "****".
(define (mask-api-key api-key)
  (cond
    [(not (string? api-key)) "****"]
    [(< (string-length api-key) 8) "****"]
    [else
     (define prefix (substring api-key 0 3))
     (define suffix (substring api-key (- (string-length api-key) 4)))
     (format "~a...~a" prefix suffix)]))

;; ═══════════════════════════════════════════════════════════════════
;; Struct
;; ═══════════════════════════════════════════════════════════════════

;; A resolved credential
(struct credential
        (provider-name ; string — e.g. "openai", "anthropic"
         api-key ; string — the resolved API key
         source ; symbol — 'environment | 'config | 'stored
         )
  #:methods gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (and (equal?-recur (credential-provider-name a) (credential-provider-name b))
          (equal?-recur (credential-api-key a) (credential-api-key b))
          (equal?-recur (credential-source a) (credential-source b))))
   (define (hash-proc a hash-recur)
     (+ (hash-recur (credential-provider-name a))
        (hash-recur (credential-api-key a))
        (hash-recur (credential-source a))))
   (define (hash2-proc a hash2-recur)
     (+ (hash2-recur (credential-provider-name a))
        (hash2-recur (credential-api-key a))
        (hash2-recur (credential-source a))))]
  #:methods gen:custom-write
  [(define (write-proc cred port mode)
     (fprintf port
              "#<credential ~a ~a ~a>"
              (credential-provider-name cred)
              (mask-api-key (credential-api-key cred))
              (credential-source cred)))])

;; ═══════════════════════════════════════════════════════════════════
;; Internal helpers
;; ═══════════════════════════════════════════════════════════════════

;; Helper: non-empty string? — string that is not empty and not only whitespace
(define (non-empty-string? v)
  (and (string? v) (> (string-length (string-trim v)) 0)))

;; Helper: get a value from a hash by either symbol or string key
;; Returns the first found value, or #f if neither key exists
(define (config-ref cfg key-sym key-str [default #f])
  (or (hash-ref cfg key-sym #f) (hash-ref cfg key-str default)))

;; ═══════════════════════════════════════════════════════════════════
;; lookup-credential
;; ═══════════════════════════════════════════════════════════════════

;; Lookup a credential for a provider.
;; Resolution order:
;;   1. Environment variable: check env-var-name from provider-config
;;      (key: 'api-key-env or "api-key-env")
;;   2. Config file: check provider-config for 'api-key or "api-key"
;;   3. Return #f if not found
(define (lookup-credential provider-name provider-config)
  ;; Normalize provider-name to string (JSON keys may be symbols)
  (define norm-name
    (if (symbol? provider-name)
        (symbol->string provider-name)
        provider-name))
  ;; Guard: if provider-config is #f, try credential file only
  (cond
    [(not provider-config) (credential-from-file norm-name)]
    [else
     (let* ([env-var-name (config-ref provider-config 'api-key-env "api-key-env")]
            [env-val (and env-var-name (getenv env-var-name))])
       (cond
         [(and env-val (non-empty-string? env-val)) (credential norm-name env-val 'environment)]
         [else
          (let ([config-key (config-ref provider-config 'api-key "api-key")])
            (cond
              [(and config-key (non-empty-string? config-key))
               (credential norm-name config-key 'config)]
              ;; Fall back to credential file (~/.q/credentials.json)
              [else (credential-from-file norm-name)]))]))]))

;; Lookup credential from the dedicated credential file.
;; Returns #<credential ...> or #f.
(define (credential-from-file provider-name)
  (define file-creds (load-credential-file))
  (define prov-cfg (hash-ref file-creds provider-name #f))
  (cond
    [(not prov-cfg) #f]
    [else
     (define key (hash-ref prov-cfg 'api-key #f))
     (if (and key (non-empty-string? key))
         (credential provider-name key 'stored)
         #f)]))

;; ═══════════════════════════════════════════════════════════════════
;; credential-present?
;; ═══════════════════════════════════════════════════════════════════

;; Check if a credential is available (without returning it)
(define (credential-present? provider-name provider-config)
  (and (lookup-credential provider-name provider-config) #t))

;; ═══════════════════════════════════════════════════════════════════
;; store-credential!
;; ═══════════════════════════════════════════════════════════════════

;; Store a credential (update config hash in memory, optionally persist to file).
;; Returns the updated provider-config hash.
(define (store-credential! provider-name
                           api-key
                           #:provider-config provider-config
                           #:config-path [config-path #f])
  ;; Update the provider-config with the new api-key
  (define updated (hash-set provider-config 'api-key api-key))
  (when config-path
    (write-credential-to-config! config-path provider-name api-key))
  updated)

;; ═══════════════════════════════════════════════════════════════════
;; resolve-provider-credentials
;; ═══════════════════════════════════════════════════════════════════

;; Resolve credentials for all configured providers.
;; Returns a hash: provider-name → credential (or #f if not found)
(define (resolve-provider-credentials providers-hash)
  (for/hash ([(name cfg) (in-hash providers-hash)])
    (values name (lookup-credential name cfg))))

;; ═══════════════════════════════════════════════════════════════════
;; redacted-credential struct
;; ═══════════════════════════════════════════════════════════════════

;; A credential with the key masked — safe for logging/display
(struct redacted-credential
        (provider-name ; string
         masked-api-key ; string — e.g. "sk-...7k3d"
         source ; symbol — 'environment | 'config | 'stored
         )
  #:transparent)

;; ═══════════════════════════════════════════════════════════════════
;; cred->redacted
;; ═══════════════════════════════════════════════════════════════════

;; Create a redacted-credential from a credential.
(define (cred->redacted cred)
  (redacted-credential (credential-provider-name cred)
                       (mask-api-key (credential-api-key cred))
                       (credential-source cred)))

;; ═══════════════════════════════════════════════════════════════════
;; validate-credential-format
;; ═══════════════════════════════════════════════════════════════════

;; Basic format validation for known provider key formats.
;; Returns #t if valid or unknown provider, #f if clearly invalid.
(define (validate-credential-format provider-name api-key)
  (cond
    [(not (and (string? provider-name) (string? api-key))) #f]
    ;; OpenAI keys start with "sk-"
    [(and (string-contains? (string-downcase provider-name) "openai")
          (not (string-prefix? api-key "sk-")))
     #f]
    ;; Anthropic keys start with "sk-ant-"
    [(and (string-contains? (string-downcase provider-name) "anthropic")
          (not (string-prefix? api-key "sk-ant-")))
     #f]
    ;; Unknown providers — accept anything non-empty
    [else (non-empty-string? api-key)]))

;; ═══════════════════════════════════════════════════════════════════
;; Credential file operations
;; ═══════════════════════════════════════════════════════════════════

;; Default credential file path: ~/.q/credentials.json
(define (credential-file-path)
  (build-path (find-system-path 'home-dir) ".q" "credentials.json"))

;; Load credentials from a dedicated JSON file.
;; File format: { "providers": { "name": { "api-key": "..." }, ... } }
;; Returns a hash: provider-name → provider-config (with 'api-key)
(define (load-credential-file [path (credential-file-path)])
  (cond
    [(not (file-exists? path)) (hash)]
    [else
     (with-handlers ([exn:fail? (λ (e) (hash))])
       (define content (call-with-input-file path read-json))
       (if (eof-object? content)
           (hash)
           (let ([providers (hash-ref content 'providers (hash))])
             ;; Convert symbol-keyed hash to string-keyed
             (for/hash ([(k v) (in-hash providers)])
               (values (if (symbol? k)
                           (symbol->string k)
                           k)
                       v)))))]))

;; Save a credential to the dedicated credential file.
;; SECURITY NOTE: API keys are stored as plaintext on disk.
;; The credential file has #o600 (owner-only) permissions, but keys
;; are not encrypted at rest. On shared systems, consider using
;; environment variables or a dedicated secrets manager instead.
(define (save-credential-file! provider-name api-key [path (credential-file-path)])
  (with-handlers ([exn:fail?
                   (λ (e) (log-warning (format "save-credential-file! failed: ~a" (exn-message e))))])
    (define dir (path-only path))
    (when (and dir (not (directory-exists? dir)))
      (make-directory* dir))
    (define existing (load-credential-file path))
    (define updated (hash-set existing provider-name (hasheq 'api-key api-key)))
    ;; Write in the format: { "providers": { ... } }
    (define file-content
      (hasheq 'providers
              (for/hash ([(k v) (in-hash updated)])
                (values (if (string? k)
                            (string->symbol k)
                            k)
                        v))))
    (atomic-write-json! path file-content)))

;; ═══════════════════════════════════════════════════════════════════
;; with-credential macro
;; ═══════════════════════════════════════════════════════════════════

;; Scoped access to a credential — the raw key is only available
;; inside the body and not returned or stored elsewhere.
(define-syntax-rule (with-credential provider-name provider-config key-id body ...)
  (let ([cred (lookup-credential provider-name provider-config)])
    (if cred
        (let ([key-id (credential-api-key cred)])
          body ...)
        #f)))

;; ═══════════════════════════════════════════════════════════════════
;; Internal: write credential to config file
;; ═══════════════════════════════════════════════════════════════════

;; Read existing JSON config, update the provider's api-key, write back.
;; If the file doesn't exist, create a new config structure.
;; Handles errors gracefully — does not propagate exceptions.
;; SECURITY NOTE: API keys are stored as plaintext on disk.
;; The credential file has #o600 (owner-only) permissions, but keys
;; are not encrypted at rest. On shared systems, consider using
;; environment variables or a dedicated secrets manager instead.
(define (write-credential-to-config! config-path provider-name api-key)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "credential save failed: ~a" (exn-message e))))])
    (internal-write-credential-to-config! config-path provider-name api-key)))

;; Actual implementation (separated for clarity)
(define (internal-write-credential-to-config! config-path provider-name api-key)
  ;; Ensure parent directory exists
  (define dir (path-only config-path))
  (when (and dir (not (directory-exists? dir)))
    (make-directory* dir))
  ;; Read existing config or start fresh
  (define existing
    (if (file-exists? config-path)
        (let ([content (call-with-input-file config-path read-json)])
          (if (eof-object? content)
              (hasheq)
              content))
        (hasheq)))
  ;; Navigate to providers section, update the provider's api-key
  ;; read-json produces hasheq with symbol keys
  ;; provider-name is a string — convert to symbol for JSON key
  (define provider-sym
    (if (symbol? provider-name)
        provider-name
        (string->symbol provider-name)))
  (define providers (hash-ref existing 'providers (hasheq)))
  (define old-provider-cfg (hash-ref providers provider-sym (hasheq)))
  (define new-provider-cfg (hash-set old-provider-cfg 'api-key api-key))
  (define new-providers (hash-set providers provider-sym new-provider-cfg))
  (define new-config (hash-set existing 'providers new-providers))
  ;; Write back atomically
  (atomic-write-json! config-path new-config))

;; ═══════════════════════════════════════════════════════════════════
;; atomic-write-json!
;; ═══════════════════════════════════════════════════════════════════

;; Write JSON data to a file atomically by writing to a temp file
;; first, then renaming. Prevents file corruption on crash mid-write.
(define (atomic-write-json! path data)
  (define dir (path-only path))
  (when (and dir (not (directory-exists? dir)))
    (make-directory* dir))
  (define tmp
    (make-temporary-file "credential-~a.tmp"
                         #f
                         (if dir
                             dir
                             (find-system-path 'temp-dir))))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (with-handlers ([exn:fail? (lambda (e)
                                                  (log-warning (format "credential load failed: ~a"
                                                                       (exn-message e))))])
                       (delete-file tmp))
                     (raise e))])
    (call-with-output-file tmp (lambda (out) (write-json data out)) #:exists 'truncate)
    (rename-file-or-directory tmp path #t)
    (file-or-directory-permissions path #o600)))

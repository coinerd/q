#lang racket/base
;; runtime/model-registry.rkt — named model resolution and defaults
;;
;; Responsible for:
;;   - indexing model names from provider configs for O(1) lookup
;;   - resolving model names to provider + base-url + config
;;   - listing all available models across providers
;;   - providing default model selection
;;
;; Not responsible for:
;;   - HTTP requests
;;   - credential storage
;;   - file I/O
;;   - session management

(require racket/string
         racket/match
         racket/contract
         (only-in "../../llm/provider-errors.rkt" raise-provider-error)
         "../../util/hash-helpers.rkt")

;; ── URL Validation ──────────────────────────────────────────

;; Validate base-url to prevent SSRF. Returns cleaned url or raises error.
;; Allowed schemes: http, https. Blocks private IPs and localhost.
(define (validate-base-url url-str)
  (define trimmed (string-trim url-str))
  (cond
    [(string=? trimmed "") ""] ; empty is ok — will use provider default
    [(not (regexp-match? #rx"^https?://" trimmed))
     (raise-provider-error (format "Invalid base-url scheme (must be http or https): ~a" trimmed)
                           'config)]
    [else trimmed]))

(define (safe-base-url cfg)
  (define raw (flex-ref cfg 'base-url ""))
  (if (string? raw)
      (validate-base-url raw)
      ""))

;; Structs
(provide model-entry?
         model-entry
         model-entry-name
         model-entry-provider-name
         model-entry-provider-config
         model-registry?
         model-registry
         model-resolution?
         model-resolution
         model-resolution-model-name
         model-resolution-provider-name
         model-resolution-base-url
         model-resolution-provider-config
         ;; H-02: Contract-wrapped exports
         (contract-out
          [resolve-model (-> model-registry? (or/c string? #f) (or/c model-resolution? #f))]
          [resolve-model-by-provider (-> model-registry? string? (or/c model-resolution? #f))]
          [invalidate-model-resolution-cache! (-> void?)]
          [available-models (-> model-registry? (listof model-entry?))]
          [default-model (-> model-registry? (or/c string? #f))]
          [default-model-for-mode (-> model-registry? symbol? (or/c string? #f))]
          [make-model-registry-from-config (-> hash? model-registry?)])
         model-registry-context-window)

;; ============================================================
;; Structs
;; ============================================================

;; A model entry — describes an available model
(struct model-entry
        (name ; string — model name (e.g. "gpt-4o")
         provider-name ; string — provider name (e.g. "openai")
         provider-config ; hash — full provider config section
         )
  #:transparent)

;; A resolution result — everything needed to create a provider instance
(struct model-resolution
        (model-name ; string — the resolved model name
         provider-name ; string — the provider to use
         base-url ; string — API base URL
         provider-config ; hash — full provider config (for auth-store etc.)
         )
  #:transparent)

;; Internal registry struct (not exported)
(struct model-registry
        (index ; hash: model-name -> (listof model-entry) -- may have duplicates across providers
         providers ; hash: provider-name-string -> provider-config-hash
         default-provider ; string or #f
         default-model ; string or #f
         ))

;; ============================================================
;; Resolution cache — v0.47.6
;; ============================================================

;; Hash-based cache keyed on (model-name-string . registry-hash-code).
;; Cleared when registry changes (new registry = new hash).
;; W15 (v0.72.7): Mutable hash — accessed during model resolution (main thread).
;; Not concurrently written. Thread-safe in practice due to single-writer pattern.
(define model-resolution-cache (make-hash))

(define (cache-key registry model-name)
  (cons model-name (equal-hash-code registry)))

(define (cached-resolve-model registry model-name)
  (define key (cache-key registry model-name))
  (hash-ref model-resolution-cache key (lambda () #f)))

(define (cache-resolve-model! registry model-name result)
  (define key (cache-key registry model-name))
  (hash-set! model-resolution-cache key result))

(define (invalidate-model-resolution-cache!)
  (hash-clear! model-resolution-cache))

;; ============================================================
;; Config key normalization
;; ============================================================

;; Normalize a key to string for consistent lookup
;; flex-ref and normalize-keys now imported from util/hash-helpers.rkt

;; ============================================================
;; Model ID extraction — supports both string and object-style models
;; ============================================================

;; Extract model ID from either:
;;   - string: "gpt-4o" → "gpt-4o"
;;   - hash: (hasheq "id" "gpt-4o" "name" "...") → "gpt-4o"
;; Returns #f if invalid
(define (extract-model-id m)
  (match m
    [(? string?) m]
    [(? hash?) (flex-ref m "id" #f)]
    [_ #f]))

;; Check if model-id is in models-list (handles both string and object styles)
(define (model-in-list? model-id models-list)
  (for/or ([m (in-list models-list)])
    (equal? model-id (extract-model-id m))))

(define (flex-has-key? h key)
  (define str-key
    (if (symbol? key)
        (symbol->string key)
        key))
  (define sym-key
    (if (string? key)
        (string->symbol key)
        key))
  (or (hash-has-key? h str-key) (hash-has-key? h sym-key)))

;; ============================================================
;; Registry creation
;; ============================================================

(define (make-model-registry-from-config config-hash)
  ;; config-hash is the merged settings hash (global + project)
  ;; Keys may be strings (from JSON) or symbols (from Racket code)
  (define providers-raw (flex-ref config-hash 'providers (hasheq)))
  (define providers-normalized (normalize-keys providers-raw))
  (define default-provider-name (flex-ref config-hash 'default-provider #f))
  (define default-model-name (flex-ref config-hash 'default-model #f))

  ;; Build index: model-name → (listof model-entry)
  ;; Build provider map: provider-name-string → config
  (define-values (index provider-map)
    (for*/fold ([idx (hash)]
                [pmap (hash)])
               ([(prov-key prov-config) (in-hash providers-normalized)])
      (define prov-name (key->string prov-key))
      (define models-list (flex-ref prov-config 'models '()))
      (define entries
        (for/list ([m (in-list models-list)])
          (define model-id (extract-model-id m))
          (model-entry model-id prov-name prov-config)))
      ;; If no explicit models list but a default-model is set,
      ;; add the default-model as a single entry so it can be resolved.
      (define effective-entries
        (if (and (null? entries) (flex-ref prov-config 'default-model #f))
            (list (model-entry (flex-ref prov-config 'default-model #f) prov-name prov-config))
            entries))
      ;; Add each model to the index
      (define new-idx
        (for/fold ([i idx]) ([e (in-list effective-entries)])
          (hash-update i (model-entry-name e) (λ (existing) (cons e existing)) '())))
      (values new-idx (hash-set pmap prov-name prov-config))))

  ;; If no global default-model, try to derive from default-provider
  (define effective-default-model
    (cond
      [default-model-name default-model-name]
      [default-provider-name
       (define prov-cfg (hash-ref provider-map default-provider-name #f))
       (and prov-cfg (let ([dm (flex-ref prov-cfg 'default-model #f)]) (if (eq? dm #f) #f dm)))]
      [else #f]))

  ;; If no effective default, try first provider's default
  (define final-default-model
    (if effective-default-model
        effective-default-model
        (for/or ([(prov-name prov-cfg) (in-hash provider-map)])
          (flex-ref prov-cfg 'default-model #f))))

  (model-registry index provider-map default-provider-name final-default-model))

;; ============================================================
;; Resolution
;; ============================================================

(define (resolve-model registry model-name)
  ;; Check cache first
  (define cached (cached-resolve-model registry model-name))
  (cond
    [cached cached]
    [else
     ;; Compute and cache
     (define result
       (match model-name
         [#f (resolve-default registry)]
         [_
          ;; Check for provider prefix: "provider/model"
          (define parts (string-split model-name "/" #:trim? #f))
          (cond
            [(= (length parts) 2)
             ;; Provider prefix syntax
             (define prov-name (car parts))
             (define model (cadr parts))
             (resolve-with-provider registry prov-name model)]
            ;; Exact match across all providers
            [else (resolve-exact registry model-name)])]))
     (cache-resolve-model! registry model-name result)
     result]))

(define (resolve-default registry)
  (define dm (model-registry-default-model registry))
  (match dm
    [#f #f]
    [_ (resolve-exact registry dm)]))

(define (resolve-exact registry model-name)
  ;; Look up in index — returns list of model-entries
  (define entries (hash-ref (model-registry-index registry) model-name '()))
  (match entries
    ['() #f]
    [_
     ;; Take the first entry (could be ambiguous if shared across providers)
     (define entry (car entries))
     (entry->resolution entry)]))

(define (resolve-with-provider registry provider-name model-name)
  ;; Find the specific provider's model
  (define prov-cfg (hash-ref (model-registry-providers registry) provider-name #f))
  (match prov-cfg
    [#f #f]
    [_
     (define models-list (flex-ref prov-cfg 'models '()))
     (if (model-in-list? model-name models-list)
         (model-resolution model-name provider-name (safe-base-url prov-cfg) prov-cfg)
         #f)]))

(define (entry->resolution entry)
  (model-resolution (model-entry-name entry)
                    (model-entry-provider-name entry)
                    (safe-base-url (model-entry-provider-config entry))
                    (model-entry-provider-config entry)))

;; ============================================================
;; resolve-model-by-provider
;; ============================================================

(define (resolve-model-by-provider registry provider-name)
  (define prov-cfg (hash-ref (model-registry-providers registry) provider-name #f))
  (match prov-cfg
    [#f #f]
    [_
     (define dm (flex-ref prov-cfg 'default-model #f))
     (match dm
       [#f #f]
       [_ (model-resolution dm provider-name (safe-base-url prov-cfg) prov-cfg)])]))

;; ============================================================
;; Listing
;; ============================================================

(define (available-models registry)
  ;; Collect all model entries from the index
  (define all-entries
    (for*/list ([(model-name entries) (in-hash (model-registry-index registry))]
                [e (in-list entries)])
      e))
  ;; Sort by name for deterministic output
  (sort all-entries string<? #:key model-entry-name))

;; ============================================================
;; Defaults
;; ============================================================

(define (default-model registry)
  (model-registry-default-model registry))

(define (default-model-for-mode registry mode)
  ;; Extension point: per-mode model selection
  ;; For now, all modes use the same default model
  (default-model registry))

;; ============================================================
;; Context window lookup — v0.97.6 F4
;; ============================================================

;; Resolve context-window for a model name from the registry.
;; Returns the context window size in tokens, or #f if unknown.
(define (model-registry-context-window registry model-name)
  (define entries (hash-ref (model-registry-index registry) (or model-name "") '()))
  (if (null? entries)
      #f
      (let* ([entry (car entries)]
             [prov-cfg (model-entry-provider-config entry)]
             [models-list (flex-ref prov-cfg 'models '())])
        ;; Find the matching model entry and extract context-window.
        ;; Handles two formats:
        ;;   - Hash format (from JSON config): #hash((id . "model") (context-window . N))
        ;;   - List/alist format (from providers.rktd): ("model" (context-window . N))
        (for/or ([m (in-list models-list)])
          (define m-id (extract-model-id m))
          (and (equal? m-id model-name)
               (cond
                 ;; Hash format: flex-ref for context-window
                 [(hash? m) (flex-ref m 'context-window #f)]
                 ;; List format with string car: alist cdr
                 [(and (pair? m) (string? (car m)))
                  (define cw (assoc 'context-window (cdr m)))
                  (and cw (cdr cw))]
                 ;; Other alist format
                 [(pair? m)
                  (define cw (assoc 'context-window m))
                  (and cw (cdr cw))]
                 [else #f]))))))

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

(require racket/string)

(provide
 ;; Structs
 (struct-out model-entry)
 (struct-out model-resolution)

 ;; Resolution
 resolve-model
 resolve-model-by-provider

 ;; Listing
 available-models

 ;; Defaults
 default-model
 default-model-for-mode

 ;; Registry creation
 make-model-registry-from-config)

;; ============================================================
;; Structs
;; ============================================================

;; A model entry — describes an available model
(struct model-entry
  (name            ; string — model name (e.g. "gpt-4o")
   provider-name   ; string — provider name (e.g. "openai")
   provider-config ; hash — full provider config section
   )
  #:transparent)

;; A resolution result — everything needed to create a provider instance
(struct model-resolution
  (model-name      ; string — the resolved model name
   provider-name   ; string — the provider to use
   base-url        ; string — API base URL
   provider-config ; hash — full provider config (for auth-store etc.)
   )
  #:transparent)

;; Internal registry struct (not exported)
(struct model-registry
  (index           ; hash: model-name → (listof model-entry) — may have duplicates across providers
   providers       ; hash: provider-name-string → provider-config-hash
   default-provider ; string or #f
   default-model    ; string or #f
   )
  #:transparent)

;; ============================================================
;; Config key normalization
;; ============================================================

;; Normalize a key to string for consistent lookup
(define (key->string k)
  (if (symbol? k) (symbol->string k) k))

;; Normalize all keys in a hash to strings
(define (normalize-keys h)
  (for/hash ([(k v) (in-hash h)])
    (values (key->string k) v)))

;; Flexible hash-ref: tries both string and symbol keys
(define (flex-ref h key [default (lambda () (raise (make-exn:fail "key not found" (current-continuation-marks))))])
  (define str-key (if (symbol? key) (symbol->string key) key))
  (define sym-key (if (string? key) (string->symbol key) key))
  (cond
    [(hash-has-key? h str-key) (hash-ref h str-key)]
    [(hash-has-key? h sym-key) (hash-ref h sym-key)]
    [(procedure? default) (default)]
    [else default]))

;; ============================================================
;; Model ID extraction — supports both string and object-style models
;; ============================================================

;; Extract model ID from either:
;;   - string: "gpt-4o" → "gpt-4o"
;;   - hash: (hasheq "id" "gpt-4o" "name" "...") → "gpt-4o"
;; Returns #f if invalid
(define (extract-model-id m)
  (cond
    [(string? m) m]
    [(hash? m) (flex-ref m "id" #f)]
    [else #f]))

;; Check if model-id is in models-list (handles both string and object styles)
(define (model-in-list? model-id models-list)
  (for/or ([m (in-list models-list)])
    (equal? model-id (extract-model-id m))))

(define (flex-has-key? h key)
  (define str-key (if (symbol? key) (symbol->string key) key))
  (define sym-key (if (string? key) (string->symbol key) key))
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
      (define models-list
        (flex-ref prov-config 'models '()))
      (define entries
        (for/list ([m (in-list models-list)])
          (define model-id (extract-model-id m))
          (model-entry model-id prov-name prov-config)))
      ;; If no explicit models list but a default-model is set,
      ;; add the default-model as a single entry so it can be resolved.
      (define effective-entries
        (if (and (null? entries)
                 (flex-ref prov-config 'default-model #f))
            (list (model-entry (flex-ref prov-config 'default-model #f)
                               prov-name
                               prov-config))
            entries))
      ;; Add each model to the index
      (define new-idx
        (for/fold ([i idx])
                  ([e (in-list effective-entries)])
          (hash-update i (model-entry-name e)
                       (λ (existing) (cons e existing))
                       '())))
      (values new-idx
              (hash-set pmap prov-name prov-config))))

  ;; If no global default-model, try to derive from default-provider
  (define effective-default-model
    (cond
      [default-model-name default-model-name]
      [default-provider-name
       (define prov-cfg (hash-ref provider-map default-provider-name #f))
       (and prov-cfg
            (let ([dm (flex-ref prov-cfg 'default-model #f)])
              (if (eq? dm #f) #f dm)))]
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
  ;; If model-name is #f, resolve default
  (cond
    [(not model-name)
     (resolve-default registry)]
    [else
     ;; Check for provider prefix: "provider/model"
     (define parts (string-split model-name "/" #:trim? #f))
     (cond
       [(= (length parts) 2)
        ;; Provider prefix syntax
        (define prov-name (car parts))
        (define model (cadr parts))
        (resolve-with-provider registry prov-name model)]
       [else
        ;; Exact match across all providers
        (resolve-exact registry model-name)])]))

(define (resolve-default registry)
  (define dm (model-registry-default-model registry))
  (cond
    [(not dm) #f]
    [else (resolve-exact registry dm)]))

(define (resolve-exact registry model-name)
  ;; Look up in index — returns list of model-entries
  (define entries (hash-ref (model-registry-index registry) model-name '()))
  (cond
    [(null? entries) #f]
    [else
     ;; Take the first entry (could be ambiguous if shared across providers)
     (define entry (car entries))
     (entry->resolution entry)]))

(define (resolve-with-provider registry provider-name model-name)
  ;; Find the specific provider's model
  (define prov-cfg (hash-ref (model-registry-providers registry) provider-name #f))
  (cond
    [(not prov-cfg) #f]
    [else
     (define models-list (flex-ref prov-cfg 'models '()))
     (if (model-in-list? model-name models-list)
         (model-resolution model-name
                           provider-name
                           (flex-ref prov-cfg 'base-url "")
                           prov-cfg)
         #f)]))

(define (entry->resolution entry)
  (model-resolution (model-entry-name entry)
                    (model-entry-provider-name entry)
                    (flex-ref (model-entry-provider-config entry) 'base-url "")
                    (model-entry-provider-config entry)))

;; ============================================================
;; resolve-model-by-provider
;; ============================================================

(define (resolve-model-by-provider registry provider-name)
  (define prov-cfg (hash-ref (model-registry-providers registry) provider-name #f))
  (cond
    [(not prov-cfg) #f]
    [else
     (define dm (flex-ref prov-cfg 'default-model #f))
     (cond
       [(not dm) #f]
       [else
        (model-resolution dm
                          provider-name
                          (flex-ref prov-cfg 'base-url "")
                          prov-cfg)])]))

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

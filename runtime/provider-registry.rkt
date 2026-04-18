#lang racket/base

;; runtime/provider-registry.rkt — Centralized provider registry (#700-#703)
;;
;; Dynamic runtime registration of LLM providers and models.
;; Thread-safe via semaphore. Supports:
;;   - register-provider! / unregister-provider! / lookup-provider (#700)
;;   - register-model! with id, name, context-window, max-tokens, capabilities (#701)
;;   - find-model searches by ID or name across all providers (#702)
;;
;; Provider override merges config from registration with existing config.

(require racket/contract
         racket/match
         racket/list
         racket/string
         "../llm/provider.rkt"
         "../llm/model.rkt")

(provide
 ;; Registry
 make-provider-registry
 provider-registry?
 ;; #700: Provider management
 register-provider!
 unregister-provider!
 lookup-provider
 list-providers
 ;; #701: Model registration
 register-model!
 unregister-model!
 list-models-for-provider
 ;; #702: Model search
 find-model
 find-models
 ;; Model info struct
 (struct-out provider-info)
 (struct-out registered-model)
 ;; #1220: Metadata and query helpers
 provider-metadata
 provider-summary)

;; ============================================================
;; Structs
;; ============================================================

;; Info about a registered provider
(struct provider-info
  (name          ; string
   provider      ; provider? instance
   config        ; hash — merged config
   registered-at ; exact-positive-integer — timestamp
   )
  #:transparent)

;; A dynamically registered model
(struct registered-model
  (id              ; string — unique model id (e.g. "gpt-4o")
   name            ; string — display name
   provider-name   ; string — which provider owns this model
   context-window  ; positive-integer or #f
   max-tokens      ; positive-integer or #f
   capabilities    ; hash — model capabilities
   )
  #:transparent)

;; Internal registry struct
(struct provider-registry
  (providers-box    ; box of hash: provider-name → provider-info
   models-box       ; box of hash: "provider:model-id" → registered-model
   semaphore        ; semaphore for thread safety
   )
  #:transparent)

;; ============================================================
;; Registry creation
;; ============================================================

(define (make-provider-registry)
  (provider-registry (box (hash))
                     (box (hash))
                     (make-semaphore 1)))

;; ============================================================
;; Thread-safety helper
;; ============================================================

(define (with-lock registry thunk)
  (call-with-semaphore (provider-registry-semaphore registry) thunk))

;; ============================================================
;; #700: Provider management
;; ============================================================

;; Register a provider. Merges config with any existing entry.
;; #1220: Validates that provider-instance satisfies provider? predicate.
;; Raises exn:fail:contract if validation fails.
;; Returns 'registered for new, 'updated for re-registration.
(define (register-provider! registry name provider-instance
                             #:config [config (hasheq)])
  ;; #1220: Provider validation — must be a valid provider? instance
  (unless (provider? provider-instance)
    (raise (exn:fail:contract
            (format "register-provider!: expected provider? for provider-instance, got ~a"
                    provider-instance)
            (current-continuation-marks))))
  (define pinfo (provider-info name provider-instance config (current-seconds)))
  (with-lock registry
    (lambda ()
      (define current (unbox (provider-registry-providers-box registry)))
      ;; Merge config if provider already exists
      (define existing (hash-ref current name #f))
      (define merged-config
        (if existing
            (hash-merge (provider-info-config existing) config)
            config))
      (define merged-info (struct-copy provider-info pinfo
                                        [config merged-config]))
      (set-box! (provider-registry-providers-box registry)
                (hash-set current name merged-info))
      ;; #1220: Return indicator for duplicate detection
      (if existing 'updated 'registered))))

;; Unregister a provider and all its models.
(define (unregister-provider! registry name)
  (with-lock registry
    (lambda ()
      ;; Remove provider
      (define current (unbox (provider-registry-providers-box registry)))
      (set-box! (provider-registry-providers-box registry)
                (hash-remove current name))
      ;; Remove all models for this provider
      (define current-models (unbox (provider-registry-models-box registry)))
      (define filtered
        (for/hash ([(k v) (in-hash current-models)]
                   #:unless (string=? (registered-model-provider-name v) name))
          (values k v)))
      (set-box! (provider-registry-models-box registry) filtered))))

;; Look up a provider by name. Returns provider-info or #f.
(define (lookup-provider registry name)
  (define providers (unbox (provider-registry-providers-box registry)))
  (hash-ref providers name #f))

;; List all registered providers.
(define (list-providers registry)
  (hash-values (unbox (provider-registry-providers-box registry))))

;; ============================================================
;; #701: Model registration
;; ============================================================

;; Register a model on a provider.
(define (register-model! registry
                          #:id id
                          #:name name
                          #:provider-name provider-name
                          #:context-window [context-window #f]
                          #:max-tokens [max-tokens #f]
                          #:capabilities [capabilities (hasheq)])
  (define model-key (format "~a:~a" provider-name id))
  (define model (registered-model id name provider-name
                                   context-window max-tokens capabilities))
  (with-lock registry
    (lambda ()
      (define current (unbox (provider-registry-models-box registry)))
      (set-box! (provider-registry-models-box registry)
                (hash-set current model-key model)))))

;; Unregister a specific model.
(define (unregister-model! registry provider-name model-id)
  (define model-key (format "~a:~a" provider-name model-id))
  (with-lock registry
    (lambda ()
      (define current (unbox (provider-registry-models-box registry)))
      (set-box! (provider-registry-models-box registry)
                (hash-remove current model-key)))))

;; List all models for a specific provider.
(define (list-models-for-provider registry provider-name)
  (define all-models (unbox (provider-registry-models-box registry)))
  (for/list ([(k v) (in-hash all-models)]
             #:when (string=? (registered-model-provider-name v) provider-name))
    v))

;; ============================================================
;; #702: Model search
;; ============================================================

;; Find a single model by ID or name. Returns first match or #f.
(define (find-model registry query)
  (define all-models (hash-values (unbox (provider-registry-models-box registry))))
  (define q (string-downcase query))
  (or
   ;; Exact ID match
   (for/first ([m (in-list all-models)]
               #:when (string=? (string-downcase (registered-model-id m)) q))
     m)
   ;; Exact name match
   (for/first ([m (in-list all-models)]
               #:when (string=? (string-downcase (registered-model-name m)) q))
     m)
   ;; Prefix ID match
   (for/first ([m (in-list all-models)]
               #:when (string-prefix? (string-downcase (registered-model-id m)) q))
     m)
   ;; Substring name match
   (for/first ([m (in-list all-models)]
               #:when (string-contains? (string-downcase (registered-model-name m)) q))
     m)
   ;; Not found
   #f))

;; Find all models matching a query. Returns list of registered-model.
(define (find-models registry query)
  (define all-models (hash-values (unbox (provider-registry-models-box registry)))
    )
  (define q (string-downcase query))
  (filter (lambda (m)
            (or (string-contains? (string-downcase (registered-model-id m)) q)
                (string-contains? (string-downcase (registered-model-name m)) q)))
          all-models))

;; ============================================================
;; #1220: Provider metadata and query helpers
;; ============================================================

;; Extract metadata from a provider-info for SDK/RPC consumption.
;; Returns a hash with name, capabilities, registered-at, model-count.
(define (provider-metadata pinfo)
  (hasheq 'name (provider-info-name pinfo)
          'config (provider-info-config pinfo)
          'registered-at (provider-info-registered-at pinfo)
          'provider-valid? (provider? (provider-info-provider pinfo))))

;; Get full provider summary: metadata + model list for RPC/SDK.
;; Returns a hash suitable for JSON serialization.
(define (provider-summary registry name)
  (define pinfo (lookup-provider registry name))
  (if pinfo
      (hasheq 'info (provider-metadata pinfo)
              'models (for/list ([m (list-models-for-provider registry name)])
                        (hasheq 'id (registered-model-id m)
                                'name (registered-model-name m)
                                'context-window (registered-model-context-window m)
                                'max-tokens (registered-model-max-tokens m)
                                'capabilities (registered-model-capabilities m))))
      #f))

;; ============================================================
;; Helpers
;; ============================================================

;; Merge two hashes, right-biased.
(define (hash-merge h1 h2)
  (for/fold ([result h1])
            ([(k v) (in-hash h2)])
    (hash-set result k v)))

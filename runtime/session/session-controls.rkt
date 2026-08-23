#lang racket/base

;; runtime/session-controls.rkt — session control functions
;; STABILITY: internal
;;
;; Extracted from agent-session.rkt (ARCH-05c).
;; Model switching, thinking level, and shutdown controls.
;; Uses session-types.rkt for struct accessors to avoid circular deps.

(require racket/contract
         racket/list
         (only-in "../provider/model-registry.rkt"
                  available-models
                  model-entry-name
                  model-registry?
                  resolve-model
                  model-resolution?
                  model-resolution-provider-name
                  model-resolution-base-url
                  model-resolution-provider-config
                  model-resolution-model-name)
         (only-in "../provider/provider-factory.rkt" create-provider-for-name)
         (only-in "../auth/auth-store.rkt" lookup-credential credential-api-key)
         (only-in "session-config.rkt" session-config? session-config->hash hash->session-config)
         "session-types.rkt")
(require "session-mutation.rkt")

(provide (contract-out [set-model! (->* (agent-session? string?) ((or/c model-registry? #f)) void?)]
                       [switch-model! (-> agent-session? string? model-registry? void?)]
                       [cycle-model! (-> agent-session? model-registry? (or/c string? #f))]
                       ;; Thinking level control (#1153)
                       [thinking-levels (listof symbol?)]
                       [thinking-level? (-> any/c boolean?)]
                       [thinking-level->budget (-> symbol? exact-nonnegative-integer?)]
                       [set-thinking-level! (-> agent-session? symbol? void?)]
                       ;; Graceful shutdown (#1158)
                       [request-shutdown! (-> agent-session? void?)]
                       [force-shutdown! (-> agent-session? void?)]
                       [shutdown-requested? (-> agent-session? boolean?)]
                       [force-shutdown-requested? (-> agent-session? boolean?)]
                       [reset-shutdown-flags! (-> agent-session? void?)]))

;; ============================================================
;; FEAT-65: Runtime model control
;; ============================================================

;; set-model! : agent-session? string? [model-registry?] -> void?
;; Sets the active model name for the session.
;; When a model-registry is provided, also resolves and switches the
;; underlying provider so requests go to the correct API endpoint.
;; v0.99.96: Previously only updated the model-name string, leaving the
;; original provider in place. This meant model switches sent the new
;; model name to the old (possibly rate-limited) provider's API.
(define (set-model! sess model-name [registry #f])
  (unless (string? model-name)
    (raise-argument-error 'set-model! "string?" model-name))
  (cond
    [registry (switch-model! sess model-name registry)]
    [else
     ;; Legacy path: only update model name (backward-compatible)
     (guarded-set-model-name! sess model-name)
     (define config (agent-session-config sess))
     (when config
       (define config-hash
         (if (session-config? config)
             (session-config->hash config)
             config))
       (define updated
         ;; BUG-0018 W2: mark the explicit runtime override so path-derived
         ;; model resolution cannot clobber this switch on later prompts.
         (hash-set (hash-set config-hash 'model-name model-name) 'model-override #t))
       (guarded-set-config! sess
                            (if (session-config? config)
                                (hash->session-config updated)
                                updated)))]))

;; switch-model! : agent-session? string? model-registry? -> void?
;; Resolves the model name via the registry, creates a new provider
;; instance for the resolved provider, and atomically updates both
;; the session's provider and model-name.
;; v0.99.96: This fixes the critical bug where /model <name> only changed
;; the model-name string but NOT the provider, so the request still went
;; to the old provider's (rate-limited) API endpoint.
(define (switch-model! sess model-name registry)
  (define resolution (resolve-model registry model-name))
  (unless resolution
    (raise-argument-error 'switch-model! "known model name" model-name))
  (define prov-name (model-resolution-provider-name resolution))
  (define base-url (model-resolution-base-url resolution))
  (define prov-cfg (model-resolution-provider-config resolution))
  (define resolved-model (model-resolution-model-name resolution))
  ;; Look up credentials for the new provider
  (define cred (lookup-credential prov-name prov-cfg))
  (define api-key (and cred (credential-api-key cred)))
  ;; Create the new provider instance
  (define max-tokens (and (hash? prov-cfg) (hash-ref prov-cfg 'max-tokens #f)))
  (define new-provider
    (create-provider-for-name prov-name base-url (or api-key "") resolved-model max-tokens))
  ;; Atomically update provider + model name + config
  (guarded-set-provider! sess new-provider)
  (guarded-set-model-name! sess resolved-model)
  (define config (agent-session-config sess))
  (when config
    (define config-hash
      (if (session-config? config)
          (session-config->hash config)
          config))
    (define updated
      ;; BUG-0018 W2: also record the explicit override marker so the request
      ;; path (turn-orchestrator / context build) honors this switch on every
      ;; subsequent prompt.
      (hash-set (hash-set config-hash 'model-name resolved-model) 'model-override #t))
    (guarded-set-config! sess
                         (if (session-config? config)
                             (hash->session-config updated)
                             updated))))

;; cycle-model! : agent-session? model-registry? -> (or/c string? #f)
;; Cycles to the next model in the registry's available models list.
(define (cycle-model! sess registry)
  (define models (available-models registry))
  (if (null? models)
      #f
      (let* ([current (or (agent-session-model-name sess) "")]
             [names (map model-entry-name models)]
             [unique-names (remove-duplicates names)]
             [current-idx (for/first ([n (in-list unique-names)]
                                      [i (in-naturals)]
                                      #:when (equal? n current))
                            i)]
             [next-idx (if current-idx
                           (modulo (add1 current-idx) (length unique-names))
                           0)]
             [next-model (list-ref unique-names next-idx)])
        (guarded-set-model-name! sess next-model)
        next-model)))

;; ============================================================
;; Thinking level control (#1153)
;; ============================================================

(define thinking-levels '(off minimal low medium high xhigh))

(define (thinking-level? v)
  (and (symbol? v) (member v thinking-levels) #t))

(define (thinking-level->budget level)
  (case level
    [(off) 0]
    [(minimal) 1024]
    [(low) 4096]
    [(medium) 8192]
    [(high) 16384]
    [(xhigh) 32768]
    [else 0]))

(define (set-thinking-level! sess level)
  (unless (thinking-level? level)
    (raise-argument-error 'set-thinking-level! "thinking level" level))
  (guarded-set-thinking-level! sess level))

;; ============================================================
;; Graceful shutdown (#1158)
;; ============================================================

(define (request-shutdown! sess)
  (guarded-set-shutdown-requested! sess #t))

(define (force-shutdown! sess)
  (guarded-set-force-shutdown! sess #t))

(define (shutdown-requested? sess)
  (agent-session-shutdown-requested? sess))

(define (force-shutdown-requested? sess)
  (agent-session-force-shutdown? sess))

(define (reset-shutdown-flags! sess)
  (guarded-set-shutdown-requested! sess #f)
  (guarded-set-force-shutdown! sess #f))

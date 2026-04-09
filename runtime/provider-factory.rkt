#lang racket

;; runtime/provider-factory.rkt — Build LLM providers from config + settings
;;
;; Extracted from main.rkt. Contains build-provider, local-provider?,
;; and build-mock-provider.

(require "settings.rkt"
         "auth-store.rkt"
         "model-registry.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../llm/openai-compatible.rkt")

(provide build-provider
         build-mock-provider
         local-provider?)

;; Helper: Check if a URL points to a local/self-hosted provider.
;; Local providers don't require API keys.
(define (local-provider? base-url)
  (and (string? base-url)
       (> (string-length base-url) 0)
       (or (string-contains? base-url "localhost")
           (string-contains? base-url "127.0.0.1")
           (string-contains? base-url "192.168.")
           (string-contains? base-url "10.")
           (string-contains? base-url "172."))))

;; Build the provider from CLI config + settings.
;; Resolution:
;;   1. Load settings (global + project)
;;   2. If settings are empty -> mock provider
;;   3. Resolve model (from --model flag or default from settings)
;;   4. Lookup credential for the resolved provider
;;   5. If no credential -> warn and use mock provider
;;   6. Create make-openai-compatible-provider with config hash
(define (build-provider config settings)
  (define project-dir
    (or (hash-ref config 'project-dir #f)
        (current-directory)))
  (define merged (q-settings-merged settings))
  (cond
    [(hash-empty? merged)
     ;; No config found -> mock
     (fprintf (current-error-port) "Warning: No config found (~a.q/config.json or .q/config.json), using mock provider~n" (find-system-path 'home-dir))
     (build-mock-provider)]
    [else
     (define model-name (hash-ref config 'model #f))
     (define registry (make-model-registry-from-config merged))
     (define resolution (resolve-model registry model-name))
     (cond
       [(not resolution)
        ;; Model not found -> mock
        (fprintf (current-error-port) "Warning: Model ~a not found in registry, using mock provider~n"
                 (or model-name "(default)"))
        (build-mock-provider)]
       [else
        (define prov-name (model-resolution-provider-name resolution))
        (define prov-cfg (model-resolution-provider-config resolution))
        (define base-url (model-resolution-base-url resolution))
        (define cred (lookup-credential prov-name prov-cfg))
        (cond
          [(not cred)
           ;; No credentials - check if local provider (no auth needed)
           (if (local-provider? base-url)
               (begin
                 (fprintf (current-error-port) "Info: Using local provider ~a without authentication~n" prov-name)
                 (make-openai-compatible-provider
                  (hash 'base-url base-url
                        'api-key ""  ; Empty API key for local providers
                        'model (model-resolution-model-name resolution))))
               (begin
                 ;; No credentials and not local -> mock
                 (fprintf (current-error-port) "Warning: No API key for provider ~a, using mock provider~n" prov-name)
                 (build-mock-provider)))]
          [else
           (make-openai-compatible-provider
            (hash 'base-url base-url
                  'api-key (credential-api-key cred)
                  'model (model-resolution-model-name resolution)))])])]))

;; Helper: create a mock provider
(define (build-mock-provider)
  (define mock-response
    (make-model-response
     (list (hasheq 'type "text" 'text "Mock response from q."))
     (hasheq 'prompt-tokens 0 'completion-tokens 0 'total-tokens 0)
     "mock-model"
     'stop))
  (make-mock-provider mock-response #:name "mock"))

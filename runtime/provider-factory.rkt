#lang racket/base

;; runtime/provider-factory.rkt — Build LLM providers from config + settings
;;
;; Extracted from main.rkt. Contains build-provider, local-provider?,
;; and build-mock-provider.

(require "settings.rkt"
         "auth-store.rkt"
         "model-registry.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../llm/openai-compatible.rkt"
         "../llm/gemini.rkt"
         "../llm/anthropic.rkt"
         racket/string)

(provide build-provider
         build-mock-provider
         local-provider?
         create-provider-for-name) ;; for testing

;; Build the appropriate provider based on provider name.
(define (create-provider-for-name prov-name base-url api-key model-name)
  (define config (hash 'base-url base-url
                        'api-key api-key
                        'model model-name))
  (cond
    [(equal? prov-name "gemini")
     (make-gemini-provider config)]
    [(equal? prov-name "anthropic")
     (make-anthropic-provider config)]
    [else
     (make-openai-compatible-provider config)]))

;; Helper: Check if a URL points to a local/self-hosted provider.
;; Local providers don't require API keys.
(define (local-provider? base-url)
  (and (string? base-url)
       (> (string-length base-url) 0)
       (or (string-contains? base-url "localhost")
           (string-contains? base-url "127.0.0.1")
           (string-contains? base-url "192.168.")
           (string-contains? base-url "10.")
           (rfc1918-172? base-url))))

;; RFC 1918 172.16.0.0/12 check — only 172.16.x.x through 172.31.x.x
(define (rfc1918-172? url-str)
  (and (string-contains? url-str "172.")
       (regexp-match? #rx"172\\.([0-9]+)\\." url-str)
       (let ([m (regexp-match #rx"172\\.([0-9]+)\\." url-str)])
         (and m
              (let ([octet (string->number (cadr m))])
                (and octet (<= 16 octet 31)))))))

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
     (fprintf (current-error-port)
             "Warning: No config found (~a.q/config.json or .q/config.json), using mock provider~n"
             (find-system-path 'home-dir))
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
                 (create-provider-for-name prov-name base-url "" (model-resolution-model-name resolution)))
               (begin
                 ;; No credentials and not local -> mock
                 (fprintf (current-error-port) "Warning: No API key for provider ~a, using mock provider~n" prov-name)
                 (build-mock-provider)))]
          [else
           (create-provider-for-name prov-name base-url (credential-api-key cred) (model-resolution-model-name resolution))])])]))

;; Helper: create a mock provider
(define (build-mock-provider)
  (define mock-response
    (make-model-response
     (list (hasheq 'type "text" 'text "Mock response from q."))
     (hasheq 'prompt-tokens 0 'completion-tokens 0 'total-tokens 0)
     "mock-model"
     'stop))
  (make-mock-provider mock-response #:name "mock"))

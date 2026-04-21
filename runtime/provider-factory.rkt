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
         "../llm/azure-openai.rkt"
         racket/string)

(provide build-provider
         build-mock-provider
         local-provider?
         create-provider-for-name ;; for testing
         provider-is-mock?) ;; v0.14.1: for tui-init without importing llm/

;; Build the appropriate provider based on provider name.
(define (create-provider-for-name prov-name base-url api-key model-name [max-tokens #f])
  ;; Local providers don't need real API keys — use a sentinel to
  ;; pass validation when no credential is available.
  (define effective-key
    (if (and (or (not api-key) (string=? (string-trim api-key) "")) (local-provider? base-url))
        "local-no-auth"
        api-key))
  (define config-base (hasheq 'api-key effective-key 'model model-name))
  (define config-with-url
    (if (and base-url (not (string=? (string-trim base-url) "")))
        (hash-set config-base 'base-url base-url)
        config-base))
  ;; v0.14.5: Pass max-tokens from provider config so it reaches the API
  (define config
    (if max-tokens
        (hash-set config-with-url 'max-tokens max-tokens)
        config-with-url))
  (cond
    [(equal? prov-name "gemini") (make-gemini-provider config)]
    [(equal? prov-name "anthropic") (make-anthropic-provider config)]
    [(equal? prov-name "azure") (make-azure-openai-provider config)]
    [else (make-openai-compatible-provider config)]))

;; Helper: Check if a provider is a mock provider (name = "mock").
;; v0.14.1: Moves mock detection out of tui/tui-init.rkt to avoid tui→llm import.
(define (provider-is-mock? prov)
  (and (provider? prov) (equal? (provider-name prov) "mock")))

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
         (and m (let ([octet (string->number (cadr m))]) (and octet (<= 16 octet 31)))))))

;; Build the provider from CLI config + settings.
;; Resolution:
;;   1. Load settings (global + project)
;;   2. If settings are empty -> mock provider
;;   3. Resolve model (from --model flag or default from settings)
;;   4. Lookup credential for the resolved provider
;;   5. If no credential -> warn and use mock provider
;;   6. Create make-openai-compatible-provider with config hash
(define (build-provider config settings)
  (define project-dir (or (hash-ref config 'project-dir #f) (current-directory)))
  ;; v0.14.4 Wave 0: Check for config parse errors before falling back to mock
  (define global-cfg-path (build-path (find-system-path 'home-dir) ".q" "config.json"))
  (define project-cfg-path (build-path project-dir ".q" "config.json"))
  (define global-parse-err (config-parse-error global-cfg-path))
  (define project-parse-err (config-parse-error project-cfg-path))
  (when global-parse-err
    (fprintf (current-error-port)
             "ERROR: Config file ~a has invalid JSON: ~a\nFix the syntax error and restart q.\n"
             global-cfg-path
             global-parse-err))
  (when project-parse-err
    (fprintf (current-error-port)
             "ERROR: Config file ~a has invalid JSON: ~a\nFix the syntax error and restart q.\n"
             project-cfg-path
             project-parse-err))
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
        (fprintf (current-error-port)
                 "Warning: Model ~a not found in registry, using mock provider~n"
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
                 (fprintf (current-error-port)
                          "Info: Using local provider ~a without authentication~n"
                          prov-name)
                 (create-provider-for-name prov-name
                                           base-url
                                           ""
                                           (model-resolution-model-name resolution)
                                           (hash-ref prov-cfg 'max-tokens #f)))
               (begin
                 ;; No credentials and not local -> mock
                 (fprintf (current-error-port)
                          "Warning: No API key for provider ~a, using mock provider~n"
                          prov-name)
                 (build-mock-provider)))]
          [else
           (create-provider-for-name prov-name
                                     base-url
                                     (credential-api-key cred)
                                     (model-resolution-model-name resolution)
                                     (hash-ref prov-cfg 'max-tokens #f))])])]))

;; Helper: create a mock provider
(define (build-mock-provider)
  (define mock-response
    (make-model-response (list (hasheq 'type "text" 'text "Mock response from q."))
                         (hasheq 'prompt-tokens 0 'completion-tokens 0 'total-tokens 0)
                         "mock-model"
                         'stop))
  (make-mock-provider mock-response #:name "mock"))

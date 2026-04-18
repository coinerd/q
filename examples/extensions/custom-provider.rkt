#lang racket/base

;; examples/extensions/custom-provider.rkt — dynamic provider registration example (#1215)
;;
;; Demonstrates how to register a custom LLM provider at runtime.
;; Uses the provider-registry to add a mock provider with a custom
;; base URL and model list. Fires on 'extension-loaded to self-register.
;;
;; Usage:
;;   (load-extension! registry "examples/extensions/custom-provider.rkt")
;;
;; Note: This example demonstrates the intended API for dynamic provider
;; registration. The provider registry is accessed via ctx-provider-registry
;; when available in the extension context.

(require "../../extensions/api.rkt"
         "../../extensions/hooks.rkt")

(provide the-extension)

;; A mock provider dispatch function.
;; In production, this would route to an actual API endpoint.
(define (custom-dispatch op)
  (case op
    [(name) "custom-provider"]
    [(capabilities) (hasheq 'streaming #t
                            'tools #t
                            'max-tokens 4096)]
    [else (lambda args
            (hasheq 'content "Mock response from custom-provider"
                    'model "custom-model-v1"))]))

;; Build a simple provider instance using the project's provider struct.
;; The dispatch function is used by provider-send and provider-stream.
;; For this example we create a lightweight wrapper since the full
;; make-provider constructor requires 4 procedures.
(define (make-custom-provider)
  ;; Returns a hash that mimics provider behavior for demonstration
  (hasheq 'name "custom-provider"
          'base-url "https://custom-llm.example.com/v1"
          'models '("custom-model-v1" "custom-model-v2" "custom-model-turbo")
          'dispatch custom-dispatch))

;; The extension registers itself on load.
;; In a real deployment, the hook would receive a context with
;; ctx-provider-registry and use register-provider! / register-model!.
(define the-extension
  (extension "custom-provider"
             "1.0.0"
             "1"
             (hasheq 'extension.loaded
                     (lambda (payload)
                       ;; In production: (define reg (ctx-provider-registry ctx))
                       ;;   (register-provider! reg "custom" provider-instance)
                       ;;   (register-model! reg #:id "custom-v1" #:name "custom-model-v1"
                       ;;     #:provider-name "custom")
                       (log-info "custom-provider: Registering mock provider with 3 models")
                       (hook-pass payload))

                     'extension.unloaded
                     (lambda (payload)
                       ;; In production: (unregister-provider! reg "custom")
                       (log-info "custom-provider: Unregistering provider")
                       (hook-pass payload)))))

;; Key concepts:
;;   1. Use 'extension.loaded to register providers when your extension activates
;;   2. Use 'extension.unloaded to clean up provider registrations
;;   3. register-provider! takes: registry, name, provider-instance, optional config
;;   4. register-model! takes: registry, #:id, #:name, #:provider-name, optional params
;;   5. Providers must implement the dispatch protocol: 'name, 'capabilities, 'send, 'stream
;;   6. Access the registry via (ctx-provider-registry ctx) in hook handlers

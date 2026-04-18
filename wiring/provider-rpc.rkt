#lang racket/base

;; wiring/provider-rpc.rkt — Provider query RPC methods (#1221)
;;
;; Adds RPC methods for querying dynamically registered providers:
;;   providers/list   — list all registered providers with metadata
;;   providers/models — list models, optionally filtered by provider
;;   providers/find   — find a model by ID or name query
;;
;; Each handler receives params (hash) and returns a result (hash).
;;
;; Error convention: handlers return (hasheq 'status "error" 'message "...")
;; on failure rather than raising exceptions. This keeps RPC callers in
;; control and avoids leaking internal exceptions across the wire.

(require racket/list
         (only-in "../runtime/provider-registry.rkt"
                  list-providers
                  list-models-for-provider
                  find-model
                  find-models
                  provider-info?
                  provider-info-name
                  provider-info-config
                  provider-info-registered-at
                  registered-model?
                  registered-model-id
                  registered-model-name
                  registered-model-context-window
                  registered-model-max-tokens
                  registered-model-capabilities
                  registered-model-provider-name))

(provide make-provider-rpc-handlers)

;; Create provider RPC handlers. Takes a deps hash with:
;;   'provider-registry — the provider-registry to query (required)
(define (make-provider-rpc-handlers deps)
  (define registry (hash-ref deps 'provider-registry #f))

  (make-hash
   (list
    ;; ---- List Providers ----
    (cons 'providers/list
          (lambda (params)
            (if registry
                (hasheq 'providers
                        (for/list ([p (list-providers registry)])
                          (hasheq 'name (provider-info-name p)
                                  'config (provider-info-config p)
                                  'registered-at (provider-info-registered-at p))))
                (hasheq 'status "error"
                        'message "no provider registry"))))

    ;; ---- List Models ----
    (cons 'providers/models
          (lambda (params)
            (if registry
                (let* ([provider-name (hash-ref params 'provider #f)]
                       [models (if provider-name
                                   (list-models-for-provider registry provider-name)
                                   (apply append
                                          (map (lambda (p)
                                                 (list-models-for-provider
                                                  registry
                                                  (provider-info-name p)))
                                               (list-providers registry))))])
                  (hasheq 'models
                          (for/list ([m models])
                            (hasheq 'id (registered-model-id m)
                                    'name (registered-model-name m)
                                    'provider (registered-model-provider-name m)
                                    'context-window (registered-model-context-window m)
                                    'max-tokens (registered-model-max-tokens m)
                                    'capabilities (registered-model-capabilities m)))))
                (hasheq 'status "error"
                        'message "no provider registry"))))

    ;; ---- Find Model ----
    (cons 'providers/find
          (lambda (params)
            (define query (hash-ref params 'query #f))
            (cond
              [(not query)
               (hasheq 'status "error"
                       'message "missing 'query' parameter")]
              [(not registry)
               (hasheq 'status "error"
                       'message "no provider registry")]
              [else
               (define found (find-model registry query))
               (if found
                   (hasheq 'found #t
                           'model (hasheq 'id (registered-model-id found)
                                          'name (registered-model-name found)
                                          'provider (registered-model-provider-name found)
                                          'context-window (registered-model-context-window found)
                                          'max-tokens (registered-model-max-tokens found)))
                   (hasheq 'found #f))]))))))

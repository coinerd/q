#lang racket/base

;; Provider-facing worker schemas: capture authority before callbacks, then
;; project the completed schema list without changing the shared registry.
(require "../util/worker-tool-policy.rkt"
         (only-in "../util/capability.rkt" canonical-capabilities-snapshot capability-authorized?)
         (only-in "tool-coordinator.rkt" capabilities-for-tool-execution)
         (only-in "layer-adapters.rkt" merge-tool-lists lookup-tool tool-required-capability))

(provide make-provider-tool-projector)

;; Capture execution authority immediately before the provider hook.
(define (make-provider-tool-projector reg config tool-list-proc)
  (define policy (current-worker-tool-policy))
  (define capabilities
    (and policy (canonical-capabilities-snapshot (capabilities-for-tool-execution config))))
  (lambda (base-tools ext-tools)
    (assemble-provider-tools base-tools ext-tools reg policy capabilities tool-list-proc)))

;; Registered metadata, never provider schemas, is capability authority.
;; Unknown/malformed projections and registry lookup failures fail closed.
(define (filter-worker-capabilities schemas reg capabilities)
  (for/list ([schema (in-list (or schemas '()))]
             #:when
             (with-handlers ([(lambda (_) #t) (lambda (_) #f)])
               (define name
                 (hash-ref (if (equal? (hash-ref schema 'type #f) "function")
                               (hash-ref schema 'function)
                               schema)
                           'name))
               (define tool (and (string? name) reg (lookup-tool reg name)))
               (and tool (capability-authorized? (tool-required-capability tool) capabilities))))
    schema))

;; Preserve ordinary and injected assembly semantics, but apply both worker
;; policy and capability projection AFTER every built-in/extension/custom merge.
(define (assemble-provider-tools base-tools ext-tools reg policy capabilities tool-list-proc)
  (define projected-tools
    (filter-worker-tools
     (cond
       [tool-list-proc (tool-list-proc base-tools ext-tools)]
       [(and base-tools (pair? ext-tools)) (merge-tool-lists base-tools ext-tools)]
       [else base-tools])
     policy))
  (if policy
      (filter-worker-capabilities projected-tools reg capabilities)
      projected-tools))

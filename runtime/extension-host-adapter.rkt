#lang racket/base
;; STABILITY: internal

;; runtime/extension-host-adapter.rkt — Runtime adapter for the neutral
;; extension host service protocol (v0.99.88 W1, MA-03)
;;
;; Encapsulates the concrete provider registry behind the neutral
;; provider-host-service capability (util/extension/host-services.rkt).
;; Extensions receive the capability as a value; they never import
;; runtime/provider/provider-registry.rkt.
;;
;; Boundary note: this module legitimately imports the concrete registry
;; because it is part of the Runtime layer — the import lives HERE, not in
;; extensions/. Behavior is identical to calling the registry directly
;; (pinned by dual-run tests in tests/test-extension-host-service-protocol.rkt).

(require racket/contract
         "provider/provider-registry.rkt"
         "../util/extension/host-services.rkt")

(provide (contract-out [make-provider-host-service (-> provider-registry? provider-host-service?)]))

;; Wrap a concrete provider registry as a neutral host capability.
;; The returned service closes over REGISTRY; its lifetime is the
;; registry's lifetime (session-scoped).
(define (make-provider-host-service registry)
  ;; register!
  (provider-host-service (lambda (name provider-instance #:config [config (hasheq)])
                           (register-provider! registry name provider-instance #:config config))
                         ;; unregister!
                         (lambda (name) (unregister-provider! registry name))
                         ;; list
                         (lambda () (list-providers registry))
                         ;; lookup
                         (lambda (name) (lookup-provider registry name))))

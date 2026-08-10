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
         "package.rkt"
         "layer-adapters.rkt"
         "../util/extension/host-services.rkt")

(provide (contract-out [make-provider-host-service (-> provider-registry? provider-host-service?)]
                       [make-package-host-service (-> package-host-service?)]))

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

;; ============================================================
;; Package lifecycle adapter (v0.99.88 W3, MA-04)
;; ============================================================

;; Convert a concrete qpm-package to its pure neutral summary. The concrete
;; struct never crosses into extensions; only name/version do.
(define (qpm-package->summary pkg)
  (define m (qpm-package-manifest pkg))
  (package-summary (qpm-manifest-name m) (qpm-manifest-version m)))

;; Wrap the concrete runtime/package.rkt module as a neutral
;; package-host-service capability. No install/filesystem logic lives in
;; util — the adapter merely delegates to runtime/package.rkt (the single
;; Runtime touchpoint, per MA-04). current-packages-dir is read at call time,
;; so callers may parameterize it.
(define (make-package-host-service)
  (package-host-service
   ;; list
   (lambda () (map qpm-package->summary (list-packages)))
   ;; installed?
   (lambda (name) (package-installed? name))
   ;; install
   (lambda (source-dir)
     (define result (install-package-from-dir source-dir))
     (if (qpm-package? result) (qpm-package->summary result) result))
   ;; remove
   (lambda (name) (remove-package name))))

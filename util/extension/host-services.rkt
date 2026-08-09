#lang racket/base
;; STABILITY: public

;; util/extension/host-services.rkt — Neutral extension host service protocol
;; (v0.99.88 W1, MA-03)
;;
;; Capability-oriented protocol between extensions and the Runtime host.
;; Extensions receive host capabilities as VALUES injected at ctx
;; construction; they never import concrete Runtime service modules.
;;
;; This module is intentionally dependency-free: it imports only
;; racket/base and racket/contract. It must never import runtime/, llm/,
;; tools/, tui/, or extensions/ modules (pinned by
;; tests/test-extension-host-service-protocol.rkt and test-arch-fitness.rkt).
;;
;; Risk controls (roadmap v0.99.88 W1):
;;   - NO generic service-locator map (no (hasheq 'register f ...)).
;;   - NO untyped callback bundle: every operation field carries a contract
;;     via contract-out, so domain violations blame the extension caller and
;;     range violations blame the Runtime adapter.
;;   - Every capability has a NAME, CONTRACT, LIFETIME, and OWNER
;;     (see provider-registry-capabilities below).
;;
;; Value opacity: provider descriptor values returned by lookup/list are
;; opaque to extensions (any/c). Extensions must not construct, destructure,
;; or mutate them; they may only pass them back across the boundary or
;; compare them for identity. The concrete struct remains Runtime-owned.

(require racket/contract)

;; Protocol struct (contracted fields)
(provide (contract-out (struct provider-host-service
                               ((register-provider!
                                 ;; Register a provider instance under NAME.
                                 ;; Returns 'registered (new) or 'updated (re-registration).
                                 ;; The host validates the provider instance and raises
                                 ;; exn:fail:contract on an invalid instance.
                                 (->* (string? any/c) (#:config hash?) (or/c 'registered 'updated)))
                                ;; Remove provider NAME. No-op when NAME is unknown.
                                (unregister-provider! (-> string? void?))
                                ;; All registered provider descriptors, in registry order.
                                (list-providers (-> list?))
                                ;; Provider descriptor for NAME, or #f when absent.
                                (lookup-provider (-> string? (or/c any/c #f))))))
         ;; Capability metadata
         (contract-out (struct host-capability-descriptor
                               ((name symbol?)
                                (lifetime symbol?) ; 'session — bound to one session/registry instance
                                (owner string?) ; owning layer/team identifier
                                (summary string?))))
         provider-registry-capabilities)

;; ============================================================
;; Protocol struct — contracted operation fields
;; ============================================================

;; The neutral provider-registry host capability. Constructed by the
;; Runtime adapter (runtime/extension-host-adapter.rkt) which closes over
;; the concrete provider registry; consumed through the ctx-* facade
;; (extensions/context.rkt) or directly by extension code.
(struct provider-host-service (register-provider! unregister-provider! list-providers lookup-provider)
  #:transparent)

;; ============================================================
;; Capability metadata — name, contract, lifetime, owner
;; ============================================================

(struct host-capability-descriptor (name lifetime owner summary) #:transparent)

;; Normative capability table for the provider-registry host service.
;; Contracts are stated in the struct field contracts above; this table is
;; the human/test-auditable metadata mirror.
(define provider-registry-capabilities
  (list (host-capability-descriptor 'provider-registry.register!
                                    'session
                                    "runtime"
                                    "register provider instance; 'registered or 'updated")
        (host-capability-descriptor 'provider-registry.unregister!
                                    'session
                                    "runtime"
                                    "remove provider and its models; no-op when unknown")
        (host-capability-descriptor 'provider-registry.list
                                    'session
                                    "runtime"
                                    "list registered provider descriptors (opaque)")
        (host-capability-descriptor 'provider-registry.lookup
                                    'session
                                    "runtime"
                                    "lookup provider descriptor by name, or #f")))

#lang racket/base

;; agent/registry.rkt — Agent role registry with version management
;; STABILITY: evolving
;;
;; MAS Schritt 5: Modular Hot-Swapping & Dynamic Evolution
;;
;; Maps agent role names → descriptors → factory procedures.
;; The supervisor resolves roles via the registry instead of direct require.
;;
;; This is the SEAM for future hot-swapping. Currently all factories
;; are registered statically via register-agent! at startup.
;; Future versions can add dynamic-require-based loading behind this interface.
;;
;; Design notes (deferred namespace loading):
;;   Full namespace-based dynamic-require loading is deferred to a future
;;   milestone. The current agent roles are lightweight stateless structs;
;;   the immediate value is establishing the registry seam so future
;;   hot-swapping can be plugged in without modifying production dispatch code.
;;   When roles become heavyweight enough to warrant runtime replacement,
;;   module-path fields will enable dynamic-require in a fresh namespace
;;   with module-level isolation. Until then, all loading is static require.

(require racket/contract
         racket/match
         (only-in "../util/ids.rkt" now-seconds)
         "registry-types.rkt")

;; ============================================================
;; Registry State (thread-safe)
;; ============================================================

;; Internal: hash mapping role-name → registry-entry
(define registry (box (hasheq)))
(define registry-semaphore (make-semaphore 1))

;; ============================================================
;; Registration
;; ============================================================

;; Register a new agent version. If no version exists for this role,
;; the new one becomes active. Otherwise, it's registered as inactive
;; (available for activation, but not the default).
(define (register-agent! role-name version factory #:module-path [mod-path #f])
  (call-with-semaphore
   registry-semaphore
   (lambda ()
     (define entry (hash-ref (unbox registry) role-name #f))
     (define existing (or (and entry (registry-entry-descriptors entry)) '()))
     (define is-first (null? existing))
     (define new-desc (agent-descriptor role-name version factory mod-path is-first))
     (define new-entry (registry-entry role-name (append existing (list new-desc))))
     (set-box! registry (hash-set (unbox registry) role-name new-entry)))))

;; Activate a specific version for a role. Deactivates all other versions.
(define (activate-agent-version! role-name version)
  (call-with-semaphore
   registry-semaphore
   (lambda ()
     (define entry (hash-ref (unbox registry) role-name #f))
     (unless entry
       (error 'activate-agent-version! "unknown role: ~a" role-name))
     (define updated
       (for/list ([d (in-list (registry-entry-descriptors entry))])
         (struct-copy agent-descriptor d [active? (equal? (agent-descriptor-version d) version)])))
     (set-box! registry (hash-set (unbox registry) role-name (registry-entry role-name updated))))))

;; ============================================================
;; Resolution
;; ============================================================

;; Resolve the active agent descriptor for a role.
;; Returns #f if role is not registered.
(define (resolve-agent role-name)
  (call-with-semaphore registry-semaphore
                       (lambda ()
                         (define entry (hash-ref (unbox registry) role-name #f))
                         (and entry
                              (for/first ([d (in-list (registry-entry-descriptors entry))]
                                          #:when (agent-descriptor-active? d))
                                d)))))

;; Resolve a specific version (active or not).
(define (resolve-agent-version role-name version)
  (call-with-semaphore registry-semaphore
                       (lambda ()
                         (define entry (hash-ref (unbox registry) role-name #f))
                         (and entry
                              (for/first ([d (in-list (registry-entry-descriptors entry))]
                                          #:when (equal? (agent-descriptor-version d) version))
                                d)))))

;; Create a fresh agent instance from the active descriptor.
(define (make-agent-instance role-name)
  (define desc (resolve-agent role-name))
  (if desc
      ((agent-descriptor-factory desc))
      (error 'make-agent-instance "unknown role: ~a" role-name)))

;; Create a fresh agent instance from a specific version.
(define (make-agent-instance-versioned role-name version)
  (define desc (resolve-agent-version role-name version))
  (if desc
      ((agent-descriptor-factory desc))
      (error 'make-agent-instance-versioned "unknown role/version: ~a ~a" role-name version)))

;; ============================================================
;; Version Pinning
;; ============================================================

;; Pin the current active versions of all registered agents.
;; Returns a hash: role-name → version-pin.
(define (pin-current-versions)
  (call-with-semaphore
   registry-semaphore
   (lambda ()
     (for/hash ([(role-name entry) (in-hash (unbox registry))])
       (define active
         (for/first ([d (in-list (registry-entry-descriptors entry))]
                     #:when (agent-descriptor-active? d))
           d))
       (if active
           (values role-name (version-pin role-name (agent-descriptor-version active) (now-seconds)))
           (values role-name #f))))))

;; Resolve an agent using a version pin (ignores active flag).
(define (make-agent-with-pin role-name pin)
  (cond
    [(not pin) (make-agent-instance role-name)]
    [(not (eq? (version-pin-role-name pin) role-name))
     (error 'make-agent-with-pin "pin role mismatch: ~a vs ~a" (version-pin-role-name pin) role-name)]
    [else (make-agent-instance-versioned role-name (version-pin-pinned-version pin))]))

;; ============================================================
;; Introspection
;; ============================================================

;; List all registered roles.
(define (registered-roles)
  (call-with-semaphore registry-semaphore (lambda () (hash-keys (unbox registry)))))

;; List all versions for a role.
(define (agent-versions role-name)
  (call-with-semaphore registry-semaphore
                       (lambda ()
                         (define entry (hash-ref (unbox registry) role-name #f))
                         (if entry
                             (map agent-descriptor-version (registry-entry-descriptors entry))
                             '()))))

;; Reset registry (for testing).
(define (reset-registry!)
  (call-with-semaphore registry-semaphore (lambda () (set-box! registry (hasheq)))))

;; ============================================================
;; Provides
;; ============================================================

(provide register-agent!
         activate-agent-version!)

(provide (contract-out [resolve-agent (-> symbol? (or/c agent-descriptor? #f))]
                       [resolve-agent-version (-> symbol? string? (or/c agent-descriptor? #f))]
                       [make-agent-instance (-> symbol? any/c)]
                       [make-agent-instance-versioned (-> symbol? string? any/c)]
                       [pin-current-versions (-> hash?)]
                       [make-agent-with-pin (-> symbol? (or/c version-pin? #f) any/c)]
                       [registered-roles (-> (listof symbol?))]
                       [agent-versions (-> symbol? (listof string?))]
                       [reset-registry! (-> void?)]))

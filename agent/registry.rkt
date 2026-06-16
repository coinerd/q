#lang racket/base

;; agent/registry.rkt — Agent role registry with version management
;; STABILITY: evolving
;;
;; MAS Schritt 5: Modular Hot-Swapping & Dynamic Evolution
;; v0.99.13 W2 (G-3): Added dynamic-require loading path.
;;
;; Maps agent role names → descriptors → factory procedures.
;; The supervisor resolves roles via the registry instead of direct require.
;;
;; When both #:module-path and #:factory-name are provided,
;; make-agent-instance can load the agent from source via dynamic-require
;; in a fresh namespace, enabling runtime hot-swapping. When either is #f,
;; the static factory is called directly (backward compat).
;;
;; Design notes:
;;   The dynamic path uses namespace-attach-module for critical shared
;;   modules to preserve type identity (e.g., gen:agent-role predicates)
;;   across namespace boundaries. The SHARED-MODULES list is deliberately
;;   minimal to avoid namespace pollution.

(require racket/contract
         racket/match
         (only-in "../util/ids.rkt" now-seconds)
         "registry-types.rkt")

;; ============================================================
;; Session Activity Tracking
;; ============================================================

;; Box holding #t when a session is active (versions should not change).
;; The wiring layer sets this at session start and clears it at session end.
(define session-active-box (box #f))

(define (session-active?)
  (unbox session-active-box))
(define (set-session-active! v)
  (set-box! session-active-box v))

;; ============================================================
;; Hot-Swap Feature Gate
;; ============================================================

;; When #f (default), make-agent-instance always uses the static factory.
;; When #t, make-agent-instance uses dynamic-require if module-path +
;; factory-name are available on the descriptor.
;; Controlled by mas.hot-swap.enabled config (default #f).
(define hot-swap-enabled-box (box #f))

(define (hot-swap-enabled?)
  (unbox hot-swap-enabled-box))
(define (set-hot-swap-enabled! v)
  (set-box! hot-swap-enabled-box v))

;; ============================================================
;; Shared Modules for Namespace Attachment
;; ============================================================

;; Modules whose struct/interface identity must be shared across
;; namespace boundaries for predicates to work correctly.
;; Kept minimal — only modules that define structs or generics
;; used in runtime dispatch.
(define SHARED-MODULES '())

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
(define (register-agent! role-name
                         version
                         factory
                         #:module-path [mod-path #f]
                         #:factory-name [fac-name #f])
  (call-with-semaphore
   registry-semaphore
   (lambda ()
     (define entry (hash-ref (unbox registry) role-name #f))
     (define existing (or (and entry (registry-entry-descriptors entry)) '()))
     ;; R2-2: Idempotent registration — skip if version already exists.
     (define already-registered?
       (for/or ([d (in-list existing)])
         (equal? (agent-descriptor-version d) version)))
     (unless already-registered?
       (define is-first (null? existing))
       (define new-desc (agent-descriptor role-name version factory mod-path fac-name is-first))
       (define new-entry (registry-entry role-name (append existing (list new-desc))))
       (set-box! registry (hash-set (unbox registry) role-name new-entry))))))

;; Activate a specific version for a role. Deactivates all other versions.
;; Issues a warning log if a session is active (defers switch to next session).
(define (activate-agent-version! role-name version)
  (when (session-active?)
    (log-warning
     "activate-agent-version!: session is active; version ~a for ~a will take effect on next session"
     version
     role-name))
  (call-with-semaphore
   registry-semaphore
   (lambda ()
     (define entry (hash-ref (unbox registry) role-name #f))
     (unless entry
       (error 'activate-agent-version! "unknown role: ~a" role-name))
     ;; R2-1: Validate version exists before deactivating others.
     (define has-version?
       (for/or ([d (in-list (registry-entry-descriptors entry))])
         (equal? (agent-descriptor-version d) version)))
     (unless has-version?
       (error 'activate-agent-version! "unknown version ~a for role ~a" version role-name))
     (define updated
       (for/list ([d (in-list (registry-entry-descriptors entry))])
         (struct-copy agent-descriptor d [active? (equal? (agent-descriptor-version d) version)])))
     (set-box! registry (hash-set (unbox registry) role-name (registry-entry role-name updated))))))

;; ============================================================
;; Dynamic Agent Loading (G-3)
;; ============================================================

;; Load an agent dynamically via dynamic-require in a fresh namespace.
;; Attaches shared modules to preserve type identity.
;; Falls back to static factory on any error.
(define (load-agent-dynamically desc)
  (with-handlers
      ([exn:fail?
        (lambda (e)
          (log-warning
           "load-agent-dynamically: dynamic-require failed for ~a, falling back to static: ~a"
           (agent-descriptor-role-name desc)
           (exn-message e))
          ((agent-descriptor-factory desc)))])
    (define ns (make-base-namespace))
    ;; Attach shared modules to preserve struct identity across namespaces.
    (for ([mod (in-list SHARED-MODULES)])
      (namespace-attach-module (current-namespace) mod ns))
    (define factory-proc
      (parameterize ([current-namespace ns])
        (dynamic-require (agent-descriptor-module-path desc) (agent-descriptor-factory-name desc))))
    (factory-proc)))

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
;; Uses dynamic-require path ONLY when hot-swap is enabled AND
;; module-path + factory-name are set on the descriptor.
;; Otherwise uses the static factory (backward compat, default).
(define (make-agent-instance role-name)
  (define desc (resolve-agent role-name))
  (unless desc
    (error 'make-agent-instance "unknown role: ~a" role-name))
  (cond
    ;; Dynamic path: hot-swap enabled + module-path + factory-name
    [(and (hot-swap-enabled?)
          (agent-descriptor-module-path desc)
          (agent-descriptor-factory-name desc))
     (load-agent-dynamically desc)]
    ;; Static path (backward compat)
    [else ((agent-descriptor-factory desc))]))

;; Create a fresh agent instance from a specific version.
(define (make-agent-instance-versioned role-name version)
  (define desc (resolve-agent-version role-name version))
  (unless desc
    (error 'make-agent-instance-versioned "unknown role/version: ~a ~a" role-name version))
  (cond
    [(and (hot-swap-enabled?)
          (agent-descriptor-module-path desc)
          (agent-descriptor-factory-name desc))
     (load-agent-dynamically desc)]
    [else ((agent-descriptor-factory desc))]))

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
         activate-agent-version!
         session-active?
         set-session-active!
         hot-swap-enabled?
         set-hot-swap-enabled!)

(provide (contract-out [resolve-agent (-> symbol? (or/c agent-descriptor? #f))]
                       [resolve-agent-version (-> symbol? string? (or/c agent-descriptor? #f))]
                       [make-agent-instance (-> symbol? any/c)]
                       [make-agent-instance-versioned (-> symbol? string? any/c)]
                       [pin-current-versions (-> hash?)]
                       [make-agent-with-pin (-> symbol? (or/c version-pin? #f) any/c)]
                       [registered-roles (-> (listof symbol?))]
                       [agent-versions (-> symbol? (listof string?))]
                       [reset-registry! (-> void?)]))

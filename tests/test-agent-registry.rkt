#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-agent-registry.rkt — W1 (v0.99.8) Agent Registry Tests
;;
;; Tests for agent/registry-types.rkt and agent/registry.rkt:
;;   - Struct construction and accessors
;;   - Registration and activation
;;   - Resolution (active and versioned)
;;   - Instance creation
;;   - Version pinning
;;   - Introspection
;;   - Thread safety (basic)

(require rackunit
         rackunit/text-ui
         (only-in "../agent/registry-types.rkt"
                  agent-descriptor
                  agent-descriptor?
                  agent-descriptor-role-name
                  agent-descriptor-version
                  agent-descriptor-factory
                  agent-descriptor-module-path
                  agent-descriptor-active?
                  version-pin
                  version-pin?
                  version-pin-role-name
                  version-pin-pinned-version
                  version-pin-pinned-at
                  registry-entry
                  registry-entry?
                  registry-entry-role-name
                  registry-entry-descriptors)
         (only-in "../agent/registry.rkt"
                  register-agent!
                  activate-agent-version!
                  resolve-agent
                  resolve-agent-version
                  make-agent-instance
                  make-agent-instance-versioned
                  pin-current-versions
                  make-agent-with-pin
                  registered-roles
                  agent-versions
                  reset-registry!))

;; ── Test helpers ──

(define (dummy-factory-v1)
  'dummy-v1)

(define (dummy-factory-v2)
  'dummy-v2)

(define (dummy-factory-v3)
  'dummy-v3)

(define suite
  (test-suite "Agent Registry (v0.99.8 W1)"

    ;; ── registry-types.rkt: Struct construction ──

    (test-case "agent-descriptor constructs with correct fields"
      (define d (agent-descriptor 'planner "1.0" dummy-factory-v1 #f #t))
      (check-true (agent-descriptor? d))
      (check-equal? (agent-descriptor-role-name d) 'planner)
      (check-equal? (agent-descriptor-version d) "1.0")
      (check-equal? (agent-descriptor-module-path d) #f)
      (check-true (agent-descriptor-active? d)))

    (test-case "version-pin constructs with correct fields"
      (define p (version-pin 'planner "1.0" 1700000000))
      (check-true (version-pin? p))
      (check-equal? (version-pin-role-name p) 'planner)
      (check-equal? (version-pin-pinned-version p) "1.0")
      (check-equal? (version-pin-pinned-at p) 1700000000))

    (test-case "registry-entry constructs with correct fields"
      (define e (registry-entry 'planner '()))
      (check-true (registry-entry? e))
      (check-equal? (registry-entry-role-name e) 'planner)
      (check-equal? (registry-entry-descriptors e) '()))

    (test-case "agent-descriptor is transparent (has equal? semantics)"
      (define d1 (agent-descriptor 'planner "1.0" dummy-factory-v1 #f #t))
      (define d2 (agent-descriptor 'planner "1.0" dummy-factory-v1 #f #t))
      (check-equal? d1 d2))

    ;; ── registry.rkt: Registration ──

    (test-case "register-agent! adds a role"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (check-not-false (member 'planner (registered-roles))))

    (test-case "first registered version is active"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (define d (resolve-agent 'planner))
      (check-not-false d)
      (check-true (agent-descriptor-active? d))
      (check-equal? (agent-descriptor-version d) "1.0"))

    (test-case "second registered version is inactive"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (register-agent! 'planner "2.0" dummy-factory-v2)
      (define versions (agent-versions 'planner))
      (check-equal? (length versions) 2)
      (check-not-false (member "1.0" versions))
      (check-not-false (member "2.0" versions))
      (define active (resolve-agent 'planner))
      (check-equal? (agent-descriptor-version active) "1.0"))

    ;; ── registry.rkt: Activation ──

    (test-case "activate-agent-version! switches active version"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (register-agent! 'planner "2.0" dummy-factory-v2)
      (activate-agent-version! 'planner "2.0")
      (define active (resolve-agent 'planner))
      (check-equal? (agent-descriptor-version active) "2.0"))

    (test-case "activate-agent-version! deactivates others"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (register-agent! 'planner "2.0" dummy-factory-v2)
      (activate-agent-version! 'planner "2.0")
      (define v1 (resolve-agent-version 'planner "1.0"))
      (check-false (agent-descriptor-active? v1))
      (define v2 (resolve-agent-version 'planner "2.0"))
      (check-true (agent-descriptor-active? v2)))

    (test-case "activate-agent-version! on unknown role errors"
      (reset-registry!)
      (check-exn exn:fail? (lambda () (activate-agent-version! 'unknown "1.0"))))

    ;; ── registry.rkt: Resolution ──

    (test-case "resolve-agent returns #f for unregistered role"
      (reset-registry!)
      (check-false (resolve-agent 'nonexistent)))

    (test-case "resolve-agent-version returns #f for unregistered version"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (check-false (resolve-agent-version 'planner "99.0")))

    (test-case "resolve-agent-version returns inactive version"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (register-agent! 'planner "2.0" dummy-factory-v2)
      (define v2 (resolve-agent-version 'planner "2.0"))
      (check-not-false v2)
      (check-false (agent-descriptor-active? v2)))

    ;; ── registry.rkt: Instance creation ──

    (test-case "make-agent-instance creates from active descriptor"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (define result (make-agent-instance 'planner))
      (check-equal? result 'dummy-v1))

    (test-case "make-agent-instance uses factory of active version"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (register-agent! 'planner "2.0" dummy-factory-v2)
      (activate-agent-version! 'planner "2.0")
      (define result (make-agent-instance 'planner))
      (check-equal? result 'dummy-v2))

    (test-case "make-agent-instance errors for unknown role"
      (reset-registry!)
      (check-exn exn:fail? (lambda () (make-agent-instance 'nonexistent))))

    (test-case "make-agent-instance-versioned creates from specific version"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (register-agent! 'planner "2.0" dummy-factory-v2)
      (check-equal? (make-agent-instance-versioned 'planner "1.0") 'dummy-v1)
      (check-equal? (make-agent-instance-versioned 'planner "2.0") 'dummy-v2))

    (test-case "make-agent-instance-versioned errors for unknown version"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (check-exn exn:fail? (lambda () (make-agent-instance-versioned 'planner "99.0"))))

    ;; ── registry.rkt: Version pinning ──

    (test-case "pin-current-versions returns pins for all roles"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (register-agent! 'verifier "2.0" dummy-factory-v2)
      (define pins (pin-current-versions))
      (check-equal? (hash-count pins) 2)
      (check-true (version-pin? (hash-ref pins 'planner #f)))
      (check-equal? (version-pin-pinned-version (hash-ref pins 'planner)) "1.0")
      (check-equal? (version-pin-pinned-version (hash-ref pins 'verifier)) "2.0"))

    (test-case "make-agent-with-pin uses pinned version"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (register-agent! 'planner "2.0" dummy-factory-v2)
      ;; Pin to v1, then activate v2
      (define pins (pin-current-versions))
      (activate-agent-version! 'planner "2.0")
      ;; Pin should still resolve v1
      (define pin (hash-ref pins 'planner))
      (check-equal? (make-agent-with-pin 'planner pin) 'dummy-v1))

    (test-case "make-agent-with-pin with #f falls back to active"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (check-equal? (make-agent-with-pin 'planner #f) 'dummy-v1))

    (test-case "make-agent-with-pin errors on role mismatch"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (register-agent! 'verifier "2.0" dummy-factory-v2)
      (define pins (pin-current-versions))
      (define planner-pin (hash-ref pins 'planner))
      (check-exn exn:fail? (lambda () (make-agent-with-pin 'verifier planner-pin))))

    ;; ── registry.rkt: Introspection ──

    (test-case "registered-roles lists all registered roles"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (register-agent! 'verifier "2.0" dummy-factory-v2)
      (define roles (registered-roles))
      (check-equal? (length roles) 2)
      (check-not-false (member 'planner roles))
      (check-not-false (member 'verifier roles)))

    (test-case "agent-versions lists all versions for a role"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (register-agent! 'planner "2.0" dummy-factory-v2)
      (register-agent! 'planner "3.0" dummy-factory-v3)
      (define versions (agent-versions 'planner))
      (check-equal? (length versions) 3)
      (check-not-false (member "1.0" versions))
      (check-not-false (member "2.0" versions))
      (check-not-false (member "3.0" versions)))

    (test-case "agent-versions returns empty for unregistered role"
      (reset-registry!)
      (check-equal? (agent-versions 'nonexistent) '()))

    (test-case "reset-registry! clears all registrations"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1)
      (reset-registry!)
      (check-equal? (registered-roles) '())
      (check-false (resolve-agent 'planner)))

    (test-case "register-agent! with module-path stores it"
      (reset-registry!)
      (register-agent! 'planner "1.0" dummy-factory-v1 #:module-path "agent/roles/planner.rkt")
      (define d (resolve-agent 'planner))
      (check-equal? (agent-descriptor-module-path d) "agent/roles/planner.rkt"))))

(run-tests suite)

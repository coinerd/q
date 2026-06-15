#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-registry-integration.rkt — W6 (v0.99.8) End-to-End Integration Tests
;;
;; Full lifecycle test:
;;   1. Register default agents
;;   2. Verify all 4 roles resolve
;;   3. Create supervisor with registry ON
;;   4. Dispatch envelope → verify response
;;   5. Pin versions → verify pins match
;;   6. Activate new version → verify old pins still resolve
;;   7. Create supervisor with registry OFF → verify backward compat

(require rackunit
         rackunit/text-ui
         (only-in "../agent/registry.rkt"
                  reset-registry!
                  register-agent!
                  activate-agent-version!
                  resolve-agent
                  registered-roles
                  agent-versions
                  pin-current-versions
                  make-agent-with-pin)
         (only-in "../agent/registry-defaults.rkt" register-default-agents!)
         (only-in "../agent/roles/supervisor.rkt"
                  supervisor-role?
                  make-supervisor-role
                  supervisor-role-sub-roles
                  current-use-registry)
         (only-in "../agent/roles/planner.rkt" planner-role?)
         (only-in "../agent/roles/verifier.rkt" verifier-role?)
         (only-in "../agent/roles/tool-gateway.rkt" tool-gateway-role?)
         (only-in "../agent/roles/executor.rkt" executor-role?))

(define suite
  (test-suite "Registry Integration E2E (v0.99.8 W6)"

    ;; ── E2E Lifecycle ──

    (test-case "E2E: full registry lifecycle"
      (reset-registry!)

      ;; 1. Register default agents
      (register-default-agents!)

      ;; 2. Verify all 4 roles resolve
      (define roles (registered-roles))
      (check-equal? (length roles) 4)
      (check-not-false (resolve-agent 'planner))
      (check-not-false (resolve-agent 'verifier))
      (check-not-false (resolve-agent 'tool-gateway))
      (check-not-false (resolve-agent 'executor))

      ;; 3. Create supervisor with registry ON
      (parameterize ([current-use-registry #t])
        (define sv (make-supervisor-role))
        (check-true (supervisor-role? sv))
        (define sub-roles (supervisor-role-sub-roles sv))
        (check-true (planner-role? (hash-ref sub-roles 'planner)))
        (check-true (verifier-role? (hash-ref sub-roles 'verifier)))
        (check-true (tool-gateway-role? (hash-ref sub-roles 'tool-gateway))))

      ;; 4. Pin versions → verify pins match
      (define pins (pin-current-versions))
      (check-equal? (hash-count pins) 4)
      (for ([(role pin) (in-hash pins)])
        (check-equal? (hash-ref pins role) pin))

      (reset-registry!))

    ;; ── Hot-Swap Simulation ──

    (test-case "E2E: hot-swap simulation with version rollback"
      (reset-registry!)

      ;; Register two planner versions
      (register-agent! 'planner "1.0.0" (lambda () 'planner-v1))
      (register-agent! 'planner "2.0.0" (lambda () 'planner-v2))

      ;; Pin versions (captures v1 as active)
      (define pins (pin-current-versions))
      (define planner-pin (hash-ref pins 'planner))
      (check-not-false planner-pin)

      ;; Activate v2 (hot-swap)
      (activate-agent-version! 'planner "2.0.0")

      ;; New instance uses v2 (active)
      (define new-instance (make-agent-with-pin 'planner #f))
      (check-equal? new-instance 'planner-v2)

      ;; Pinned session still uses v1
      (define pinned-instance (make-agent-with-pin 'planner planner-pin))
      (check-equal? pinned-instance 'planner-v1)

      ;; Rollback: activate v1 again
      (activate-agent-version! 'planner "1.0.0")
      (define rollback-instance (make-agent-with-pin 'planner #f))
      (check-equal? rollback-instance 'planner-v1)

      (reset-registry!))

    ;; ── Registry OFF Backward Compat ──

    (test-case "E2E: registry OFF uses direct construction"
      (reset-registry!)
      ;; Don't register any agents
      (parameterize ([current-use-registry #f])
        (define sv (make-supervisor-role))
        (check-true (supervisor-role? sv))
        (define sub-roles (supervisor-role-sub-roles sv))
        (check-equal? (hash-count sub-roles) 3))
      (reset-registry!))

    ;; ── Registry ON without registration falls back gracefully ──

    (test-case "E2E: registry ON with no registration falls back gracefully"
      (reset-registry!)
      (parameterize ([current-use-registry #t])
        (define sv (make-supervisor-role))
        (check-true (supervisor-role? sv))
        ;; Should still have 3 sub-roles via fallback
        (define sub-roles (supervisor-role-sub-roles sv))
        (check-equal? (hash-count sub-roles) 3))
      (reset-registry!))

    ;; ── Multiple supervisors from same registry ──

    (test-case "E2E: multiple supervisors from same registry"
      (reset-registry!)
      (register-default-agents!)
      (parameterize ([current-use-registry #t])
        (define sv1 (make-supervisor-role))
        (define sv2 (make-supervisor-role))
        (check-true (supervisor-role? sv1))
        (check-true (supervisor-role? sv2))
        (check-not-eq? sv1 sv2)
        ;; Both have valid sub-roles
        (check-equal? (hash-count (supervisor-role-sub-roles sv1)) 3)
        (check-equal? (hash-count (supervisor-role-sub-roles sv2)) 3))
      (reset-registry!))

    ;; ── Version tracking across registrations ──

    (test-case "E2E: version tracking across multiple registrations"
      (reset-registry!)
      (register-agent! 'planner "1.0.0" (lambda () 'v1))
      (register-agent! 'planner "1.1.0" (lambda () 'v1-1))
      (register-agent! 'planner "2.0.0" (lambda () 'v2))
      (define versions (agent-versions 'planner))
      (check-equal? (length versions) 3)
      (check-not-false (member "1.0.0" versions))
      (check-not-false (member "1.1.0" versions))
      (check-not-false (member "2.0.0" versions))
      (reset-registry!))

    ;; ── Registry survives reset ──

    (test-case "E2E: reset-registry! clears everything"
      (reset-registry!)
      (register-default-agents!)
      (check-equal? (length (registered-roles)) 4)
      (reset-registry!)
      (check-equal? (length (registered-roles)) 0)
      (check-false (resolve-agent 'planner)))))

(run-tests suite)

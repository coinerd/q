#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-registry-supervisor.rkt — W2 (v0.99.8) Registry-Enhanced Supervisor Tests
;;
;; Tests that the supervisor can use either direct construction (backward compat)
;; or registry-based resolution (hot-swap path), controlled by current-use-registry.

(require rackunit
         rackunit/text-ui
         (only-in "../agent/roles/supervisor.rkt"
                  supervisor-role?
                  make-supervisor-role
                  supervisor-role-sub-roles
                  current-use-registry)
         (only-in "../agent/roles/planner.rkt" planner-role? make-planner-role)
         (only-in "../agent/roles/verifier.rkt" verifier-role? make-verifier-role)
         (only-in "../agent/roles/tool-gateway.rkt" tool-gateway-role?)
         (only-in "../agent/registry.rkt" register-agent! reset-registry! registered-roles))

(define suite
  (test-suite "Registry-Enhanced Supervisor (v0.99.8 W2)"

    ;; ── Parameter behavior ──

    (test-case "current-use-registry defaults to #f"
      (check-false (current-use-registry)))

    (test-case "current-use-registry can be set to #t"
      (parameterize ([current-use-registry #t])
        (check-true (current-use-registry)))
      ;; Restored after parameterize
      (check-false (current-use-registry)))

    ;; ── Direct construction path (registry OFF) ──

    (test-case "supervisor with registry OFF uses direct construction"
      (parameterize ([current-use-registry #f])
        (define sv (make-supervisor-role))
        (check-true (supervisor-role? sv))
        (define sub-roles (supervisor-role-sub-roles sv))
        (check-true (planner-role? (hash-ref sub-roles 'planner)))
        (check-true (verifier-role? (hash-ref sub-roles 'verifier)))
        (check-true (tool-gateway-role? (hash-ref sub-roles 'tool-gateway)))))

    (test-case "supervisor with registry OFF has 3 sub-roles"
      (parameterize ([current-use-registry #f])
        (define sv (make-supervisor-role))
        (define sub-roles (supervisor-role-sub-roles sv))
        (check-equal? (hash-count sub-roles) 3)))

    ;; ── Registry path (registry ON, but no roles registered) ──

    (test-case "supervisor with registry ON falls back when no roles registered"
      (reset-registry!)
      (parameterize ([current-use-registry #t])
        ;; No roles registered — should fall back to direct construction
        (define sv (make-supervisor-role))
        (check-true (supervisor-role? sv))
        (define sub-roles (supervisor-role-sub-roles sv))
        (check-true (planner-role? (hash-ref sub-roles 'planner)))
        (check-true (verifier-role? (hash-ref sub-roles 'verifier)))
        (check-true (tool-gateway-role? (hash-ref sub-roles 'tool-gateway)))))

    ;; ── Registry path (registry ON, roles registered) ──

    (test-case "supervisor with registry ON uses registered factories"
      (reset-registry!)
      (register-agent! 'planner "1.0" make-planner-role)
      (register-agent! 'verifier "1.0" make-verifier-role)
      (register-agent! 'tool-gateway
                       "1.0"
                       (lambda ()
                         ;; Use dynamic require to avoid circular dep
                         ((dynamic-require "agent/roles/tool-gateway.rkt" 'make-tool-gateway-role))))
      (parameterize ([current-use-registry #t])
        (define sv (make-supervisor-role))
        (check-true (supervisor-role? sv))
        (define sub-roles (supervisor-role-sub-roles sv))
        (check-true (planner-role? (hash-ref sub-roles 'planner)))
        (check-true (verifier-role? (hash-ref sub-roles 'verifier)))
        (check-true (tool-gateway-role? (hash-ref sub-roles 'tool-gateway))))
      (reset-registry!))

    ;; ── Mixed path (registry ON, only some roles registered) ──

    (test-case "supervisor with registry ON uses mix when partial registration"
      (reset-registry!)
      ;; Only register planner, not verifier/tool-gateway
      (register-agent! 'planner "2.0" make-planner-role)
      (parameterize ([current-use-registry #t])
        (define sv (make-supervisor-role))
        (check-true (supervisor-role? sv))
        (define sub-roles (supervisor-role-sub-roles sv))
        ;; All three should still be present (mix of registry + fallback)
        (check-true (planner-role? (hash-ref sub-roles 'planner)))
        (check-true (verifier-role? (hash-ref sub-roles 'verifier)))
        (check-true (tool-gateway-role? (hash-ref sub-roles 'tool-gateway))))
      (reset-registry!))

    ;; ── Behavioral equivalence ──

    (test-case "both paths produce valid supervisors with 3 sub-roles"
      ;; Registry OFF
      (parameterize ([current-use-registry #f])
        (define sv-off (make-supervisor-role))
        (check-equal? (hash-count (supervisor-role-sub-roles sv-off)) 3))
      ;; Registry ON with full registration
      (reset-registry!)
      (register-agent! 'planner "1.0" make-planner-role)
      (register-agent! 'verifier "1.0" make-verifier-role)
      (register-agent! 'tool-gateway
                       "1.0"
                       (lambda ()
                         ((dynamic-require "agent/roles/tool-gateway.rkt" 'make-tool-gateway-role))))
      (parameterize ([current-use-registry #t])
        (define sv-on (make-supervisor-role))
        (check-equal? (hash-count (supervisor-role-sub-roles sv-on)) 3))
      (reset-registry!))

    ;; ── Idempotency ──

    (test-case "multiple make-supervisor-role calls work with registry OFF"
      (parameterize ([current-use-registry #f])
        (define sv1 (make-supervisor-role))
        (define sv2 (make-supervisor-role))
        (check-true (supervisor-role? sv1))
        (check-true (supervisor-role? sv2))
        ;; Different instances
        (check-not-eq? sv1 sv2)))

    (test-case "multiple make-supervisor-role calls work with registry ON"
      (reset-registry!)
      (register-agent! 'planner "1.0" make-planner-role)
      (register-agent! 'verifier "1.0" make-verifier-role)
      (register-agent! 'tool-gateway
                       "1.0"
                       (lambda ()
                         ((dynamic-require "agent/roles/tool-gateway.rkt" 'make-tool-gateway-role))))
      (parameterize ([current-use-registry #t])
        (define sv1 (make-supervisor-role))
        (define sv2 (make-supervisor-role))
        (check-true (supervisor-role? sv1))
        (check-true (supervisor-role? sv2))
        (check-not-eq? sv1 sv2))
      (reset-registry!))))

(run-tests suite)

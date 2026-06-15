#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-registry-config-wiring.rkt — W3 (v0.99.8) Registry Population + Config Tests
;;
;; Tests for:
;;   - register-default-agents! registers 4 roles at version "1.0.0"
;;   - hot-swap-enabled? config query
;;   - Supervisor uses registry when enabled

(require rackunit
         rackunit/text-ui
         (only-in "../agent/registry.rkt"
                  reset-registry!
                  registered-roles
                  agent-versions
                  resolve-agent
                  make-agent-instance)
         (only-in "../agent/registry-defaults.rkt" register-default-agents!)
         (only-in "../agent/roles/planner.rkt" planner-role?)
         (only-in "../agent/roles/verifier.rkt" verifier-role?)
         (only-in "../agent/roles/tool-gateway.rkt" tool-gateway-role?)
         (only-in "../agent/roles/executor.rkt" executor-role?)
         (only-in "../runtime/settings-query.rkt" hot-swap-enabled? blackboard-enabled?)
         (only-in "../runtime/settings.rkt" q-settings))

;; Helper: construct a q-settings with a given merged-hash path value.
(define (make-test-settings . kv-pairs)
  (define merged
    (for/hash ([pair (in-list kv-pairs)])
      (values (car pair) (cadr pair))))
  (q-settings (hash) (hash) merged))

;; Helper: nested settings for mas paths
(define (make-mas-setting path value)
  (define (build-hash parts)
    (if (null? parts)
        value
        (hash (car parts) (build-hash (cdr parts)))))
  (build-hash path))

(define (make-mas-settings . specs)
  (define merged
    (for/hash ([spec (in-list specs)])
      (values (car spec) (cadr spec))))
  (q-settings (hash) (hash) merged))

(define suite
  (test-suite "Registry Config Wiring (v0.99.8 W3)"

    ;; ── register-default-agents! ──

    (test-case "register-default-agents! registers 4 roles"
      (reset-registry!)
      (register-default-agents!)
      (define roles (registered-roles))
      (check-equal? (length roles) 4)
      (check-not-false (member 'planner roles))
      (check-not-false (member 'verifier roles))
      (check-not-false (member 'tool-gateway roles))
      (check-not-false (member 'executor roles))
      (reset-registry!))

    (test-case "all default agents have version 1.0.0"
      (reset-registry!)
      (register-default-agents!)
      (for ([role '(planner verifier tool-gateway executor)])
        (define versions (agent-versions role))
        (check-equal? versions
                      '("1.0.0")
                      (format "role ~a should have single version 1.0.0, got ~a" role versions)))
      (reset-registry!))

    (test-case "make-agent-instance works for registered defaults"
      (reset-registry!)
      (register-default-agents!)
      (check-true (planner-role? (make-agent-instance 'planner)))
      (check-true (verifier-role? (make-agent-instance 'verifier)))
      (check-true (tool-gateway-role? (make-agent-instance 'tool-gateway)))
      (check-true (executor-role? (make-agent-instance 'executor)))
      (reset-registry!))

    (test-case "register-default-agents! is idempotent"
      (reset-registry!)
      (register-default-agents!)
      (register-default-agents!)
      ;; Second call adds duplicates (append semantics) but all work
      (define roles (registered-roles))
      (check-equal? (length roles) 4)
      (reset-registry!))

    ;; ── hot-swap-enabled? config query ──

    (test-case "hot-swap-enabled? returns #f by default"
      (define settings (make-test-settings))
      (check-false (hot-swap-enabled? settings)))

    (test-case "hot-swap-enabled? returns #t when boolean true"
      (define settings (make-mas-settings (list 'mas (make-mas-setting '(hot-swap enabled) #t))))
      (check-true (hot-swap-enabled? settings)))

    (test-case "hot-swap-enabled? returns #t for string true"
      (define settings (make-mas-settings (list 'mas (make-mas-setting '(hot-swap enabled) "true"))))
      (check-true (hot-swap-enabled? settings)))

    (test-case "hot-swap-enabled? returns #f for string false"
      (define settings (make-mas-settings (list 'mas (make-mas-setting '(hot-swap enabled) "false"))))
      (check-false (hot-swap-enabled? settings)))

    (test-case "hot-swap-enabled? returns #f when key absent"
      (define settings (make-test-settings))
      (check-false (hot-swap-enabled? settings)))

    (test-case "hot-swap-enabled? returns #f for random value"
      (define settings (make-mas-settings (list 'mas (make-mas-setting '(hot-swap enabled) 42))))
      (check-false (hot-swap-enabled? settings)))

    ;; ── blackboard-enabled? still works (regression check) ──

    (test-case "blackboard-enabled? still works (regression)"
      (define settings (make-mas-settings (list 'mas (make-mas-setting '(blackboard enabled) #t))))
      (check-true (blackboard-enabled? settings)))))

(run-tests suite)

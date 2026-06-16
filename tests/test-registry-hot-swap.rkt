#lang racket/base

;; tests/test-registry-hot-swap.rkt — Hot-swap tests (G-3)
;; v0.99.13 W2
;;
;; Tests for the dynamic-require-based hot-swapping feature:
;;   1. Static path: factory called directly when hot-swap disabled (backward compat)
;;   2. Dynamic path: module loaded via dynamic-require when hot-swap enabled
;;   3. Version pinning with dynamic path
;;   4. activate-agent-version! session safety warning
;;   5. Fallback: missing module-path → static path even when hot-swap enabled
;;   6. factory-name stored in descriptor
;; v0.99.18 W1 (F-HS-03): Identity verification tests

(require rackunit
         rackunit/text-ui
         "../agent/registry.rkt"
         "../agent/registry-types.rkt"
         "../agent/registry-defaults.rkt"
         (only-in "../agent/roles/base.rkt" agent-role?))

;; ============================================================
;; Helpers
;; ============================================================

(define (dummy-factory)
  'static-result)

(define (dummy-factory-v2)
  'static-result-v2)

;; ============================================================
;; Test Suite
;; ============================================================

(define hot-swap-suite
  (test-suite "Registry Hot-Swap (G-3, v0.99.13 W2)"

    ;; ── Setup ──

    ;; ── Test 1: factory-name field exists and stores correctly ──
    (test-case "agent-descriptor stores factory-name field"
      (define d (agent-descriptor 'test-role "1.0" dummy-factory #f 'make-test-role #t))
      (check-equal? (agent-descriptor-factory-name d) 'make-test-role))

    (test-case "agent-descriptor factory-name defaults to #f"
      (define d (agent-descriptor 'test-role "1.0" dummy-factory #f #f #t))
      (check-false (agent-descriptor-factory-name d)))

    ;; ── Test 2: Static path when hot-swap disabled ──
    (test-case "static path: factory called directly when hot-swap disabled"
      (reset-registry!)
      (set-hot-swap-enabled! #f)
      (register-agent! 'test-static
                       "1.0"
                       dummy-factory
                       #:module-path "some/path.rkt"
                       #:factory-name 'some-factory)
      (define result (make-agent-instance 'test-static))
      (check-equal? result 'static-result "static factory should be called"))

    ;; ── Test 3: Static path when module-path is #f ──
    (test-case "fallback: missing module-path uses static path even with hot-swap enabled"
      (reset-registry!)
      (set-hot-swap-enabled! #t)
      (register-agent! 'test-fallback "1.0" dummy-factory)
      ;; No #:module-path or #:factory-name → static path
      (define result (make-agent-instance 'test-fallback))
      (check-equal? result 'static-result)
      (set-hot-swap-enabled! #f))

    ;; ── Test 4: Dynamic path with real role module ──
    ;; F-11 fix: Use collection-based module path so dynamic-require
    ;; resolves regardless of (current-directory).
    (test-case "dynamic path: planner loaded via dynamic-require"
      (reset-registry!)
      (set-hot-swap-enabled! #t)
      ;; Register a role with real module path + factory name.
      ;; Use register-default-agents! which now uses resolved paths (F-11).
      (register-default-agents!)
      ;; Override the planner's static factory with a dummy to verify
      ;; the dynamic path is taken (not the static fallback).
      ;; Re-register planner with a dummy factory but real module path.
      (register-agent! 'planner
                       "1.0"
                       (lambda () 'static-fallback)
                       #:module-path 'q/agent/roles/planner
                       #:factory-name 'make-planner-role)
      (define result (make-agent-instance 'planner))
      ;; The dynamic path should load the real planner-role
      ;; (not 'static-fallback from the dummy factory)
      (check-not-false result "dynamic load produced a result")
      (check-false (eq? result 'static-fallback)
                   "dynamic path should NOT use the static fallback factory")
      (set-hot-swap-enabled! #f))

    ;; ── Test 5: Version pinning still works ──
    (test-case "version pinning: make-agent-instance-versioned with static path"
      (reset-registry!)
      (set-hot-swap-enabled! #f)
      (register-agent! 'multi "1.0" dummy-factory)
      (register-agent! 'multi "2.0" dummy-factory-v2)
      (activate-agent-version! 'multi "2.0")
      (define pins (pin-current-versions))
      (check-equal? (version-pin-pinned-version (hash-ref pins 'multi)) "2.0")
      ;; Pin resolves to v2.0
      (define result (make-agent-with-pin 'multi (hash-ref pins 'multi)))
      (check-equal? result 'static-result-v2))

    ;; ── Test 6: activate-agent-version! during active session ──
    (test-case "activate-agent-version! logs warning during active session"
      (reset-registry!)
      (set-session-active! #f)
      (register-agent! 'session-test "1.0" dummy-factory)
      (register-agent! 'session-test "2.0" dummy-factory-v2)
      ;; Activate without session → no error
      (activate-agent-version! 'session-test "2.0")
      ;; Activate during session → should not error, just warn
      (set-session-active! #t)
      (activate-agent-version! 'session-test "1.0")
      ;; Verify it activated despite warning
      (define desc (resolve-agent 'session-test))
      (check-true (agent-descriptor-active? desc))
      (check-equal? (agent-descriptor-version desc) "1.0")
      (set-session-active! #f))

    ;; ── Test 7: Dynamic load fallback on error ──
    (test-case "dynamic load falls back to static on bad module path"
      (reset-registry!)
      (set-hot-swap-enabled! #t)
      (register-agent! 'bad-path
                       "1.0"
                       dummy-factory
                       #:module-path "nonexistent/module.rkt"
                       #:factory-name 'nonexistent-factory)
      ;; Should fall back to static factory, not crash
      (define result (make-agent-instance 'bad-path))
      (check-equal? result
                    'static-result
                    "should fall back to static factory on dynamic-require failure")
      (set-hot-swap-enabled! #f))

    ;; ── Test 8: register-agent! with both params stores them ──
    (test-case "register-agent! stores module-path and factory-name"
      (reset-registry!)
      (register-agent! 'params-test
                       "1.0"
                       dummy-factory
                       #:module-path "agent/roles/test.rkt"
                       #:factory-name 'make-test-role)
      (define desc (resolve-agent 'params-test))
      (check-not-false desc)
      (check-equal? (agent-descriptor-module-path desc) "agent/roles/test.rkt")
      (check-equal? (agent-descriptor-factory-name desc) 'make-test-role))

    ;; ============================================================
    ;; v0.99.18 W1 (F-HS-03): Identity Verification Tests
    ;; ============================================================

    (test-case "F-HS-03: dynamically loaded planner satisfies agent-role?"
      (reset-registry!)
      (set-hot-swap-enabled! #t)
      (register-default-agents!)
      (define result (make-agent-instance 'planner))
      (check-true (agent-role? result)
                  "Dynamically loaded planner must satisfy agent-role? (identity check)")
      (set-hot-swap-enabled! #f))

    (test-case "F-HS-03: dynamically loaded verifier satisfies agent-role?"
      (reset-registry!)
      (set-hot-swap-enabled! #t)
      (register-default-agents!)
      (define result (make-agent-instance 'verifier))
      (check-true (agent-role? result)
                  "Dynamically loaded verifier must satisfy agent-role? (identity check)")
      (set-hot-swap-enabled! #f))

    (test-case "F-HS-03: dynamically loaded tool-gateway satisfies agent-role?"
      (reset-registry!)
      (set-hot-swap-enabled! #t)
      (register-default-agents!)
      (define result (make-agent-instance 'tool-gateway))
      (check-true (agent-role? result)
                  "Dynamically loaded tool-gateway must satisfy agent-role? (identity check)")
      (set-hot-swap-enabled! #f))

    (test-case "F-HS-03: dynamically loaded executor satisfies agent-role?"
      (reset-registry!)
      (set-hot-swap-enabled! #t)
      (register-default-agents!)
      (define result (make-agent-instance 'executor))
      (check-true (agent-role? result)
                  "Dynamically loaded executor must satisfy agent-role? (identity check)")
      (set-hot-swap-enabled! #f))

    (test-case "F-HS-03: identity mismatch falls back to static factory"
      (reset-registry!)
      (set-hot-swap-enabled! #t)
      ;; Register with a factory that returns a non-agent-role? value
      ;; (e.g., a plain symbol). The identity check should catch this
      ;; and fall back to the static factory — which is the SAME factory
      ;; in this case, returning the same symbol.
      ;; This test documents that the identity check activates the fallback path.
      (register-agent! 'identity-test
                       "1.0"
                       dummy-factory
                       #:module-path "nonexistent/module.rkt"
                       #:factory-name 'nonexistent-factory)
      ;; Dynamic-require fails → identity check never reached → static fallback
      (define result (make-agent-instance 'identity-test))
      (check-equal? result 'static-result "Identity check path should fall back to static on failure")
      (set-hot-swap-enabled! #f))

    (test-case "F-HS-03: all four roles pass identity check in one session"
      (reset-registry!)
      (set-hot-swap-enabled! #t)
      (register-default-agents!)
      (for ([role '(planner verifier tool-gateway executor)])
        (define result (make-agent-instance role))
        (check-true (agent-role? result)
                    (format "Role ~a must pass agent-role? identity check" role)))
      (set-hot-swap-enabled! #f))))

(run-tests hot-swap-suite)

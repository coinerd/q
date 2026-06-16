#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-hot-swap-deployment-gate.rkt
;; v0.99.18 W2: Hot-Swap Deployment Gate Tests (F-HS-02)
;;
;; Tests the hot-swap subsystem from a deployment perspective:
;;   1. Settings layer: mas.hot-swap.enabled config parsing
;;   2. Registry layer: feature gate + fallback chains
;;   3. Supervisor resolution: registry-based vs direct construction
;;   4. Auto-reload watcher: stays off by default
;;
;; These tests verify the infrastructure is sound BEFORE the W3
;; default flip. Post-flip, the same tests should still pass
;; (the settings-layer tests already test both states).

(require rackunit
         rackunit/text-ui
         "../agent/registry.rkt"
         "../agent/registry-types.rkt"
         "../agent/registry-defaults.rkt"
         "../agent/registry-watcher.rkt"
         (prefix-in settings: "../runtime/settings-query.rkt")
         "../runtime/settings-core.rkt"
         (only-in "../agent/roles/base.rkt" agent-role?)
         (only-in "../agent/roles/supervisor.rkt"
                  make-supervisor-role
                  current-use-registry
                  supervisor-role?))

;; ============================================================
;; Helpers
;; ============================================================

;; Build a q-settings from a flat hash (simulates config loading).
(define (settings-from-hash h)
  (q-settings (hash) (hash) h))

(define empty-settings (settings-from-hash (hash)))

(define hot-swap-true-settings (settings-from-hash (hash 'mas (hash 'hot-swap (hash 'enabled #t)))))

(define hot-swap-false-settings (settings-from-hash (hash 'mas (hash 'hot-swap (hash 'enabled #f)))))

(define hot-swap-string-true (settings-from-hash (hash 'mas (hash 'hot-swap (hash 'enabled "true")))))

(define hot-swap-string-false
  (settings-from-hash (hash 'mas (hash 'hot-swap (hash 'enabled "false")))))

(define hot-swap-string-yes (settings-from-hash (hash 'mas (hash 'hot-swap (hash 'enabled "yes")))))

(define hot-swap-string-1 (settings-from-hash (hash 'mas (hash 'hot-swap (hash 'enabled "1")))))

;; ============================================================
;; Test Suite
;; ============================================================

(define deployment-gate-suite
  (test-suite "Hot-Swap Deployment Gate (v0.99.18 W2, F-HS-02)"

    ;; ============================================================
    ;; DG-1: Settings layer — hot-swap-enabled? config parsing
    ;; ============================================================

    (test-case "DG-1a: hot-swap-enabled? returns #f for empty settings (pre-flip default)"
      ;; Pre-flip: default is #f. Post-flip (W3): default will be #t.
      ;; This test will be updated in W3 to check #t.
      (check-false (settings:hot-swap-enabled? empty-settings)
                   "pre-flip: empty settings should default to #f"))

    (test-case "DG-1b: hot-swap-enabled? returns #t when config explicitly enables"
      (check-true (settings:hot-swap-enabled? hot-swap-true-settings)
                  "explicit #t in config should return #t"))

    (test-case "DG-1c: hot-swap-enabled? returns #f when config explicitly disables"
      (check-false (settings:hot-swap-enabled? hot-swap-false-settings)
                   "explicit #f in config should return #f"))

    (test-case "DG-1d: hot-swap-enabled? parses string 'true' as enabled"
      (check-true (settings:hot-swap-enabled? hot-swap-string-true)
                  "string 'true' should be parsed as #t"))

    (test-case "DG-1e: hot-swap-enabled? parses string 'false' as disabled"
      (check-false (settings:hot-swap-enabled? hot-swap-string-false)
                   "string 'false' should be parsed as #f"))

    (test-case "DG-1f: hot-swap-enabled? parses 'yes' and '1' as enabled"
      (check-true (settings:hot-swap-enabled? hot-swap-string-yes)
                  "string 'yes' should be parsed as #t")
      (check-true (settings:hot-swap-enabled? hot-swap-string-1) "string '1' should be parsed as #t"))

    ;; ============================================================
    ;; DG-2: Registry layer — feature gate toggles + fallbacks
    ;; ============================================================

    (test-case "DG-2a: set-hot-swap-enabled! toggles runtime gate"
      (set-hot-swap-enabled! #f)
      (check-false (hot-swap-enabled?))
      (set-hot-swap-enabled! #t)
      (check-true (hot-swap-enabled?))
      (set-hot-swap-enabled! #f))

    (test-case "DG-2b: load-agent-dynamically falls back on namespace error"
      (reset-registry!)
      (set-hot-swap-enabled! #t)
      (register-agent! 'fallback-test
                       "1.0"
                       (lambda () 'static-fallback-ok)
                       #:module-path "totally/nonexistent/path.rkt"
                       #:factory-name 'no-such-factory)
      (define result (make-agent-instance 'fallback-test))
      (check-equal? result
                    'static-fallback-ok
                    "Should fall back to static factory on dynamic-require error")
      (set-hot-swap-enabled! #f))

    (test-case "DG-2c: all four roles load successfully with hot-swap ON"
      (reset-registry!)
      (set-hot-swap-enabled! #t)
      (register-default-agents!)
      (for ([role '(planner verifier tool-gateway executor)])
        (define result (make-agent-instance role))
        (check-true (agent-role? result) (format "Role ~a should load and satisfy agent-role?" role)))
      (set-hot-swap-enabled! #f))

    ;; ============================================================
    ;; DG-3: Supervisor resolution paths
    ;; ============================================================

    (test-case "DG-3a: supervisor without registry uses direct construction"
      (reset-registry!)
      (parameterize ([current-use-registry #f])
        (define sup (make-supervisor-role))
        (check-not-false sup "supervisor should be created without registry")))

    (test-case "DG-3b: supervisor with registry enabled creates successfully"
      (reset-registry!)
      (register-default-agents!)
      (parameterize ([current-use-registry #t])
        (define sup (make-supervisor-role))
        (check-not-false sup "supervisor should be created with registry"))
      ;; Also verify it's a supervisor-role
      (parameterize ([current-use-registry #t])
        (define sup (make-supervisor-role))
        (check-true (supervisor-role? sup) "should satisfy supervisor-role?")))

    (test-case "DG-3c: supervisor with registry ON but missing roles does not crash"
      ;; Empty registry + current-use-registry #t should still produce
      ;; a supervisor (graceful degradation — fallback to direct construction)
      (reset-registry!)
      (parameterize ([current-use-registry #t])
        (define sup (make-supervisor-role))
        (check-not-false sup "supervisor should be created even with empty registry")))

    ;; ============================================================
    ;; DG-4: Auto-reload watcher stays off by default
    ;; ============================================================

    (test-case "DG-4a: registry watcher is not running by default"
      (check-false (watcher-running?)
                   "watcher should not be running by default (auto-reload stays off)"))

    ;; ============================================================
    ;; DG-5: Session activity tracking
    ;; ============================================================

    (test-case "DG-5a: session-active? is #f after cleanup"
      (set-session-active! #t)
      (check-true (session-active?))
      (set-session-active! #f)
      (check-false (session-active?) "session-active? should be #f after cleanup"))

    (test-case "DG-5b: activate-agent-version! during session warns but succeeds"
      (reset-registry!)
      (set-session-active! #f)
      (register-agent! 'session-role "1.0" (lambda () 'v1))
      (register-agent! 'session-role "2.0" (lambda () 'v2))
      (set-session-active! #t)
      ;; Should not error — just log warning
      (activate-agent-version! 'session-role "2.0")
      (define desc (resolve-agent 'session-role))
      (check-not-false desc)
      (check-equal? (agent-descriptor-version desc) "2.0")
      (set-session-active! #f))

    ;; ============================================================
    ;; DG-6: End-to-end integration with hot-swap ON
    ;; ============================================================

    (test-case "DG-6a: full hot-swap cycle — register, enable, load, verify identity"
      (reset-registry!)
      (set-hot-swap-enabled! #f)
      (register-default-agents!)
      ;; Enable hot-swap and load all roles
      (set-hot-swap-enabled! #t)
      (for ([role '(planner verifier tool-gateway executor)])
        (define r (make-agent-instance role))
        (check-true (agent-role? r) (format "~a should pass identity check with hot-swap ON" role)))
      ;; Disable and verify static path still works
      (set-hot-swap-enabled! #f)
      (for ([role '(planner verifier tool-gateway executor)])
        (define r (make-agent-instance role))
        (check-true (agent-role? r)
                    (format "~a should pass identity check with hot-swap OFF" role))))))

(run-tests deployment-gate-suite)

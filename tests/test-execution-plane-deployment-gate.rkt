#lang racket/base

;; tests/test-execution-plane-deployment-gate.rkt
;; Deployment gate tests for execution plane default-on (F-EP-06 / Phase 3).
;;
;; Verifies that mas.execution-plane.enabled defaults to #t (Phase 3)
;; and that the flag can still be explicitly disabled.

;; @speed fast
(require rackunit
         rackunit/text-ui
         (only-in "../runtime/settings-core.rkt" q-settings)
         (only-in "../runtime/settings-query.rkt"
                  execution-plane-enabled?
                  execution-plane-timeout-ms))

;; ── Helpers ─────────────────────────────────────────────────────

;; Build a q-settings with a given merged hash.
(define (make-test-settings merged-hash)
  (q-settings #f #f merged-hash))

;; Sentinel to distinguish "not provided" from "provided as #f"
(define UNSET (gensym 'unset))

(define (make-mas-settings #:enabled [enabled UNSET] #:timeout-ms [timeout-ms UNSET])
  (define enabled-pair
    (if (eq? enabled UNSET)
        '()
        (list 'enabled enabled)))
  (define timeout-pair
    (if (eq? timeout-ms UNSET)
        '()
        (list 'timeout-ms timeout-ms)))
  (define ep-keys (append enabled-pair timeout-pair))
  (define mas-hash
    (if (null? ep-keys)
        (hasheq)
        (hasheq 'execution-plane (apply hasheq ep-keys))))
  (hasheq 'mas mas-hash))

;; ── Test Suite ──────────────────────────────────────────────────

(define suite
  (test-suite "Execution Plane Deployment Gate (F-EP-06 / Phase 3)"

    ;; ── Default value: enabled by default ──

    (test-case "execution-plane-enabled? defaults to #t (Phase 3)"
      ;; No mas key in merged settings at all
      (define settings (make-test-settings (hasheq)))
      (check-true (execution-plane-enabled? settings) "execution-plane-enabled? must default to #t"))

    (test-case "execution-plane-enabled? returns #t when mas key exists but no execution-plane sub-key"
      (define settings (make-test-settings (hasheq 'mas (hasheq))))
      (check-true (execution-plane-enabled? settings)))

    (test-case "execution-plane-enabled? returns #t when execution-plane key exists but no enabled sub-key"
      (define settings (make-test-settings (make-mas-settings)))
      (check-true (execution-plane-enabled? settings)))

    ;; ── Explicit disable ──

    (test-case "execution-plane-enabled? returns #f when explicitly disabled"
      (define settings (make-test-settings (make-mas-settings #:enabled #f)))
      (check-false (execution-plane-enabled? settings)))

    (test-case "execution-plane-enabled? returns #t when explicitly enabled"
      (define settings (make-test-settings (make-mas-settings #:enabled #t)))
      (check-true (execution-plane-enabled? settings)))

    ;; ── Timeout default (unchanged) ──

    (test-case "execution-plane-timeout-ms defaults to 120000"
      (define settings (make-test-settings (hasheq)))
      (check-equal? (execution-plane-timeout-ms settings) 120000))

    (test-case "execution-plane-timeout-ms respects explicit value"
      (define settings (make-test-settings (make-mas-settings #:timeout-ms 30000)))
      (check-equal? (execution-plane-timeout-ms settings) 30000))))

;; ── Run ─────────────────────────────────────────────────────────

(run-tests suite)

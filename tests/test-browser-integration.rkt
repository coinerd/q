#lang racket/base

;; tests/test-browser-integration.rkt — Regression tests for browser-integration extraction (AXIS1-F01)
;; @speed fast  ;; @suite runtime
;; Verifies that extracted browser adapter functions work independently.

(require rackunit
         (only-in "../runtime/browser-integration.rkt"
                  make-mock-browser-adapter
                  try-make-playwright-adapter
                  configure-session-browser!)
         (only-in "../browser/adapter.rkt" browser-adapter?)
         (only-in "../browser/settings.rkt"
                  default-browser-settings
                  browser-settings-sidecar-path
                  browser-settings-sidecar-timeout-ms
                  browser-settings-headless?))

(test-case "make-mock-browser-adapter: returns browser-adapter? struct"
  (define adapter (make-mock-browser-adapter))
  (check-true (browser-adapter? adapter)))

(test-case "make-mock-browser-adapter: produces distinct adapters"
  (define a1 (make-mock-browser-adapter))
  (define a2 (make-mock-browser-adapter))
  (check-not-eq? a1 a2))

(test-case "try-make-playwright-adapter: falls back to mock without node"
  (define defaults (default-browser-settings))
  (define-values (adapter state) (try-make-playwright-adapter defaults))
  (check-true (browser-adapter? adapter))
  (check-false state))

(test-case "configure-session-browser!: no crash with #f settings"
  (configure-session-browser! #f #f))

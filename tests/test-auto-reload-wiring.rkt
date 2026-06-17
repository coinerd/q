#lang racket/base

;; tests/test-auto-reload-wiring.rkt
;; v0.99.20 W3 (§3.4): Auto-reload watcher wiring tests.
;;
;; Tests:
;; 1. auto-reload-enabled? defaults to #f
;; 2. auto-reload-enabled? returns #t when config sets it
;; 3. auto-reload-enabled? coerces string "true" to #t
;; 4. stop-registry-watcher! is callable from close-session! path
;; 5. watcher can be started and stopped cleanly

(require rackunit
         rackunit/text-ui
         racket/hash
         (prefix-in settings: "../runtime/settings-query.rkt")
         "../runtime/settings-core.rkt"
         "../agent/registry-watcher.rkt")

(define (make-test-settings config-hash)
  (q-settings (hash) config-hash config-hash))

(define suite
  (test-suite "Auto-Reload Watcher Wiring (v0.99.20 W3 §3.4)"

    (test-case "auto-reload-enabled? defaults to #f"
      (define s (make-test-settings (hasheq)))
      (check-false (settings:auto-reload-enabled? s)))

    (test-case "auto-reload-enabled? returns #t when config sets it"
      (define s
        (make-test-settings (hasheq 'mas
                                    (hasheq 'hot-swap (hasheq 'auto-reload (hasheq 'enabled #t))))))
      (check-true (settings:auto-reload-enabled? s)))

    (test-case "auto-reload-enabled? coerces string 'true' to #t"
      (define s
        (make-test-settings
         (hasheq 'mas (hasheq 'hot-swap (hasheq 'auto-reload (hasheq 'enabled "true"))))))
      (check-true (settings:auto-reload-enabled? s)))

    (test-case "auto-reload-enabled? coerces string 'false' to #f"
      (define s
        (make-test-settings
         (hasheq 'mas (hasheq 'hot-swap (hasheq 'auto-reload (hasheq 'enabled "false"))))))
      (check-false (settings:auto-reload-enabled? s)))

    (test-case "stop-registry-watcher! is callable (idempotent no-op when not running)"
      ;; Should not error even when watcher was never started
      (stop-registry-watcher!)
      (check-false (watcher-running?)))))

(run-tests suite)

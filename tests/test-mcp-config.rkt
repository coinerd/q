#lang racket

;; @speed fast  ;; @suite runtime

;; tests/test-mcp-config.rkt — W4 (v0.99.9) MCP Config Settings Tests
;;
;; Tests MCP config query functions in runtime/settings-query.rkt.
;; All MCP settings default to disabled/false — zero behavioral change.

(require rackunit
         rackunit/text-ui
         "../runtime/settings-query.rkt"
         (only-in "../runtime/settings.rkt" q-settings))

;; Helper: construct q-settings with nested mas.mcp path
(define (make-mcp-settings mas-hash)
  (define merged (hash 'mas mas-hash))
  (q-settings (hash) (hash) merged))

;; Helper: create settings from nested specs like '((mas mcp enabled) #t)
(define (make-settings-from-paths . path-vals)
  (define merged
    (for/fold ([acc (hash)]) ([pv (in-list path-vals)])
      (merge-nested acc (car pv) (cadr pv))))
  (q-settings (hash) (hash) merged))

;; Recursively merge a nested path into a hash
(define (merge-nested h path value)
  (cond
    [(null? path) value]
    [(null? (cdr path)) (hash-set h (car path) value)]
    [else (hash-set h (car path) (merge-nested (hash-ref h (car path) (hash)) (cdr path) value))]))

(define suite
  (test-suite "MCP Config Settings (v0.99.9 W4)"

    ;; ── mcp-enabled? ──

    (test-case "mcp-enabled? defaults to #f"
      (define settings (make-mcp-settings (hash)))
      (check-false (mcp-enabled? settings)))

    (test-case "mcp-enabled? returns #t when mas.mcp.enabled is true"
      (define settings (make-settings-from-paths '((mas mcp enabled) #t)))
      (check-true (mcp-enabled? settings)))

    (test-case "mcp-enabled? accepts string true"
      (define settings (make-settings-from-paths '((mas mcp enabled) "true")))
      (check-true (mcp-enabled? settings)))

    (test-case "mcp-enabled? accepts string yes"
      (define settings (make-settings-from-paths '((mas mcp enabled) "yes")))
      (check-true (mcp-enabled? settings)))

    (test-case "mcp-enabled? returns #f for string false"
      (define settings (make-settings-from-paths '((mas mcp enabled) "false")))
      (check-false (mcp-enabled? settings)))

    ;; ── mcp-server-enabled? ──

    (test-case "mcp-server-enabled? defaults to #f"
      (define settings (make-mcp-settings (hash)))
      (check-false (mcp-server-enabled? settings)))

    (test-case "mcp-server-enabled? returns #t when set"
      (define settings (make-settings-from-paths '((mas mcp server enabled) #t)))
      (check-true (mcp-server-enabled? settings)))

    (test-case "mcp-server-enabled? accepts string 1"
      (define settings (make-settings-from-paths '((mas mcp server enabled) "1")))
      (check-true (mcp-server-enabled? settings)))

    ;; ── mcp-server-transport ──

    (test-case "mcp-server-transport defaults to stdio"
      (define settings (make-mcp-settings (hash)))
      (check-equal? (mcp-server-transport settings) "stdio"))

    (test-case "mcp-server-transport rejects unsupported Phase 1 transports"
      (define settings (make-settings-from-paths '((mas mcp server transport) "tcp")))
      (check-equal? (mcp-server-transport settings) "stdio"))

    (test-case "mcp-server-transport coerces symbol to string"
      (define settings (make-settings-from-paths (list '(mas mcp server transport) 'stdio)))
      (check-equal? (mcp-server-transport settings) "stdio"))

    (test-case "mcp-server-transport defaults for invalid types (M5 fix)"
      ;; v0.99.10 W1: non-string/symbol values return "stdio" instead of crashing
      (check-equal? (mcp-server-transport (make-settings-from-paths '((mas mcp server transport) #t)))
                    "stdio"
                    "boolean transport returns default")
      (check-equal? (mcp-server-transport (make-settings-from-paths '((mas mcp server transport) 42)))
                    "stdio"
                    "number transport returns default"))

    ;; ── mcp-client-servers ──

    (test-case "mcp-client-servers defaults to empty list"
      (define settings (make-mcp-settings (hash)))
      (check-equal? (mcp-client-servers settings) '()))

    (test-case "mcp-client-servers returns configured list"
      (define settings
        (make-settings-from-paths (list '(mas mcp client servers) (list "/usr/bin/mcp-server"))))
      (check-equal? (mcp-client-servers settings) (list "/usr/bin/mcp-server")))

    (test-case "mcp-client-servers returns empty for non-list"
      (define settings (make-settings-from-paths '((mas mcp client servers) "not-a-list")))
      (check-equal? (mcp-client-servers settings) '()))

    ;; ── broker-enabled? ──

    (test-case "broker-enabled? always returns #f in Phase 1"
      (define settings (make-mcp-settings (hash)))
      (check-false (broker-enabled? settings)))

    (test-case "broker-enabled? returns #t when set in config"
      (define settings (make-settings-from-paths '((mas broker enabled) #t)))
      (check-true (broker-enabled? settings)))))

(run-tests suite)

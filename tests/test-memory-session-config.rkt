#lang racket/base
;; tests/test-memory-session-config.rkt — Memory config wiring tests
;;
;; Verifies: memory disabled by default, config accessors work,
;; existing session-config behavior unchanged when memory key absent.

(require racket/dict
         rackunit
         "../runtime/session/session-config.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/backends/memory-hash.rkt")

;; ---------------------------------------------------------------------------
;; Default behavior: memory disabled
;; ---------------------------------------------------------------------------

(test-case "memory-backend defaults to #f"
  (define cfg (hash->session-config (hash)))
  (check-false (config-memory-backend cfg)))

(test-case "memory-enabled? defaults to #f"
  (define cfg (hash->session-config (hash)))
  (check-false (config-memory-enabled? cfg)))

(test-case "memory-backend not in known-keys is preserved"
  ;; Normalize should accept memory-backend without warning
  (define cfg (hash->session-config (hash 'memory-backend 'something)))
  (check-equal? (config-memory-backend cfg) 'something)
  (check-true (config-memory-enabled? cfg)))

;; ---------------------------------------------------------------------------
;; Memory backend can be set
;; ---------------------------------------------------------------------------

(test-case "config with in-memory hash backend"
  (define backend (make-memory-hash-backend))
  (define cfg (hash->session-config (hash 'memory-backend backend)))
  (check-equal? (config-memory-backend cfg) backend)
  (check-true (config-memory-enabled? cfg))
  (check-true (valid-backend? (config-memory-backend cfg))))

(test-case "config-memory-backend works with dict-set"
  (define cfg1 (hash->session-config (hash)))
  (check-false (config-memory-enabled? cfg1))
  (define backend (make-memory-hash-backend))
  (define cfg2 (dict-set cfg1 'memory-backend backend))
  (check-true (config-memory-enabled? cfg2)))

;; ---------------------------------------------------------------------------
;; Existing config accessors unchanged
;; ---------------------------------------------------------------------------

(test-case "existing accessors work with memory key present"
  (define cfg
    (hash->session-config
     (hash 'memory-backend 'dummy 'model-name "test-model" 'max-iterations 42 'thinking-level 'high)))
  (check-equal? (config-model-name cfg) "test-model")
  (check-equal? (config-max-iterations cfg) 42)
  (check-equal? (config-thinking-level cfg) 'high)
  (check-equal? (config-memory-backend cfg) 'dummy))

(test-case "session-config->hash includes memory-backend"
  (define backend (make-memory-hash-backend))
  (define cfg (hash->session-config (hash 'memory-backend backend)))
  (define h (session-config->hash cfg))
  (check-true (hash-has-key? h 'memory-backend)))

(test-case "session-config->hash works without memory-backend"
  (define cfg (hash->session-config (hash 'model-name "m")))
  (define h (session-config->hash cfg))
  (check-false (hash-has-key? h 'memory-backend)))

;; ---------------------------------------------------------------------------
;; Memory disabled preserves existing behavior
;; ---------------------------------------------------------------------------

(test-case "disabled memory does not affect other config values"
  (define cfg (hash->session-config (hash 'max-context-tokens 64000 'max-tokens 4096 'verbose? #t)))
  (check-false (config-memory-enabled? cfg))
  (check-equal? (config-max-context-tokens cfg) 64000)
  (check-equal? (config-max-tokens cfg) 4096)
  (check-true (config-verbose? cfg)))

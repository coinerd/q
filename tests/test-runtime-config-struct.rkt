#lang racket/base

;; tests/test-runtime-config-struct.rkt — Runtime config struct (W-03)
;;
;; Tests that build-runtime-from-cli returns a session-config,
;; that the immutable construction works, and that dict-ref access
;; is equivalent to the old hash-ref access.

(require rackunit
         racket/dict
         "../runtime/session-config.rkt"
         "../runtime/settings.rkt")

;; ============================================================
;; session-config construction and accessors
;; ============================================================

(test-case "session-config wraps immutable hash"
  (define h (make-immutable-hash '((a . 1) (b . "two"))))
  (define cfg (session-config h))
  (check-true (session-config? cfg))
  (check-equal? (dict-ref cfg 'a) 1)
  (check-equal? (dict-ref cfg 'b) "two"))

(test-case "hash->session-config conversion"
  (define h
    (make-immutable-hash '((provider . test-prov) (model-name . "gpt-4") (session-dir . "/tmp"))))
  (define cfg (hash->session-config h))
  (check-true (session-config? cfg))
  (check-equal? (config-model-name cfg) "gpt-4")
  (check-equal? (config-session-dir cfg) "/tmp"))

(test-case "hash->session-config with mutable hash"
  (define h (make-hash '((provider . test-prov) (model-name . "claude-3"))))
  (define cfg (hash->session-config h))
  (check-true (session-config? cfg))
  (check-equal? (config-model-name cfg) "claude-3"))

(test-case "session-config->hash round-trip"
  (define h (make-immutable-hash '((a . 1) (b . 2))))
  (define cfg (hash->session-config h))
  (define h2 (session-config->hash cfg))
  (check-equal? (dict-ref h2 'a) 1)
  (check-equal? (dict-ref h2 'b) 2))

(test-case "session-config gen:dict operations"
  (define cfg (hash->session-config (make-immutable-hash '((x . 10) (y . 20)))))
  (check-equal? (dict-ref cfg 'x) 10)
  (check-true (dict-has-key? cfg 'y))
  (check-false (dict-has-key? cfg 'z))
  (define cfg2 (dict-set cfg 'z 30))
  (check-equal? (dict-ref cfg2 'z) 30)
  (check-false (dict-has-key? cfg 'z)))

(test-case "config accessors with defaults"
  (define cfg (hash->session-config (make-immutable-hash)))
  (check-false (config-provider cfg))
  (check-false (config-tool-registry cfg))
  (check-equal? (config-system-instructions cfg) '())
  (check-equal? (config-max-iterations cfg) 50)
  (check-equal? (config-thinking-level cfg) 'medium))

(test-case "config accessors with values"
  (define cfg
    (hash->session-config (make-immutable-hash '((provider . my-prov) (model-name . "test-model")
                                                                      (max-iterations . 100)
                                                                      (session-dir . "/tmp/test")))))
  (check-equal? (config-provider cfg) 'my-prov)
  (check-equal? (config-model-name cfg) "test-model")
  (check-equal? (config-max-iterations cfg) 100)
  (check-equal? (config-session-dir cfg) "/tmp/test"))

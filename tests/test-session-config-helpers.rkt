#lang racket/base

;; test-session-config-helpers.rkt — Tests for session config helpers (T3-2/3/4)
;; Part of v0.80.5 Polish Sweep

(require rackunit
         racket/contract
         "../runtime/session/session-config.rkt"
         "../runtime/session/session-types.rkt")

(test-case "session-config: hash->session-config creates config"
  (define cfg (hash->session-config (hasheq 'model "test-model")))
  (check-pred session-config? cfg))

(test-case "session-config: round-trip hash conversion"
  (define h (hasheq 'model "test-model" 'provider 'openai))
  (define cfg (hash->session-config h))
  (define h2 (session-config->hash cfg))
  (check-equal? (hash-ref h2 'model) "test-model"))

(test-case "session-config: config-model-name accessor works"
  (define cfg (hash->session-config (hasheq 'model-name "test-model")))
  (check-equal? (config-model-name cfg) "test-model"))

(test-case "agent-session: predicate exists and is a procedure"
  (check-pred procedure? agent-session?))

(module+ main
  (require rackunit/text-ui)
  (run-tests 'test-session-config-helpers))

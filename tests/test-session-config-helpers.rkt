#lang racket/base

;; @speed fast
;; @suite runtime

;; test-session-config-helpers.rkt — Tests for session config helpers (T3-2/3/4)
;; Part of v0.80.5 Polish Sweep

(require rackunit
         rackunit/text-ui
         racket/contract
         "../runtime/session/session-config.rkt"
         "../runtime/session/session-types.rkt")

(define session-config-tests
  (test-suite "session-config-helpers"

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
      (check-pred procedure? agent-session?))))

(module+ test
  (run-tests session-config-tests))

(module+ main
  (run-tests session-config-tests))

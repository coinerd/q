#lang racket

;; @speed fast
;; @suite default
;; @boundary unit
;; BOUNDARY: integration

;; tests/test-provider-retry-ceiling-config.rkt
;; PN-7: per-model retry-ceiling-secs config override.
;;
;; Verifies that the cumulative retry ceiling resolution in turn-orchestrator
;; reads `providers.<name>.retry-ceiling-secs` from session-config settings
;; and overrides the module default when present.

(require rackunit
         "../runtime/turn-orchestrator.rkt"
         "../runtime/session/session-config.rkt"
         "../runtime/settings-core.rkt"
         "../runtime/auto-retry.rkt")

;; Helper: build a session-config with the given merged settings hash and model name.
(define (make-config-with-settings merged-settings model-name)
  (hash->session-config
   (hash 'model-name model-name 'settings (q-settings (hash) (hash) merged-settings))))

(test-case "PN-7: providers.<name>.retry-ceiling-secs overrides the default"
  (define cfg
    (make-config-with-settings (hasheq 'providers
                                       (hasheq 'deepseek-v4-flash (hasheq 'retry-ceiling-secs 45)))
                               "deepseek-v4-flash"))
  (check-equal? (resolve-retry-ceiling-secs cfg) 45))

(test-case "PN-7: a different model's override does not leak to the active model"
  (define cfg
    (make-config-with-settings (hasheq 'providers
                                       (hasheq 'other-model (hasheq 'retry-ceiling-secs 45)))
                               "deepseek-v4-flash"))
  (check-equal? (resolve-retry-ceiling-secs cfg) default-cumulative-ceiling-secs))

(test-case "PN-7: absent setting falls back to the module default"
  (define cfg (make-config-with-settings (hash) "deepseek-v4-flash"))
  (check-equal? (resolve-retry-ceiling-secs cfg) default-cumulative-ceiling-secs))

(test-case "PN-7: absent model-name falls back to the module default"
  (define cfg
    (make-config-with-settings (hasheq 'providers
                                       (hasheq 'deepseek-v4-flash (hasheq 'retry-ceiling-secs 45)))
                               #f))
  (check-equal? (resolve-retry-ceiling-secs cfg) default-cumulative-ceiling-secs))

#lang racket/base

;; wiring/mode-helpers.rkt — Extracted helpers from run-modes.rkt
;;
;; Purpose: Reduce require fan-in in run-modes.rkt by bundling
;; security configuration, timeout wiring, trace logger setup,
;; and project tree initialization into a single module.
;; Layer: wiring (interface construction helpers)
;;
;; NOTE (v0.29.14): This module still imports tools/builtins/bash.rkt
;; and sandbox/subprocess.rkt directly. True decoupling would move
;; those current-parameter bindings into runtime/settings.rkt.
;; run-modes.rkt decomposition deferred to v0.29.15.

(require (only-in "../runtime/settings.rkt"
                  setting-ref
                  security-config-from-settings
                  http-request-timeout
                  q-settings-merged
                  setting-memory-injection-budget)
         (only-in "../tools/builtins/bash.rkt" current-execution-policy current-allowed-commands)
         (only-in "../sandbox/subprocess.rkt"
                  current-secret-scrub-denylist
                  current-secret-scrub-allowlist)
         (only-in "../llm/stream.rkt" current-http-request-timeout current-model-timeouts)
         (only-in "../runtime/trace-logger.rkt" make-trace-logger start-trace-logger!)
         (only-in "../runtime/project-tree.rkt" project-tree->string)
         (only-in "../runtime/context-assembly/memory-builder.rkt" current-memory-injection-budget))

(provide wire-security-config!
         wire-timeouts!
         wire-memory-settings!
         make-trace-logger
         start-trace-logger!
         project-tree->string)

;; Apply security settings from config to current parameters.
;; Extracted from build-runtime-from-cli to reduce fan-in.
(define (wire-security-config! settings)
  (define sec-config (security-config-from-settings settings))
  (define policy-mode (hash-ref sec-config 'execution-policy-mode #f))
  (when policy-mode
    (current-execution-policy (if (string? policy-mode)
                                  (string->symbol policy-mode)
                                  policy-mode)))
  (define allowed-cmds (hash-ref sec-config 'execution-policy-allowed '()))
  (when (and (pair? allowed-cmds) (eq? (current-execution-policy) 'allowlist))
    (current-allowed-commands allowed-cmds))
  (define extra-denylist (hash-ref sec-config 'secret-scrub-extra-denylist '()))
  (when (pair? extra-denylist)
    (current-secret-scrub-denylist (map (lambda (s)
                                          (if (regexp? s)
                                              s
                                              (regexp s)))
                                        extra-denylist)))
  (define scrub-allowlist (hash-ref sec-config 'secret-scrub-allowlist '()))
  (when (pair? scrub-allowlist)
    (current-secret-scrub-allowlist (map (lambda (s)
                                           (if (regexp? s)
                                               s
                                               (regexp s)))
                                         scrub-allowlist))))

;; Apply timeout settings from config to current parameters.
;; Used in both build-runtime-from-cli and reload-config!.
(define (wire-timeouts! settings)
  (define models-config
    (hash-ref (hash-ref (q-settings-merged settings) 'timeouts (hash)) 'models (hash)))
  (define model-timeouts
    (for/fold ([acc (hash)]) ([(k v) (in-hash models-config)])
      (if (and (hash? v) (hash-has-key? v 'request))
          (hash-set acc
                    (if (symbol? k)
                        (symbol->string k)
                        k)
                    (hash-ref v 'request))
          acc)))
  (current-model-timeouts model-timeouts)
  (current-http-request-timeout (http-request-timeout settings)))

;; Apply memory settings from config to current parameters.
;; Sets injection budget from settings.
;; Auto-extraction is controlled by current-auto-extraction-enabled parameter directly.
(define (wire-memory-settings! settings)
  (define budget (setting-memory-injection-budget settings))
  (when budget
    (current-memory-injection-budget budget)))

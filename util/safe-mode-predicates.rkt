#lang racket/base

;; util/safe-mode-predicates.rkt — canonical import point for safe-mode predicates
;;
;; ARCH-02 (v0.22.0): Re-exports from util/safe-mode-state.rkt (peer import).
;; No more upward import from runtime/safe-mode.rkt.
;;
;; Tools and scheduler import predicates from HERE so they don't
;; depend on the runtime layer directly.

(require racket/contract
         (only-in "safe-mode-state.rkt"
                  safe-mode?
                  allowed-tool?
                  allowed-path?
                  safe-mode-project-root
                  trust-level
                  blocked-tools))

(provide (contract-out [safe-mode? (-> boolean?)]
                       [allowed-tool? (-> string? boolean?)]
                       [allowed-path? (-> (or/c path? string?) boolean?)]
                       [safe-mode-project-root (-> string?)]
                       [trust-level (-> symbol?)])
         blocked-tools)

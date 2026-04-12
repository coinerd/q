#lang racket/base

;; util/safe-mode-predicates.rkt — canonical import point for safe-mode predicates
;;
;; Re-exports pure safe-mode query functions from runtime/safe-mode.rkt.
;; Tools and scheduler import predicates from HERE (ARCH-02/QUAL-07).
;;
;; The actual definitions live in runtime/safe-mode.rkt alongside
;; the parameters and struct they read from.

(require (only-in "../runtime/safe-mode.rkt"
                  safe-mode? allowed-tool? allowed-path?
                  safe-mode-project-root trust-level
                  blocked-tools))

(provide safe-mode? allowed-tool? allowed-path? safe-mode-project-root
         trust-level blocked-tools)

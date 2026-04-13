#lang racket/base

;; util/safe-mode-predicates.rkt — canonical import point for safe-mode predicates
;;
;; Re-exports pure safe-mode query functions from runtime/safe-mode.rkt.
;; Tools and scheduler import predicates from HERE (ARCH-02/QUAL-07).
;;
;; *** LAYER EXCEPTION (ARCH-02) ***
;; util/ is normally a leaf layer with zero upward imports.  This file
;; breaks that rule by reaching into runtime/safe-mode.rkt so that
;; tool/scheduler modules can query safe-mode state without depending
;; on the runtime layer directly.  The re-export is narrow (only-in)
;; and the predicates are pure readers — no mutation leaks upward.
;; Do NOT add further upward imports from util/ without a documented
;; exception in this header.

(require (only-in "../runtime/safe-mode.rkt"
                  safe-mode? allowed-tool? allowed-path?
                  safe-mode-project-root trust-level
                  blocked-tools))

(provide safe-mode? allowed-tool? allowed-path? safe-mode-project-root
         trust-level blocked-tools)

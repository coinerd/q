#lang racket/base

;; extensions/gsd/transition-logic.rkt — GSD transition facade (W1 v0.99.89)
;;
;; W5 v0.99.35: Pure transition logic extracted from state-machine.rkt.
;; v0.99.89 W1: All transition computation now lives in the pure kernel
;; (transition-kernel.rkt). This module is a thin facade that:
;;   1. re-exports the kernel's neutral-domain bindings verbatim, and
;;   2. adapts the gsd-runtime-state struct to the kernel's neutral
;;      gsd-transition-state view for the two runtime-typed entry points
;;      (compute-next-gsm-state, check-state-invariants).
;;
;; Runtime-only concerns that stay here (NOT in the kernel):
;;   - wave-executor clearing when leaving executing mode
;;   - the executor-presence invariant (executing/verifying with waves
;;     requires an executor)
;;
;; Public API is unchanged — state-machine.rkt, responsibility-inventory.rkt,
;; runtime/task-memory/gsd-adapter.rkt and all tests import this module with
;; identical names. State names and persistence formats are untouched.

(require racket/match
         racket/set
         "runtime-state-types.rkt"
         "transition-kernel.rkt")

;; Re-export the pure kernel surface verbatim.
(provide (all-from-out "transition-kernel.rkt"))

;; Runtime-typed transition + invariant entry points (adapted here).
(provide compute-next-gsm-state
         check-state-invariants)

;; ============================================================
;; gsd-runtime-state → neutral kernel view
;; ============================================================

;; Projects the runtime aggregate onto the neutral transition state.
(define (rt->kernel rt)
  (make-gsd-transition-state (gsd-runtime-state-mode rt)
                             (gsd-runtime-state-total-waves rt)
                             (gsd-runtime-state-current-wave rt)
                             (gsd-runtime-state-completed-waves rt)))

;; ============================================================
;; Runtime-typed transition function
;; ============================================================

;; Pure transition kernel (Finding 3.1.3)
;; Compute next state without side effects.
;; Returns (values (or/c ok-result? err-result?) gsd-runtime-state?).
;; The kernel decides the transition on the neutral view; the facade applies
;; the mode change to the runtime aggregate and enforces the runtime policy
;; of clearing the wave-executor when leaving executing mode.
(define (compute-next-gsm-state current-state target #:event [event #f])
  (define current (gsd-runtime-state-mode current-state))
  (define-values (result neutral-next)
    (compute-next-state (rt->kernel current-state) target #:event event))
  (cond
    [(ok? result)
     (define state*
       (if (and (eq? current 'executing) (not (eq? target 'executing)))
           (struct-copy gsd-runtime-state current-state [wave-executor #f])
           current-state))
     (values result (struct-copy gsd-runtime-state state* [mode (gts-mode neutral-next)]))]
    [else (values result current-state)]))

;; ============================================================
;; Runtime-typed invariant checker
;; ============================================================

;; Returns (values ok? error-message-or-#f).
;; Checks the kernel's structural invariants on the neutral view, then the
;; facade-only executor-presence rule.
(define (check-state-invariants state)
  (define mode (gsd-runtime-state-mode state))
  (define tw (gsd-runtime-state-total-waves state))
  (define exec (gsd-runtime-state-wave-executor state))
  (define-values (kernel-ok? kernel-msg) (check-transition-invariants (rt->kernel state)))
  (cond
    [(not kernel-ok?) (values #f kernel-msg)]
    ;; If in executing/verifying, wave-executor should be set when waves exist
    [(and (memq mode '(executing verifying)) (> tw 0) (not exec))
     (values #f (format "in ~a with ~a waves but no wave-executor" mode tw))]
    [else (values #t #f)]))

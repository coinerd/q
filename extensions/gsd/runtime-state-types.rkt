#lang racket/base

;; extensions/gsd/runtime-state-types.rkt — Runtime state struct definition
;; STABILITY: evolving
;;
;; Defines the gsd-runtime-state struct used by both session-state.rkt
;; (storage) and state-machine.rkt (API). Kept separate to avoid
;; circular dependencies.

(require racket/set)

(provide gsd-runtime-state
         gsd-runtime-state?
         gsd-runtime-state-mode
         gsd-runtime-state-total-waves
         gsd-runtime-state-current-wave
         gsd-runtime-state-completed-waves
         gsd-runtime-state-wave-executor
         gsd-runtime-state-plan-path
         gsd-runtime-state-pinned-dir
         gsd-runtime-state-edit-limit
         gsd-runtime-state-transition-history
         make-initial-gsd-state)

;; Single canonical runtime state aggregate (F1 fix).
;; Replaces the hasheq-based state with a proper struct.
(struct gsd-runtime-state
        (mode ; symbol: idle|exploring|plan-written|executing|verifying
         total-waves ; non-negative integer
         current-wave ; non-negative integer
         completed-waves ; set of integers
         wave-executor ; wave-executor? | #f
         plan-path ; string | #f (path to active plan)
         pinned-dir ; string | #f
         edit-limit ; positive integer
         transition-history ; list of (from to timestamp)
         )
  #:transparent)

;; Default initial state
(define (make-initial-gsd-state)
  (gsd-runtime-state 'idle 0 0 (set) #f #f #f 500 '()))

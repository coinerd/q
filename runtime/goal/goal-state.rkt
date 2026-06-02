#lang racket/base

;; q/runtime/goal-state.rkt — Facade: re-exports from goal-types + goal-codec
;;
;; Backward-compatible facade. All identifiers previously provided
;; by this module are still available from the same import path.

(require "goal-types.rkt"
         "goal-codec.rkt")

(provide (all-from-out "goal-types.rkt")
         (all-from-out "goal-codec.rkt"))

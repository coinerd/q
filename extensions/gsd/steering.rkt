#lang racket/base

;; extensions/gsd/steering.rkt — Steering module (DISABLED)
;;
;; v0.21.3: All steering removed. The GSD prompt is now self-correcting
;; by design — no runtime guards, budgets, or steering messages.
;;
;; This module exports no-op stubs for backward compatibility with
;; any code that still calls these functions during the transition.

(provide gsd-steering-check
         reset-steering-state!
         stall-threshold)

(define stall-threshold 999)

(define (gsd-steering-check tool-name tool-args result)
  #f)

(define (reset-steering-state!)
  (void))

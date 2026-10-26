#lang racket

;; tests/test-gsd-planning-preflight-kernel.rkt — historical alias (W1).
;;
;; The plan-validation/preflight pins for W1 (GSD-SIMPLIFICATION-PLAN) now
;; live in tests/test-gsd-plan-validation.rkt (the canonical name the
;; wave's gate list uses). This module keeps the previous path working for
;; any reference to it: requiring the canonical module runs its checks in
;; this module's instantiation, so `raco test` on either path exercises the
;; same pins exactly once.

(require "test-gsd-plan-validation.rkt")

(displayln "PASS test-gsd-planning-preflight-kernel (alias -> test-gsd-plan-validation)")

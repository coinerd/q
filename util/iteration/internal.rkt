#lang racket/base

;; util/iteration/internal.rkt — Shared internal helpers for iteration sub-modules
;;
;; v0.99.86: Moved from runtime/iteration/internal.rkt to util/iteration/
;; for neutral ownership. Used by both Agent and Runtime iteration code.
;;
;; Extracted from main-loop.rkt and step-interpreter.rkt during v0.34.7
;; to eliminate duplication (finding A-02).

(require racket/contract)

(provide assert-payload)

(define (assert-payload topic-name payload ctrct)
  (unless (ctrct payload)
    (raise-argument-error topic-name "valid event payload" payload))
  payload)

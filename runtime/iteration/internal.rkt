#lang racket/base

;; runtime/iteration/internal.rkt — Shared internal helpers for iteration sub-modules
;;
;; Extracted from main-loop.rkt and step-interpreter.rkt during v0.34.7
;; to eliminate duplication (finding A-02).

(require racket/contract)

(provide assert-payload)

(define (assert-payload topic-name payload ctrct)
  (unless (ctrct payload)
    (raise-argument-error topic-name "valid event payload" payload))
  payload)

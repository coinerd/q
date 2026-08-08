#lang racket/base

;; runtime/iteration/internal.rkt — COMPATIBILITY RE-EXPORT
;;
;; v0.99.86: assert-payload moved to util/iteration/internal.rkt.
;; This file re-exports it for backward compatibility.
;; TODO: Remove once all consumers import from util/iteration/internal.rkt.

(require (only-in "../../util/iteration/internal.rkt" assert-payload))

(provide assert-payload)

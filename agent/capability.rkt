#lang racket/base

;; agent/capability.rkt — RE-EXPORT SHIM (deprecated, use util/capability.rkt)
;;
;; This file exists for backward compatibility during v0.99.x transition.
;; Will be removed in v0.100.
;;
;; The canonical location for capability definitions is now
;; util/capability.rkt, which eliminates the layer violation where
;; util/ and tools/ modules depended on agent/ (findings A1/M2, A4).

(require "../util/capability.rkt")

(provide (all-from-out "../util/capability.rkt"))

#lang racket/base
;; STABILITY: stable

;; interfaces/sdk.rkt — embeddable library surface (facade)
;;
;; v0.22.9 W3: Decomposed into:
;;   sdk-core.rkt   — runtime types, session operations, factories
;;   sdk-compat.rkt — enriched API aliases, tree API, GSD convenience
;; This module re-exports everything from both for backward compatibility.

(require "sdk-core.rkt"
         "sdk-compat.rkt")

;; WARNING: all-from-out leaks submodule additions -- keep submodules stable
(provide (all-from-out "sdk-core.rkt")
         (all-from-out "sdk-compat.rkt"))

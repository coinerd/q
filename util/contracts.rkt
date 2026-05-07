#lang racket/base

;; util/contracts.rkt — Shared contracts and predicates (foundation layer)
;;
;; Provides contracts needed by both runtime/ and extensions/,
;; eliminating upward imports from runtime/ to extensions/.

(require racket/contract
         (only-in "../extensions/api.rkt" extension-registry?))

(provide extension-registry?)

;; extension-registry? is re-exported from extensions/api.rkt.
;; Defined here so runtime/ can reference it without importing extensions/api.rkt.

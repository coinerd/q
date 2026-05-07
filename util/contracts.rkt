#lang racket/base

;; util/contracts.rkt — Shared contracts and predicates (foundation layer)
;;
;; Provides contracts needed by both runtime/ and extensions/,
;; eliminating upward imports from runtime/ to extensions/.

(require racket/contract
         "../extensions/api.rkt")

(provide extension-registry?)

;; Contract predicate for extension-registry structs.
;; Defined here so runtime/ can reference it without importing extensions/api.rkt.
;; The actual struct remains in extensions/api.rkt.
(define extension-registry? extension-registry?)

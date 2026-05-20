#lang racket/base

;; util/shared.rkt — Shared utility functions extracted from duplication sites
;;
;; v0.32.1 Wave 1: DRY extraction — single canonical definition for
;; functions previously duplicated across multiple modules.

(require racket/contract
         racket/list)

(provide (contract-out [take-at-most (-> list? exact-nonnegative-integer? list?)]))

;; take-at-most : (listof any/c) exact-nonnegative-integer? -> (listof any/c)
;; Returns the first n elements of lst, or lst if shorter than n.
;; Previously duplicated in tui/builtins.rkt, runtime/iteration/tool-turn-bridge.rkt,
;; and runtime/iteration/retry-policy.rkt.
(define (take-at-most lst n)
  (take lst (min n (length lst))))

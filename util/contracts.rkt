#lang racket/base

;; util/contracts.rkt — Shared contracts and predicates (foundation layer)
;;
;; Provides contracts needed by both runtime/ and extensions/,
;; eliminating upward imports from runtime/ to extensions/.
;;
;; NOTE: extension-registry? was previously re-exported here from
;; extensions/api.rkt, but that created a circular upward import.
;; Consumers now import directly from extensions/api.rkt.

(require racket/base)

(provide)

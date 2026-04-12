#lang racket/base

;; util/hook-types.rkt — canonical hook result types
;; Shared between extensions layer and agent core.
;; ARCH-01: extracted from extensions/hooks.rkt to eliminate layer violations.

(provide
 (struct-out hook-result)
 hook-pass hook-amend hook-block)

(struct hook-result (action payload) #:transparent)

(define (hook-pass [payload #f])
  (hook-result 'pass payload))

(define (hook-amend payload)
  (hook-result 'amend payload))

(define (hook-block [reason #f])
  (hook-result 'block reason))

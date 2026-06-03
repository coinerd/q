#lang racket/base
;; util/types/working-set.rkt — Shared type predicate (forwarding)
;; A1-02: Extracted from runtime/ to break agent→runtime upward dependency.
;; Agent layer imports from here instead of runtime/ directly.
(require (only-in "../../runtime/working-set.rkt" working-set?))
(provide working-set?)

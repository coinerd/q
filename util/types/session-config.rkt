#lang racket/base
;; util/types/session-config.rkt — Shared type predicate (forwarding)
;; A1-02: Extracted from runtime/ to break agent→runtime upward dependency.
;; Agent layer imports from here instead of runtime/ directly.
(require (only-in "../../runtime/session/session-config.rkt" session-config?))
(provide session-config?)

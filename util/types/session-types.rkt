#lang racket/base
;; util/types/session-types.rkt — Shared type predicate (forwarding)
;; A1-02: Extracted from runtime/ to break agent→runtime upward dependency.
;; Agent layer imports from here instead of runtime/ directly.
(require (only-in "../../runtime/session/session-types.rkt" agent-session?))
(provide agent-session?)

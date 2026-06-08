#lang racket/base

;; @speed fast  ;; @suite extensions

;; BOUNDARY: integration

;; tests/helpers/gsd-test-helpers.rkt — Test helper for GSD context
;;
;; Re-exports make-gsd-context from session-state.rkt for test convenience.
;; v0.32.5: Updated to re-export struct-based factory.

(provide make-gsd-context)

(require (only-in "../../extensions/gsd/session-state.rkt" make-gsd-context))

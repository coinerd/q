#lang racket/base

;; q/ui-core/feature-flags.rkt — Feature flag parameters for UI behavior
;;
;; STABILITY: internal
;;
;; Centralizes feature flags so both TUI and GUI can reference the same
;; parameter without coupling to each other.  Each flag defaults to the
;; legacy behavior so flipping it is a deliberate, reversible act.

(provide ui-reasoning-artifacts-enabled
         ui-reasoning-artifacts-max-bytes
         with-reasoning-artifacts
         without-reasoning-artifacts
         tui-multiline-composer-enabled
         tui-multiline-composer-max-rows
         with-multiline-composer
         without-multiline-composer)

;; ──────────────────────────────────────────────────────
;; tui.multiline-composer.enabled
;;
;; W5 (v0.99.96): default flipped to #t after validation. The shared
;; multiline composer (q/ui-core/composer-model +
;; q/ui-core/composer-layout) drives editing, visual lines, the
;; software cursor, and composer height. The legacy single-line
;; rendering path has been removed.
;; ──────────────────────────────────────────────────────
(define tui-multiline-composer-enabled
  (make-parameter #t))

;; Maximum number of composer text rows shown before an internal
;; vertical viewport (with indicator) kicks in. Minimum is always 1.
(define tui-multiline-composer-max-rows
  (make-parameter 6))

(define (with-multiline-composer thunk)
  (parameterize ([tui-multiline-composer-enabled #t])
    (thunk)))

(define (without-multiline-composer thunk)
  (parameterize ([tui-multiline-composer-enabled #f])
    (thunk)))

;; ──────────────────────────────────────────────────────
;; ui.reasoning.artifacts.enabled
;;
;; W5 (v0.99.96): default flipped to #t after validation. Durable
;; reasoning artifacts flow through the shared idempotent reducer and
;; are persisted per policy.
;; ──────────────────────────────────────────────────────
(define ui-reasoning-artifacts-enabled
  (make-parameter #t))

;; Maximum byte size of a reasoning artifact body at the persistence
;; boundary.  Enforced ONLY when persisting, never during live streaming.
(define ui-reasoning-artifacts-max-bytes
  (make-parameter (* 256 1024)))  ; 256 KiB default

;; Convenience wrappers for test isolation.
(define (with-reasoning-artifacts thunk)
  (parameterize ([ui-reasoning-artifacts-enabled #t])
    (thunk)))

(define (without-reasoning-artifacts thunk)
  (parameterize ([ui-reasoning-artifacts-enabled #f])
    (thunk)))

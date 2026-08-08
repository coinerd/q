#lang racket/base

;; util/exn.rkt — Shared exception types for cross-layer error recovery.
;;
;; Defines exception types that carry recovery data across layer boundaries.
;; Both Agent and Runtime import from this module.
;;
;; Architecture: replaces dynamic parameters (current-partial-text,
;; current-loop-state-for-error-recovery) that were used as hidden
;; side-channels for error recovery data.

(provide (struct-out exn:fail:stream-error))

;; ============================================================
;; Stream error with recovery data
;; ============================================================

;; Raised by stream-from-provider when a provider stream error occurs
;; after partial output has been accumulated. Carries:
;;   partial-text     — accumulated text string (or #f if none)
;;   partial-messages — messages from loop-state at time of error
;;   original-exn     — the underlying exception that caused the failure
;;
;; The exception is exn:fail? so it propagates through standard handlers.
;; Consumers:
;;   - call-with-provider-retry: extracts partial-text for retry recovery
;;   - session-lifecycle.rkt: extracts partial-messages for transcript flush
;;
;; The message field mirrors the original exception's message so that
;; error classification (which reads exn-message) continues to work.
(struct exn:fail:stream-error exn:fail (partial-text partial-messages original-exn) #:transparent)

#lang racket/base

;; llm/provider-errors.rkt — Structured error types for LLM providers
;;
;; Provides a typed error hierarchy so that auto-retry and callers
;; can classify errors by category instead of string matching.
;;
;; Categories:
;;   'rate-limit       — HTTP 429, quota exceeded
;;   'timeout          — connection/read timeout
;;   'auth             — HTTP 401/403, authentication failure
;;   'context-overflow — input too long for model context
;;   'server           — HTTP 5xx
;;   'network          — DNS failure, connection refused

(require racket/contract)

(provide (struct-out provider-error)
         provider-error?
         provider-error-category
         provider-error-status-code
         raise-provider-error
         classify-http-status)

;; ============================================================
;; Struct
;; ============================================================

;; Subtype of exn:fail with structured metadata.
(struct provider-error exn:fail (status-code category) #:transparent)

;; ============================================================
;; Constructor helper
;; ============================================================

;; Raise a provider-error with the given category and optional HTTP status code.
(define (raise-provider-error message category [status-code #f])
  (raise (provider-error message (current-continuation-marks) status-code category)))

;; ============================================================
;; HTTP status → category mapping
;; ============================================================

;; Classify an HTTP status code into a provider-error category symbol.
;; Returns #f if the status code is not an error (< 400).
(define (classify-http-status status-code)
  (cond
    [(not (and (exact-integer? status-code) (>= status-code 400))) #f]
    [(= status-code 401) 'auth]
    [(= status-code 403) 'auth]
    [(= status-code 429) 'rate-limit]
    [(>= status-code 500) 'server]
    [else 'network]))

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

(require racket/contract
         racket/string
         (only-in "../util/error/errors.rkt"
                  q-error
                  q-error?
                  q-error-context
                  q-llm-error
                  q-llm-error?
                  q-llm-error-category))

(provide provider-error
         provider-error?
         provider-error-category
         ;; v1.00.13 W3 (#9473): structured failure-context accessor (RL-7) —
         ;; alias of the inherited q-error context field, named for the
         ;; provider boundary so retry code reads intent.
         provider-error-context
         provider-error-status-code
         q-llm-error?
         transient-provider-error-categories
         (contract-out [raise-provider-error (->* (string? symbol?) ((or/c exact-integer? #f)) any)]
                       [classify-http-status (-> (or/c exact-integer? #f) (or/c symbol? #f))]
                       [provider-error-transient? (-> provider-error? boolean?)]
                       [transient-llm-failure? (-> any/c boolean?)]))

;; ============================================================
;; Struct
;; ============================================================

;; Subtype of q-llm-error (Branch 1: LLM errors).
;; provider-error inherits category from q-llm-error and adds status-code.
(struct provider-error q-llm-error (status-code) #:transparent)

;; Backward-compat: provider-error-category reads from q-llm-error parent field.
(define provider-error-category q-llm-error-category)

;; v1.00.13 W3 (#9473): structured failure context accessor (RL-5/RL-7).
(define provider-error-context q-error-context)

;; ============================================================
;; Constructor helper
;; ============================================================

;; Raise a provider-error with the given category and optional HTTP status code.
(define (raise-provider-error message category [status-code #f])
  (raise (provider-error message (current-continuation-marks) (hash) category status-code)))

;; ============================================================
;; HTTP status → category mapping
;; ============================================================

;; Classify an HTTP status code into a provider-error category symbol.
;; Returns #f if the status code is not an error (< 400).
(define (classify-http-status status-code)
  (cond
    [(not (and (exact-integer? status-code) (>= status-code 400))) #f]
    [(= status-code 400) 'bad-request]
    [(= status-code 401) 'auth]
    [(= status-code 403) 'auth]
    [(= status-code 413) 'context-overflow]
    [(= status-code 429) 'rate-limit]
    [(>= status-code 500) 'server]
    [else 'network]))

;; ============================================================
;; Transient-failure classification (BUG-0011 / W6)
;; ============================================================

;; Categories that are known to be transient (safe to retry with backoff):
;; network 5xx, timeouts, rate limits, and reconnectable provider errors.
;; This is the machine-readable single source of truth; the runtime
;; auto-retry layer and the agent turn-retry layer both consume it.
(define transient-provider-error-categories '(rate-limit timeout server server-error network))

;; Structured predicate: is this provider error transient?
(define (provider-error-transient? e)
  (and (memq (provider-error-category e) transient-provider-error-categories) #t))

;; General predicate over any raised value. Covers:
;;  - structured provider-error (category-based, see above)
;;  - raw Racket network exns (connection refused/reset, DNS) — these wrap
;;    SSE transport failures that surface before classification
;;  - stream timeout/stall exns raised by the SSE layer (recognized via
;;    their exn:fail:network? supertype or message, keeping this module
;;    decoupled from llm/stream.rkt)
(define (transient-llm-failure? e)
  (cond
    [(provider-error? e) (provider-error-transient? e)]
    [(exn:fail:network? e) #t]
    [(exn:fail? e)
     (define msg (exn-message e))
     (for/or ([needle (in-list '("timed out" "timeout"
                                             "stream stalled"
                                             "connection"
                                             "network"
                                             "temporarily unavailable"))])
       (and (string-contains? (string-downcase msg) needle) #t))]
    [else #f]))

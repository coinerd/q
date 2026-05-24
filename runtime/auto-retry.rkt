#lang racket/base

;;; runtime/auto-retry.rkt — Auto-retry with exponential backoff
;;;
;;; Provides retry logic for transient provider errors (429, 5xx, timeouts).
;;; Wraps a thunk with configurable retry attempts and exponential backoff.

(require racket/contract
         racket/match
         racket/string
         "../util/protocol-types.rkt"
         "../llm/provider-errors.rkt")

;; Predicates
(provide retryable-error?
         context-overflow-error?
         permanent-tool-error?
         classify-error
         timeout-error?
         rate-limit-error?
         ;; Error classification data table
         ERROR-CLASSIFICATION-TABLE
         classify-error-from-table
         ;; Retry execution
         (contract-out [with-auto-retry
                        (->* (procedure?)
                             (#:max-retries exact-nonnegative-integer?
                                            #:base-delay-ms exact-nonnegative-integer?
                                            #:rate-limit-base-delay-ms exact-nonnegative-integer?
                                            #:max-delay-ms exact-nonnegative-integer?
                                            #:on-retry (or/c procedure? #f)
                                            #:per-type-budgets hash?)
                             any/c)]
                       [with-retry-policy
                        (->* (any/c procedure?) (#:on-retry (or/c procedure? #f)) any/c)]
                       [make-default-retry-policy
                        (->* ()
                             (#:max-retries exact-nonnegative-integer?
                                            #:base-delay-ms exact-nonnegative-integer?
                                            #:rate-limit-base-delay-ms exact-nonnegative-integer?
                                            #:max-delay-ms exact-nonnegative-integer?
                                            #:per-type-budgets hash?)
                             any/c)])
         ;; Configuration
         default-max-retries
         default-base-delay-ms
         default-rate-limit-base-delay-ms
         default-max-delay-ms
         ;; Struct for retry stats
         retry-stats
         retry-stats?
         retry-stats-attempts
         retry-stats-final-delay-ms
         retry-stats-succeeded?
         ;; Struct for retry exhaustion (A3)
         retry-exhausted
         retry-exhausted?
         retry-exhausted-original-exn
         retry-exhausted-attempts
         retry-exhausted-last-error-type
         retry-exhausted-total-delay-ms
         retry-exhausted-error-history
         ;; Struct for retry policy (A21)
         retry-policy
         retry-policy?
         retry-policy-max-retries
         retry-policy-base-delay-ms
         retry-policy-rate-limit-base-delay-ms
         retry-policy-max-delay-ms
         retry-policy-per-type-budgets)

;; ============================================================
;; Configuration
;; ============================================================

(define default-max-retries 2)
(define default-base-delay-ms 1000)
(define default-rate-limit-base-delay-ms 10000)
(define default-max-delay-ms 60000)

;; ============================================================
;; Structs
;; ============================================================

(struct retry-stats (attempts final-delay-ms succeeded?) #:transparent)

;; Retry policy struct — encapsulates retry configuration as a first-class value.
;; Can be composed, tested, and passed to with-retry-policy.
(struct retry-policy
        (max-retries base-delay-ms rate-limit-base-delay-ms max-delay-ms per-type-budgets)
  #:transparent)

(define (make-default-retry-policy
         #:max-retries [max-retries default-max-retries]
         #:base-delay-ms [base-delay-ms default-base-delay-ms]
         #:rate-limit-base-delay-ms [rl-base-delay-ms default-rate-limit-base-delay-ms]
         #:max-delay-ms [max-delay-ms default-max-delay-ms]
         #:per-type-budgets [per-type-budgets (hash 'timeout 2 'rate-limit 4 'provider-error 2)])
  (retry-policy max-retries base-delay-ms rl-base-delay-ms max-delay-ms per-type-budgets))

;; Raised when retries are exhausted. Wraps the original exception with metadata
;; so callers (agent-session, TUI) can distinguish exhaustion from first failure.
(struct retry-exhausted exn:fail (original-exn attempts last-error-type total-delay-ms error-history)
  #:transparent)

;; ============================================================
;; Predicates
;; ============================================================

;; Check if an error is retryable (transient / rate-limit / server error).
;; Permanent tool errors (validation failures) are NEVER retryable.
(define (retryable-error? exn)
  (match (permanent-tool-error? exn)
    [#t #f]
    [_
     ;; M-11: Use structured provider-error-category as primary classification.
     ;; Falls back to string matching only for unknown/non-structured errors.
     (match (provider-error? exn)
       [#t
        (define cat (provider-error-category exn))
        (memq cat '(rate-limit timeout server network))]
       [_
        ;; String fallback for non-structured errors
        (define msg (exn-message exn))
        (define retryable-patterns
          '("429" "rate"
                  "overloaded"
                  "quota"
                  "too many"
                  "500"
                  "502"
                  "503"
                  "504"
                  "server error"
                  "timeout"
                  "timed out"
                  "connection"
                  "network"
                  "retry"
                  "backoff"))
        (for/or ([pattern (in-list retryable-patterns)])
          (string-contains? (string-downcase msg) pattern))])]))

;; FEAT-66: Check if an error is a context overflow / token limit error.
;; These errors indicate the context was too long for the model.
(define CONTEXT_OVERFLOW_PATTERNS
  '("context_length" "context length"
                     "maximum context"
                     "too many tokens"
                     "token limit"
                     "max_tokens"
                     "input is too long"
                     "request too large"
                     "reduce the length"
                     "exceeds the maximum"))

(define (context-overflow-error? exn)
  (define msg (exn-message exn))
  (for/or ([pattern (in-list CONTEXT_OVERFLOW_PATTERNS)])
    (string-contains? (string-downcase msg) pattern)))

;; ============================================================
;; Permanent tool error predicate (v0.19.3 Wave 2)
;; ============================================================

;; A permanent tool error is one where retrying will never succeed.
;; Tool-call validation failures (missing/wrong-type args) are permanent:
;; the LLM must be given the error feedback immediately to correct its call.
(define PERMANENT_TOOL_PATTERNS
  '("validate-tool-args" "missing required argument"
                         "expected type"
                         "args must be a hash"
                         "post-hook validation failed"
                         "unknown tool:"))

(define (permanent-tool-error? exn)
  (define msg (exn-message exn))
  (for/or ([pattern (in-list PERMANENT_TOOL_PATTERNS)])
    (string-contains? (string-downcase msg) (string-downcase pattern))))

;; ============================================================
;; Error classification (v0.11.2 Wave 3)
;; ============================================================

;; Timeout patterns — errors from HTTP read timeouts, connection drops, etc.
(define TIMEOUT_PATTERNS '("timeout" "timed out" "connection reset" "broken pipe" "read error" "eof"))

(define (timeout-error? exn)
  (define msg (exn-message exn))
  (for/or ([pattern (in-list TIMEOUT_PATTERNS)])
    (string-contains? (string-downcase msg) pattern)))

(define RATE_LIMIT_PATTERNS '("429" "rate" "overloaded" "quota" "too many requests"))

(define (rate-limit-error? exn)
  (define msg (exn-message exn))
  (for/or ([pattern (in-list RATE_LIMIT_PATTERNS)])
    (string-contains? (string-downcase msg) pattern)))

;; Classify an error into a symbolic type for recovery hint rendering.
;; Returns one of: 'timeout, 'rate-limit, 'auth, 'context-overflow,
;; 'max-iterations, 'provider-error

;; R-23: Error classification data table.
;; Each entry: (category . (pattern ...))
;; classify-error looks up this table instead of inline pattern matching.
(define ERROR-CLASSIFICATION-TABLE
  '((max-iterations . ("max.iterations"))
    (rate-limit . ("429" "rate" "overloaded" "quota"))
    (auth . ("401" "403" "auth" "unauthorized" "permission"))
    (context-overflow . ("context_length" "context length"
                                          "too many tokens"
                                          "token limit"
                                          "max_tokens"
                                          "input is too long"
                                          "exceeds the maximum"))
    (timeout . ("timeout" "timed out" "connection reset" "broken pipe" "read error" "eof"))))

(define (classify-error-from-table msg-down)
  (for/or ([entry (in-list ERROR-CLASSIFICATION-TABLE)])
    (define category (car entry))
    (define patterns (cdr entry))
    (for/or ([p (in-list patterns)])
      (and (string-contains? msg-down p) category))))

(define (classify-error exn)
  ;; Fast path: structured provider-error carries its own category.
  (match (provider-error? exn)
    [#t
     (define cat (provider-error-category exn))
     (if cat cat 'provider-error)]
    [_
     ;; Fallback: table-based classification for non-structured errors
     (define msg
       (if (exn:fail? exn)
           (exn-message exn)
           (format "~a" exn)))
     (define msg-down (string-downcase msg))
     (or (classify-error-from-table msg-down) 'provider-error)]))

;; ============================================================
;; Retry logic
;; ============================================================

;; Execute a thunk using a retry-policy struct.
;; Policy-first entry point — prefer over with-auto-retry for new code.
(define (with-retry-policy policy thunk #:on-retry [on-retry #f])
  (with-auto-retry thunk
                   #:max-retries (retry-policy-max-retries policy)
                   #:base-delay-ms (retry-policy-base-delay-ms policy)
                   #:rate-limit-base-delay-ms (retry-policy-rate-limit-base-delay-ms policy)
                   #:max-delay-ms (retry-policy-max-delay-ms policy)
                   #:on-retry on-retry
                   #:per-type-budgets (retry-policy-per-type-budgets policy)))

;; Execute a thunk with automatic retry on retryable errors.
;; Returns the thunk result on success, or re-raises on non-retryable
;; error or after max-retries exhausted.
(define (with-auto-retry
         thunk
         #:max-retries [max-retries default-max-retries]
         #:base-delay-ms [base-delay-ms default-base-delay-ms]
         #:rate-limit-base-delay-ms [rl-base-delay-ms default-rate-limit-base-delay-ms]
         #:max-delay-ms [max-delay-ms default-max-delay-ms]
         #:on-retry [on-retry #f]
         #:per-type-budgets [per-type-budgets (hash 'timeout 2 'rate-limit 4 'provider-error 2)])
  ;; v0.14.2: Per-type retry budget. Each error type has its own budget.
  ;; Rate-limit retries don't consume timeout budget, etc.
  (let loop ([attempt 0]
             [delay-ms 0]
             [total-delay 0]
             [last-error-type #f]
             [type-attempts (hash)] ; hash of error-type -> count
             [error-history '()]) ; list of error types encountered
    (with-handlers
        ([exn:fail?
          (lambda (exn)
            (define err-type (classify-error exn))
            (define current-type-count (hash-ref type-attempts err-type 0))
            (define type-budget (hash-ref per-type-budgets err-type max-retries))
            ;; Can retry if: globally under max-retries AND per-type under budget
            (match (and (retryable-error? exn)
                        (< attempt max-retries)
                        (< current-type-count type-budget))
              [#t
               ;; A1: Use longer backoff for rate-limit errors
               (define rl-base (if (eq? err-type 'rate-limit) rl-base-delay-ms base-delay-ms))
               (define next-delay (min (* rl-base (expt 2 attempt)) max-delay-ms))
               ;; v0.13.2: No context reduction on retry — retries use same thunk.
               ;; Context management is a separate concern (v0.14.0 context manager).
               ;; Call retry callback if provided (include error-type)
               (when on-retry
                 (on-retry (add1 attempt) max-retries next-delay (exn-message exn) err-type))
               (sleep (/ next-delay 1000.0))
               (loop (add1 attempt)
                     next-delay
                     (+ total-delay next-delay)
                     err-type
                     (hash-set type-attempts err-type (add1 current-type-count))
                     (append error-history (list err-type)))]
              [_
               ;; A3: Wrap in retry-exhausted if we attempted retries
               (define final-history (append error-history (list err-type)))
               (if (> attempt 0)
                   (raise (retry-exhausted (format "~a (after ~a retries)" (exn-message exn) attempt)
                                           (current-continuation-marks)
                                           exn
                                           attempt
                                           last-error-type
                                           total-delay
                                           final-history))
                   (raise exn))]))])
      (thunk))))

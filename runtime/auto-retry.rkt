#lang racket/base

;;; runtime/auto-retry.rkt — Auto-retry with exponential backoff
;;;
;;; Provides retry logic for transient provider errors (429, 5xx, timeouts).
;;; Wraps a thunk with configurable retry attempts and exponential backoff.

(require racket/match
         racket/string
         "../util/protocol-types.rkt"
         "../llm/provider-errors.rkt")

;; Predicates
(provide retryable-error?
         context-overflow-error?
         classify-error
         timeout-error?
         rate-limit-error?
         ;; Retry execution
         with-auto-retry
         ;; Configuration
         default-max-retries
         default-base-delay-ms
         default-rate-limit-base-delay-ms
         default-max-delay-ms
         ;; Struct for retry stats
         (struct-out retry-stats)
         ;; Struct for retry exhaustion (A3)
         (struct-out retry-exhausted))

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

;; Raised when retries are exhausted. Wraps the original exception with metadata
;; so callers (agent-session, TUI) can distinguish exhaustion from first failure.
(struct retry-exhausted exn:fail (original-exn attempts last-error-type total-delay-ms) #:transparent)

;; ============================================================
;; Predicates
;; ============================================================

;; Check if an error is retryable (transient / rate-limit / server error).
(define (retryable-error? exn)
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
    (string-contains? (string-downcase msg) pattern)))

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
;; Error classification (v0.11.2 Wave 3)
;; ============================================================

;; Timeout patterns — errors from HTTP read timeouts, connection drops, etc.
(define TIMEOUT_PATTERNS '("timeout" "timed out" "connection reset" "broken pipe" "read error" "eof"))

(define (timeout-error? exn)
  (define msg (exn-message exn))
  (for/or ([pattern (in-list TIMEOUT_PATTERNS)])
    (string-contains? (string-downcase msg) pattern)))

(define RATE_LIMIT_PATTERNS '("429" "rate" "overloaded" "quota" "too many"))

(define (rate-limit-error? exn)
  (define msg (exn-message exn))
  (for/or ([pattern (in-list RATE_LIMIT_PATTERNS)])
    (string-contains? (string-downcase msg) pattern)))

;; Classify an error into a symbolic type for recovery hint rendering.
;; Returns one of: 'timeout, 'rate-limit, 'auth, 'context-overflow,
;; 'max-iterations, 'provider-error
(define (classify-error exn)
  ;; Fast path: structured provider-error carries its own category.
  (when (provider-error? exn)
    (define cat (provider-error-category exn))
    (when cat
      (set! cat cat))) ; already correct
  (cond
    [(provider-error? exn)
     (define cat (provider-error-category exn))
     (if cat cat 'provider-error)]
    [else
     ;; Fallback: string-based classification for non-structured errors
     (define msg
       (if (exn:fail? exn)
           (exn-message exn)
           (format "~a" exn)))
     (define msg-down (string-downcase msg))
     (cond
       [(string-contains? msg-down "max.iterations") 'max-iterations]
       [(string-contains? msg-down "429") 'rate-limit]
       [(string-contains? msg-down "rate") 'rate-limit]
       [(string-contains? msg-down "overloaded") 'rate-limit]
       [(string-contains? msg-down "quota") 'rate-limit]
       [(for/or ([p (in-list '("401" "403" "auth" "unauthorized" "permission"))])
          (string-contains? msg-down p))
        'auth]
       [(for/or ([p (in-list '("context_length" "context length"
                                                "too many tokens"
                                                "token limit"
                                                "max_tokens"
                                                "input is too long"
                                                "exceeds the maximum"))])
          (string-contains? msg-down p))
        'context-overflow]
       [(for/or ([p (in-list TIMEOUT_PATTERNS)])
          (string-contains? msg-down p))
        'timeout]
       [else 'provider-error])]))

;; ============================================================
;; Retry logic
;; ============================================================

;; Execute a thunk with automatic retry on retryable errors.
;; Returns the thunk result on success, or re-raises on non-retryable
;; error or after max-retries exhausted.
(define (with-auto-retry thunk
                         #:max-retries [max-retries default-max-retries]
                         #:base-delay-ms [base-delay-ms default-base-delay-ms]
                         #:rate-limit-base-delay-ms
                         [rl-base-delay-ms default-rate-limit-base-delay-ms]
                         #:max-delay-ms [max-delay-ms default-max-delay-ms]
                         #:on-retry [on-retry #f])
  (let loop ([attempt 0]
             [delay-ms 0]
             [total-delay 0]
             [last-error-type #f])
    (with-handlers
        ([exn:fail?
          (lambda (exn)
            (define err-type (classify-error exn))
            (cond
              [(and (retryable-error? exn) (< attempt max-retries))
               ;; A1: Use longer backoff for rate-limit errors
               (define rl-base (if (eq? err-type 'rate-limit) rl-base-delay-ms base-delay-ms))
               (define next-delay (min (* rl-base (expt 2 attempt)) max-delay-ms))
               ;; v0.13.2: No context reduction on retry — retries use same thunk.
               ;; Context management is a separate concern (v0.14.0 context manager).
               ;; Call retry callback if provided
               (when on-retry
                 (on-retry (add1 attempt) max-retries next-delay (exn-message exn)))
               (sleep (/ next-delay 1000.0))
               (loop (add1 attempt) next-delay (+ total-delay next-delay) err-type)]
              [else
               ;; A3: Wrap in retry-exhausted if we attempted retries
               (if (> attempt 0)
                   (raise (retry-exhausted (format "~a (after ~a retries)" (exn-message exn) attempt)
                                           (current-continuation-marks)
                                           exn
                                           attempt
                                           last-error-type
                                           total-delay))
                   (raise exn))]))])
      (thunk))))

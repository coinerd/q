#lang racket/base

;;; runtime/auto-retry.rkt — Auto-retry with exponential backoff
;;;
;;; Provides retry logic for transient provider errors (429, 5xx, timeouts).
;;; Wraps a thunk with configurable retry attempts and exponential backoff.

(require racket/contract
         racket/match
         racket/string
         racket/random
         racket/math
         "../llm/provider-errors.rkt" ; whole-module: provides retryable predicates + transient-llm-failure?
         "../llm/stream.rkt"
         (only-in "../util/cancellation.rkt" cancellation-token? cancellation-token-cancelled?)
         (only-in "../util/exn.rkt" exn:fail:stream-error? exn:fail:stream-error-original-exn))

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
         held-request?
         minimal-output-stall?
         stall-severity
         default-stall-min-output-chars
         default-stall-max-consecutive
         default-cumulative-ceiling-secs
         ;; Retry execution
         (contract-out
          [with-auto-retry
           (->* (procedure?)
                (#:max-retries exact-nonnegative-integer?
                               #:base-delay-ms exact-nonnegative-integer?
                               #:rate-limit-base-delay-ms exact-nonnegative-integer?
                               #:max-delay-ms exact-nonnegative-integer?
                               #:on-retry (or/c procedure? #f)
                               #:on-circuit-break (or/c procedure? #f)
                               #:per-type-budgets hash?
                               #:cumulative-ceiling-secs (or/c exact-positive-integer? #f)
                               #:now-proc (or/c procedure? #f)
                               #:stall-min-output-chars exact-nonnegative-integer?
                               #:stall-max-consecutive exact-nonnegative-integer?
                               #:health-check-proc (or/c procedure? #f)
                               #:cancellation-token (or/c cancellation-token? #f)
                               #:on-success (or/c procedure? #f))
                any/c)]
          [with-retry-policy (->* (retry-policy? procedure?) (#:on-retry (or/c procedure? #f)) any/c)]
          [make-default-retry-policy
           (->* ()
                (#:max-retries exact-nonnegative-integer?
                               #:base-delay-ms exact-nonnegative-integer?
                               #:rate-limit-base-delay-ms exact-nonnegative-integer?
                               #:max-delay-ms exact-nonnegative-integer?
                               #:per-type-budgets hash?)
                retry-policy?)])
         ;; Configuration
         default-max-retries
         default-base-delay-ms
         default-rate-limit-base-delay-ms
         default-max-delay-ms
         default-cumulative-ceiling-secs
         ;; Struct for retry stat
         retry-stats
         retry-stats?
         retry-stats-attempts
         retry-stats-final-delay-ms
         retry-stats-succeeded?
         retry-stats-selected-delay-ms
         ;; Struct for retry exhaustion (A3)
         retry-exhausted
         retry-exhausted?
         retry-exhausted-original-exn
         retry-exhausted-attempts
         retry-exhausted-last-error-type
         retry-exhausted-total-delay-ms
         retry-exhausted-error-history
         retry-exhausted-delays
         ;; Cancellation-aware backoff (W0-F5)
         retry-cancelled
         retry-cancelled?
         find-retry-exhausted
         ;; Struct for retry policy (A21)
         retry-policy
         retry-policy?
         retry-policy-max-retries
         retry-policy-base-delay-ms
         retry-policy-rate-limit-base-delay-ms
         retry-policy-max-delay-ms
         retry-policy-per-type-budgets
         ;; Jitter computation (W1)
         (contract-out [compute-retry-delay
                        (-> exact-nonnegative-integer?
                            exact-nonnegative-integer?
                            exact-nonnegative-integer?
                            exact-nonnegative-integer?
                            (or/c #f (-> any/c))
                            exact-nonnegative-integer?)])
         ;; Injectable random source (W1)
         current-random-source
         ;; Retry-after parsing (W1)
         (contract-out [parse-retry-after (-> (or/c string? #f) (or/c exact-nonnegative-integer? #f))]
                       ;; v1.00.13 W3 (#9473): structured retry-after source (RL-7)
                       [structured-retry-after-ms (-> any/c (or/c exact-nonnegative-integer? #f))]))

;; ============================================================
;; Configuration
;; ============================================================

(define default-max-retries 2)
(define default-base-delay-ms 1000)
(define default-rate-limit-base-delay-ms 10000)
(define default-max-delay-ms 60000)

;; v0.99.81 W2 PN-7: Default cumulative ceiling across retries (5 minutes).
;; When #f, no cumulative wall-clock bound is enforced (backward compat).
;; v1.00.05 W2 (#9394): raised 300 → 900 so a 5-retry budget with 120s per-read
;; timeouts (plus backoff) is actually reachable; the old 300s ceiling truncated
;; the retry loop after ~2 attempts.
(define default-cumulative-ceiling-secs 900)

;; ============================================================
;; Injectable random source (W1)
;; ============================================================

;; Parameter for injecting a random source.
;; Default: #f means use the system random source (random).
;; For deterministic testing, set to a thunk that returns values between 0.0 and 1.0.
(define current-random-source (make-parameter #f))

;; Generate a random float in [0.0, 1.0) using the injected source or system random.
(define (random-float)
  (define src (current-random-source))
  (if src
      (src)
      (random)))

;; ============================================================
;; Structs
;; ============================================================

(struct retry-stats (attempts final-delay-ms succeeded? selected-delay-ms) #:transparent)

;; ============================================================
;; Delay computation with jitter (W1)
;; ============================================================

;; Parse a Retry-After header value. Returns milliseconds or #f.
;; Accepts:
;;   - Integer seconds (e.g., "30")
;;   - Float seconds (e.g., "2.5")
(define (parse-retry-after header-val)
  (and header-val
       (string? header-val)
       (let ([trimmed (string-trim header-val)])
         (and (> (string-length trimmed) 0)
              (with-handlers ([exn:fail? (lambda (_) #f)])
                (exact-floor (* (string->number trimmed) 1000)))))))

;; v1.00.13 W3 (#9473, RL-7): the retry-delay source reads STRUCTURED failure
;; metadata only — provider errors carry 'retry-after-ms in their context
;; hash (populated from the actual response header by
;; make-provider-http-request). Human exception text is never parsed: an
;; error whose message mentions "Retry-After" but carries no structured
;; context gets no header-derived delay.
(define (structured-retry-after-ms exn)
  (and (provider-error? exn) (hash-ref (provider-error-context exn) 'retry-after-ms #f)))

;; Compute retry delay with full jitter.
;;
;; The exponential backoff is: base * 2^attempt, capped at max-delay-ms.
;; Full jitter: random [0, capped-delay]
;;
;; Parameters:
;;   attempt: 0-based attempt number
;;   base-delay-ms: base delay in ms
;;   max-delay-ms: maximum delay cap in ms
;;   retry-after-ms: optional Retry-After value (0 means none)
;;   random-fn: optional random function returning [0.0, 1.0); #f means use system random
;;
;; Returns: delay in ms, always within [0, max-delay-ms]
(define (compute-retry-delay attempt base-delay-ms max-delay-ms [retry-after-ms 0] [random-fn #f])
  (cond
    ;; Retry-After header takes precedence (capped to max-delay)
    [(and (positive? retry-after-ms) retry-after-ms) (min retry-after-ms max-delay-ms)]
    [else
     ;; Exponential backoff: base * 2^attempt, capped
     (define exponential (min (* base-delay-ms (expt 2 attempt)) max-delay-ms))
     ;; Full jitter: random [0, exponential]
     (define r
       (if random-fn
           (random-fn)
           (random)))
     (exact-floor (* r exponential))]))

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
(struct retry-exhausted
        exn:fail
        (original-exn attempts last-error-type total-delay-ms error-history delays)
  #:transparent)

;; Raised when the cancellation token fires during backoff sleep (W0-F5).
;; Deliberately NOT retried: with-auto-retry's handler re-raises it immediately
;; so a cancellation can never be mis-classified as a transient provider error.
(struct retry-cancelled exn:fail () #:transparent)

;; Deep-unwrap retry metadata (W0-F5): partial-recovery wrapping may re-wrap a
;; retry-exhausted inside exn:fail:stream-error. This walks the chain so the
;; metadata (attempts, delays, history) survives partial wrapping.
(define (find-retry-exhausted exn)
  (let loop ([e exn])
    (cond
      [(retry-exhausted? e) e]
      [(and (exn:fail:stream-error? e) (exn:fail:stream-error-original-exn e))
       (loop (exn:fail:stream-error-original-exn e))]
      [else #f])))

;; Cancellation-aware sleep: polls the token every 50ms so a user interrupt
;; during backoff aborts promptly instead of waiting out the full delay (W0-F5).
(define (sleep-cancellable! delay-ms token)
  (define deadline (+ (current-inexact-milliseconds) delay-ms))
  (let poll ()
    (when (and token (cancellation-token-cancelled? token))
      (raise (retry-cancelled "retry backoff aborted by cancellation" (current-continuation-marks))))
    (define remaining (- deadline (current-inexact-milliseconds)))
    (when (> remaining 0)
      (sleep (min 0.05 (/ remaining 1000.0)))
      (poll))))

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
     ;; W6 (BUG-0011): structured branch delegates to provider-errors'
     ;; transient classification — single source of truth, no duplication.
     (match (provider-error? exn)
       [#t (provider-error-transient? exn)]
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
  ;; Fast path: structured provider-error
  (or (and (provider-error? exn) (eq? (provider-error-category exn) 'context-overflow))
      ;; Fallback: string matching for non-structured errors
      (let ()
        (define msg (exn-message exn))
        (for/or ([pattern (in-list CONTEXT_OVERFLOW_PATTERNS)])
          (string-contains? (string-downcase msg) pattern)))))

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
  ;; Fast path: structured provider-error
  (or (and (provider-error? exn) (eq? (provider-error-category exn) 'timeout))
      ;; Fallback: string matching for non-structured errors
      (let ()
        (define msg (exn-message exn))
        (for/or ([pattern (in-list TIMEOUT_PATTERNS)])
          (string-contains? (string-downcase msg) pattern)))))

(define RATE_LIMIT_PATTERNS '("429" "rate" "overloaded" "quota" "too many requests"))

(define (rate-limit-error? exn)
  ;; Fast path: structured provider-error
  (or (and (provider-error? exn) (eq? (provider-error-category exn) 'rate-limit))
      ;; Fallback: string matching for non-structured errors
      (let ()
        (define msg (exn-message exn))
        (for/or ([pattern (in-list RATE_LIMIT_PATTERNS)])
          (string-contains? (string-downcase msg) pattern)))))

;; Classify an error into a symbolic type for recovery hint rendering.
;; Returns one of: 'timeout, 'rate-limit, 'auth, 'context-overflow,
;; 'max-iterations, 'provider-error

;; R-23: Error classification data table.
;; Each entry: (category . (pattern ...))
;; classify-error looks up this table instead of inline pattern matching.
(define ERROR-CLASSIFICATION-TABLE
  '((max-iterations . ("max.iterations"))
    (bad-request . ("bad request" "400" "tool_call_ids did not"))
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
;; W2 PN-4: Circuit Breaker — held request classification
;; ============================================================

;; A "held request" is one where the provider returned HTTP 200 with SSE
;; headers but sent ZERO chunks before stalling. The stream timeout metadata
;; records received-any-data?=#f and phase='initial. Retrying such a request
;; is wasteful — the provider is likely to hold again. The circuit breaker
;; classifies this as non-retryable.
;;
;; v1.00.13 W4 (#9478, RL-8): heartbeat metadata participates in the
;; classification. A stream that received SSE comments (: heartbeats) proved
;; the peer is ALIVE (live-but-no-content) — not the same signal as a
;; zero-liveness dead peer — so it no longer trips the held-request breaker.
;; The total deadline and the empty/comment flood ceiling remain the bounds
;; for heartbeat-only streams.
;;
;; Mid-stream stalls (data received, or phase='thinking/'content) ARE
;; retryable — the provider was alive and producing, just slow.
(define (held-request? exn)
  (and (exn:fail:network:timeout:stream? exn)
       (not (exn:fail:network:timeout:stream-received-heartbeats? exn))
       (not (exn:fail:network:timeout:stream-received-any-data? exn))
       (eq? (exn:fail:network:timeout:stream-phase exn) 'initial)))

;; ============================================================
;; v0.99.82 W1 NR-1: Mid-stream stall classification
;; ============================================================

;; Configuration defaults for progressive circuit breaker.
(define default-stall-min-output-chars 100)
(define default-stall-max-consecutive 2)

;; A minimal-output stall: the stream received SOME data (so it's not a
;; held request) but less than threshold characters before stalling. This
;; indicates a sick provider that starts responding but cannot sustain
;; output. Retrying is likely to produce the same minimal result.
(define (minimal-output-stall? exn #:min-chars [threshold default-stall-min-output-chars])
  (and (exn:fail:network:timeout:stream? exn)
       (exn:fail:network:timeout:stream-received-any-data? exn)
       (< (exn:fail:network:timeout:stream-output-chars exn) threshold)))

;; Classify stall severity for diagnostics and circuit-breaker decisions.
;; Returns: 'initial-hold (zero data), 'minimal-output (< threshold chars),
;; or 'partial-output (substantial output, full retry budget).
(define (stall-severity exn)
  (cond
    [(held-request? exn) 'initial-hold]
    [(minimal-output-stall? exn) 'minimal-output]
    [else 'partial-output]))

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
;;
;; v0.99.81 W2:
;;   - Circuit breaker (PN-4): held-request timeouts (zero chunks, initial
;;     phase) skip all retries.
;;   - Cumulative ceiling (PN-7): wall-clock across retries bounded to
;;     #:cumulative-ceiling-secs (default #f = no bound for backward compat).
(define (with-auto-retry
         thunk
         #:max-retries [max-retries default-max-retries]
         #:base-delay-ms [base-delay-ms default-base-delay-ms]
         #:rate-limit-base-delay-ms [rl-base-delay-ms default-rate-limit-base-delay-ms]
         #:max-delay-ms [max-delay-ms default-max-delay-ms]
         #:on-retry [on-retry #f]
         #:on-circuit-break [on-circuit-break #f]
         #:per-type-budgets [per-type-budgets (hash 'timeout 2 'rate-limit 4 'provider-error 2)]
         #:cumulative-ceiling-secs [ceiling-secs #f]
         #:now-proc [now-proc #f]
         #:stall-min-output-chars [stall-min-chars default-stall-min-output-chars]
         #:stall-max-consecutive [stall-max-consecutive default-stall-max-consecutive]
         #:health-check-proc [health-check-proc #f]
         #:cancellation-token [cancellation-token #f]
         #:on-success [on-success #f])
  (define now (or now-proc current-inexact-milliseconds))
  (define start-ms (now))
  (define ceiling-ms (and ceiling-secs (* ceiling-secs 1000)))
  ;; v0.14.2: Per-type retry budget. Each error type has its own budget.
  ;; Rate-limit retries don't consume timeout budget, etc.
  (let loop ([attempt 0]
             [delay-ms 0]
             [total-delay 0]
             [last-error-type #f]
             [type-attempts (hash)] ; hash of error-type -> count
             [error-history '()] ; list of error types encountered
             [delay-history '()] ; list of actual delays used
             [consecutive-stalls 0]) ; v0.99.82 W1 NR-1: consecutive minimal-output stalls
    (with-handlers
        ([retry-cancelled? (lambda (exn) (raise exn))] ; never retry a cancellation (W0-F5)
         [exn:fail?
          (lambda (exn)
            (define err-type (classify-error exn))
            (define current-type-count (hash-ref type-attempts err-type 0))
            (define type-budget (hash-ref per-type-budgets err-type max-retries))
            ;; v0.99.81 W2 PN-4: Circuit breaker for held requests.
            ;; A held request (zero chunks, initial phase) is classified as
            ;; non-retryable — the provider will likely hold again.
            (when (and on-circuit-break (held-request? exn))
              (on-circuit-break 'held-request exn))
            ;; v0.99.82 W1 NR-1: Progressive circuit breaker for minimal-output stalls.
            ;; Track consecutive minimal-output stalls. After N consecutive
            ;; stalls (default 2), skip remaining retries — the provider is sick.
            (define is-minimal-stall? (minimal-output-stall? exn #:min-chars stall-min-chars))
            (define new-consecutive
              (if is-minimal-stall?
                  (add1 consecutive-stalls)
                  0))
            (define progressive-break?
              (and is-minimal-stall? (>= new-consecutive stall-max-consecutive)))
            (when (and on-circuit-break progressive-break?)
              (on-circuit-break 'progressive-stall exn))
            ;; v0.99.82 W2 NR-3: Health gate. If provided, health-check-proc is
            ;; called before the retry decision. If it returns #f, the provider
            ;; is unhealthy and retry is denied.
            ;; Can retry if: retryable AND not held AND not progressive-break
            ;; AND health-check passes AND under budget
            (match (and (retryable-error? exn)
                        (not (and cancellation-token
                                  (cancellation-token-cancelled? cancellation-token)))
                        (not (held-request? exn))
                        (not progressive-break?)
                        (or (not health-check-proc) (health-check-proc exn attempt))
                        (< attempt max-retries)
                        (< current-type-count type-budget))
              [#t
               ;; v0.99.81 W2 PN-7: Cumulative ceiling check.
               ;; Before retrying, check if total wall-clock has exceeded ceiling.
               ;; v0.99.83 W3 FIX: Only enforce ceiling on retry attempts (attempt > 0).
               ;; The first attempt's time is legitimate streaming/providers compute time —
               ;; penalizing it prevents all retries on long-generation turns. The ceiling
               ;; still bounds the combined retry delay chain.
               (define elapsed (- (now) start-ms))
               (when (and ceiling-ms (> attempt 0) (> elapsed ceiling-ms))
                 (raise (retry-exhausted (format "~a (cumulative ceiling ~as exceeded after ~as)"
                                                 (exn-message exn)
                                                 ceiling-secs
                                                 (/ elapsed 1000.0))
                                         (current-continuation-marks)
                                         exn
                                         attempt
                                         last-error-type
                                         total-delay
                                         (append error-history (list err-type))
                                         delay-history)))
               ;; A1: Use longer backoff for rate-limit errors
               (define rl-base (if (eq? err-type 'rate-limit) rl-base-delay-ms base-delay-ms))
               ;; v1.00.13 W3 (#9473, RL-7): retry delay comes from the
               ;; structured failure context (Retry-After read from the
               ;; actual response header at the request boundary), never
               ;; from parsing the human exception message.
               (define retry-after-ms (structured-retry-after-ms exn))
               (define next-delay
                 (compute-retry-delay attempt
                                      rl-base
                                      max-delay-ms
                                      (or retry-after-ms 0)
                                      (current-random-source)))
               ;; v0.13.2: No context reduction on retry — retries use same thunk.
               ;; Context management is a separate concern (v0.14.0 context manager).
               ;; Call retry callback if provided (include error-type and selected delay)
               (when on-retry
                 (on-retry (add1 attempt) max-retries next-delay (exn-message exn) err-type))
               (sleep-cancellable! next-delay cancellation-token)
               (loop (add1 attempt)
                     next-delay
                     (+ total-delay next-delay)
                     err-type
                     (hash-set type-attempts err-type (add1 current-type-count))
                     (append error-history (list err-type))
                     (append delay-history (list next-delay))
                     new-consecutive)]
              [_
               ;; A3: Wrap in retry-exhausted if we attempted retries
               (define final-history (append error-history (list err-type)))
               (define final-delays delay-history)
               (cond
                 ;; A cancelled token must surface as retry-cancelled, never as
                 ;; exhaustion of a provider error (W0-F5).
                 [(and cancellation-token (cancellation-token-cancelled? cancellation-token))
                  (raise (retry-cancelled "retry aborted by cancellation"
                                          (current-continuation-marks)))]
                 [(> attempt 0)
                  (raise (retry-exhausted (format "~a (after ~a retries)" (exn-message exn) attempt)
                                          (current-continuation-marks)
                                          exn
                                          attempt
                                          last-error-type
                                          total-delay
                                          final-history
                                          final-delays))]
                 [else (raise exn)])]))])
      (define result (thunk))
      ;; v0.99.82 W2 NR-3: Notify success hook for health tracking.
      (when on-success
        (on-success))
      result)))

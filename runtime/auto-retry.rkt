#lang racket/base

;;; runtime/auto-retry.rkt — Auto-retry with exponential backoff
;;;
;;; Provides retry logic for transient provider errors (429, 5xx, timeouts).
;;; Wraps a thunk with configurable retry attempts and exponential backoff.

(require racket/match
         racket/string
         "../util/protocol-types.rkt")

(provide
 ;; Predicates
 retryable-error?
 ;; Retry execution
 with-auto-retry
 ;; Configuration
 default-max-retries
 default-base-delay-ms
 default-max-delay-ms
 ;; Struct for retry stats
 (struct-out retry-stats))

;; ============================================================
;; Configuration
;; ============================================================

(define default-max-retries 2)
(define default-base-delay-ms 1000)
(define default-max-delay-ms 30000)

;; ============================================================
;; Structs
;; ============================================================

(struct retry-stats (attempts final-delay-ms succeeded?) #:transparent)

;; ============================================================
;; Predicates
;; ============================================================

;; Check if an error is retryable (transient / rate-limit / server error).
(define (retryable-error? exn)
  (define msg (exn-message exn))
  (define retryable-patterns
    '("429" "rate" "overloaded" "quota" "too many"
      "500" "502" "503" "504" "server error"
      "timeout" "timed out" "connection" "network"
      "retry" "backoff"))
  (for/or ([pattern (in-list retryable-patterns)])
    (string-contains? (string-downcase msg) pattern)))

;; ============================================================
;; Retry logic
;; ============================================================

;; Execute a thunk with automatic retry on retryable errors.
;; Returns the thunk result on success, or re-raises on non-retryable
;; error or after max-retries exhausted.
(define (with-auto-retry thunk
                          #:max-retries [max-retries default-max-retries]
                          #:base-delay-ms [base-delay-ms default-base-delay-ms]
                          #:max-delay-ms [max-delay-ms default-max-delay-ms]
                          #:on-retry [on-retry #f])
  (let loop ([attempt 0] [delay-ms 0])
    (with-handlers
        ([exn:fail?
          (lambda (exn)
            (cond
              [(and (retryable-error? exn) (< attempt max-retries))
               (define next-delay (min (* base-delay-ms (expt 2 attempt)) max-delay-ms))
               ;; Call retry callback if provided
               (when on-retry
                 (on-retry (add1 attempt) max-retries next-delay (exn-message exn)))
               (sleep (/ next-delay 1000.0))
               (loop (add1 attempt) next-delay)]
              [else (raise exn)]))])
      (thunk))))

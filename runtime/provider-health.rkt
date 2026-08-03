#lang racket/base

;; runtime/provider-health.rkt — Sliding-window provider health tracker (NR-3)
;;
;; Tracks recent provider failures and successes within a configurable time
;; window. When the failure count exceeds a threshold, the provider is marked
;; unhealthy and retries are skipped — preventing futile retry loops against
;; a consistently failing provider.
;;
;; Design:
;;   - Mutable struct with timestamped failure/success lists (ms epoch).
;;   - `record-failure!` prepends a timestamp; `record-success!` clears the
;;     failure list (a success means the provider is alive again).
;;   - `provider-healthy?` counts failures within window-secs.
;;   - Thread-safety: callers (call-with-provider-retry) operate within a
;;     single agent turn. The tracker is shared across turns via session
;;     state, but q's turn model is synchronous per session.

(require racket/contract/base)

;; ============================================================
;; Struct definition
;; ============================================================

;; recent-failures: list of timestamps (ms) of recent failures, newest first.
;; recent-successes: list of timestamps (ms) of recent successes, newest first.
;; last-success-ms: timestamp of most recent success (for diagnostic only).
(define-struct provider-health
               ([recent-failures #:mutable] [recent-successes #:mutable] [last-success-ms #:mutable])
  #:constructor-name make-provider-health/internal
  #:transparent)

;; ============================================================
;; Configuration defaults
;; ============================================================

(define default-health-window-secs 60)
(define default-health-failure-threshold 3)
(define max-tracked-entries 100) ; cap list size to prevent unbounded growth

;; ============================================================
;; Construction
;; ============================================================

(define (make-provider-health)
  (make-provider-health/internal '() '() #f))

;; ============================================================
;; Recording events
;; ============================================================

;; Record a failure. Prepends the current timestamp and caps the list.
(define (record-failure! health #:now-proc [now-proc #f])
  (define now
    (if now-proc
        (now-proc)
        (current-inexact-milliseconds)))
  (define old (provider-health-recent-failures health))
  (set-provider-health-recent-failures! health (take-at-most (cons now old) max-tracked-entries)))

;; Record a success. Clears the failure list — a successful response means
;; the provider is alive, so past failures are no longer predictive.
(define (record-success! health #:now-proc [now-proc #f])
  (define now
    (if now-proc
        (now-proc)
        (current-inexact-milliseconds)))
  (set-provider-health-recent-failures! health '())
  (define old-s (provider-health-recent-successes health))
  (set-provider-health-recent-successes! health (take-at-most (cons now old-s) max-tracked-entries))
  (set-provider-health-last-success-ms! health now))

;; ============================================================
;; Health checking
;; ============================================================

;; Returns #t if the provider is healthy (fewer than threshold failures
;; in the last window-secs), #f if unhealthy.
(define (provider-healthy? health
                           #:window-secs [window-secs default-health-window-secs]
                           #:threshold [threshold default-health-failure-threshold]
                           #:now-proc [now-proc #f])
  (define now
    (if now-proc
        (now-proc)
        (current-inexact-milliseconds)))
  (define window-ms (* window-secs 1000.0))
  (define cutoff (- now window-ms))
  ;; Count failures within the window
  (define recent-count
    (for/sum ([ts (in-list (provider-health-recent-failures health))]) (if (>= ts cutoff) 1 0)))
  (< recent-count threshold))

;; Diagnostic: count of failures within the default window.
(define (recent-failure-count health
                              #:window-secs [window-secs default-health-window-secs]
                              #:now-proc [now-proc #f])
  (define now
    (if now-proc
        (now-proc)
        (current-inexact-milliseconds)))
  (define cutoff (- now (* window-secs 1000.0)))
  (for/sum ([ts (in-list (provider-health-recent-failures health))]) (if (>= ts cutoff) 1 0)))

;; ============================================================
;; Helpers
;; ============================================================

(define (take-at-most lst n)
  (let loop ([lst lst]
             [n n])
    (cond
      [(<= n 0) '()]
      [(null? lst) '()]
      [else (cons (car lst) (loop (cdr lst) (sub1 n)))])))

;; ============================================================
;; Exports
;; ============================================================

(provide (contract-out
          (struct provider-health
                  ([recent-failures list?] [recent-successes list?]
                                           [last-success-ms (or/c real? #f)]))
          [make-provider-health (-> provider-health?)]
          [record-failure! (->* (provider-health?) (#:now-proc (or/c procedure? #f)) void?)]
          [record-success! (->* (provider-health?) (#:now-proc (or/c procedure? #f)) void?)]
          [provider-healthy?
           (->* (provider-health?)
                (#:window-secs exact-positive-integer?
                               #:threshold exact-nonnegative-integer?
                               #:now-proc (or/c procedure? #f))
                boolean?)]
          [recent-failure-count
           (->* (provider-health?)
                (#:window-secs exact-positive-integer? #:now-proc (or/c procedure? #f))
                exact-nonnegative-integer?)])
         default-health-window-secs
         default-health-failure-threshold)

#lang racket

;; q/tests/helpers/fast-fixtures.rkt — shared builders for fast-suite tests.
;;
;; W2 fast-timesink remediation (current release train): hoisted from
;; tests/test-provider-retry-telemetry.rkt, where the same deterministic-retry
;; parameterize block was constructed three times. The fixture activates the
;; auto-retry fake-clock seam so provider-retry tests exercise the full retry
;; loop (events, attempt counts, computed backoff delays) without paying real
;; wall-clock backoff sleeps.
;;
;; This file is a support module: the fast-suite inventory excludes
;; q/tests/helpers/*.rkt, so adding builders here never changes the suite
;; inventory or shard plan.

(provide with-deterministic-retries
         wait-until)

(require racket/base
         "../../runtime/auto-retry.rkt"
         "../../runtime/provider-retry.rkt")

;; Polls pred every 10 ms until it returns a truthy value or the timeout
;; (seconds, default 5) elapses; returns the last pred value.  Replaces fixed
;; `sleep`/alarm-evt waits in tests with synchronization on the actual
;; condition being asserted, so tests complete as soon as the event happens
;; instead of after a worst-case fixed delay.  This is a test-only utility
;; (q/tests/helpers/*.rkt is excluded from the fast-suite inventory).
(define (wait-until pred [timeout-sec 5.0])
  (define deadline (+ (current-inexact-milliseconds) (* timeout-sec 1000)))
  (let loop ()
    (define v (pred))
    (cond
      [v v]
      [(>= (current-inexact-milliseconds) deadline) v]
      [else
       (sleep 0.01)
       (loop)])))

;; Runs thunk with deterministic retry behavior:
;;  - current-random-source returns a deterministic 1.0 (max backoff), and
;;  - current-auto-retry-sleep-scale is 0.0 by default — backoff delays are
;;    still computed and reported in retry events/stats, only the wall-clock
;;    sleep is skipped (production default scale is 1.0, unchanged).
;; When #:max-retries is given, current-provider-retry-max-retries is pinned
;; to that value for the dynamic extent; otherwise the ambient value is kept.
;; W2 v1.00.24: #:sleep-scale (default 0.0) re-enables the REAL wall-clock
;; sleep path for the bounded slow/L4 timer canary
;; (tests/test-auto-retry-timer-canary.rkt) without changing the fast-suite
;; default of skipping sleeps.
(define (with-deterministic-retries thunk
                                    #:max-retries [max-retries #f]
                                    #:sleep-scale [sleep-scale 0.0])
  (parameterize ([current-random-source (lambda () 1.0)]
                 [current-auto-retry-sleep-scale sleep-scale]
                 [current-provider-retry-max-retries (or max-retries
                                                         (current-provider-retry-max-retries))])
    (thunk)))

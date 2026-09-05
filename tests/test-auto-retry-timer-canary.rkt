#lang racket/base

;; @speed slow
;; @suite default
;; @boundary integration
;; tests/test-auto-retry-timer-canary.rkt
;;
;; W2 NR-2: the single bounded real-timer retry canary (slow/L4).
;;
;; The fast suite (tests/test-partial-result-preservation.rkt,
;; tests/test-agent-session-basic.rkt, tests/test-auto-retry.rkt) asserts
;; computed logical retry semantics with the sleep-scale seam at 0.0 —
;; they pay no production backoff and own delay/attempt/exhaustion logic.
;;
;; This canary is the one executable destination that proves the REAL
;; wall-clock sleep path stays connected: `current-auto-retry-sleep-scale`
;; at a small nonzero value must produce proportionally scaled real sleeps
;; that are observable (not a silent no-op) while the reported logical
;; delays remain the full computed backoff. It deliberately uses tiny
;; nonzero delays and GENEROUS one-sided bounds so it cannot take
;; production-sized seconds and does not depend on exact scheduler timing:
;;   - too-early  => the sleep path is disconnected (scaled sleeps skipped);
;;   - too-late   => the bounded budget is exceeded;
;;   - the window between them is wide enough to tolerate the 50ms poll
;;     chunks of `sleep-cancellable!` and OS scheduling jitter.
;;
;; Cases live inside a named suite so `module+ main` runs them exactly once
;; and the process exit code reflects real failures (top-level rackunit
;; test-case forms print failures yet still exit 0 — they must not be used
;; as the sole runner of this canary).

(require rackunit
         rackunit/text-ui
         "../runtime/auto-retry.rkt"
         "helpers/fast-fixtures.rkt")

(define (ms)
  (current-inexact-milliseconds))

;; Flaky thunk: raises `count` retryable failures, then returns 'completed.
(define (make-flaky count)
  (define remaining count)
  (lambda ()
    (if (> remaining 0)
        (begin
          (set! remaining (sub1 remaining))
          (raise (exn:fail "HTTP 500 server error" (current-continuation-marks))))
        'completed)))

(define auto-retry-timer-canary-suite
  (test-suite "auto-retry real-timer canary"

    ;;; ---------------------------------------------------------
    ;;; Canary 1: success after real scaled retries
    ;;;
    ;;; compute-retry-delay = base * 2^(attempt-1) with full jitter, so with
    ;;; the deterministic random source (1.0 = max backoff) base 40ms yields
    ;;; computed delays (40 80). With #:sleep-scale 0.5 the real sleeps are
    ;;; (20 40) = 60ms wall clock. Production scale for this config would be
    ;;; 120ms — still tiny, but the guards below prove the seam scaled the
    ;;; sleeps rather than skipping or inflating them.
    ;;; ---------------------------------------------------------
    (test-case "real timer path stays connected: scaled sleeps run, logical delays preserved"
      (define logical-delays '())
      (define start (ms))
      (define result
        (with-deterministic-retries
         #:sleep-scale 0.5
         (lambda ()
           (with-auto-retry (make-flaky 2)
                            #:max-retries 3
                            #:base-delay-ms 40
                            #:max-delay-ms 480
                            #:on-retry (lambda (attempt max-retries delay-ms _message _err-type)
                                         (set! logical-delays
                                               (append logical-delays (list delay-ms))))))))
      (define elapsed (- (ms) start))
      (check-equal? result 'completed)
      ;; Logical (computed) delays are reported at full production value even
      ;; though the wall-clock sleeps were scaled down.
      (check-equal? logical-delays
                    (list 40 80)
                    "on-retry must report full computed backoff, not scaled sleeps")
      ;; Too-early guard: the scaled real sleeps (20+40=60ms) must actually run.
      ;; A 5ms scheduler slack absorbs coarse clock granularity; anything faster
      ;; means the real sleep path was skipped (scale seam disconnected).
      (check-true
       (>= elapsed (- 60 5))
       (format
        "real timer path not connected: retry returned after ~ams; ~
                          expected the ~ams of scaled sleeps to elapse"
        (round elapsed)
        60))
      ;; Bounded guard: generous ceiling covering the two scaled sleeps plus
      ;; up to ~50ms poll-chunk overhead each and slow CI scheduling. This canary
      ;; can never approach production-sized backoff (max-delay would be 480ms
      ;; per sleep at scale 1.0).
      (check-true
       (< elapsed 2000)
       (format
        "canary exceeded bounded budget: took ~ams for a config whose ~
                          scaled sleeps total 60ms"
        (round elapsed)))
      ;; Proportionality guard: proves the scale seam shrank the sleeps below
      ;; the production-scale wall clock (40+80=120ms) — too-early vs timeout
      ;; are distinguished by the two guards above, and this one by the seam.
      (check-true (< elapsed (+ 120 500))
                  (format "sleep-scale seam did not shrink sleeps: ~ams >= production-scale ~ams"
                          (round elapsed)
                          120)))

    ;;; ---------------------------------------------------------
    ;;; Canary 2: retry exhaustion still traverses the real sleep path
    ;;;
    ;;; One attempt, one scaled sleep (~10ms real at scale 0.5 for the 20ms
    ;;; computed delay), then retry-exhausted carries the full logical delays.
    ;;; ---------------------------------------------------------
    (test-case "retry exhaustion traverses real scaled sleep and preserves logical delays"
      (define logical-delays '())
      (define start (ms))
      (define outcome
        (with-handlers ([(lambda (e) #t) (lambda (e) e)])
          (with-deterministic-retries
           #:sleep-scale 0.5
           (lambda ()
             (with-auto-retry (make-flaky 5)
                              #:max-retries 1
                              #:base-delay-ms 20
                              #:max-delay-ms 240
                              #:on-retry (lambda (attempt max-retries delay-ms _message _err-type)
                                           (set! logical-delays
                                                 (append logical-delays (list delay-ms)))))))))
      (define elapsed (- (ms) start))
      (check-true (retry-exhausted? outcome)
                  "flaky-thunk-forever must exhaust and raise retry-exhausted")
      (check-equal? logical-delays (list 20) "exhaustion must report the full computed delay")
      (check-equal? (retry-exhausted-attempts outcome) 1)
      (check-equal? (retry-exhausted-delays outcome) (list 20))
      ;; Too-early guard: the single ~10ms scaled sleep must elapse (5ms slack).
      (check-true (>= elapsed 5)
                  (format "real timer path not connected on exhaustion: returned after ~ams"
                          (round elapsed)))
      ;; Bounded guard: one tiny sleep plus poll overhead, ceiling 1000ms.
      (check-true (< elapsed 1000)
                  (format "exhaustion canary exceeded bounded budget: ~ams" (round elapsed))))))

(module+ main
  (define exit-code (run-tests auto-retry-timer-canary-suite))
  (exit (if (zero? exit-code) 0 1)))

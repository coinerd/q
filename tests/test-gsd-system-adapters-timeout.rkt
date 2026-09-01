#lang racket/base

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-system-adapters-timeout.rkt — v1.00.24 W3 (BUG-0056/BUG-0057 cluster)
;;
;; Timeout-truth tests for the concrete wave-runner timeout adapter
;; `run-wave-with-timeout` in extensions/gsd/system-adapters.rkt.
;;
;; Contract under test (deterministic, fake ports):
;;   - runner finishes inside the deadline -> its outcome passes through exactly once
;;   - deadline passes                     -> cancel! is requested, the runner gets a
;;                                            bounded grace period, and the adapter
;;                                            returns a 'timed-out outcome exactly once
;;   - cancellation already requested      -> 'cancelled outcome, runner interrupted
;;
;; Real timers are only used at millisecond scale (plus the adapter's own 2 s
;; cancel-grace constant on the stubborn-runner leg, which is production
;; behavior we deliberately do not parameterize away).

(require rackunit
         rackunit/text-ui
         racket/format
         racket/function
         (only-in "../extensions/gsd/wave-runner-port.rkt"
                  wave-execution-outcome
                  wave-execution-outcome-kind
                  wave-execution-outcome-message
                  gsd-wave-runner-port)
         (only-in "../extensions/gsd/system-adapters.rkt"
                  run-wave-with-timeout))

;; ============================================================
;; Fake port helpers
;; ============================================================

;; A port whose runner finishes immediately with a done outcome.
(define (make-fast-port cancel-box)
  (gsd-wave-runner-port
   (lambda (idx)
     (wave-execution-outcome 'done (format "wave ~a complete" idx)))
   (lambda () (set-box! cancel-box (add1 (unbox cancel-box))))
   (lambda () #f)))

;; A port whose runner blocks forever until cancel! posts the release
;; semaphore (a well-behaved runner that honors cancellation).
;; requested-fn overrides cancel-requested? (defaults to #f).
(define (make-cancel-honoring-port cancel-box [requested-fn (lambda () #f)])
  (define release (make-semaphore 0))
  (gsd-wave-runner-port
   (lambda (idx)
     (sync release)
     (wave-execution-outcome 'interrupted "runner stopped on cancellation"))
   (lambda ()
     (set-box! cancel-box (add1 (unbox cancel-box)))
     (semaphore-post release))
   requested-fn))

;; A port whose runner ignores cancellation entirely (the pathological
;; case the adapter must survive by force-killing after the grace period).
(define (make-stubborn-port cancel-box)
  (define never (make-semaphore 0))
  (gsd-wave-runner-port
   (lambda (idx)
     (sync/timeout 30 never) ; would only finish via an external 30 s bound
     (wave-execution-outcome 'interrupted "unreachable"))
   (lambda () (set-box! cancel-box (add1 (unbox cancel-box))))
   (lambda () #f)))

;; A port whose runner waits on `release` OR its own natural deadline,
;; whichever comes first — used for deadline-preemption assertions.
(define (make-natural-deadline-port cancel-box natural-sec release)
  (gsd-wave-runner-port
   (lambda (idx)
     (sync/timeout natural-sec release)
     (wave-execution-outcome 'done "natural completion"))
   (lambda ()
     (set-box! cancel-box (add1 (unbox cancel-box)))
     (semaphore-post release))
   (lambda () #f)))

;; ============================================================
;; Tests
;; ============================================================

(define adapter-suite
  (test-suite "run-wave-with-timeout"

    (test-case "completes-in-time: outcome passes through exactly once"
      (define cancel-box (box 0))
      (define t0 (current-inexact-milliseconds))
      (define outcome (run-wave-with-timeout (make-fast-port cancel-box) 5 0))
      (define elapsed (- (current-inexact-milliseconds) t0))
      (check-equal? (wave-execution-outcome-kind outcome) 'done)
      (check-equal? (wave-execution-outcome-message outcome) "wave 0 complete")
      (check-equal? (unbox cancel-box) 0 "cancel! must not fire on a timely runner")
      (check-true (< elapsed 2000) (~a "timely run took " elapsed "ms")))

    (test-case "deadline-truth: overdue runner is cancelled and reported timed-out"
      (define cancel-box (box 0))
      (define t0 (current-inexact-milliseconds))
      (define outcome
        (run-wave-with-timeout (make-cancel-honoring-port cancel-box) 0.15 0))
      (define elapsed (- (current-inexact-milliseconds) t0))
      (check-equal? (wave-execution-outcome-kind outcome) 'timed-out)
      (check-true (regexp-match? #rx"runner exceeded" (wave-execution-outcome-message outcome)))
      (check-equal? (unbox cancel-box) 1 "cancel! must be requested exactly once on timeout")
      (check-true (>= elapsed 100) "deadline must actually elapse (real timer connected)")
      (check-true (< elapsed 5000) (~a "timeout path took " elapsed "ms (honoring cancel)")))

    (test-case "stubborn-runner: adapter survives ignoring cancellation via bounded grace"
      (define cancel-box (box 0))
      (define t0 (current-inexact-milliseconds))
      (define outcome
        (run-wave-with-timeout (make-stubborn-port cancel-box) 0.1 0))
      (define elapsed (- (current-inexact-milliseconds) t0))
      (check-equal? (wave-execution-outcome-kind outcome) 'timed-out)
      (check-equal? (unbox cancel-box) 1)
      (check-true (>= elapsed 2000) "grace period must be honored before force-kill")
      (check-true (< elapsed 15000) (~a "force-kill path took " elapsed "ms")))

    (test-case "cancellation-requested: reported cancelled, not timed-out"
      (define cancel-box (box 0))
      (define t0 (current-inexact-milliseconds))
      ;; Drive the adapter against a runner that blocks until cancelled:
      ;; the honoring port but with cancel-requested? true from the start.
      (define pre-cancelled
        (make-cancel-honoring-port cancel-box (lambda () #t)))
      (define outcome (run-wave-with-timeout pre-cancelled 30 0))
      (define elapsed (- (current-inexact-milliseconds) t0))
      (check-equal? (wave-execution-outcome-kind outcome) 'cancelled)
      (check-equal? (wave-execution-outcome-message outcome)
                    "campaign cancellation requested")
      (check-equal? (unbox cancel-box) 1 "cancel! must fire on the cancellation path")
      (check-true (< elapsed 5000) (~a "cancellation path took " elapsed "ms")))

    (test-case "deadline-wins-over-natural-finish: overdue cancel beats a slow healthy runner"
      ;; The runner would finish on its own at natural-sec; the deadline at
      ;; 0.05 s must preempt it. Proves the timeout is connected to real time
      ;; rather than to runner completion (BUG-0056/BUG-0057 truth).
      (define cancel-box (box 0))
      (define release (make-semaphore 0))
      (define t0 (current-inexact-milliseconds))
      (define outcome
        (run-wave-with-timeout
         (make-natural-deadline-port cancel-box 0.5 release) 0.05 0))
      (define elapsed (- (current-inexact-milliseconds) t0))
      (check-equal? (wave-execution-outcome-kind outcome) 'timed-out)
      (check-true (< elapsed 400) (~a "cancelled at " elapsed "ms, well before the 500 ms natural finish"))
      (check-equal? (unbox cancel-box) 1))))

(module+ main
  (exit (if (zero? (run-tests adapter-suite)) 0 1)))

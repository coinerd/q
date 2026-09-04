#lang racket/base

;; @speed slow  ;; @suite extensions

;; tests/test-gsd-wave-timeout-canary.rkt — v1.00.24 W4 L4 canary
;;
;; The single retained real-clock witness for the GSD wave timeout adapter
;; (v1.00.24 W4). Every deterministic deadline/cancel/grace/force-kill
;; scenario lives in tests/test-gsd-system-adapters-timeout.rkt behind the
;; injected timeout clock/wait seam; this canary is the only place where the
;; production adapter (current-inexact-milliseconds + sync/timeout + the
;; two-second cancellation grace) is exercised against real threads.
;;
;;   1. a runner finishing well inside the deadline is reported done;
;;   2. a runner that neither honors cancellation nor finishes cannot hang
;;      the adapter — the small deadline plus the bounded grace force-kill
;;      return 'timed-out inside a hard wall-clock ceiling and the stubborn
;;      worker thread is reaped.
;;
;; Real-clock legs tolerate CI jitter (generous, scheduler-fact assertions)
;; while capping total duration: both checks fail loudly long before a
;; regression could stall CI.

(require rackunit
         rackunit/text-ui
         racket/format
         (only-in "../extensions/gsd/wave-runner-port.rkt"
                  wave-execution-outcome
                  wave-execution-outcome-kind
                  gsd-wave-runner-port)
         (only-in "../extensions/gsd/system-adapters.rkt" run-wave-with-timeout))

(define (make-sleeping-port cancel-box worker-thread-box sleep-ms)
  (gsd-wave-runner-port
   (lambda (idx)
     (set-box! worker-thread-box (current-thread))
     (sleep (/ sleep-ms 1000.0))
     (wave-execution-outcome 'done (format "wave ~a finished after ~ams" idx sleep-ms)))
   (lambda () (set-box! cancel-box (add1 (unbox cancel-box))))
   (lambda () #f)))

(define wave-timeout-canary-suite
  (test-suite "wave timeout canary (L4, real clock)"

    (test-case "canary 1: timely runner completes inside a real deadline"
      (define cancel-box (box 0))
      (define worker-thread-box (box #f))
      (define t0 (current-inexact-milliseconds))
      (define outcome
        (run-wave-with-timeout (make-sleeping-port cancel-box worker-thread-box 40) 5 0))
      (define elapsed (- (current-inexact-milliseconds) t0))
      (check-equal? (wave-execution-outcome-kind outcome) 'done)
      (check-equal? (unbox cancel-box) 0 "completion inside the deadline must not cancel")
      (check-true (< elapsed 3000) (~a "timely canary took " elapsed "ms")))

    (test-case "canary 2: stubborn runner is force-reaped inside a hard ceiling"
      ;; The worker sleeps 60s (ignores cancellation, never self-finishes),
      ;; so the only path to a reap is the production deadline + grace
      ;; force-kill. Elapsed must reflect the real deadline and the real
      ;; grace, and the worker thread must actually be dead afterwards.
      (define cancel-box (box 0))
      (define worker-thread-box (box #f))
      (define t0 (current-inexact-milliseconds))
      (define outcome
        (run-wave-with-timeout (make-sleeping-port cancel-box worker-thread-box 60000) 0.1 0))
      (define elapsed (- (current-inexact-milliseconds) t0))
      (check-equal? (wave-execution-outcome-kind outcome) 'timed-out)
      (check-equal? (unbox cancel-box) 1 "cancel! must have been requested")
      (check-true (>= elapsed 100) "deadline must elapse before reap")
      (check-true (< elapsed 12000) (~a "canary reaped at " elapsed "ms — adapter did not hang"))
      (check-true (thread-dead? (unbox worker-thread-box))
                  "stubborn worker must be dead after the force-kill reap"))))

(module+ main
  (exit (if (zero? (run-tests wave-timeout-canary-suite)) 0 1)))

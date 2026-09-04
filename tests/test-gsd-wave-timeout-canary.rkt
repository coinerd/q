#lang racket/base

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-wave-timeout-canary.rkt — v1.00.24 W3 canary (BUG-0056/BUG-0057)
;;
;; Real-timer smoke for the wave runner timeout adapter. Two canaries:
;;   1. a runner finishing well inside the deadline is reported done;
;;   2. a runner that ignores cancellation cannot hang the adapter — the
;;      deadline plus the bounded grace force-kill return 'timed-out within
;;      a hard wall-clock ceiling.
;;
;; Unlike tests/test-gsd-system-adapters-timeout.rkt (deterministic fake
;; ports + semaphores), these canaries use plain thread sleeps and generous
;; bounds so a regression in the adapter's real-time wiring cannot hide
;; behind a synchronous test artifact.

(require rackunit
         rackunit/text-ui
         racket/format
         (only-in "../extensions/gsd/wave-runner-port.rkt"
                  wave-execution-outcome
                  wave-execution-outcome-kind
                  gsd-wave-runner-port)
         (only-in "../extensions/gsd/system-adapters.rkt"
                  run-wave-with-timeout))

(define (make-sleeping-port cancel-box sleep-ms)
  (gsd-wave-runner-port
   (lambda (idx)
     (sleep (/ sleep-ms 1000.0))
     (wave-execution-outcome 'done (format "wave ~a finished after ~ams" idx sleep-ms)))
   (lambda () (set-box! cancel-box (add1 (unbox cancel-box))))
   (lambda () #f)))

(define wave-timeout-canary-suite
  (test-suite "wave timeout canary"

    (test-case "canary 1: timely runner completes inside a real deadline"
      (define cancel-box (box 0))
      (define t0 (current-inexact-milliseconds))
      (define outcome
        (run-wave-with-timeout (make-sleeping-port cancel-box 40) 5 0))
      (define elapsed (- (current-inexact-milliseconds) t0))
      (check-equal? (wave-execution-outcome-kind outcome) 'done)
      (check-equal? (unbox cancel-box) 0)
      (check-true (< elapsed 3000) (~a "timely canary took " elapsed "ms")))

    (test-case "canary 2: cancel-ignoring runner is force-reaped inside a hard ceiling"
      (define cancel-box (box 0))
      (define t0 (current-inexact-milliseconds))
      (define outcome
        (run-wave-with-timeout (make-sleeping-port cancel-box 600) 0.1 0))
      (define elapsed (- (current-inexact-milliseconds) t0))
      (check-equal? (wave-execution-outcome-kind outcome) 'timed-out)
      (check-equal? (unbox cancel-box) 1 "cancel! must have been requested")
      (check-true (>= elapsed 100) "deadline must elapse before reap")
      (check-true (< elapsed 12000) (~a "canary reaped at " elapsed "ms — adapter did not hang")))))

(module+ main
  (exit (if (zero? (run-tests wave-timeout-canary-suite)) 0 1)))

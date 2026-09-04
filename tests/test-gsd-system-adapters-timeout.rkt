#lang racket/base

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-system-adapters-timeout.rkt — v1.00.24 W3 (BUG-0056/BUG-0057),
;; re-owned by W4 on the deterministic deadline/event seam.
;;
;; Timeout-truth tests for the concrete wave-runner timeout adapter
;; `run-wave-with-timeout` in extensions/gsd/system-adapters.rkt.
;;
;; Contract under test (deterministic, fake ports + fake clock):
;;   - runner finishes inside the deadline -> its outcome passes through exactly once
;;   - deadline passes                     -> cancel! is requested exactly once, the
;;                                            runner gets a bounded grace period, and
;;                                            the adapter returns a 'timed-out outcome
;;                                            exactly once
;;   - cancellation requested              -> 'cancelled outcome, runner interrupted
;;   - cooperative runner                  -> finishes during the grace wait (its marker
;;                                            is observable after the call)
;;   - stubborn runner                     -> force-killed after the grace wait and can
;;                                            never finish afterwards (worker cleanup)
;;
;; W4: the deadline and the cancellation grace are driven by the
;; `current-gsd-timeout-now-ms` / `current-gsd-timeout-wait` seam with the
;; shared fake clock (tests/helpers/gsd-timeout-fake.rkt), so this file pays
;; NO one-second deadline and NO two-second cancellation-grace wall-clock
;; wait. Real wall-clock integration of the production adapter (real
;; `current-inexact-milliseconds` + real `sync/timeout`, including the 2 s
;; grace on a cancel-ignoring runner) is owned exclusively by the L4 canary
;; tests/test-gsd-wave-timeout-canary.rkt.

(require rackunit
         rackunit/text-ui
         racket/format
         racket/list
         (only-in "../extensions/gsd/wave-runner-port.rkt"
                  wave-execution-outcome
                  wave-execution-outcome-kind
                  wave-execution-outcome-message
                  gsd-wave-runner-port)
         (only-in "../extensions/gsd/system-adapters.rkt"
                  run-wave-with-timeout
                  current-gsd-timeout-now-ms
                  current-gsd-timeout-wait)
         "helpers/gsd-timeout-fake.rkt")

;; ============================================================
;; Fake port helpers
;; ============================================================

;; A runner that blocks on `release` until cancellation posts it, then
;; finishes. Order matters: the finished marker is posted BEFORE the outcome
;; is produced, so whenever the adapter's grace wait observes the done
;; semaphore the runner's own completion marker is already observable.
;; Returns (values port release finished).
(define (make-cooperative-port cancel-box outcome [requested-fn (lambda () #f)])
  (define release (make-semaphore 0))
  (define finished (make-semaphore 0))
  (define port
    (gsd-wave-runner-port (lambda (idx)
                            (sync release)
                            (semaphore-post finished)
                            (wave-execution-outcome outcome (format "wave ~a finished" idx)))
                          (lambda ()
                            (set-box! cancel-box (add1 (unbox cancel-box)))
                            (semaphore-post release))
                          requested-fn))
  (values port release finished))

;; A runner that ignores cancellation entirely (the pathological case the
;; adapter must survive by force-killing after the grace period). The runner
;; blocks on `never`, which cancel! does NOT post — even if the test posts it
;; later, a force-killed worker can never observe the post. That is exactly
;; the cleanup proof. Returns (values port never finished).
(define (make-stubborn-port cancel-box)
  (define never (make-semaphore 0))
  (define finished (make-semaphore 0))
  (define port
    (gsd-wave-runner-port (lambda (idx)
                            (sync never) ; unreachable unless the worker survives (leak)
                            (semaphore-post finished)
                            (wave-execution-outcome 'interrupted "unreachable"))
                          (lambda () (set-box! cancel-box (add1 (unbox cancel-box))))
                          (lambda () #f)))
  (values port never finished))

;; A runner that waits on `release` OR its own natural real-time deadline,
;; whichever comes first — used for deadline-preemption assertions.
(define (make-natural-deadline-port cancel-box natural-sec release)
  (gsd-wave-runner-port (lambda (idx)
                          (sync/timeout natural-sec release)
                          (wave-execution-outcome 'done "natural completion"))
                        (lambda ()
                          (set-box! cancel-box (add1 (unbox cancel-box)))
                          (semaphore-post release))
                        (lambda () #f)))

;; Run the adapter under an explicit fake clock with staged waits.
;; Returns (values outcome fake-clock-ms wait-call-count).
(define (with-fake stages run)
  (define-values (now wait clock waits) (make-timeout-fake #:stages stages))
  (define outcome
    (parameterize ([current-gsd-timeout-now-ms now]
                   [current-gsd-timeout-wait wait])
      (run)))
  (values outcome (clock) (waits)))

;; A timeout-sec of 1 s at the default 0.1 s poll quantum produces exactly
;; ten deadline waits before the remaining-time check expires the runner;
;; the 11th wait call is the cancellation-grace wait.
(define deadline-ticks (make-list 10 'tick))

;; ============================================================
;; Tests
;; ============================================================

(define adapter-suite
  (test-suite "run-wave-with-timeout"

    (test-case "completes-before-deadline: outcome passes through exactly once"
      (define cancel-box (box 0))
      (define ran-once (box 0))
      (define release (make-semaphore 0))
      (define t0 (current-inexact-milliseconds))
      (define-values (outcome clock waits)
        (with-fake
         (list (list 'fire (lambda () (semaphore-post release))))
         (lambda ()
           (define port
             (gsd-wave-runner-port (lambda (idx)
                                     (set-box! ran-once (add1 (unbox ran-once)))
                                     (sync release)
                                     (wave-execution-outcome 'done (format "wave ~a complete" idx)))
                                   (lambda () (set-box! cancel-box (add1 (unbox cancel-box))))
                                   (lambda () #f)))
           (run-wave-with-timeout port 30 0))))
      (define elapsed (- (current-inexact-milliseconds) t0))
      (check-equal? (wave-execution-outcome-kind outcome) 'done)
      (check-equal? (wave-execution-outcome-message outcome) "wave 0 complete")
      (check-equal? (unbox ran-once) 1 "runner must be invoked exactly once")
      (check-equal? (unbox cancel-box) 0 "cancel! must not fire on a timely runner")
      (check-equal? clock 100.0 "completion happens on the first fake tick")
      (check-equal? waits 1)
      (check-true (< elapsed 2000) (~a "timely run took " elapsed "ms")))

    (test-case "deadline-expires with cooperative grace: timed-out once, runner not killed"
      (define cancel-box (box 0))
      (define-values (port release finished) (make-cooperative-port cancel-box 'interrupted))
      (define t0 (current-inexact-milliseconds))
      (define-values (outcome clock waits)
        (with-fake (append deadline-ticks (list (list 'fire void)))
                   (lambda () (run-wave-with-timeout port 1 0))))
      (define elapsed (- (current-inexact-milliseconds) t0))
      (check-equal? (wave-execution-outcome-kind outcome) 'timed-out)
      (check-true (regexp-match? #rx"runner exceeded" (wave-execution-outcome-message outcome)))
      (check-equal? (unbox cancel-box) 1 "cancel! must be requested exactly once on timeout")
      (check-equal? clock 3000.0 "deadline (1000ms) + full grace (2000ms) elapse on the fake clock")
      (check-equal? waits 11 "ten deadline polls + one grace wait")
      ;; Cooperative runner: cancellation released it during the grace wait,
      ;; so its completion marker is observable shortly after the call (the
      ;; worker thread needs one scheduling beat; bounded handshake).
      (check-not-false (sync/timeout 0.3 finished)
                       "cooperative runner must finish during the grace wait")
      (check-true (< elapsed 2000) (~a "timeout path took " elapsed "ms (no real grace paid)")))

    (test-case "forced-kill-after-grace: stubborn runner is force-stopped (worker cleanup)"
      (define cancel-box (box 0))
      (define-values (port never finished) (make-stubborn-port cancel-box))
      (define t0 (current-inexact-milliseconds))
      (define-values (outcome clock waits)
        (with-fake (append deadline-ticks (list 'tick)) (lambda () (run-wave-with-timeout port 1 0))))
      (define elapsed (- (current-inexact-milliseconds) t0))
      (check-equal? (wave-execution-outcome-kind outcome) 'timed-out)
      (check-equal? (unbox cancel-box) 1)
      (check-equal? clock 3000.0 "deadline + full grace elapse on the fake clock")
      (check-equal? waits 11)
      (check-true (< elapsed 2000) (~a "force-kill path took " elapsed "ms"))
      ;; Worker cleanup proof: the stubborn runner ignored cancellation and
      ;; stayed blocked on `never`; the adapter returned, so the only way the
      ;; runner could ever finish is a surviving (leaked) worker. Release the
      ;; runner now: a leaked worker would wake up and post its finished
      ;; marker; a force-killed worker can never do so.
      (semaphore-post never)
      (check-false (sync/timeout 0.3 finished)
                   "force-killed worker must not survive the adapter call"))

    (test-case "external cancellation wins over the deadline: reported cancelled, not timed-out"
      (define cancel-box (box 0))
      (define requested? (box #f))
      (define-values (port release finished)
        (make-cooperative-port cancel-box 'interrupted (lambda () (unbox requested?))))
      (define t0 (current-inexact-milliseconds))
      (define-values (outcome clock waits)
        (with-fake (list (list 'quiet (lambda () (set-box! requested? #t))) (list 'fire void))
                   (lambda () (run-wave-with-timeout port 30 0))))
      (define elapsed (- (current-inexact-milliseconds) t0))
      (check-equal? (wave-execution-outcome-kind outcome) 'cancelled)
      (check-equal? (wave-execution-outcome-message outcome) "campaign cancellation requested")
      (check-equal? (unbox cancel-box) 1 "cancel! must fire on the cancellation path")
      (check-equal? clock 2100.0 "cancellation preempts the 30s deadline after one poll tick + grace")
      (check-equal? waits 2)
      (check-true (< elapsed 2000) (~a "cancellation path took " elapsed "ms")))

    (test-case "already-requested cancellation: cancelled before any deadline wait"
      (define cancel-box (box 0))
      (define-values (port release finished)
        (make-cooperative-port cancel-box 'interrupted (lambda () #t)))
      (define t0 (current-inexact-milliseconds))
      (define-values (outcome clock waits)
        (with-fake (list (list 'fire void)) (lambda () (run-wave-with-timeout port 30 0))))
      (define elapsed (- (current-inexact-milliseconds) t0))
      (check-equal? (wave-execution-outcome-kind outcome) 'cancelled)
      (check-equal? (unbox cancel-box) 1)
      (check-equal? clock 2000.0 "only the grace wait elapses (no deadline poll)")
      (check-equal? waits 1)
      (check-true (< elapsed 2000) (~a "cancellation path took " elapsed "ms")))

    (test-case "deadline-wins-over-natural-finish: overdue cancel beats a slow healthy runner"
      ;; The runner would finish on its own at natural-sec; the fake deadline
      ;; at 0.05 s must preempt it. Proves the timeout is connected to the
      ;; (faked) clock rather than to runner completion (BUG-0056/BUG-0057
      ;; truth, now deterministic in fake time).
      (define cancel-box (box 0))
      (define release (make-semaphore 0))
      (define t0 (current-inexact-milliseconds))
      (define-values (outcome clock waits)
        (with-fake
         (list (list 'quiet void) 'tick)
         (lambda ()
           (run-wave-with-timeout (make-natural-deadline-port cancel-box 0.5 release) 0.05 0))))
      (define elapsed (- (current-inexact-milliseconds) t0))
      (check-equal? (wave-execution-outcome-kind outcome) 'timed-out)
      (check-equal? clock 2050.0 "fake deadline (50ms) + fake grace (2000ms)")
      (check-equal? waits 2)
      (check-true (< elapsed 400)
                  (~a "cancelled at " elapsed "ms, well before the 500 ms natural finish"))
      (check-equal? (unbox cancel-box) 1))

    (test-case "raising-runner: exception propagates via done post (no deadline spin)"
      ;; Mandatory-deadline follow-up: the wrapper's worker must post `done`
      ;; even when the runner raises, and the waiting thread re-raises the
      ;; captured exception at the FIRST done poll. Before the fix a raising
      ;; runner never posted `done`, so the waiter ticked to the deadline and
      ;; reported 'timed-out instead of propagating the exception.
      (define port
        (gsd-wave-runner-port (lambda (idx) (exn:fail "runner exploded" (current-continuation-marks)))
                              (lambda () #f)
                              (lambda () #f)))
      (check-exn exn:fail? (lambda () (with-fake '() (lambda () (run-wave-with-timeout port 30 0))))))

    (test-case "seam guards: bad fake clock/wait values are rejected"
      (check-exn exn:fail?
                 (lambda ()
                   (parameterize ([current-gsd-timeout-now-ms (lambda (x) x)])
                     (void))))
      (check-exn exn:fail?
                 (lambda ()
                   (parameterize ([current-gsd-timeout-wait (lambda (evt) evt)])
                     (void))))
      (check-exn exn:fail?
                 (lambda ()
                   (parameterize ([current-gsd-timeout-now-ms 42])
                     (void)))))))

(module+ main
  (exit (if (zero? (run-tests adapter-suite)) 0 1)))

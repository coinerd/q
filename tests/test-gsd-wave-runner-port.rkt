#lang racket/base
;; @covers extensions/gsd/wave-runner-port.rkt

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-wave-runner-port.rkt — Wave executor port (W3 #9234)
;;
;; TDD tests for the executor boundary: ONE structured terminal outcome per
;; invocation (done|failed|cancelled|timed-out|interrupted), explicit
;; cancellation via the runner port, and a deterministic timeout adapter.
;; @boundary unit

(require rackunit
         rackunit/text-ui
         racket/contract
         "../extensions/gsd/wave-runner-port.rkt"
         (only-in "../extensions/gsd/system-adapters.rkt" run-wave-with-timeout))

;; ============================================================
;; Outcome struct contract
;; ============================================================

(define outcome-suite
  (test-suite "wave-execution-outcome"

    (test-case "valid terminal kinds construct and round-trip"
      (for ([k '(done failed cancelled timed-out interrupted)])
        (define o (wave-execution-outcome k "msg"))
        (check-true (wave-execution-outcome? o))
        (check-eq? (wave-execution-outcome-kind o) k)
        (check-equal? (wave-execution-outcome-message o) "msg")))

    (test-case "contract rejects unknown kind"
      (check-exn exn:fail:contract? (lambda () (wave-execution-outcome 'bogus "nope"))))

    (test-case "message must be a string"
      (check-exn exn:fail:contract? (lambda () (wave-execution-outcome 'done 42))))

    (test-case "coerce-run-result: legacy symbols map to structured outcomes"
      (check-eq? (wave-execution-outcome-kind (coerce-run-result 'ok)) 'done)
      (check-eq? (wave-execution-outcome-kind (coerce-run-result 'error)) 'failed)
      (check-eq? (wave-execution-outcome-kind (coerce-run-result 'cancelled)) 'cancelled)
      (check-eq? (wave-execution-outcome-kind (coerce-run-result 'bogus))
                 'failed
                 "unknown legacy result fails closed")
      (define o (wave-execution-outcome 'interrupted "x"))
      (check-eq? (coerce-run-result o) o "outcomes pass through unchanged"))))

;; ============================================================
;; Runner port construction
;; ============================================================

(define port-suite
  (test-suite "gsd-wave-runner-port"

    (test-case "make-wave-runner-port wraps a plain run function"
      (define calls '())
      (define port
        (make-wave-runner-port (lambda (idx)
                                 (set! calls (cons idx calls))
                                 (wave-execution-outcome 'done "ok"))))
      (check-true (gsd-wave-runner-port? port))
      (define result ((gsd-wave-runner-port-run port) 3))
      (check-eq? (wave-execution-outcome-kind result) 'done)
      (check-equal? calls '(3)))

    (test-case "cancel! / cancel-requested? default to inert"
      (define port (make-wave-runner-port (lambda (idx) (wave-execution-outcome 'done ""))))
      (check-false ((gsd-wave-runner-port-cancel-requested? port)))
      ((gsd-wave-runner-port-cancel! port))
      (check-false ((gsd-wave-runner-port-cancel-requested? port))))

    (test-case "custom cancel wiring is honored"
      (define cancelled? #f)
      (define port
        (make-wave-runner-port (lambda (idx) (wave-execution-outcome 'done ""))
                               #:cancel! (lambda () (set! cancelled? #t))
                               #:cancel-requested? (lambda () cancelled?)))
      (check-false ((gsd-wave-runner-port-cancel-requested? port)))
      ((gsd-wave-runner-port-cancel! port))
      (check-true ((gsd-wave-runner-port-cancel-requested? port))))

    (test-case "port contract enforces outcome shape at call time"
      (define bad-port (make-wave-runner-port (lambda (idx) 'ok)))
      (check-exn exn:fail:contract?
                 (lambda () ((gsd-wave-runner-port-run bad-port) 0))
                 "run must return a wave-execution-outcome")
      (define bad-flag
        (make-wave-runner-port (lambda (idx) (wave-execution-outcome 'done ""))
                               #:cancel-requested? "not-a-thunk"))
      (check-exn exn:fail:contract?
                 (lambda () ((gsd-wave-runner-port-cancel-requested? bad-flag)))
                 "cancel-requested? must be a predicate"))))

;; ============================================================
;; Deterministic timeout adapter
;; ============================================================

(define timeout-suite
  (test-suite "run-wave-with-timeout"

    (test-case "fast runner returns its outcome before deadline"
      (define port (make-wave-runner-port (lambda (idx) (wave-execution-outcome 'done "fast"))))
      (define result (run-wave-with-timeout port 5 0))
      (check-eq? (wave-execution-outcome-kind result) 'done)
      (check-equal? (wave-execution-outcome-message result) "fast"))

    (test-case "blocking runner → timed-out exactly once, cancel! invoked"
      (define started (make-semaphore 0))
      (define release (make-semaphore 0))
      (define cancel-invoked? #f)
      (define runner-exited? #f)
      (define port
        (make-wave-runner-port (lambda (idx)
                                 (semaphore-post started)
                                 (semaphore-wait release) ;; pending tool: blocks until cancelled
                                 (set! runner-exited? #t)
                                 (wave-execution-outcome 'cancelled "tool aborted"))
                               #:cancel! (lambda ()
                                           (set! cancel-invoked? #t)
                                           (semaphore-post release))))
      (define result-box (box #f))
      (define t (thread (lambda () (set-box! result-box (run-wave-with-timeout port 1 0)))))
      (semaphore-wait started) ;; runner is now mid-flight (pending tool)
      (thread-wait t)
      (define result (unbox result-box))
      (check-eq? (wave-execution-outcome-kind result)
                 'timed-out
                 "deadline is authoritative — the late cancelled outcome must not leak through")
      (check-true cancel-invoked? "timeout must request cancellation of the pending tool")
      (check-true runner-exited?
                  "runner thread must exit after cancellation (no leak into next wave)"))

    (test-case "cancel-requested? polling: pending tool aborts mid-run → cancelled"
      (define started (make-semaphore 0))
      (define abort (make-semaphore 0))
      (define cancelled? #f)
      (define port
        (make-wave-runner-port (lambda (idx)
                                 (semaphore-post started)
                                 (semaphore-wait abort) ;; simulate tool polling loop
                                 (if ((gsd-wave-runner-port-cancel-requested? port))
                                     (wave-execution-outcome 'cancelled "pending tool cancelled")
                                     (wave-execution-outcome 'done "completed")))
                               #:cancel! (lambda ()
                                           (set! cancelled? #t)
                                           (semaphore-post abort))
                               #:cancel-requested? (lambda () cancelled?)))
      (define result-box (box #f))
      (define t (thread (lambda () (set-box! result-box (run-wave-with-timeout port 10 0)))))
      (semaphore-wait started)
      ;; mid-run: signal cancellation exactly as a campaign /cancel would
      ((gsd-wave-runner-port-cancel! port))
      (thread-wait t)
      (check-eq? (wave-execution-outcome-kind (unbox result-box)) 'cancelled))

    (test-case "timeout never yields a done outcome"
      (define port
        (make-wave-runner-port (lambda (idx)
                                 (sleep 10)
                                 (wave-execution-outcome 'done "too late"))))
      (define result (run-wave-with-timeout port 1 0))
      (check-eq? (wave-execution-outcome-kind result) 'timed-out))))

;; ============================================================
;; Run
;; ============================================================

(define all-suites
  (test-suite "wave runner port"
    outcome-suite
    port-suite
    timeout-suite))

(exit (if (zero? (run-tests all-suites)) 0 1))

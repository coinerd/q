#lang racket/base

;; tests/test-provider-health.rkt — Provider Health Gate (NR-3) tests
;;
;; Tests the sliding-window failure tracker that prevents futile retries
;; when a provider is consistently failing.

(require rackunit
         "../runtime/provider-health.rkt"
         "../runtime/auto-retry.rkt")

;; -------------------------------------------------------
;; 1. Struct API and constructor
;; -------------------------------------------------------

(test-case "make-provider-health creates empty tracker"
  (define h (make-provider-health))
  (check-pred provider-health? h)
  (check-equal? (recent-failure-count h) 0 "no failures initially"))

;; -------------------------------------------------------
;; 2. provider-healthy? with sliding window
;; -------------------------------------------------------

(test-case "healthy with 0 failures"
  (define h (make-provider-health))
  (check-true (provider-healthy? h) "no failures → healthy"))

(test-case "healthy with 1 failure (below threshold)"
  (define h (make-provider-health))
  (record-failure! h)
  (check-true (provider-healthy? h) "1 failure < 3 threshold → healthy"))

(test-case "healthy with 2 failures (below threshold)"
  (define h (make-provider-health))
  (record-failure! h)
  (record-failure! h)
  (check-true (provider-healthy? h) "2 failures < 3 threshold → healthy"))

(test-case "unhealthy with 3 failures (at threshold)"
  (define h (make-provider-health))
  (record-failure! h)
  (record-failure! h)
  (record-failure! h)
  (check-false (provider-healthy? h) "3 failures ≥ 3 threshold → unhealthy"))

;; -------------------------------------------------------
;; 3. Sliding window: old failures pruned
;; -------------------------------------------------------

(test-case "old failures outside window don't count"
  (define h (make-provider-health))
  (define fake-now 1000000.0) ; 1000s in ms
  ;; Record 3 failures at fake-now
  (record-failure! h #:now-proc (lambda () fake-now))
  (record-failure! h #:now-proc (lambda () fake-now))
  (record-failure! h #:now-proc (lambda () fake-now))
  ;; At fake-now (inside window), unhealthy
  (check-false (provider-healthy? h #:window-secs 60 #:now-proc (lambda () fake-now)))
  ;; At fake-now + 120s (outside window), healthy again
  (check-true (provider-healthy? h #:window-secs 60 #:now-proc (lambda () (+ fake-now 120000)))))

;; -------------------------------------------------------
;; 4. Success resets the failure window
;; -------------------------------------------------------

(test-case "record-success! resets failure tracking"
  (define h (make-provider-health))
  (record-failure! h)
  (record-failure! h)
  (record-failure! h)
  (check-false (provider-healthy? h) "unhealthy before success")
  (record-success! h)
  ;; After a success, failures before the success are pruned
  (check-true (provider-healthy? h) "healthy after success resets"))

;; -------------------------------------------------------
;; 5. Configurable threshold and window
;; -------------------------------------------------------

(test-case "custom threshold: unhealthy at 5 failures"
  (define h (make-provider-health))
  (for ([_ (in-range 4)])
    (record-failure! h))
  (check-true (provider-healthy? h #:threshold 5) "4 < 5 threshold → healthy")
  (record-failure! h)
  (check-false (provider-healthy? h #:threshold 5) "5 ≥ 5 threshold → unhealthy"))

;; -------------------------------------------------------
;; 6. with-auto-retry integration: health-check-proc
;; -------------------------------------------------------

(test-case "with-auto-retry: health-check-proc denies retry"
  (define call-count (box 0))
  (define attempts (box 0))
  (define (fail-thunk)
    (set-box! call-count (add1 (unbox call-count)))
    (raise (exn:fail:network "simulated timeout" (current-continuation-marks))))
  (define (deny-retry exn attempt)
    (set-box! attempts (add1 (unbox attempts)))
    #f)
  ;; With health-check-proc returning #f, no retries should happen
  (check-exn exn:fail?
             (lambda () (with-auto-retry fail-thunk #:max-retries 3 #:health-check-proc deny-retry)))
  (check-equal? (unbox call-count) 1 "thunk called exactly once (no retry)")
  (check-equal? (unbox attempts) 1 "health-check-proc called once"))

(test-case "with-auto-retry: health-check-proc allows retry"
  (define call-count (box 0))
  (define (fail-thunk)
    (set-box! call-count (add1 (unbox call-count)))
    (when (< (unbox call-count) 2)
      (raise (exn:fail:network "simulated timeout" (current-continuation-marks))))
    'success)
  (define (allow-retry exn attempt)
    #t)
  (check-equal?
   (with-auto-retry fail-thunk #:max-retries 3 #:base-delay-ms 1 #:health-check-proc allow-retry)
   'success)
  (check-equal? (unbox call-count) 2 "thunk called twice (1 retry)"))

;; -------------------------------------------------------
;; 7. Health tracker integration with with-auto-retry
;; -------------------------------------------------------

(test-case "health tracker: 3 failures → skip retry"
  (define h (make-provider-health))
  (define call-count (box 0))
  (define (fail-thunk)
    (set-box! call-count (add1 (unbox call-count)))
    (raise (exn:fail:network "timeout" (current-continuation-marks))))
  (define (health-check exn attempt)
    (record-failure! h)
    (provider-healthy? h))
  ;; With health gate, after 1 failure (tracker still healthy), allow 1 retry.
  ;; But the first call records a failure (count=1, healthy=true → retry).
  ;; Second call records failure (count=2, healthy=true → retry).
  ;; Third call records failure (count=3, healthy=false → deny).
  ;; So 3 calls before circuit breaks.
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry fail-thunk
                                #:max-retries 10
                                #:base-delay-ms 1
                                #:health-check-proc health-check)))
  ;; At least 3 attempts (1 initial + 2 retries before health gate triggers)
  ;; Then health-check on 3rd failure denies → no 4th attempt
  (check-true (>= (unbox call-count) 3) "at least 3 attempts before health gate")
  (check-true (<= (unbox call-count) 4) "health gate stops retries quickly")
  (check-false (provider-healthy? h) "tracker should be unhealthy"))

;; -------------------------------------------------------
;; 8. On-success hook records success
;; -------------------------------------------------------

(test-case "with-auto-retry: #:on-success called after success"
  (define success-called (box #f))
  (define (succeed)
    'ok)
  (check-equal? (with-auto-retry succeed #:on-success (lambda () (set-box! success-called #t))) 'ok)
  (check-true (unbox success-called) "on-success called"))

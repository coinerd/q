#lang racket/base

;; W1 NR-1: Mid-Stream Stall Classification & Progressive Circuit Breaker
;; Tests for output-chars metadata, minimal-output-stall? predicate,
;; stall-severity classification, and progressive circuit breaker behavior.

(require rackunit
         "../runtime/auto-retry.rkt"
         "../llm/stream.rkt")

;; Helper: construct a stream timeout exception with output-chars
(define (stream-timeout-exn #:heartbeats? [hb? #f]
                            #:any-data? [any? #t]
                            #:phase [phase 'content]
                            #:output-chars [chars 0])
  (exn:fail:network:timeout:stream "Stream timeout"
                                   (current-continuation-marks)
                                   hb?
                                   any?
                                   phase
                                   chars))

(test-case "exn:fail:network:timeout:stream carries output-chars field"
  (define e (stream-timeout-exn #:output-chars 250))
  (check-equal? (exn:fail:network:timeout:stream-output-chars e) 250))

(test-case "minimal-output-stall? classifies < threshold chars as stall"
  (check-true (minimal-output-stall? (stream-timeout-exn #:output-chars 50)))
  (check-true (minimal-output-stall? (stream-timeout-exn #:output-chars 99)))
  (check-false (minimal-output-stall? (stream-timeout-exn #:output-chars 100)))
  (check-false (minimal-output-stall? (stream-timeout-exn #:output-chars 500))))

(test-case "held request (zero data) is not minimal-output-stall"
  (check-false (minimal-output-stall? (stream-timeout-exn #:any-data? #f #:phase 'initial))))

(test-case "stall-severity returns correct classification"
  (check-equal? (stall-severity (stream-timeout-exn #:any-data? #f #:phase 'initial)) 'initial-hold)
  (check-equal? (stall-severity (stream-timeout-exn #:output-chars 50)) 'minimal-output)
  (check-equal? (stall-severity (stream-timeout-exn #:output-chars 500)) 'partial-output))

(test-case "progressive breaker: 2 consecutive minimal-output stalls → circuit-break"
  (define attempt (box 0))
  (define circuit-breaks (box 0))
  (define retries (box 0))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (set-box! attempt (add1 (unbox attempt)))
                                  (raise (stream-timeout-exn #:output-chars 10)))
                                #:max-retries 3
                                #:base-delay-ms 1
                                #:stall-max-consecutive 2
                                #:stall-min-output-chars 100
                                #:on-retry (lambda args (set-box! retries (add1 (unbox retries))))
                                #:on-circuit-break
                                (lambda (_ exn)
                                  (set-box! circuit-breaks (add1 (unbox circuit-breaks)))))))
  ;; First stall retries, second consecutive triggers progressive break
  (check-equal? (unbox attempt) 2)
  (check-equal? (unbox retries) 1)
  (check-equal? (unbox circuit-breaks) 1))

(test-case "progressive breaker: partial-output stall gets full retry budget"
  (define attempt (box 0))
  (define retries (box 0))
  (define circuit-breaks (box 0))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (set-box! attempt (add1 (unbox attempt)))
                                  (raise (stream-timeout-exn #:output-chars 500)))
                                #:max-retries 2
                                #:base-delay-ms 1
                                #:stall-max-consecutive 2
                                #:stall-min-output-chars 100
                                #:on-retry (lambda args (set-box! retries (add1 (unbox retries))))
                                #:on-circuit-break
                                (lambda (_ exn)
                                  (set-box! circuit-breaks (add1 (unbox circuit-breaks)))))))
  ;; 500 chars is partial-output → full retry, no progressive break
  (check-equal? (unbox attempt) 3)
  (check-equal? (unbox retries) 2)
  (check-equal? (unbox circuit-breaks) 0))

(test-case "progressive breaker: non-stall error resets consecutive counter"
  (define attempt (box 0))
  (define retries (box 0))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry
                (lambda ()
                  (set-box! attempt (add1 (unbox attempt)))
                  (cond
                    [(= (unbox attempt) 1)
                     ;; First: rate-limit error (not a stall)
                     (raise (exn:fail "429 Too Many Requests" (current-continuation-marks)))]
                    ;; Then minimal-output stalls
                    [(<= (unbox attempt) 4) (raise (stream-timeout-exn #:output-chars 10))]))
                #:max-retries 5
                #:base-delay-ms 1
                #:stall-max-consecutive 2
                #:stall-min-output-chars 100
                #:on-retry (lambda args (set-box! retries (add1 (unbox retries)))))))
  ;; Attempt 1: rate-limit → retry (reset stall counter)
  ;; Attempt 2: minimal stall → retry (stall counter = 1)
  ;; Attempt 3: minimal stall → progressive break (stall counter = 2)
  (check-equal? (unbox attempt) 3)
  (check-equal? (unbox retries) 2))

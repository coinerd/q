#lang racket

;; @speed fast
;; @suite provider

;; BOUNDARY: integration

;;; tests/test-provider-retry-telemetry.rkt — NR-5: telemetry delay correctness
;;;
;;; Tests that call-with-provider-retry passes the actual computed backoff
;;; delay to the auto-retry.start event, NOT a hardcoded 0.

(require rackunit
         rackunit/text-ui
         racket/match
         "../runtime/provider-retry.rkt"
         "../runtime/auto-retry.rkt"
         "../llm/stream.rkt"
         "../agent/event-emitter.rkt"
         "../util/event/event.rkt"
         "../util/event/event-bus.rkt")

(define event-payload* event-payload)

;; Helper: capture all events emitted to a bus
(define (make-capture-bus)
  (define bus (make-event-bus))
  (define events (box '()))
  (subscribe! bus
              (lambda (evt)
                (define payload (event-payload* evt))
                (set-box! events (append (unbox events) (list payload))))
              #:filter (lambda (evt) (equal? (event-ev evt) "auto-retry.start")))
  (values bus (lambda () (unbox events))))

(define-test-suite
 provider-retry-telemetry-tests
 (test-case "NR-5: retry event carries actual delay, not 0"
   ;; Set up a bus to capture retry events
   (define-values (bus get-events) (make-capture-bus))

   ;; Create a thunk that always fails with a retryable timeout error
   ;; @boundary unit
   (define attempt-count (box 0))
   (define (failing-attempt ctx settings)
     (set-box! attempt-count (add1 (unbox attempt-count)))
     (raise (exn:fail:network (format "timeout waiting for response (attempt ~a)"
                                      (unbox attempt-count))
                              (current-continuation-marks))))

   ;; Use a deterministic random source so the delay is predictable
   (parameterize ([current-random-source (lambda () 1.0)])
     ;; call-with-provider-retry should emit auto-retry.start with a delay > 0
     (with-handlers ([exn:fail? (lambda (_) (void))])
       (call-with-provider-retry failing-attempt
                                 (list (hash 'role "user" 'content "test"))
                                 (hash 'max-tokens 1000)
                                 bus
                                 "test-session"
                                 "test-turn"
                                 300)))

   ;; Wait for events to propagate (async emit)
   (sleep 0.1)
   (define events (get-events))
   (check-true (positive? (length events)) "at least one retry event should be emitted")
   (for ([payload (in-list events)])
     (define delay (hash-ref payload 'delayMs 0))
     (check-true (positive? delay)
                 (format "delayMs should be > 0 (actual: ~a), not hardcoded 0" delay))))
 (test-case "NR-5: circuit-break event has delay 0"
   ;; Circuit breaker skips retry, so delay should be 0
   (define-values (bus get-events) (make-capture-bus))

   (define (held-request-attempt ctx settings)
     ;; Raise a held-request timeout (zero data, initial phase)
     (raise (exn:fail:network:timeout:stream "timeout: zero chunks received in initial phase"
                                             (current-continuation-marks)
                                             #f ; received-heartbeats?
                                             #f ; received-any-data?
                                             'initial
                                             0))) ; output-chars

   (with-handlers ([exn:fail? (lambda (_) (void))])
     (call-with-provider-retry held-request-attempt
                               (list (hash 'role "user" 'content "test"))
                               (hash 'max-tokens 1000)
                               bus
                               "test-session"
                               "test-turn"
                               300))

   (sleep 0.1)
   (define events (get-events))
   ;; Circuit-break events should have delay 0 (no retry delay)
   (for ([payload (in-list events)])
     (define delay (hash-ref payload 'delayMs 'missing))
     (check-equal? delay 0 (format "circuit-break delay should be 0 (actual: ~a)" delay)))))

(run-tests provider-retry-telemetry-tests)

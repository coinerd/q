#lang racket

;; tests/test-wait-idle.rkt — FEAT-78: waitForIdle in SDK

(require rackunit
         rackunit/text-ui
         "../agent/event-bus.rkt"
         "../util/protocol-types.rkt"
         "../agent/wait-idle.rkt")

(define (make-timestamp)
  (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000))))

;; Helper: publish events from a thread after the subscriber is ready.
;; Uses a channel to synchronize instead of sleep.
(define (publish-after-ready bus ready-ch . events)
  (thread (lambda ()
            ;; Wait for signal that subscriber is ready
            (channel-get ready-ch)
            ;; Small yield to ensure the subscriber's sync is entered
            (sync/timeout 0.01 never-evt)
            (for ([evt (in-list events)])
              (publish! bus evt)))))

(define wait-idle-tests
  (test-suite "waitForIdle"

    (test-case "wait-for-idle! returns idle on iteration.completed"
      (define bus (make-event-bus))
      (define sid "test-session-1")
      (define ready-ch (make-channel))
      (publish-after-ready bus
                           ready-ch
                           (make-event "iteration.completed" (make-timestamp) sid #f (hasheq)))
      ;; Start waiting in a thread so we can signal readiness
      (define result-box (box #f))
      (thread (lambda ()
                (set-box! result-box (wait-for-idle! bus sid))
                ;; Signal that we're done (result available)
                (void)))
      ;; Give the waiter a moment to subscribe, then signal the publisher
      (sync/timeout 0.05 never-evt)
      (channel-put ready-ch 'go)
      ;; Wait for result
      (sync/timeout 2.0 (alarm-evt (+ (current-inexact-milliseconds) 2000)))
      (check-equal? (unbox result-box) 'idle))

    (test-case "wait-for-idle! returns timeout when no event"
      (define bus (make-event-bus))
      (define sid "test-session-2")
      (define result (wait-for-idle! bus sid 100))
      (check-equal? result 'timeout))

    (test-case "wait-for-idle! ignores events from other sessions"
      (define bus (make-event-bus))
      (define sid "correct-session")
      (define ready-ch (make-channel))
      (publish-after-ready
       bus
       ready-ch
       (make-event "iteration.completed" (make-timestamp) "wrong-session" #f (hasheq))
       (make-event "iteration.completed" (make-timestamp) sid #f (hasheq)))
      (define result-box (box #f))
      (thread (lambda ()
                (set-box! result-box (wait-for-idle! bus sid 2000))
                (void)))
      (sync/timeout 0.05 never-evt)
      (channel-put ready-ch 'go)
      (sync/timeout 2.0 (alarm-evt (+ (current-inexact-milliseconds) 2000)))
      (check-equal? (unbox result-box) 'idle))

    (test-case "wait-for-idle! responds to iteration.ended too"
      (define bus (make-event-bus))
      (define sid "test-session-3")
      (define ready-ch (make-channel))
      (publish-after-ready bus
                           ready-ch
                           (make-event "iteration.ended" (make-timestamp) sid #f (hasheq)))
      (define result-box (box #f))
      (thread (lambda ()
                (set-box! result-box (wait-for-idle! bus sid))
                (void)))
      (sync/timeout 0.05 never-evt)
      (channel-put ready-ch 'go)
      (sync/timeout 2.0 (alarm-evt (+ (current-inexact-milliseconds) 2000)))
      (check-equal? (unbox result-box) 'idle))))

(run-tests wait-idle-tests)

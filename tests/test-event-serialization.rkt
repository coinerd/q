#lang racket

;;; tests/test-event-serialization.rkt — tests for event serialization queue (#775)

(require rackunit
         rackunit/text-ui
         "../agent/event-bus.rkt"
         "../agent/types.rkt")

(define (make-test-event topic payload)
  (make-event topic (current-inexact-milliseconds) "test-session" "test-turn" payload))

(test-case "serialization disabled by default"
  (define bus (make-event-bus))
  (check-false (serialization-enabled? bus)))

(test-case "enable-serialization! enables serialization"
  (define bus (make-event-bus))
  (enable-serialization! bus)
  (check-true (serialization-enabled? bus)))

(test-case "events processed in emission order"
  (define bus (make-event-bus))
  (enable-serialization! bus)
  (define results (box '()))
  (subscribe! bus (lambda (evt)
                    (set-box! results
                              (cons (event-ev evt) (unbox results)))))
  (publish! bus (make-test-event "a" '()))
  (publish! bus (make-test-event "b" '()))
  (publish! bus (make-test-event "c" '()))
  (drain-event-queue! bus)
  (define final (reverse (unbox results)))
  (check-equal? final '("a" "b" "c")))

(test-case "failing handler doesn't block subsequent events"
  (define bus (make-event-bus))
  (enable-serialization! bus)
  (define results (box '()))
  (subscribe! bus (lambda (evt)
                    (when (string=? (event-ev evt) "fail")
                      (error "boom"))
                    (set-box! results
                              (cons (event-ev evt) (unbox results)))))
  (publish! bus (make-test-event "before" '()))
  (publish! bus (make-test-event "fail" '()))
  (publish! bus (make-test-event "after" '()))
  (drain-event-queue! bus)
  (define final (reverse (unbox results)))
  (check-true (not (not (index-of final "before"))))
  (check-true (not (not (index-of final "after"))))
  (check-true (not (index-of final "fail"))))

(test-case "drain-event-queue! completes all pending events"
  (define bus (make-event-bus))
  (enable-serialization! bus)
  (define count (box 0))
  (subscribe! bus (lambda (evt)
                    (set-box! count (add1 (unbox count)))))
  (for ([i (in-range 100)])
    (publish! bus (make-test-event "evt" '())))
  (drain-event-queue! bus)
  (check-equal? (unbox count) 100))

(test-case "non-serialized bus still works normally"
  (define bus (make-event-bus))
  (define results (box '()))
  (subscribe! bus (lambda (evt)
                    (set-box! results
                              (cons (event-ev evt) (unbox results)))))
  (publish! bus (make-test-event "sync" '()))
  (check-equal? (reverse (unbox results)) '("sync")))

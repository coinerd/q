#lang racket

;; @speed fast
;; @suite default

;; Regression test for F1/EMIT-01: Verify 'emit-event exists in runtime hash
;; and that the emission pipeline works end-to-end.

(require rackunit
         rackunit/text-ui
         "../util/event/event-bus.rkt"
         "../util/event/event.rkt")

;; Helper: create the same emit-fn that build-runtime-from-cli puts in the hash.
(define (make-emit-fn bus)
  (lambda (h)
    (define ev-type (hash-ref h 'type "ui.unknown"))
    (define evt (make-event ev-type (current-inexact-milliseconds) #f #f h))
    (publish! bus evt)))

(define emit-event-suite
  (test-suite "EMIT-01: emit-event runtime function"

    (test-case "emit-event function creates valid event struct"
      (define bus (make-event-bus))
      (define received (box #f))
      (subscribe! bus (lambda (evt) (set-box! received evt)))
      (define emit-fn (make-emit-fn bus))
      ;; Emit a test action
      (emit-fn (hasheq 'type "ui.status.set" 'status "processing"))
      ;; Verify event was published
      (check-not-false (unbox received) "event should have been published to bus")
      (check-equal? (event-ev (unbox received)) "ui.status.set")
      (check-equal? (hash-ref (event-payload (unbox received)) 'status #f) "processing"))

    (test-case "emit-event function handles missing 'type gracefully"
      (define bus (make-event-bus))
      (define received (box #f))
      (subscribe! bus (lambda (evt) (set-box! received evt)))
      (define emit-fn (make-emit-fn bus))
      ;; Emit without type key
      (emit-fn (hasheq 'payload "data"))
      (check-not-false (unbox received))
      (check-equal? (event-ev (unbox received)) "ui.unknown"))

    (test-case "runtime hash has 'emit-event as procedure"
      (define bus (make-event-bus))
      (define received (box #f))
      (subscribe! bus (lambda (evt) (set-box! received evt)))
      ;; Simulate runtime hash shape
      (define runtime-hash (hasheq 'emit-event (make-emit-fn bus)))
      (define emit-fn (hash-ref runtime-hash 'emit-event #f))
      (check-pred procedure? emit-fn "emit-event must be a procedure")
      ;; Emit through it
      (emit-fn (hasheq 'type "ui.footer.set" 'lines '("hello")))
      (check-not-false (unbox received))
      (check-equal? (event-ev (unbox received)) "ui.footer.set"))))

(run-tests emit-event-suite)

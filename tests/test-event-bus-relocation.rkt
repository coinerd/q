#lang racket/base

;; q/tests/test-event-bus-relocation.rkt — Regression test for AXIS1-F12
;; Verify event bus works from canonical location util/event/event-bus.rkt.

(require rackunit
         "../util/event/event-bus.rkt"
         "../util/event/event.rkt"
         "../agent/event-bus.rkt")

(test-case "event-bus relocation: import from util/event/ works"
  (define bus (make-event-bus))
  (check-pred event-bus? bus)
  (define results '())
  (subscribe! bus (lambda (e) (set! results (cons e results))))
  (publish! bus (make-event "test-relocation" 0 "s1" #f (hasheq)))
  (check-equal? (length results) 1))

(test-case "event-bus relocation: shim at agent/ still works"
  ;; Both imports coexist — no conflicts
  (define bus1 (make-event-bus))
  (check-pred event-bus? bus1))

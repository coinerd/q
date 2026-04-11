#lang racket

(require rackunit
         "../agent/state.rkt")

;; ============================================================
;; Constructor & accessors
;; ============================================================

(test-case "make-loop-state creates state with correct fields"
  (define st (make-loop-state "sess-1" "turn-1"))
  (check-pred loop-state? st)
  (check-equal? (loop-state-session-id st) "sess-1")
  (check-equal? (loop-state-turn-id st) "turn-1")
  (check-equal? (loop-state-messages st) '())
  (check-equal? (loop-state-events st) '()))

;; ============================================================
;; Adding messages
;; ============================================================

(test-case "state-add-message! appends a message"
  (define st (make-loop-state "s" "t"))
  (state-add-message! st 'msg1)
  (check-equal? (loop-state-messages st) '(msg1)))

(test-case "state-add-message! preserves order"
  (define st (make-loop-state "s" "t"))
  (state-add-message! st 'first)
  (state-add-message! st 'second)
  (state-add-message! st 'third)
  (check-equal? (loop-state-messages st) '(first second third)))

;; ============================================================
;; Adding events
;; ============================================================

(test-case "state-add-event! appends an event"
  (define st (make-loop-state "s" "t"))
  (state-add-event! st 'evt1)
  (check-equal? (loop-state-events st) '(evt1)))

(test-case "state-add-event! preserves order"
  (define st (make-loop-state "s" "t"))
  (state-add-event! st 'a)
  (state-add-event! st 'b)
  (check-equal? (loop-state-events st) '(a b)))

;; ============================================================
;; Independence of messages and events
;; ============================================================

(test-case "messages and events are independent boxes"
  (define st (make-loop-state "s" "t"))
  (state-add-message! st 'm)
  (state-add-event! st 'e)
  (check-equal? (loop-state-messages st) '(m))
  (check-equal? (loop-state-events st) '(e)))

(test-case "multiple snapshots are independent"
  (define st (make-loop-state "s" "t"))
  (state-add-message! st 'm1)
  (define snap1 (loop-state-messages st))
  (state-add-message! st 'm2)
  (define snap2 (loop-state-messages st))
  (check-equal? snap1 '(m1))
  (check-equal? snap2 '(m1 m2)))

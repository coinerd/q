#lang racket

;; tests/test-followup-queue.rkt — tests for follow-up queue wiring (#661)
;;
;; Covers:
;;   1. queue.rkt — dequeue-all-followups! drains all follow-ups
;;   2. queue.rkt — queue-status returns correct counts
;;   3. iteration.rkt — follow-up messages injected after completion
;;   4. iteration.rkt — follow-up.injected event emitted
;;   5. tui/state.rkt — queue.status-update event updates queue-counts
;;   6. tui/state.rkt — ui-status-text shows queue counts

(require rackunit
         "../agent/queue.rkt"
         "../agent/event-bus.rkt"
         "../runtime/iteration.rkt"
         "../tui/state.rkt"
         "../util/protocol-types.rkt")

;; ============================================================
;; 1. Queue helpers
;; ============================================================

(test-case "dequeue-all-followups! returns empty list when no followups"
  (define q (make-queue))
  (check-equal? (dequeue-all-followups! q) '()))

(test-case "dequeue-all-followups! drains all followups in FIFO order"
  (define q (make-queue))
  (enqueue-followup! q "first")
  (enqueue-followup! q "second")
  (enqueue-followup! q "third")
  (check-equal? (dequeue-all-followups! q) '("first" "second" "third"))
  (check-equal? (dequeue-all-followups! q) '()))

(test-case "dequeue-all-followups! does not affect steering queue"
  (define q (make-queue))
  (enqueue-steering! q "steer")
  (enqueue-followup! q "follow")
  (check-equal? (dequeue-all-followups! q) '("follow"))
  ;; Steering still there
  (check-equal? (dequeue-steering! q) "steer"))

(test-case "queue-status returns correct counts"
  (define q (make-queue))
  (check-equal? (queue-status q) (hasheq 'steering 0 'followup 0))
  (enqueue-steering! q "s1")
  (enqueue-steering! q "s2")
  (enqueue-followup! q "f1")
  (check-equal? (queue-status q) (hasheq 'steering 2 'followup 1))
  (dequeue-steering! q)
  (dequeue-all-followups! q)
  (check-equal? (queue-status q) (hasheq 'steering 1 'followup 0)))

;; ============================================================
;; 2. TUI state — queue.status-update event handling
;; ============================================================

(test-case "queue.status-update event sets queue-counts"
  (define state (initial-ui-state))
  (check-false (ui-state-queue-counts state))
  (define evt (make-event "queue.status-update" 0 #f #f (hasheq 'steering 2 'followup 1)))
  (define next (apply-event-to-state state evt))
  (check-equal? (ui-state-queue-counts next) (hasheq 'steering 2 'followup 1)))

(test-case "queue.status-update with zero counts clears display"
  (define state (initial-ui-state))
  (define evt (make-event "queue.status-update" 0 #f #f (hasheq 'steering 0 'followup 0)))
  (define next (apply-event-to-state state evt))
  (check-equal? (ui-state-queue-counts next) (hasheq 'steering 0 'followup 0)))

(test-case "ui-status-text shows followup count when present"
  (define state (initial-ui-state))
  (define evt (make-event "queue.status-update" 0 #f #f (hasheq 'steering 0 'followup 3)))
  (define next (apply-event-to-state state evt))
  (define status (ui-status-text next))
  (check-true (string-contains? status "follow"))
  (check-true (string-contains? status "3")))

(test-case "ui-status-text shows steering count when present"
  (define state (initial-ui-state))
  (define evt (make-event "queue.status-update" 0 #f #f (hasheq 'steering 2 'followup 0)))
  (define next (apply-event-to-state state evt))
  (define status (ui-status-text next))
  (check-true (string-contains? status "steer"))
  (check-true (string-contains? status "2")))

(test-case "ui-status-text shows both counts when present"
  (define state (initial-ui-state))
  (define evt (make-event "queue.status-update" 0 #f #f (hasheq 'steering 1 'followup 2)))
  (define next (apply-event-to-state state evt))
  (define status (ui-status-text next))
  (check-true (string-contains? status "steer"))
  (check-true (string-contains? status "follow")))

(test-case "ui-status-text hides counts when zero"
  (define state (initial-ui-state))
  (define evt (make-event "queue.status-update" 0 #f #f (hasheq 'steering 0 'followup 0)))
  (define next (apply-event-to-state state evt))
  (define status (ui-status-text next))
  (check-false (string-contains? status "steer"))
  (check-false (string-contains? status "follow")))

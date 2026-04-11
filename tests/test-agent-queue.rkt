#lang racket

(require rackunit
         "../agent/queue.rkt")

;; ============================================================
;; Constructor
;; ============================================================

(test-case "make-queue creates an empty queue"
  (define q (make-queue))
  (check-pred queue? q)
  (check-true (queue-empty? q)))

;; ============================================================
;; Steering queue
;; ============================================================

(test-case "enqueue/dequeue steering FIFO"
  (define q (make-queue))
  (enqueue-steering! q "first")
  (enqueue-steering! q "second")
  (check-false (queue-empty? q))
  (check-equal? (dequeue-steering! q) "first")
  (check-equal? (dequeue-steering! q) "second")
  (check-true (queue-empty? q)))

(test-case "dequeue-steering returns #f when empty"
  (define q (make-queue))
  (check-false (dequeue-steering! q)))

;; ============================================================
;; Follow-up queue
;; ============================================================

(test-case "enqueue/dequeue followup FIFO"
  (define q (make-queue))
  (enqueue-followup! q "later")
  (check-false (queue-empty? q))
  (check-equal? (dequeue-followup! q) "later")
  (check-true (queue-empty? q)))

(test-case "dequeue-followup returns #f when empty"
  (define q (make-queue))
  (check-false (dequeue-followup! q)))

;; ============================================================
;; Independence of queues
;; ============================================================

(test-case "steering and followup are independent"
  (define q (make-queue))
  (enqueue-steering! q "steer")
  (enqueue-followup! q "follow")
  (check-false (queue-empty? q))
  (check-equal? (dequeue-steering! q) "steer")
  (check-false (queue-empty? q))
  (check-equal? (dequeue-followup! q) "follow")
  (check-true (queue-empty? q)))

(test-case "queue-empty? requires both sub-queues to be empty"
  (define q (make-queue))
  (enqueue-steering! q "x")
  (check-false (queue-empty? q))
  (dequeue-steering! q)
  (check-true (queue-empty? q)))

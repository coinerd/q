#lang racket

;; tests/test-queue-events.rkt — FEAT-67: queue.update events + O(1) deque

(require rackunit
         "../agent/queue.rkt")

;; ============================================================
;; O(1) correctness
;; ============================================================

(test-case "enqueue/dequeue preserves FIFO order"
  (define q (make-queue))
  (enqueue-steering! q "first")
  (enqueue-steering! q "second")
  (enqueue-steering! q "third")
  (check-equal? (dequeue-steering! q) "first")
  (check-equal? (dequeue-steering! q) "second")
  (check-equal? (dequeue-steering! q) "third")
  (check-false (dequeue-steering! q)))

(test-case "followup FIFO order"
  (define q (make-queue))
  (enqueue-followup! q "a")
  (enqueue-followup! q "b")
  (check-equal? (dequeue-followup! q) "a")
  (check-equal? (dequeue-followup! q) "b"))

(test-case "dequeue-all-followups returns all in order"
  (define q (make-queue))
  (enqueue-followup! q "x")
  (enqueue-followup! q "y")
  (enqueue-followup! q "z")
  (define all (dequeue-all-followups! q))
  (check-equal? all '("x" "y" "z"))
  (check-true (queue-empty? q)))

(test-case "queue-status reports correct counts"
  (define q (make-queue))
  (enqueue-steering! q "s1")
  (enqueue-steering! q "s2")
  (enqueue-followup! q "f1")
  (define status (queue-status q))
  (check-equal? (hash-ref status 'steering) 2)
  (check-equal? (hash-ref status 'followup) 1))

;; ============================================================
;; FEAT-67: Event emission
;; ============================================================

(test-case "enqueue-steering! emits queue.update event"
  (define q (make-queue))
  (define events '())
  (queue-set-event-callback! q
                             (lambda (type details) (set! events (cons (list type details) events))))
  (enqueue-steering! q "msg")
  (check-equal? (length events) 1)
  (define evt (car events))
  (check-equal? (car evt) 'queue.update)
  (check-equal? (hash-ref (cadr evt) 'action) 'enqueue)
  (check-equal? (hash-ref (cadr evt) 'queue) 'steering))

(test-case "dequeue-steering! emits queue.update event"
  (define q (make-queue))
  (enqueue-steering! q "msg")
  (define events '())
  (queue-set-event-callback! q
                             (lambda (type details) (set! events (cons (list type details) events))))
  (dequeue-steering! q)
  (check-equal? (length events) 1)
  (check-equal? (hash-ref (cadr (car events)) 'action) 'dequeue))

(test-case "enqueue-followup! emits queue.update event"
  (define q (make-queue))
  (define events '())
  (queue-set-event-callback! q
                             (lambda (type details) (set! events (cons (list type details) events))))
  (enqueue-followup! q "fmsg")
  (check-equal? (length events) 1)
  (check-equal? (hash-ref (cadr (car events)) 'queue) 'followup))

(test-case "dequeue-all-followups! emits queue.update event"
  (define q (make-queue))
  (enqueue-followup! q "a")
  (enqueue-followup! q "b")
  (define events '())
  (queue-set-event-callback! q
                             (lambda (type details) (set! events (cons (list type details) events))))
  (dequeue-all-followups! q)
  (check-equal? (length events) 1)
  (check-equal? (hash-ref (cadr (car events)) 'action) 'dequeue-all)
  (check-equal? (hash-ref (cadr (car events)) 'count) 0))

(test-case "no events without callback"
  (define q (make-queue))
  ;; Should not error
  (enqueue-steering! q "x")
  (dequeue-steering! q))

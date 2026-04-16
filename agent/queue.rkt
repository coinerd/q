#lang racket/base

;; agent/queue.rkt — steering and follow-up queues
;;
;; Provides two FIFO queues for user input during agent execution:
;;   - Steering: input intended to affect the currently running task
;;   - Follow-up: input intended for the next task after completion
;;
;; FEAT-67: O(1) enqueue/dequeue via two-list queue + queue.update events.

(require racket/contract)

;; Agent steering/followup message queue
(provide make-queue
         queue?
         enqueue-steering!
         enqueue-followup!
         dequeue-steering!
         dequeue-followup!
         dequeue-all-followups!
         queue-empty?
         queue-status
         ;; FEAT-67: event emission callback
         queue-set-event-callback!)

;; ============================================================
;; Queue struct — two-list FIFO for O(1) amortized ops
;; ============================================================

;; Each sub-queue uses head (front) and tail (reversed back) lists.
;; Enqueue conses onto tail: O(1)
;; Dequeue pops from head, reverses tail if head empty: amortized O(1)

(struct sub-queue (head-box tail-box) #:transparent)

(struct queue (steering followup semaphore event-callback-box) #:transparent)

;; ============================================================
;; Constructor
;; ============================================================

(define (make-queue)
  (queue (sub-queue (box '()) (box '())) (sub-queue (box '()) (box '())) (make-semaphore 1) (box #f)))

;; ============================================================
;; FEAT-67: Event callback
;; ============================================================

(define (queue-set-event-callback! q cb)
  (set-box! (queue-event-callback-box q) cb))

(define (emit-queue-event! q event-type details)
  (define cb (unbox (queue-event-callback-box q)))
  (when cb
    (cb event-type details)))

;; ============================================================
;; Internal O(1) two-list queue operations
;; ============================================================

(define (sq-enqueue! sq v)
  (set-box! (sub-queue-tail-box sq) (cons v (unbox (sub-queue-tail-box sq)))))

(define (sq-dequeue! sq)
  (when (null? (unbox (sub-queue-head-box sq)))
    ;; Reverse tail into head
    (set-box! (sub-queue-head-box sq) (reverse (unbox (sub-queue-tail-box sq))))
    (set-box! (sub-queue-tail-box sq) '()))
  (define head (unbox (sub-queue-head-box sq)))
  (if (null? head)
      #f
      (begin
        (set-box! (sub-queue-head-box sq) (cdr head))
        (car head))))

(define (sq-empty? sq)
  (and (null? (unbox (sub-queue-head-box sq))) (null? (unbox (sub-queue-tail-box sq)))))

(define (sq-count sq)
  (+ (length (unbox (sub-queue-head-box sq))) (length (unbox (sub-queue-tail-box sq)))))

;; ============================================================
;; Public API
;; ============================================================

(define (enqueue-steering! q input)
  (call-with-semaphore
   (queue-semaphore q)
   (lambda ()
     (sq-enqueue! (queue-steering q) input)
     (emit-queue-event!
      q
      'queue.update
      (hasheq 'action 'enqueue 'queue 'steering 'count (sq-count (queue-steering q)))))))

(define (enqueue-followup! q input)
  (call-with-semaphore
   (queue-semaphore q)
   (lambda ()
     (sq-enqueue! (queue-followup q) input)
     (emit-queue-event!
      q
      'queue.update
      (hasheq 'action 'enqueue 'queue 'followup 'count (sq-count (queue-followup q)))))))

(define (dequeue-steering! q)
  (call-with-semaphore
   (queue-semaphore q)
   (lambda ()
     (define v (sq-dequeue! (queue-steering q)))
     (emit-queue-event!
      q
      'queue.update
      (hasheq 'action 'dequeue 'queue 'steering 'count (sq-count (queue-steering q))))
     v)))

(define (dequeue-followup! q)
  (call-with-semaphore
   (queue-semaphore q)
   (lambda ()
     (define v (sq-dequeue! (queue-followup q)))
     (emit-queue-event!
      q
      'queue.update
      (hasheq 'action 'dequeue 'queue 'followup 'count (sq-count (queue-followup q))))
     v)))

(define (queue-empty? q)
  (call-with-semaphore (queue-semaphore q)
                       (lambda ()
                         (and (sq-empty? (queue-steering q)) (sq-empty? (queue-followup q))))))

(define (dequeue-all-followups! q)
  (call-with-semaphore
   (queue-semaphore q)
   (lambda ()
     (define sq (queue-followup q))
     ;; Reverse tail into head
     (when (null? (unbox (sub-queue-head-box sq)))
       (set-box! (sub-queue-head-box sq) (reverse (unbox (sub-queue-tail-box sq))))
       (set-box! (sub-queue-tail-box sq) '()))
     (define head (unbox (sub-queue-head-box sq)))
     (set-box! (sub-queue-head-box sq) '())
     (emit-queue-event! q 'queue.update (hasheq 'action 'dequeue-all 'queue 'followup 'count 0))
     head)))

(define (queue-status q)
  (call-with-semaphore
   (queue-semaphore q)
   (lambda ()
     (hasheq 'steering (sq-count (queue-steering q)) 'followup (sq-count (queue-followup q))))))

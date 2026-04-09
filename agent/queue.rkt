#lang racket/base

;; agent/queue.rkt — steering and follow-up queues
;;
;; Provides two FIFO queues for user input during agent execution:
;;   - Steering: input intended to affect the currently running task
;;   - Follow-up: input intended for the next task after completion

(require racket/contract)

(provide
 make-queue
 queue?
 enqueue-steering!
 enqueue-followup!
 dequeue-steering!
 dequeue-followup!
 queue-empty?)

;; ============================================================
;; Queue struct — two mutable lists (head/tail for FIFO)
;; ============================================================

(struct queue (steering-box followup-box)
  #:transparent)

;; ============================================================
;; Constructor
;; ============================================================

(define (make-queue)
  (queue (box '()) (box '())))

;; ============================================================
;; Internal FIFO helpers
;; ============================================================

;; Enqueue: add to end
(define (box-enqueue! b v)
  (set-box! b (append (unbox b) (list v))))

;; Dequeue: remove from front, return #f if empty
(define (box-dequeue! b)
  (define lst (unbox b))
  (if (null? lst)
      #f
      (begin
        (set-box! b (cdr lst))
        (car lst))))

;; Check empty
(define (box-empty? b)
  (null? (unbox b)))

;; ============================================================
;; Public API
;; ============================================================

(define (enqueue-steering! q input)
  (box-enqueue! (queue-steering-box q) input))

(define (enqueue-followup! q input)
  (box-enqueue! (queue-followup-box q) input))

(define (dequeue-steering! q)
  (box-dequeue! (queue-steering-box q)))

(define (dequeue-followup! q)
  (box-dequeue! (queue-followup-box q)))

(define (queue-empty? q)
  (and (box-empty? (queue-steering-box q))
       (box-empty? (queue-followup-box q))))

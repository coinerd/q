#lang racket/base

(require racket/list)

;; Shared subagent rate reservation. Capacity checks happen before approval;
;; reservation happens only after approval and is atomic across concurrent jobs.

(provide current-spawn-timestamps
         spawn-rate-capacity?
         reserve-spawn-rate!)

(define current-spawn-timestamps (make-parameter (box '())))
(define SPAWN-RATE-LIMIT 30)
(define SPAWN-RATE-WINDOW 60000)
(define spawn-rate-semaphore (make-semaphore 1))

(define (recent-spawn-timestamps now timestamps)
  (filter (lambda (timestamp)
            (define age (- now timestamp))
            (and (>= age 0) (< age SPAWN-RATE-WINDOW)))
          timestamps))

(define (spawn-rate-capacity? count)
  (call-with-semaphore spawn-rate-semaphore
                       (lambda ()
                         (define now (current-inexact-milliseconds))
                         (define recent
                           (recent-spawn-timestamps now (unbox (current-spawn-timestamps))))
                         (<= (+ (length recent) count) SPAWN-RATE-LIMIT))))

(define (reserve-spawn-rate! count)
  (call-with-semaphore spawn-rate-semaphore
                       (lambda ()
                         (define now (current-inexact-milliseconds))
                         (define timestamps-box (current-spawn-timestamps))
                         (define recent (recent-spawn-timestamps now (unbox timestamps-box)))
                         (cond
                           [(> (+ (length recent) count) SPAWN-RATE-LIMIT) #f]
                           [else
                            (set-box! timestamps-box (append (make-list count now) recent))
                            #t]))))

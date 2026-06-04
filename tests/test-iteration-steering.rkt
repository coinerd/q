#lang racket/base

;; BOUNDARY: unit
;; @suite runtime
;; @speed fast
;; @boundary unit
;; @mutates none
;; tests/test-iteration-steering.rkt — steering queue normalization contracts

(require rackunit
         "../agent/event-bus.rkt"
         "../agent/queue.rkt"
         "../agent/iteration/loop-phases.rkt"
         "../util/message/message.rkt"
         "../util/content/content-parts.rkt")

(define (message-text msg)
  (define part (car (message-content msg)))
  (and (text-part? part) (text-part-text part)))

(test-case "prepare-iteration-context converts raw steering strings to user messages"
  (define bus (make-event-bus))
  (define q (make-queue))
  (enqueue-steering! q "No, use /home/user/src/q-agent/q/website")
  (define result (prepare-iteration-context '() q #f bus #f "s1"))
  (check-equal? (length result) 1)
  (define msg (car result))
  (check-true (message? msg))
  (check-equal? (message-role msg) 'user)
  (check-equal? (message-kind msg) 'message)
  (check-equal? (message-text msg) "No, use /home/user/src/q-agent/q/website")
  (check-equal? (hash-ref (message-meta msg) 'source #f) 'steering))

(test-case "prepare-iteration-context preserves existing message steering items"
  (define bus (make-event-bus))
  (define q (make-queue))
  (define original
    (make-message "mid" #f 'user 'message (list (make-text-part "already a message")) 0 (hasheq)))
  (enqueue-steering! q (message-text original))
  (define result (prepare-iteration-context '() q #f bus #f "s1"))
  (check-true (and (= (length result) 1)
                   (equal? (message-role (car result)) 'user)
                   (string? (message-id (car result))))))

(test-case "prepared context with raw steering can be consumed by message-id"
  (define bus (make-event-bus))
  (define q (make-queue))
  (define base
    (make-message "base" #f 'user 'message (list (make-text-part "base")) 0 (hasheq)))
  (enqueue-steering! q "steer text")
  (define result (prepare-iteration-context (list base) q #f bus #f "s1"))
  (check-not-exn (lambda () (map message-id result))))

#lang racket/base

;; Session isolation tests for pending-reflection-event lifecycle-state field.
;; v0.99.89: Replaced process-global current-reflection-event parameter
;; with session-owned lifecycle-state.pending-reflection-event.

(require (only-in "../runtime/session/lifecycle-state.rkt"
                  make-lifecycle-state
                  lifecycle-state-pending-reflection-event
                  set-lifecycle-state-pending-reflection-event!
                  consume-reflection-event!)
         (only-in rackunit check-equal? check-false check-true check-not-false))

(define evt-a (hasheq 'tools '("read" "bash") 'message "Large results from A"))
(define evt-b (hasheq 'tools '("edit") 'message "Large results from B"))

;; 1. Default is #f
(check-false (lifecycle-state-pending-reflection-event (make-lifecycle-state))
             "Default reflection event is #f")

;; 2. Write + read
(let ([ls (make-lifecycle-state)])
  (set-lifecycle-state-pending-reflection-event! ls evt-a)
  (check-equal? (lifecycle-state-pending-reflection-event ls)
                evt-a
                "Reflection event stored correctly"))

;; 3. consume-reflection-event! returns and clears
(let ([ls (make-lifecycle-state)])
  (set-lifecycle-state-pending-reflection-event! ls evt-a)
  (define consumed (consume-reflection-event! ls))
  (check-equal? consumed evt-a "consume returns the event")
  (check-false (lifecycle-state-pending-reflection-event ls) "Event cleared after consumption"))

;; 4. consume-reflection-event! returns #f when empty
(let ([ls (make-lifecycle-state)])
  (define consumed (consume-reflection-event! ls))
  (check-false consumed "consume returns #f when no event"))

;; 5. consume-reflection-event! is one-shot
(let ([ls (make-lifecycle-state)])
  (set-lifecycle-state-pending-reflection-event! ls evt-a)
  (check-not-false (consume-reflection-event! ls) "first consume returns event")
  (check-false (consume-reflection-event! ls) "second consume returns #f"))

;; 6. Last-write-wins
(let ([ls (make-lifecycle-state)])
  (set-lifecycle-state-pending-reflection-event! ls evt-a)
  (set-lifecycle-state-pending-reflection-event! ls evt-b)
  (define consumed (consume-reflection-event! ls))
  (check-equal? consumed evt-b "last-write-wins: evt-b replaces evt-a"))

;; 7. Session isolation — two lifecycle-states don't share
(let ([ls-a (make-lifecycle-state)]
      [ls-b (make-lifecycle-state)])
  (set-lifecycle-state-pending-reflection-event! ls-a evt-a)
  (check-false (lifecycle-state-pending-reflection-event ls-b)
               "Session B sees no event from Session A")
  (check-false (consume-reflection-event! ls-b) "Session B consume returns #f")
  (check-equal? (lifecycle-state-pending-reflection-event ls-a)
                evt-a
                "Session A event unaffected by Session B consume"))

;; 8. Session A consumes independently from Session B
(let ([ls-a (make-lifecycle-state)]
      [ls-b (make-lifecycle-state)])
  (set-lifecycle-state-pending-reflection-event! ls-a evt-a)
  (set-lifecycle-state-pending-reflection-event! ls-b evt-b)
  (check-equal? (consume-reflection-event! ls-a) evt-a "A consumes A's event")
  (check-equal? (consume-reflection-event! ls-b) evt-b "B consumes B's event")
  (check-false (consume-reflection-event! ls-a) "A already consumed")
  (check-false (consume-reflection-event! ls-b) "B already consumed"))

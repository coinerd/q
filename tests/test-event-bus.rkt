#lang racket

(require rackunit
         racket/match
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt")

;; ============================================================
;; Test suite: agent/event-bus.rkt — publish/subscribe event bus
;; ============================================================

;; Helper: create a simple test event
(define (test-event #:name [ev "test.event"]
                    #:session-id [sid "sess-1"]
                    #:turn-id [tid "turn-1"]
                    #:payload [payload '#hasheq((ok . #t))])
  (make-event ev (current-inexact-milliseconds) sid tid payload))

;; ============================================================
;; 1. Bus creation and basic subscribe/publish
;; ============================================================

(test-case "make-event-bus returns a bus"
  (define bus (make-event-bus))
  (check-pred event-bus? bus))

(test-case "single subscriber receives published event"
  (define bus (make-event-bus))
  (define received (box '()))
  (subscribe! bus (λ (evt) (set-box! received (cons evt (unbox received)))))
  (define evt (test-event))
  (publish! bus evt)
  (check-equal? (length (unbox received)) 1)
  (check-equal? (event-ev (car (unbox received))) "test.event"))

(test-case "multiple subscribers receive the same event in subscription order"
  (define bus (make-event-bus))
  (define order (box '()))
  (subscribe! bus (λ (evt) (set-box! order (cons 'A (unbox order)))))
  (subscribe! bus (λ (evt) (set-box! order (cons 'B (unbox order)))))
  (subscribe! bus (λ (evt) (set-box! order (cons 'C (unbox order)))))
  (publish! bus (test-event))
  ;; A runs first, then B, then C; order cons'd in reverse
  (check-equal? (reverse (unbox order)) '(A B C)))

(test-case "subscribers receive identical event object"
  (define bus (make-event-bus))
  (define received-a (box #f))
  (define received-b (box #f))
  (subscribe! bus (λ (evt) (set-box! received-a evt)))
  (subscribe! bus (λ (evt) (set-box! received-b evt)))
  (define evt (test-event))
  (publish! bus evt)
  (check-eq? (unbox received-a) (unbox received-b))
  (check-eq? (unbox received-a) evt))

;; ============================================================
;; 2. publish! returns the event (for chaining)
;; ============================================================

(test-case "publish! returns the event"
  (define bus (make-event-bus))
  (define evt (test-event))
  (check-eq? (publish! bus evt) evt))

(test-case "publish! chains correctly"
  (define bus (make-event-bus))
  (define evt (test-event))
  (define result (publish! bus evt))
  (check-equal? (event-ev result) "test.event"))

;; ============================================================
;; 3. Unsubscribe
;; ============================================================

(test-case "unsubscribe removes subscriber"
  (define bus (make-event-bus))
  (define count (box 0))
  (define sub-id (subscribe! bus (λ (evt) (set-box! count (add1 (unbox count))))))
  (publish! bus (test-event))
  (check-equal? (unbox count) 1)
  (unsubscribe! bus sub-id)
  (publish! bus (test-event))
  (check-equal? (unbox count) 1 "should not have been called again"))

(test-case "unsubscribe one subscriber leaves others intact"
  (define bus (make-event-bus))
  (define order (box '()))
  (define sub-a (subscribe! bus (λ (evt) (set-box! order (cons 'A (unbox order))))))
  (subscribe! bus (λ (evt) (set-box! order (cons 'B (unbox order)))))
  (unsubscribe! bus sub-a)
  (publish! bus (test-event))
  (check-equal? (reverse (unbox order)) '(B)))

(test-case "unsubscribe with invalid id is a no-op"
  (define bus (make-event-bus))
  (define count (box 0))
  (subscribe! bus (λ (evt) (set-box! count (add1 (unbox count)))))
  ;; Should not raise
  (unsubscribe! bus 999)
  (publish! bus (test-event))
  (check-equal? (unbox count) 1))

;; ============================================================
;; 4. Subscriber exception isolation
;; ============================================================

(test-case "subscriber throwing exception does not break other subscribers"
  (define bus (make-event-bus))
  (define order (box '()))
  (subscribe! bus (λ (evt) (set-box! order (cons 'A (unbox order)))))
  (subscribe! bus (λ (evt) (error 'bad-subscriber "boom")))
  (subscribe! bus (λ (evt) (set-box! order (cons 'C (unbox order)))))
  ;; publish! should not raise; A and C still get called
  (publish! bus (test-event))
  (check-equal? (reverse (unbox order)) '(A C)))

(test-case "subscriber exception is logged, not swallowed silently"
  (define bus (make-event-bus))
  (define errors (box '()))
  (parameterize ([current-event-bus-error-handler
                   (λ (evt handler exn)
                     (set-box! errors (cons exn (unbox errors))))])
    (subscribe! bus (λ (evt) (error 'bad "boom")))
    (publish! bus (test-event))
    (check-equal? (length (unbox errors)) 1)
    (check-pred exn:fail? (car (unbox errors)))))

;; ============================================================
;; 5. Event filtering (predicate-based subscribe)
;; ============================================================

(test-case "subscribe with predicate filter receives only matching events"
  (define bus (make-event-bus))
  (define received (box '()))
  (subscribe! bus
              (λ (evt) (set-box! received (cons (event-ev evt) (unbox received))))
              #:filter (λ (evt) (string-prefix? (event-ev evt) "model.")))
  (publish! bus (test-event #:name "session.started"))
  (publish! bus (test-event #:name "model.stream.delta"))
  (publish! bus (test-event #:name "model.stream.completed"))
  (publish! bus (test-event #:name "turn.completed"))
  (check-equal? (reverse (unbox received)) '("model.stream.delta" "model.stream.completed")))

(test-case "filtered subscriber can be unsubscribed"
  (define bus (make-event-bus))
  (define count (box 0))
  (define sub-id
    (subscribe! bus
                (λ (evt) (set-box! count (add1 (unbox count))))
                #:filter (λ (evt) (string=? (event-ev evt) "model.stream.delta"))))
  (publish! bus (test-event #:name "model.stream.delta"))
  (check-equal? (unbox count) 1)
  (unsubscribe! bus sub-id)
  (publish! bus (test-event #:name "model.stream.delta"))
  (check-equal? (unbox count) 1))

(test-case "subscriber without filter receives all events"
  (define bus (make-event-bus))
  (define count (box 0))
  (subscribe! bus (λ (evt) (set-box! count (add1 (unbox count)))))
  (publish! bus (test-event #:name "session.started"))
  (publish! bus (test-event #:name "model.stream.delta"))
  (check-equal? (unbox count) 2))

;; ============================================================
;; 6. Events remain serializable after passing through the bus
;; ============================================================

(test-case "event is serializable after bus transit"
  (define bus (make-event-bus))
  (define received-event (box #f))
  (subscribe! bus (λ (evt) (set-box! received-event evt)))
  (define evt (test-event #:name "model.stream.completed"
                          #:payload '#hasheq((tokens . 42))))
  (publish! bus evt)
  (define out (unbox received-event))
  (check-not-false out)
  ;; Round-trip through JSON
  (define jsexpr (event->jsexpr out))
  (define round-tripped (jsexpr->event jsexpr))
  (check-equal? (event-ev round-tripped) "model.stream.completed")
  (check-equal? (event-version round-tripped) 1)
  (check-equal? (hash-ref (event-payload round-tripped) 'tokens) 42))

;; ============================================================
;; 7. Publishing to empty bus is fine
;; ============================================================

(test-case "publish to bus with no subscribers is a no-op"
  (define bus (make-event-bus))
  (define result (publish! bus (test-event)))
  (check-pred event? result))

;; ============================================================
;; 8. Thread safety (basic check)
;; ============================================================

(test-case "concurrent publish and subscribe do not crash"
  (define bus (make-event-bus))
  (define count (box 0))
  (define barrier (make-semaphore 0))
  ;; Start 10 threads that each subscribe and publish 100 events
  (define threads
    (for/list ([i (in-range 10)])
      (thread
       (λ ()
         (semaphore-wait barrier)
         (subscribe! bus (λ (evt) (set-box! count (add1 (unbox count)))))
         (for ([j (in-range 100)])
           (publish! bus (test-event #:name (format "thread-~a-~a" i j))))))))
  ;; Release all threads at once
  (for ([_ (in-range 10)]) (semaphore-post barrier))
  ;; Wait for all threads
  (for-each thread-wait threads)
  ;; Should have received events — exact count is nondeterministic,
  ;; but must be > 0 and no crash
  (check-true (> (unbox count) 0)))

;; ============================================================
;; 9. Property: publish always returns the input event
;; ============================================================

(test-case "publish is identity on the event"
  (define bus (make-event-bus))
  (subscribe! bus (λ (evt) (void)))  ; one subscriber
  (for ([name '("session.started" "turn.started" "model.stream.delta")])
    (define evt (test-event #:name name))
    (check-eq? (publish! bus evt) evt)))

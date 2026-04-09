#lang racket/base

;; agent/event-bus.rkt — publish/subscribe event bus
;;
;; Provides a simple, synchronous event bus with:
;;   - ordered subscriber notification
;;   - exception isolation (one failing subscriber doesn't break others)
;;   - optional predicate-based filtering
;;   - thread-safe mutable state (semaphore-guarded)
;;   - publish! returns the event for chaining

(require racket/contract
         "../agent/types.rkt")

(provide
 make-event-bus
 event-bus?
 subscribe!
 unsubscribe!
 publish!
 current-event-bus-error-handler)

;; ============================================================
;; Error handler parameter
;; ============================================================

;; (or/c event? #f) (or/c procedure? #f) exn:fail? -> any
;; Called when a subscriber throws. Default: log to stderr.
(define current-event-bus-error-handler
  (make-parameter
   (λ (evt handler exn)
     (log-warning "event-bus: subscriber raised ~a for event ~a"
                  (exn-message exn)
                  (and evt (event-ev evt))))))

;; ============================================================
;; Internal subscription record
;; ============================================================

(struct subscription (id handler filter) #:transparent)

;; ============================================================
;; Event bus struct
;; ============================================================

(struct event-bus (subscriptions-box semaphore next-id-box)
  #:constructor-name make-event-bus-internal)

;; ============================================================
;; Public constructor
;; ============================================================

(define (make-event-bus)
  (make-event-bus-internal (box '())
                           (make-semaphore 1)
                           (box 0)))

;; ============================================================
;; subscribe! : event-bus? handler [#:filter pred] -> exact-nonnegative-integer?
;; ============================================================

(define (subscribe! bus handler #:filter [filter #f])
  (call-with-semaphore (event-bus-semaphore bus)
    (λ ()
      (define id (unbox (event-bus-next-id-box bus)))
      (set-box! (event-bus-next-id-box bus) (add1 id))
      (set-box! (event-bus-subscriptions-box bus)
                (cons (subscription id handler filter)
                      (unbox (event-bus-subscriptions-box bus))))
      id)))

;; ============================================================
;; unsubscribe! : event-bus? exact-nonnegative-integer? -> void?
;; ============================================================

(define (unsubscribe! bus sub-id)
  (call-with-semaphore (event-bus-semaphore bus)
    (λ ()
      (set-box! (event-bus-subscriptions-box bus)
                (filter (λ (s) (not (= (subscription-id s) sub-id)))
                        (unbox (event-bus-subscriptions-box bus)))))))

;; ============================================================
;; publish! : event-bus? event? -> event?
;; ============================================================

(define (publish! bus evt)
  ;; Snapshot subscribers under lock, then notify outside the lock
  ;; to avoid deadlock if a subscriber calls publish!/subscribe!.
  (define subs
    (call-with-semaphore (event-bus-semaphore bus)
      (λ ()
        ;; Return in subscription order (oldest first)
        (reverse (unbox (event-bus-subscriptions-box bus))))))
  (define err-handler (current-event-bus-error-handler))
  (for ([s (in-list subs)])
    (define pred (subscription-filter s))
    (when (or (not pred) (pred evt))
      (with-handlers ([exn:fail?
                        (λ (exn)
                          (err-handler evt (subscription-handler s) exn))])
        ((subscription-handler s) evt))))
  evt)

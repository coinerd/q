#lang racket/base

;; agent/event-bus.rkt — publish/subscribe event bus
;;
;; Provides a simple, synchronous event bus with:
;;   - ordered subscriber notification
;;   - exception isolation (one failing subscriber doesn't break others)
;;   - optional predicate-based filtering
;;   - thread-safe mutable state (semaphore-guarded)
;;   - per-bus circuit breaker state (thread-safe)
;;   - publish! returns the event for chaining

(require racket/contract
         "types.rkt")

;; Pub/sub event bus
(provide (contract-out [make-event-bus (-> event-bus?)]
                       [subscribe!
                        (->* (event-bus? procedure?) (#:filter (or/c procedure? #f)) any/c)]
                       [unsubscribe! (-> event-bus? any/c void?)]
                       [publish! (-> event-bus? any/c any/c)])
         event-bus?
         ;; Error handler parameter — reserved for SDK consumers
         current-event-bus-error-handler
         ;; Circuit breaker configuration
         current-circuit-breaker-threshold
         current-circuit-breaker-cooldown-secs
         circuit-breaker-state
         ;; #775: Event serialization queue
         enable-serialization!
         drain-event-queue!
         serialization-enabled?)

;; ============================================================
;; Error handler parameter
;; ============================================================

;; (or/c event? #f) (or/c procedure? #f) exn:fail? -> any
;; Called when a subscriber throws. Default: log to stderr.
(define current-event-bus-error-handler
  (make-parameter (lambda (evt handler exn)
                    (log-warning "event-bus: subscriber raised ~a for event ~a"
                                 (exn-message exn)
                                 (and evt (event-ev evt))))))

;; ============================================================
;; Circuit breaker (#430, #444)
;; ============================================================

;; Number of consecutive failures before a subscriber is disabled.
;; Set to #f to disable circuit breaker.
;; v0.15.1: Raised from 5 to 100 — trace logger payload sanitization
;; now prevents the most common failure mode, but we keep a generous
;; threshold as a safety net rather than disabling the logger entirely.
(define current-circuit-breaker-threshold (make-parameter 100))

;; Cooldown period in seconds before a circuit-broken subscriber
;; is automatically re-enabled. Set to #f for permanent disable.
(define current-circuit-breaker-cooldown-secs (make-parameter 60))

;; Per-subscriber circuit breaker state: hash of sub-id -> (cons failure-count last-failure-secs)
;; Kept as a parameter for backward compatibility. Each event-bus binds its own
;; per-bus breaker hash via parameterize in publish! so concurrent buses don't share state.
(define circuit-breaker-state (make-parameter (make-hash)))

;; Dedicated semaphore for circuit breaker state mutations (#444).
;; Avoids ABBA deadlock with the bus semaphore (publish! calls
;; circuit breaker functions outside the bus lock).
(define circuit-breaker-semaphore (make-semaphore 1))

;; Check if a subscriber is circuit-broken (thread-safe)
(define (circuit-broken? sub-id)
  (define threshold (current-circuit-breaker-threshold))
  (if (not threshold)
      #f
      (call-with-semaphore circuit-breaker-semaphore
                           (lambda ()
                             (let ([entry (hash-ref (circuit-breaker-state) sub-id #f)])
                               (and entry
                                    (>= (car entry) threshold)
                                    (let ([cooldown (current-circuit-breaker-cooldown-secs)])
                                      (if (and cooldown
                                               (> (- (current-seconds) (cdr entry)) cooldown))
                                          (begin
                                            ;; Cooldown elapsed, reset
                                            (hash-remove! (circuit-breaker-state) sub-id)
                                            #f)
                                          #t))))))))

;; Record a failure for a subscriber (thread-safe)
(define (record-failure! sub-id)
  (define threshold (current-circuit-breaker-threshold))
  (when threshold
    (call-with-semaphore
     circuit-breaker-semaphore
     (lambda ()
       (define state (circuit-breaker-state))
       (define entry (hash-ref state sub-id (cons 0 0)))
       (define new-count (add1 (car entry)))
       (hash-set! state sub-id (cons new-count (current-seconds)))
       (when (= new-count threshold)
         (log-warning "event-bus: subscriber ~a circuit-broken after ~a consecutive failures"
                      sub-id
                      threshold))))))

;; Reset failure count on success (thread-safe)
(define (record-success! sub-id)
  (call-with-semaphore circuit-breaker-semaphore
                       (lambda ()
                         (define state (circuit-breaker-state))
                         (when (hash-has-key? state sub-id)
                           (hash-remove! state sub-id)))))

;; ============================================================
;; Internal subscription record
;; ============================================================

(struct subscription (id handler filter) #:transparent)

;; ============================================================
;; Event bus struct
;; ============================================================

(struct event-bus
        (subscriptions-box semaphore
                           next-id-box
                           breaker-state
                           [queue-ch #:mutable]
                           [queue-thread #:mutable])
  #:constructor-name make-event-bus-internal)

;; ============================================================
;; Public constructor
;; ============================================================

(define (make-event-bus)
  (define ch (make-channel))
  (define bus
    (make-event-bus-internal (box '())
                             (make-semaphore 1)
                             (box 0)
                             (make-hash)
                             #f ; queue-ch (enabled on demand)
                             #f)) ; queue-thread
  bus)

;; ============================================================
;; subscribe! : event-bus? handler [#:filter pred] -> exact-nonnegative-integer?
;; ============================================================

(define (subscribe! bus handler #:filter [filter #f])
  (call-with-semaphore (event-bus-semaphore bus)
                       (lambda ()
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
                       (lambda ()
                         (set-box! (event-bus-subscriptions-box bus)
                                   (filter (lambda (s) (not (= (subscription-id s) sub-id)))
                                           (unbox (event-bus-subscriptions-box bus)))))))

;; ============================================================
;; publish! : event-bus? event? -> event?
;; ============================================================

(define (publish! bus evt)
  ;; #775: If serialization enabled, enqueue; otherwise direct dispatch
  (if (serialization-enabled? bus)
      (begin
        (channel-put (event-bus-queue-ch bus) (queue-item evt #f))
        evt)
      (publish-internal! bus evt)))

;; ============================================================
;; #775: Event Serialization Queue
;; ============================================================
;; When enabled, publish! enqueues events to a channel processed
;; by a dedicated background thread. This ensures events are
;; processed sequentially even if handlers are slow or async.

(struct queue-item (evt reply-ch) #:transparent)

;; Sentinel for drain operations (not dispatched to subscribers)
(struct drain-signal () #:transparent)

;; Enable serialization mode — starts background processing thread.
;; event-bus? -> void?
(define (enable-serialization! bus)
  (when (not (event-bus-queue-thread bus))
    (define ch (make-channel))
    (set-event-bus-queue-ch! bus ch)
    (define thread-id
      (thread (lambda ()
                (let loop ()
                  (define item (channel-get ch))
                  (cond
                    [(eq? item 'shutdown) (void)]
                    [(queue-item? item)
                     (cond
                       [(drain-signal? (queue-item-evt item))
                        ;; Drain signal — just reply, don't dispatch
                        (when (queue-item-reply-ch item)
                          (channel-put (queue-item-reply-ch item) 'done))]
                       [else
                        ;; Process the event synchronously using the existing logic
                        (define result (publish-internal! bus (queue-item-evt item)))
                        ;; Send reply if requested
                        (when (queue-item-reply-ch item)
                          (channel-put (queue-item-reply-ch item) result))])
                     (loop)]
                    [else (loop)])))))
    (set-event-bus-queue-thread! bus thread-id)))

;; Drain all pending events and return.
;; Blocks until all queued events have been processed.
;; event-bus? -> void?
(define (drain-event-queue! bus)
  (when (serialization-enabled? bus)
    ;; Send a sync item through the queue — when it completes,
    ;; all previously queued items have been processed.
    (define reply-ch (make-channel))
    (channel-put (event-bus-queue-ch bus) (queue-item (drain-signal) reply-ch))
    (channel-get reply-ch)))

;; Check if serialization is enabled.
;; event-bus? -> boolean?
(define (serialization-enabled? bus)
  (and (event-bus-queue-ch bus) (event-bus-queue-thread bus) #t))

;; Internal: the actual synchronous publish logic (extracted from publish!)
(define (publish-internal! bus evt)
  (define subs
    (call-with-semaphore (event-bus-semaphore bus)
                         (lambda () (reverse (unbox (event-bus-subscriptions-box bus))))))
  (parameterize ([circuit-breaker-state (event-bus-breaker-state bus)])
    (define err-handler (current-event-bus-error-handler))
    (for ([s (in-list subs)])
      (define sub-id (subscription-id s))
      (define pred (subscription-filter s))
      (cond
        [(circuit-broken? sub-id) (void)]
        [(or (not pred) (pred evt))
         (with-handlers ([exn:fail? (lambda (exn)
                                      (record-failure! sub-id)
                                      (err-handler evt (subscription-handler s) exn))])
           ((subscription-handler s) evt)
           (record-success! sub-id))]
        [else (void)])))
  evt)

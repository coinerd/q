#lang racket/base

;; agent/event-bus.rkt — publish/subscribe event bus
;; STABILITY: stable
;;
;; Provides a simple, synchronous event bus with:
;;   - ordered subscriber notification
;;   - exception isolation (one failing subscriber doesn't break others)
;;   - optional predicate-based filtering
;;   - thread-safe mutable state (semaphore-guarded)
;;   - per-bus circuit breaker state (thread-safe)
;;   - publish! returns the event for chaining

(require racket/contract
         racket/match
         "../util/protocol-types.rkt"
         "../util/event.rkt"
         "event-types.rkt")

;; Pub/sub event bus
(provide (contract-out
          [make-event-bus
           (->* ()
                (#:threshold (or/c exact-nonnegative-integer? #f)
                             #:cooldown-secs (or/c exact-nonnegative-integer? #f))
                event-bus?)]
          [subscribe!
           (->* (event-bus? procedure?) (#:filter (or/c procedure? #f)) exact-nonnegative-integer?)]
          [subscribe-map! (-> event-bus? procedure? procedure? exact-nonnegative-integer?)]
          [subscribe-filter! (-> event-bus? procedure? procedure? exact-nonnegative-integer?)]
          [unsubscribe! (-> event-bus? exact-nonnegative-integer? void?)]
          [publish! (-> event-bus? event? event?)]
          [typed-event->event (-> typed-event? event?)]
          [bus-emit-typed! (-> event-bus? typed-event? event?)])
         event-bus?
         ;; Error handler parameter — reserved for SDK consumers
         current-event-bus-error-handler
         ;; Circuit breaker configuration
         current-circuit-breaker-threshold
         current-circuit-breaker-cooldown-secs
         ;; New per-bus isolated circuit breaker functions
         circuit-broken?
         record-failure!
         record-success!)

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
;; Deprecated global parameter — kept for backward compatibility only.
;; New code should use the explicit breaker-state hash argument.
;; DEPRECATED (N-08): Not exported. State is passed explicitly.
(define circuit-breaker-state (make-parameter (make-hash)))

;; Dedicated semaphore for circuit breaker state mutations (#444).
;; Avoids ABBA deadlock with the bus semaphore (publish! calls
;; circuit breaker functions outside the bus lock).
(define circuit-breaker-semaphore (make-semaphore 1))

;; Check if a subscriber is circuit-broken (thread-safe)
;; Uses per-bus threshold/cooldown if available, falls back to global params.
(define (circuit-broken? breaker-state sub-id #:bus [bus #f])
  (define threshold
    (if bus
        (event-bus-cb-threshold bus)
        (current-circuit-breaker-threshold)))
  (if (not threshold)
      #f
      (with-circuit-breaker-lock (lambda ()
                                   (define entry (hash-ref breaker-state sub-id #f))
                                   (and entry
                                        (>= (car entry) threshold)
                                        (let ([cooldown (if bus
                                                            (event-bus-cb-cooldown-secs bus)
                                                            (current-circuit-breaker-cooldown-secs))])
                                          (if (and cooldown
                                                   (> (- (current-seconds) (cdr entry)) cooldown))
                                              (begin
                                                ;; Cooldown elapsed, reset
                                                (hash-remove! breaker-state sub-id)
                                                #f)
                                              #t)))))))

;; Record a failure for a subscriber (thread-safe)
;; breaker-state: hash of sub-id -> (cons failure-count last-failure-secs)
(define (record-failure! breaker-state sub-id #:bus [bus #f])
  (define threshold
    (if bus
        (event-bus-cb-threshold bus)
        (current-circuit-breaker-threshold)))
  (when threshold
    (with-circuit-breaker-lock
     (lambda ()
       (define entry (hash-ref breaker-state sub-id (cons 0 0)))
       (define new-count (add1 (car entry)))
       (hash-set! breaker-state sub-id (cons new-count (current-seconds)))
       (when (= new-count threshold)
         (log-warning "event-bus: subscriber ~a circuit-broken after ~a consecutive failures"
                      sub-id
                      threshold))))))

;; Reset failure count on success (thread-safe)
;; breaker-state: hash of sub-id -> (cons failure-count last-failure-secs)
(define (record-success! breaker-state sub-id)
  (with-circuit-breaker-lock (lambda ()
                               (when (hash-has-key? breaker-state sub-id)
                                 (hash-remove! breaker-state sub-id)))))

;; ============================================================
;; Internal subscription record
;; ============================================================

(struct subscription (id handler filter) #:transparent)

;; ============================================================
;; Event bus struct
;; ============================================================

(struct event-bus
        (subscriptions-box semaphore next-id-box breaker-state cb-threshold cb-cooldown-secs)
  #:constructor-name make-event-bus-internal)

;; ============================================================
;; Public constructor
;; ============================================================

;; L-07: Configurable circuit breaker per bus.
;; #:threshold — consecutive failures before circuit breaks (default from parameter)
;; #:cooldown-secs — seconds before retrying a broken circuit (default from parameter)
(define (make-event-bus #:threshold [threshold (current-circuit-breaker-threshold)]
                        #:cooldown-secs [cooldown (current-circuit-breaker-cooldown-secs)])
  (make-event-bus-internal (box '()) (make-semaphore 1) (box 0) (make-hash) threshold cooldown))

;; Thread-safe event bus lock helper (Finding A3)
(define (with-event-bus-lock bus thunk)
  (call-with-semaphore (event-bus-semaphore bus) thunk))

;; Thread-safe circuit-breaker lock helper (Finding A5)
(define (with-circuit-breaker-lock thunk)
  (call-with-semaphore circuit-breaker-semaphore thunk))

;; ============================================================
;; subscribe! : event-bus? handler [#:filter pred] -> exact-nonnegative-integer?
;; ============================================================

(define (subscribe! bus handler #:filter [filter #f])
  (with-event-bus-lock bus
                       (lambda ()
                         (define id (unbox (event-bus-next-id-box bus)))
                         (set-box! (event-bus-next-id-box bus) (add1 id))
                         (set-box! (event-bus-subscriptions-box bus)
                                   (cons (subscription id handler filter)
                                         (unbox (event-bus-subscriptions-box bus))))
                         id)))

;; ============================================================
;; subscribe-map! : event-bus? (event? -> event?) handler -> sub-id
;; v0.33.4 W0: Transform events before delivering to handler (RA-11)
;; ============================================================

(define (subscribe-map! bus transform handler)
  (subscribe! bus (lambda (evt) (handler (transform evt)))))

;; ============================================================
;; subscribe-filter! : event-bus? (event? -> boolean?) handler -> sub-id
;; v0.33.4 W0: Subscribe with predicate filter (RA-11)
;; ============================================================

(define (subscribe-filter! bus pred handler)
  (subscribe! bus handler #:filter pred))

;; ============================================================
;; unsubscribe! : event-bus? exact-nonnegative-integer? -> void?
;; ============================================================

(define (unsubscribe! bus sub-id)
  (with-event-bus-lock bus
                       (lambda ()
                         (set-box! (event-bus-subscriptions-box bus)
                                   (filter (lambda (s) (not (= (subscription-id s) sub-id)))
                                           (unbox (event-bus-subscriptions-box bus)))))))

;; ============================================================
;; publish! : event-bus? event? -> event?
;; ============================================================

(define (publish! bus evt)
  (define subs
    (with-event-bus-lock bus (lambda () (reverse (unbox (event-bus-subscriptions-box bus))))))
  (define breaker-state (event-bus-breaker-state bus))
  (define err-handler (current-event-bus-error-handler))
  (when (null? subs)
    (log-warning "q-event-bus: event ~a dropped, no subscribers" (event-ev evt)))
  (for ([s (in-list subs)])
    (define sub-id (subscription-id s))
    (define pred (subscription-filter s))
    (cond
      [(circuit-broken? breaker-state sub-id #:bus bus) (void)]
      [(and pred (not (pred evt))) (void)] ; predicate filter rejected
      [else
       (with-handlers ([exn:fail? (lambda (exn)
                                    (record-failure! breaker-state sub-id #:bus bus)
                                    (err-handler evt (subscription-handler s) exn))])
         ((subscription-handler s) evt)
         (record-success! breaker-state sub-id))]))
  evt)

;; ============================================================
;; Typed event bridge (EVT-01)
;; ============================================================

;; W-06: Convert camelCase JSON keys to snake_case Racket symbols.
;; This ensures bus-emit-typed! produces payloads compatible with TUI handlers.
(define (json-key->racket-key k)
  (define s (symbol->string k))
  (string->symbol (regexp-replace* #rx"([a-z])([A-Z])" s "\\1-\\2")))

;; Convert a typed-event to a raw event struct for bus transport.
;; The typed event's type field becomes the event name.
;; Extra fields are serialized via typed-event->jsexpr and stripped
;; of the base fields (type, timestamp, sessionId, turnId).
;; W-06: Keys are converted from camelCase to snake_case for TUI compatibility.
(define (typed-event->event te)
  (define h (typed-event->jsexpr te))
  (define payload
    (for/hash ([(k v) (in-hash h)]
               #:when (not (memq k '(type timestamp sessionId turnId))))
      (values (json-key->racket-key k) v)))
  (make-event (typed-event-type te)
              (typed-event-timestamp te)
              (typed-event-session-id te)
              (typed-event-turn-id te)
              payload))

;; Publish a typed event on the bus, converting to raw event first.
;; Returns the raw event struct.
(define (bus-emit-typed! bus te)
  (define evt (typed-event->event te))
  (publish! bus evt)
  evt)

#lang racket/base
;; STABILITY: public

;; util/event/event-bus.rkt — publish/subscribe event bus
;; STABILITY: stable
;; v0.98.15 (AXIS1-F12): Relocated from agent/event-bus.rkt.
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
         "event.rkt"
         "../../agent/event-types.rkt")

;; Pub/sub event bus
(provide (contract-out
          [make-event-bus
           (->* ()
                (#:threshold (or/c exact-nonnegative-integer? #f)
                             #:cooldown-secs (or/c exact-nonnegative-integer? #f)
                             #:clock procedure?)
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

(define current-event-bus-error-handler
  (make-parameter (lambda (evt handler exn)
                    (log-warning "event-bus: subscriber raised ~a for event ~a"
                                 (exn-message exn)
                                 (and evt (event-ev evt))))))

;; ============================================================
;; Circuit breaker (#430, #444)
;; ============================================================

(define current-circuit-breaker-threshold (make-parameter 100))
(define current-circuit-breaker-cooldown-secs (make-parameter 60))

(define circuit-breaker-semaphore (make-semaphore 1))

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
                                                            (current-circuit-breaker-cooldown-secs))]
                                              [clock (if bus
                                                         (event-bus-clock bus)
                                                         current-seconds)])
                                          (if (and cooldown (> (- (clock) (cdr entry)) cooldown))
                                              (begin
                                                (hash-remove! breaker-state sub-id)
                                                #f)
                                              #t)))))))

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
       (hash-set! breaker-state
                  sub-id
                  (cons new-count
                        ((if bus
                             (event-bus-clock bus)
                             current-seconds))))
       (when (= new-count threshold)
         (log-warning "event-bus: subscriber ~a circuit-broken after ~a consecutive failures"
                      sub-id
                      threshold))))))

(define (record-success! breaker-state sub-id)
  (with-circuit-breaker-lock (lambda ()
                               (when (hash-has-key? breaker-state sub-id)
                                 (hash-remove! breaker-state sub-id)))))

;; ============================================================
;; Internal subscription record
;; ============================================================

(struct subscription (id handler filter))

;; ============================================================
;; Event bus struct
;; ============================================================

(struct event-bus
        (subscriptions-box semaphore next-id-box breaker-state cb-threshold cb-cooldown-secs clock)
  #:constructor-name make-event-bus-internal)

;; ============================================================
;; Public constructor
;; ============================================================

(define (make-event-bus #:threshold [threshold (current-circuit-breaker-threshold)]
                        #:cooldown-secs [cooldown (current-circuit-breaker-cooldown-secs)]
                        #:clock [clock current-seconds])
  (make-event-bus-internal (box '()) (make-semaphore 1) (box 0) (make-hash) threshold cooldown clock))

(define (with-event-bus-lock bus thunk)
  (call-with-semaphore (event-bus-semaphore bus) thunk))

(define (with-circuit-breaker-lock thunk)
  (call-with-semaphore circuit-breaker-semaphore thunk))

;; ============================================================
;; subscribe! / subscribe-map! / subscribe-filter! / unsubscribe!
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

(define (subscribe-map! bus transform handler)
  (subscribe! bus (lambda (evt) (handler (transform evt)))))

(define (subscribe-filter! bus pred handler)
  (subscribe! bus handler #:filter pred))

(define (unsubscribe! bus sub-id)
  (with-event-bus-lock bus
                       (lambda ()
                         (set-box! (event-bus-subscriptions-box bus)
                                   (filter (lambda (s) (not (= (subscription-id s) sub-id)))
                                           (unbox (event-bus-subscriptions-box bus)))))))

;; ============================================================
;; publish!
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
      [(and pred (not (pred evt))) (void)]
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

(define (json-key->racket-key k)
  (define s (symbol->string k))
  (string->symbol (string-downcase (regexp-replace* #rx"([a-z])([A-Z])" s "\\1-\\2"))))

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

(define (bus-emit-typed! bus te)
  (define evt (typed-event->event te))
  (publish! bus evt)
  evt)

#lang racket/base

;; agent/blackboard-subscriber.rkt — Event bus subscriber for blackboard
;; STABILITY: evolving
;;
;; W4 (v0.99.7): Subscribe to event bus for zero-latency blackboard updates.
;; v0.99.13 W1 (G-2): Extracted log-replay and event-filtering logic to
;;   blackboard-follower.rkt.  This module retains the subscription lifecycle
;;   and re-exports follower symbols for backward compatibility.
;;
;; Design:
;;   - start-blackboard-subscriber!: subscribe, reset blackboard, return sub-id
;;   - stop-blackboard-subscriber!: clean unsubscribe
;;   - blackboard-relevant-event?: (moved to follower, re-exported)
;;   - rebuild-blackboard-from-log!: (moved to follower, re-exported)
;;   - event->reducer-hash: convert event? struct to reducer hash format
;;
;; Feature-gated via mas.blackboard.enabled (default false).
;; Exception isolation: subscriber errors don't crash the event bus.
;;
;; Part of MAS Schritt 4: Blackboard & Event Log (milestone #793).

(require racket/contract
         racket/match
         (only-in "../util/event/event.rkt" event? event-ev event-payload event-time)
         (only-in "../util/event/event-bus.rkt" subscribe! unsubscribe! event-bus?)
         "blackboard.rkt"
         "blackboard-reducer.rkt"
         "blackboard-follower.rkt")

;; ============================================================
;; Event Conversion
;; ============================================================

;; Convert an event? struct to the hash format the reducer expects.
;; The reducer expects: (hasheq 'event <symbol> 'data <hash> 'timestamp <number>)
(define (event->reducer-hash evt)
  (define ev-name (event-ev evt))
  (define sym-name
    (if (string? ev-name)
        (string->symbol ev-name)
        ev-name))
  (define payload (event-payload evt))
  (hasheq 'event
          sym-name
          'data
          (if (hash? payload)
              payload
              (hasheq))
          'timestamp
          (event-time evt)))

;; ============================================================
;; Subscription Lifecycle
;; ============================================================

;; Box holding (cons sub-id bus) for the active subscription, or #f.
(define current-subscription (box #f))

;; Subscribe to the event bus for blackboard updates.
;; Resets the blackboard to empty before subscribing.
;; Returns the subscription ID.
(define (start-blackboard-subscriber! bus [bb (current-blackboard)])
  (when bb
    (reset-blackboard! bb))
  (define sub-id (subscribe! bus (make-blackboard-handler bb) #:filter blackboard-relevant-event?))
  (set-box! current-subscription (cons sub-id bus))
  sub-id)

;; Create the subscriber handler with exception isolation.
(define (make-blackboard-handler bb)
  (lambda (evt)
    (with-handlers ([exn:fail? (lambda (e)
                                 (log-warning "blackboard-subscriber: error processing event: ~a"
                                              (exn-message e)))])
      (define reducer-hash (event->reducer-hash evt))
      (when bb
        (update-blackboard! (lambda (state) (apply-event state reducer-hash)) bb)))))

;; Unsubscribe from the event bus.
(define (stop-blackboard-subscriber! [bus #f])
  (define stored (unbox current-subscription))
  (when (and stored (pair? stored))
    (define sub-id (car stored))
    (define sub-bus (or bus (cdr stored)))
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (unsubscribe! sub-bus sub-id))
    (set-box! current-subscription #f)))

;; ============================================================
;; Provides
;; ============================================================

(provide relevant-event-names
         current-subscription)

(provide (contract-out [blackboard-relevant-event? (-> any/c boolean?)]
                       [event->reducer-hash (-> event? hash?)]
                       [start-blackboard-subscriber!
                        (->* (event-bus?) (blackboard-container?) exact-nonnegative-integer?)]
                       [stop-blackboard-subscriber! (->* () (event-bus?) void?)]
                       [rebuild-blackboard-from-log!
                        (->* (path-string?) (blackboard-container?) blackboard-state?)]))

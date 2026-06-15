#lang racket/base

;; agent/blackboard-subscriber.rkt — Event bus subscriber for blackboard
;; STABILITY: evolving
;;
;; W4 (v0.99.7): Subscribe to event bus for zero-latency blackboard updates.
;; Also provides crash recovery by replaying events from JSONL log.
;;
;; Design:
;;   - start-blackboard-subscriber!: subscribe, reset blackboard, return sub-id
;;   - stop-blackboard-subscriber!: clean unsubscribe
;;   - rebuild-blackboard-from-log!: replay JSONL events for crash recovery
;;   - blackboard-relevant-event?: filter predicate for relevant events
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
         (only-in "../util/json/jsonl.rkt" jsonl-read-all-valid strip-format-header)
         "blackboard.rkt"
         "blackboard-reducer.rkt")

;; ============================================================
;; Relevant Event Predicate
;; ============================================================

;; Set of event names the blackboard reducer handles.
(define relevant-event-names
  '(gsd.wave.started gsd.wave.completed
                     gsd.wave.failed
                     gsd.wave.skipped
                     gsd.plan.parsed
                     gsd.verification.completed
                     gsd.verification.escalated
                     mas.artifact.produced
                     mas.test.result
                     mas.hypothesis.opened
                     mas.hypothesis.resolved
                     mas.blackboard.sync
                     mas.agent.version.pinned
                     mas.agent.registered
                     mas.agent.activated))

;; Check if an event is relevant to the blackboard.
;; Accepts both event? structs (from the bus) and hashes (from JSONL).
(define (blackboard-relevant-event? evt)
  (define ev-name
    (cond
      [(event? evt) (event-ev evt)]
      [(hash? evt) (or (hash-ref evt 'event #f) (hash-ref evt 'ev #f))]
      [else #f]))
  (and ev-name
       (let ([sym (if (string? ev-name)
                      (string->symbol ev-name)
                      ev-name)])
         (and (memq sym relevant-event-names) #t))))

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
;; Crash Recovery
;; ============================================================

;; Replay events from a JSONL log file to rebuild blackboard state.
;; Convert string event names from JSON entries to symbols for the reducer.
;; JSON round-trips symbols as strings; the reducer uses eq? with symbols.
(define (normalize-jsonl-entry entry)
  (cond
    [(not (hash? entry)) entry]
    [else
     (define ev-name (or (hash-ref entry 'event #f) (hash-ref entry "event" #f)))
     (cond
       [(string? ev-name) (hash-set entry 'event (string->symbol ev-name))]
       [else entry])]))

;; Reads all valid entries, strips format header, applies relevant events.
;; Returns the final blackboard-state.
(define (rebuild-blackboard-from-log! log-path [bb (current-blackboard)])
  (when bb
    (reset-blackboard! bb))
  (define entries (jsonl-read-all-valid log-path))
  (define events (strip-format-header entries))
  (define normalized (map normalize-jsonl-entry events))
  (define relevant (filter blackboard-relevant-event? normalized))
  (define initial (or (and bb (read-blackboard bb)) empty-blackboard))
  (define final-state (apply-events relevant initial))
  (when bb
    (update-blackboard! (lambda (_) final-state) bb))
  final-state)

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

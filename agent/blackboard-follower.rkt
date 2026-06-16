#lang racket/base

;; agent/blackboard-follower.rkt — Blackboard log follower & event relevance
;; STABILITY: evolving
;;
;; Extracted from blackboard-subscriber.rkt (v0.99.13 W1, G-2).
;; Contains the event-filtering predicate and JSONL replay logic that are
;; independent of the event-bus subscription lifecycle.  This separation
;; allows crash-recovery / replay to be used without pulling in the full
;; subscriber (which depends on event-bus.rkt).
;;
;; Design:
;;   - relevant-event-names: set of event symbols the blackboard reducer handles
;;   - blackboard-relevant-event?: filter predicate (accepts event? structs and hashes)
;;   - normalize-jsonl-entry: convert string event names to symbols (JSON round-trip)
;;   - rebuild-blackboard-from-log!: replay JSONL events for crash recovery
;;
;; Part of MAS Schritt 4: Blackboard & Event Log.

(require racket/contract
         racket/match
         (only-in "../util/event/event.rkt" event? event-ev)
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
                     mas.agent.activated
                     mas.mcp.connected
                     mas.mcp.tool.called))

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
;; JSONL Normalisation
;; ============================================================

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

;; ============================================================
;; Crash Recovery (Log Replay)
;; ============================================================

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

(provide relevant-event-names)

(provide (contract-out [blackboard-relevant-event? (-> any/c boolean?)]
                       [normalize-jsonl-entry (-> any/c any/c)]
                       [rebuild-blackboard-from-log!
                        (->* (path-string?) (blackboard-container?) blackboard-state?)]))

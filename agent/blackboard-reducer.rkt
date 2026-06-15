#lang racket/base

;; agent/blackboard-reducer.rkt — Pure event reducer for blackboard state
;; STABILITY: evolving
;;
;; W2 (v0.99.7): Pure function that folds events into blackboard state.
;;
;; Design:
;;   - Pure: (blackboard-state, event-hash) -> blackboard-state
;;   - No side effects, no I/O, fully testable
;;   - Unknown events → state unchanged
;;   - Missing fields → safe defaults (no crashes)
;;
;; Event format (from gsd/events.rkt emit-gsd-event!):
;;   (hasheq 'event <symbol> 'data <hash> 'timestamp <ms> 'correlation-id <id>)
;;
;; Part of MAS Schritt 4: Blackboard & Event Log (milestone #793).

(require racket/contract
         racket/match
         (only-in "../util/ids.rkt" now-seconds)
         "blackboard.rkt")

;; ============================================================
;; Helper: Extract event name from event hash
;; ============================================================

;; Events published by the event bus may use either 'event or 'ev as the key.
;; The value is a symbol like 'gsd.wave.started.
(define (event-name evt)
  (cond
    [(hash? evt) (or (hash-ref evt 'event #f) (hash-ref evt 'ev #f))]
    [else #f]))

;; Helper: Extract the data payload from an event hash.
;; Handles both GSD-wrapped format (with 'data sub-hash) and flat format.
(define (event-data evt)
  (define data (hash-ref evt 'data #f))
  (if (hash? data)
      data
      (if (hash? evt)
          evt
          (hasheq))))

;; Helper: Safely extract a field from the event data.
(define (data-ref evt key [default #f])
  (define d (event-data evt))
  (hash-ref d key default))

;; Helper: Get timestamp from event, fallback to now-seconds.
(define (event-timestamp evt)
  (or (hash-ref evt 'timestamp #f) (hash-ref evt 'time #f) (now-seconds)))

;; ============================================================
;; R1 fix (v0.99.8): Cap list fields to prevent unbounded growth.
;; ============================================================

(define MAX-BLACKBOARD-ENTRIES 50)

;; Append item to list, dropping oldest if at capacity.
(define (capped-append lst item)
  (if (>= (length lst) MAX-BLACKBOARD-ENTRIES)
      (append (cdr lst) (list item)) ; drop oldest
      (append lst (list item))))

;; ============================================================
;; Wave Status Handlers
;; ============================================================

(define (handle-wave-started state evt)
  (define wave (data-ref evt 'wave "unknown"))
  (define wave-name
    (if (symbol? wave)
        (symbol->string wave)
        wave))
  (struct-copy blackboard-state
               state
               [wave-status (hash-set (blackboard-state-wave-status state) wave-name 'in-progress)]
               [last-updated (event-timestamp evt)]))

(define (handle-wave-completed state evt)
  (define wave (data-ref evt 'wave "unknown"))
  (define wave-name
    (if (symbol? wave)
        (symbol->string wave)
        wave))
  (struct-copy blackboard-state
               state
               [wave-status (hash-set (blackboard-state-wave-status state) wave-name 'completed)]
               [last-updated (event-timestamp evt)]))

(define (handle-wave-failed state evt)
  (define wave (data-ref evt 'wave "unknown"))
  (define wave-name
    (if (symbol? wave)
        (symbol->string wave)
        wave))
  (struct-copy blackboard-state
               state
               [wave-status (hash-set (blackboard-state-wave-status state) wave-name 'failed)]
               [last-updated (event-timestamp evt)]))

(define (handle-wave-skipped state evt)
  (define wave (data-ref evt 'wave "unknown"))
  (define wave-name
    (if (symbol? wave)
        (symbol->string wave)
        wave))
  (struct-copy blackboard-state
               state
               [wave-status (hash-set (blackboard-state-wave-status state) wave-name 'skipped)]
               [last-updated (event-timestamp evt)]))

;; ============================================================
;; Plan Handler
;; ============================================================

(define (handle-plan-parsed state evt)
  (define plan-data
    (hasheq 'wave-count (data-ref evt 'wave-count 0) 'timestamp (event-timestamp evt)))
  (struct-copy blackboard-state state [active-plan plan-data] [last-updated (event-timestamp evt)]))

;; ============================================================
;; Verification Handlers
;; ============================================================

(define (handle-verification-completed state evt)
  (define decision
    (hasheq 'verdict
            (data-ref evt 'verdict 'approve)
            'reason
            (data-ref evt 'reason "")
            'risk-level
            (data-ref evt 'risk-level 'low)
            'requires-human?
            (data-ref evt 'requires-human? #f)
            'timestamp
            (event-timestamp evt)))
  (struct-copy blackboard-state
               state
               [verifier-decisions
                (capped-append (blackboard-state-verifier-decisions state) decision)]
               [last-updated (event-timestamp evt)]))

(define (handle-verification-escalated state evt)
  (define decision
    (hasheq 'verdict
            'escalate
            'reason
            (data-ref evt 'reason "")
            'risk-level
            (data-ref evt 'risk-level 'high)
            'requires-human?
            #t
            'timestamp
            (event-timestamp evt)))
  (struct-copy blackboard-state
               state
               [verifier-decisions
                (capped-append (blackboard-state-verifier-decisions state) decision)]
               [last-updated (event-timestamp evt)]))

;; ============================================================
;; MAS Event Handlers
;; ============================================================

(define (handle-artifact-produced state evt)
  (define artifact
    (hasheq 'name
            (data-ref evt 'name "unknown")
            'path
            (data-ref evt 'path #f)
            'artifact-type
            (data-ref evt 'artifact-type 'generic)
            'timestamp
            (event-timestamp evt)))
  (struct-copy blackboard-state
               state
               [artifact-refs (capped-append (blackboard-state-artifact-refs state) artifact)]
               [last-updated (event-timestamp evt)]))

(define (handle-test-result state evt)
  (define result
    (hasheq 'file
            (data-ref evt 'file "unknown")
            'result
            (data-ref evt 'result 'unknown)
            'duration-ms
            (data-ref evt 'duration-ms 0)
            'timestamp
            (event-timestamp evt)))
  (struct-copy blackboard-state
               state
               [test-results (capped-append (blackboard-state-test-results state) result)]
               [last-updated (event-timestamp evt)]))

(define (handle-hypothesis-opened state evt)
  (define hyp
    (hasheq 'id
            (data-ref evt 'id "unknown")
            'question
            (data-ref evt 'question "")
            'agent-name
            (data-ref evt 'agent-name "unknown")
            'timestamp
            (event-timestamp evt)))
  (struct-copy blackboard-state
               state
               [open-hypotheses (capped-append (blackboard-state-open-hypotheses state) hyp)]
               [last-updated (event-timestamp evt)]))

(define (handle-hypothesis-resolved state evt)
  (define hyp-id (data-ref evt 'id #f))
  (define filtered
    (if hyp-id
        (filter (lambda (h) (not (equal? (hash-ref h 'id #f) hyp-id)))
                (blackboard-state-open-hypotheses state))
        (blackboard-state-open-hypotheses state)))
  (struct-copy blackboard-state
               state
               [open-hypotheses filtered]
               [last-updated (event-timestamp evt)]))

;; ============================================================
;; A1 fix (v0.99.8): Blackboard sync handler (heartbeat).
;; ============================================================

(define (handle-blackboard-sync state evt)
  (struct-copy blackboard-state state [last-updated (event-timestamp evt)]))

;; ============================================================
;; Main Reducer
;; ============================================================

;; Dispatch table: event symbol -> handler function.
;; Returns the new state, or the original state for unknown events.
(define (apply-event state evt)
  (define ev-name (event-name evt))
  (cond
    ;; GSD wave events
    [(eq? ev-name 'gsd.wave.started) (handle-wave-started state evt)]
    [(eq? ev-name 'gsd.wave.completed) (handle-wave-completed state evt)]
    [(eq? ev-name 'gsd.wave.failed) (handle-wave-failed state evt)]
    [(eq? ev-name 'gsd.wave.skipped) (handle-wave-skipped state evt)]
    ;; GSD plan events
    [(eq? ev-name 'gsd.plan.parsed) (handle-plan-parsed state evt)]
    ;; GSD verification events
    [(eq? ev-name 'gsd.verification.completed) (handle-verification-completed state evt)]
    [(eq? ev-name 'gsd.verification.escalated) (handle-verification-escalated state evt)]
    ;; MAS events
    [(eq? ev-name 'mas.artifact.produced) (handle-artifact-produced state evt)]
    [(eq? ev-name 'mas.test.result) (handle-test-result state evt)]
    [(eq? ev-name 'mas.hypothesis.opened) (handle-hypothesis-opened state evt)]
    [(eq? ev-name 'mas.hypothesis.resolved) (handle-hypothesis-resolved state evt)]
    ;; MAS sync (heartbeat)
    [(eq? ev-name 'mas.blackboard.sync) (handle-blackboard-sync state evt)]
    ;; Unknown event → unchanged
    [else state]))

;; Fold a sequence of events into state, starting from initial-state.
(define (apply-events events [initial-state empty-blackboard])
  (foldl (lambda (evt state) (apply-event state evt)) initial-state events))

;; ============================================================
;; Provides
;; ============================================================

(provide event-name
         event-data
         data-ref
         capped-append
         MAX-BLACKBOARD-ENTRIES)

(provide (contract-out [apply-event (-> blackboard-state? hash? blackboard-state?)]
                       [apply-events (->* ((listof hash?)) (blackboard-state?) blackboard-state?)]))

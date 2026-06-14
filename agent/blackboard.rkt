#lang racket/base

;; agent/blackboard.rkt — Shared Blackboard state for inter-agent coordination
;; STABILITY: evolving
;;
;; W1 (v0.99.7): Thread-safe in-memory view over the event stream.
;; The blackboard provides a structured snapshot of session state
;; that all agents can read for context.
;;
;; Design:
;;   - blackboard-state: immutable struct with 8 fields
;;   - Container: mutable box guarded by a semaphore for thread safety
;;   - read-blackboard: atomic snapshot read
;;   - update-blackboard!: atomic read-modify-write via reducer function
;;   - reset-blackboard!: atomically restore to empty state
;;
;; Feature-gated via mas.blackboard.enabled (default false).
;;
;; Part of MAS Schritt 4: Blackboard & Event Log (milestone #793).

(require racket/contract
         racket/match
         (only-in "../util/ids.rkt" now-seconds))

;; ============================================================
;; Data Types
;; ============================================================

;; The blackboard state is an immutable snapshot of session activity.
;; All list fields default to empty lists.
;; active-plan: #f or hash with plan metadata (summary, wave-count, etc.)
;; open-hypotheses: list of hashes with question/agent-name
;; test-results: list of hashes with file/result/duration-ms
;; artifact-refs: list of hashes with name/path/artifact-type
;; wave-status: hash mapping wave name/id to status symbol
;; verifier-decisions: list of verifier-decision hashes
;; agent-activities: list of hashes with agent-name/action/timestamp
;; last-updated: epoch seconds of the most recent update
(struct blackboard-state
        (active-plan open-hypotheses
                     test-results
                     artifact-refs
                     wave-status
                     verifier-decisions
                     agent-activities
                     last-updated)
  #:transparent)

;; The empty blackboard — fresh session state.
(define empty-blackboard
  (blackboard-state #f ; active-plan
                    '() ; open-hypotheses
                    '() ; test-results
                    '() ; artifact-refs
                    (hasheq) ; wave-status
                    '() ; verifier-decisions
                    '() ; agent-activities
                    0)) ; last-updated

;; ============================================================
;; Thread-Safe Container
;; ============================================================

;; Container: a mutable box + semaphore for serialized access.
(struct blackboard-container (box semaphore) #:transparent)

;; Create a fresh blackboard container.
(define (make-blackboard)
  (blackboard-container (box empty-blackboard) (make-semaphore 1)))

;; The global blackboard instance for the current session.
;; Set by session lifecycle code when blackboard is enabled.
(define current-blackboard (make-parameter #f))

;; ============================================================
;; Operations
;; ============================================================

;; Read the current blackboard state atomically.
;; Returns #f if no blackboard is set.
(define (read-blackboard [bb (current-blackboard)])
  (if (not bb)
      #f
      (call-with-semaphore (blackboard-container-semaphore bb)
                           (lambda () (unbox (blackboard-container-box bb))))))

;; Atomically update the blackboard by applying a reducer function.
;; reducer: (blackboard-state -> blackboard-state)
;; Returns the new blackboard-state.
(define (update-blackboard! reducer [bb (current-blackboard)])
  (if (not bb)
      empty-blackboard
      (call-with-semaphore (blackboard-container-semaphore bb)
                           (lambda ()
                             (define old-state (unbox (blackboard-container-box bb)))
                             (define new-state (reducer old-state))
                             (set-box! (blackboard-container-box bb) new-state)
                             new-state))))

;; Atomically reset the blackboard to its empty state.
(define (reset-blackboard! [bb (current-blackboard)])
  (if (not bb)
      (void)
      (call-with-semaphore (blackboard-container-semaphore bb)
                           (lambda ()
                             (set-box! (blackboard-container-box bb) empty-blackboard)
                             (void)))))

;; Convenience: update the last-updated timestamp.
(define (touch-blackboard state)
  (struct-copy blackboard-state state [last-updated (now-seconds)]))

;; ============================================================
;; Provides
;; ============================================================

(provide blackboard-state
         blackboard-state?
         blackboard-state-active-plan
         blackboard-state-open-hypotheses
         blackboard-state-test-results
         blackboard-state-artifact-refs
         blackboard-state-wave-status
         blackboard-state-verifier-decisions
         blackboard-state-agent-activities
         blackboard-state-last-updated)

(provide empty-blackboard
         make-blackboard
         blackboard-container
         blackboard-container?
         blackboard-container-box
         blackboard-container-semaphore
         current-blackboard
         touch-blackboard)

(provide (contract-out
          [read-blackboard (->* () (blackboard-container?) (or/c blackboard-state? #f))]
          [update-blackboard!
           (->* ((-> blackboard-state? blackboard-state?)) (blackboard-container?) blackboard-state?)]
          [reset-blackboard! (->* () (blackboard-container?) void?)]))

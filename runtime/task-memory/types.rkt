#lang racket

;; runtime/task-memory/types.rkt — Task-ledger type contracts
;; STABILITY: internal
;;
;; Defines the canonical event and checkpoint contracts for the versioned
;; active-task ledger. All ledger events are append-only, versioned,
;; replayable, and branch-aware.
;;
;; Design decisions:
;; - structs with contracts validate at construction time
;; - enums are symbol-set predicates for simplicity
;; - ordering uses (session-seq, event-id), never timestamps
;; - idempotency key is (session-seq, event-id)

(require racket/contract
         racket/set
         (only-in racket/function const))

;; ============================================================
;; Stable enums
;; ============================================================

(define source-classes (set 'runtime-observed 'user-asserted 'model-asserted 'imported 'migrated))

(define event-kinds
  (set 'task-started
       'task-updated
       'objective-set
       'constraint-set
       'artifact-created
       'artifact-modified
       'test-executed
       'verification-passed
       'verification-failed
       'commit-created
       'push-completed
       'tool-invoked
       'checkpoint-created
       'phase-changed
       'wave-started
       'wave-completed
       'branch-forked
       'state-archived
       'memory-promoted
       'memory-expired
       'error-occurred
       'cancelled
       'unknown))

(define phases
  (set 'analysis 'planning 'implementation 'verification 'review 'hardening 'docs 'release 'none))

;; ============================================================
;; Enum predicates
;; ============================================================

(provide valid-source-class?
         valid-event-kind?
         valid-phase?
         event-kind->phase
         source-classes
         event-kinds
         phases)

(define (valid-source-class? s)
  (and (symbol? s) (set-member? source-classes s)))

(define (valid-event-kind? k)
  (and (symbol? k) (set-member? event-kinds k)))

(define (valid-phase? p)
  (and (symbol? p) (set-member? phases p)))

;; Map an event kind to its typical phase for checkpoint inference.
(define (event-kind->phase kind)
  (case kind
    [(task-started objective-set constraint-set) 'analysis]
    [(task-updated artifact-created artifact-modified tool-invoked test-executed) 'implementation]
    [(verification-passed verification-failed) 'verification]
    [(commit-created push-completed) 'hardening]
    [(state-archived memory-expired) 'docs]
    [(error-occurred) 'implementation]
    [else 'none]))

;; ============================================================
;; Ledger event struct
;; ============================================================

(struct task-ledger-event
        (schema-version session-seq
                        event-id
                        session-id
                        project-id
                        task-id
                        parent-task-id ; string or #f
                        branch-id
                        turn-id
                        request-id
                        assembly-id
                        correlation-id
                        causation-id ; string or #f
                        source-class ; one of source-classes
                        event-kind ; one of event-kinds
                        payload ; hash (safe, no raw content)
                        timestamp ; exact integer or #f
                        evidence-refs ; (listof string)
                        content-digest) ; keyed hash for integrity
  #:transparent
  #:constructor-name make-task-ledger-event
  #:guard (lambda (sv ss ev s-id p-id t-id pt b-id tu r a cr cs sc ek pl ts er cd . _)
            (unless (and (exact-positive-integer? sv)
                         (exact-positive-integer? ss)
                         (and (string? ev) (positive? (string-length ev)))
                         (string? s-id)
                         (string? p-id)
                         (string? t-id)
                         (or (not pt) (string? pt))
                         (string? b-id)
                         (string? tu)
                         (string? r)
                         (string? a)
                         (string? cr)
                         (or (not cs) (string? cs))
                         (valid-source-class? sc)
                         (valid-event-kind? ek)
                         (hash? pl)
                         (or (not ts) (exact-integer? ts))
                         (andmap string? er)
                         (and (string? cd) (positive? (string-length cd))))
              (error "task-ledger-event: invalid field values"
                     sv
                     ss
                     ev
                     s-id
                     p-id
                     t-id
                     pt
                     b-id
                     tu
                     r
                     a
                     cr
                     cs
                     sc
                     ek
                     pl
                     ts
                     er
                     cd))
            (values sv ss ev s-id p-id t-id pt b-id tu r a cr cs sc ek pl ts er cd)))

(provide task-ledger-event?
         make-task-ledger-event
         task-ledger-event-schema-version
         task-ledger-event-session-seq
         task-ledger-event-event-id
         task-ledger-event-session-id
         task-ledger-event-project-id
         task-ledger-event-task-id
         task-ledger-event-parent-task-id
         task-ledger-event-branch-id
         task-ledger-event-turn-id
         task-ledger-event-request-id
         task-ledger-event-assembly-id
         task-ledger-event-correlation-id
         task-ledger-event-causation-id
         task-ledger-event-source-class
         task-ledger-event-event-kind
         task-ledger-event-payload
         task-ledger-event-timestamp
         task-ledger-event-evidence-refs
         task-ledger-event-content-digest)

;; ============================================================
;; Checkpoint struct — immutable reduced state
;; ============================================================

(provide task-checkpoint?
         make-task-checkpoint
         task-checkpoint-session-id
         task-checkpoint-task-id
         task-checkpoint-branch-id
         task-checkpoint-last-event-id
         task-checkpoint-seq-count
         task-checkpoint-payload
         task-checkpoint-event-count
         task-checkpoint-content-digest
         task-checkpoint-timestamp)

(struct task-checkpoint
        (session-id task-id
                    branch-id
                    last-event-id
                    seq-count ; last processed session-seq (0 for empty)
                    payload ; hash with reduced state
                    event-count ; total events applied
                    content-digest ; integrity hash
                    timestamp) ; exact integer or #f
  #:transparent
  #:constructor-name make-task-checkpoint
  #:guard (lambda (s-id t-id b-id le sc pl ec cd ts . _)
            (unless (and (string? s-id)
                         (string? t-id)
                         (string? b-id)
                         (string? le)
                         (exact-nonnegative-integer? sc)
                         (hash? pl)
                         (exact-nonnegative-integer? ec)
                         (and (string? cd) (positive? (string-length cd)))
                         (or (not ts) (exact-integer? ts)))
              (error "task-checkpoint: invalid field values" s-id t-id b-id le sc pl ec cd ts))
            (values s-id t-id b-id le sc pl ec cd ts)))

;; ============================================================
;; Ordering
;; ============================================================

(provide ledger-event-before?
         events-equal?)

;; Deterministic ordering by (session-seq, event-id).
;; Timestamps are never used for ordering.
(define (ledger-event-before? a b)
  (define sa (task-ledger-event-session-seq a))
  (define sb (task-ledger-event-session-seq b))
  (cond
    [(< sa sb) #t]
    [(> sa sb) #f]
    [else (string<? (task-ledger-event-event-id a) (task-ledger-event-event-id b))]))

;; Idempotency: two events are equal if same session-seq and event-id.
(define (events-equal? a b)
  (and (= (task-ledger-event-session-seq a) (task-ledger-event-session-seq b))
       (equal? (task-ledger-event-event-id a) (task-ledger-event-event-id b))))

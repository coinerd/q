#lang racket/base

;; @speed fast  ;; @suite default
;; @boundary unit
;; tests/test-task-ledger-types.rkt — Task-ledger type contracts
;; STABILITY: internal
;;
;; Tests ledger event contracts, checkpoint state contracts, stable enums,
;; deterministic ordering, idempotency semantics, and clock independence.

(require rackunit
         racket/match
         racket/list
         racket/hash
         racket/string
         "../runtime/task-memory/types.rkt")

;; ============================================================
;; Fixture helpers
;; ============================================================

(define (make-sample-event meta)
  (make-task-ledger-event (hash-ref meta 'schema-version 1)
                          (hash-ref meta 'session-seq 1)
                          (hash-ref meta 'event-id "evt-1")
                          (hash-ref meta 'session-id "ses-1")
                          (hash-ref meta 'project-id "proj-1")
                          (hash-ref meta 'task-id "task-1")
                          (hash-ref meta 'parent-task-id #f)
                          (hash-ref meta 'branch-id "branch-1")
                          (hash-ref meta 'turn-id "turn-1")
                          (hash-ref meta 'request-id "req-1")
                          (hash-ref meta 'assembly-id "asm-1")
                          (hash-ref meta 'correlation-id "corr-1")
                          (hash-ref meta 'causation-id #f)
                          (hash-ref meta 'source-class 'runtime-observed)
                          (hash-ref meta 'event-kind 'task-started)
                          (hash-ref meta 'payload #hasheq((description . "Start task")))
                          (hash-ref meta 'timestamp 1234567890)
                          (hash-ref meta 'evidence-refs '())
                          (hash-ref meta 'content-digest "abc123")))

(define (make-sample-checkpoint meta)
  (make-task-checkpoint (hash-ref meta 'session-id "ses-1")
                        (hash-ref meta 'task-id "task-1")
                        (hash-ref meta 'branch-id "branch-1")
                        (hash-ref meta 'last-event-id "evt-42")
                        (hash-ref meta 'seq-count 42)
                        (hash-ref meta 'payload #hasheq((status . "running")))
                        (hash-ref meta 'event-count 10)
                        (hash-ref meta 'content-digest "def456")
                        (hash-ref meta 'timestamp 1234567899)))

;; ============================================================
;; 1. Ledger event struct contract
;; ============================================================

(test-case "make-task-ledger-event creates valid event"
  (define e (make-sample-event #hasheq()))
  (check-true (task-ledger-event? e))
  (check-equal? (task-ledger-event-schema-version e) 1)
  (check-equal? (task-ledger-event-session-seq e) 1)
  (check-equal? (task-ledger-event-event-id e) "evt-1")
  (check-true (string? (task-ledger-event-session-id e)))
  (check-true (string? (task-ledger-event-project-id e)))
  (check-true (string? (task-ledger-event-task-id e)))
  (check-equal? (task-ledger-event-parent-task-id e) #f)
  (check-true (string? (task-ledger-event-branch-id e)))
  (check-true (string? (task-ledger-event-turn-id e)))
  (check-true (string? (task-ledger-event-request-id e)))
  (check-true (string? (task-ledger-event-assembly-id e)))
  (check-true (string? (task-ledger-event-correlation-id e)))
  (check-equal? (task-ledger-event-causation-id e) #f))

(test-case "ledger event source-class is required symbol"
  (define e (make-sample-event #hasheq()))
  (check-not-false (memq (task-ledger-event-source-class e)
                         '(runtime-observed user-asserted model-asserted imported migrated)))
  (check-eq? (task-ledger-event-source-class e) 'runtime-observed))

(test-case "ledger event event-kind is required symbol"
  (define e (make-sample-event #hasheq()))
  (check-eq? (task-ledger-event-event-kind e) 'task-started))

(test-case "ledger event carries safe payload"
  (define e (make-sample-event #hasheq()))
  (check-true (hash? (task-ledger-event-payload e)))
  (check-equal? (hash-ref (task-ledger-event-payload e) 'description) "Start task"))

(test-case "ledger event cannot have #f session-seq or event-id"
  (define e (make-sample-event #hasheq()))
  (check-false (zero? (task-ledger-event-session-seq e)))
  (check-false (string=? "" (task-ledger-event-event-id e))))

;; ============================================================
;; 2. Source-class enum
;; ============================================================

(test-case "valid-source-class? accepts all known classes"
  (check-true (valid-source-class? 'runtime-observed))
  (check-true (valid-source-class? 'user-asserted))
  (check-true (valid-source-class? 'model-asserted))
  (check-true (valid-source-class? 'imported))
  (check-true (valid-source-class? 'migrated))
  (check-false (valid-source-class? 'bogus))
  (check-false (valid-source-class? "runtime-observed")))

;; ============================================================
;; 3. Event-kind enum
;; ============================================================

(test-case "valid-event-kind? accepts all known kinds"
  (for ([k '(task-started task-updated
                          objective-set
                          constraint-set
                          artifact-created
                          artifact-modified
                          test-executed
                          verification-passed
                          verification-failed
                          commit-created
                          push-completed
                          tool-invoked
                          checkpoint-created
                          phase-changed
                          wave-started
                          wave-completed
                          branch-forked
                          state-archived
                          memory-promoted
                          memory-expired
                          error-occurred
                          cancelled
                          unknown)])
    (check-true (valid-event-kind? k) (format "kind ~a should be valid" k)))
  (check-false (valid-event-kind? 'nonsense))
  (check-false (valid-event-kind? 42)))

;; ============================================================
;; 4. Phase enum
;; ============================================================

(test-case "valid-phase? accepts all known phases"
  (for ([p '(analysis planning implementation verification review hardening docs release none)])
    (check-true (valid-phase? p) (format "phase ~a should be valid" p)))
  (check-false (valid-phase? 'flying))
  (check-false (valid-phase? "")))

;; ============================================================
;; 5. Checkpoint struct
;; ============================================================

(test-case "make-task-checkpoint creates valid checkpoint"
  (define cp (make-sample-checkpoint #hasheq()))
  (check-true (task-checkpoint? cp))
  (check-equal? (task-checkpoint-session-id cp) "ses-1")
  (check-equal? (task-checkpoint-task-id cp) "task-1")
  (check-equal? (task-checkpoint-branch-id cp) "branch-1")
  (check-equal? (task-checkpoint-last-event-id cp) "evt-42")
  (check-equal? (task-checkpoint-seq-count cp) 42)
  (check-equal? (task-checkpoint-event-count cp) 10))

(test-case "checkpoint payload is hash"
  (define cp (make-sample-checkpoint #hasheq()))
  (check-true (hash? (task-checkpoint-payload cp))))

(test-case "checkpoint timestamp is integer or #f"
  (define cp1 (make-sample-checkpoint #hasheq()))
  (check-true (exact-integer? (task-checkpoint-timestamp cp1)))
  (define cp2 (make-sample-checkpoint #hasheq((timestamp . #f))))
  (check-equal? (task-checkpoint-timestamp cp2) #f))

;; ============================================================
;; 6. Event kind -> phase mapping
;; ============================================================

(test-case "event-kind->phase maps correctly"
  (check-eq? (event-kind->phase 'task-started) 'analysis)
  (check-eq? (event-kind->phase 'task-updated) 'implementation)
  (check-eq? (event-kind->phase 'objective-set) 'analysis)
  (check-eq? (event-kind->phase 'constraint-set) 'analysis)
  (check-eq? (event-kind->phase 'artifact-created) 'implementation)
  (check-eq? (event-kind->phase 'verification-passed) 'verification)
  (check-eq? (event-kind->phase 'verification-failed) 'verification)
  (check-eq? (event-kind->phase 'commit-created) 'hardening)
  (check-eq? (event-kind->phase 'phase-changed) 'none)
  (check-eq? (event-kind->phase 'error-occurred) 'implementation)
  (check-eq? (event-kind->phase 'cancelled) 'none)
  (check-eq? (event-kind->phase 'unknown) 'none))

;; ============================================================
;; 7. Sequence ordering
;; ============================================================

(test-case "ledger-event-before? orders by session-seq"
  (define e1 (make-sample-event #hasheq((session-seq . 1) (event-id . "a"))))
  (define e2 (make-sample-event #hasheq((session-seq . 2) (event-id . "b"))))
  (check-true (ledger-event-before? e1 e2))
  (check-false (ledger-event-before? e2 e1)))

(test-case "ledger-event-before? same-seq falls back to event-id"
  (define e1 (make-sample-event #hasheq((session-seq . 5) (event-id . "aaa"))))
  (define e2 (make-sample-event #hasheq((session-seq . 5) (event-id . "zzz"))))
  (check-true (ledger-event-before? e1 e2))
  (check-false (ledger-event-before? e2 e1)))

(test-case "ledger-event-before? equal events are not before"
  (define e (make-sample-event #hasheq((session-seq . 3) (event-id . "x"))))
  (check-false (ledger-event-before? e e)))

;; ============================================================
;; 8. Idempotency
;; ============================================================

(test-case "events-equal? uses session-seq and event-id"
  (define e1 (make-sample-event #hasheq((session-seq . 10) (event-id . "evt-10"))))
  (define e2 (make-sample-event #hasheq((session-seq . 10) (event-id . "evt-10"))))
  (check-true (events-equal? e1 e2)))

(test-case "events-equal? different seq is not equal"
  (define e1 (make-sample-event #hasheq((session-seq . 1) (event-id . "evt-x"))))
  (define e2 (make-sample-event #hasheq((session-seq . 2) (event-id . "evt-x"))))
  (check-false (events-equal? e1 e2)))

(test-case "events-equal? different id is not equal"
  (define e1 (make-sample-event #hasheq((session-seq . 1) (event-id . "evt-a"))))
  (define e2 (make-sample-event #hasheq((session-seq . 1) (event-id . "evt-b"))))
  (check-false (events-equal? e1 e2)))

;; ============================================================
;; 9. Timestamps are not used for ordering
;; ============================================================

(test-case "ordering ignores timestamp"
  (define e1 (make-sample-event #hasheq((session-seq . 1) (event-id . "a") (timestamp . 9999999))))
  (define e2 (make-sample-event #hasheq((session-seq . 2) (event-id . "b") (timestamp . 1111111))))
  ;; e1 has larger timestamp but lower seq, so it comes first
  (check-true (ledger-event-before? e1 e2)))

;; ============================================================
;; 10. Edge cases: optional fields
;; ============================================================

(test-case "parent-task-id may be #f"
  (define e (make-sample-event #hasheq((parent-task-id . #f))))
  (check-equal? (task-ledger-event-parent-task-id e) #f)
  (define e2 (make-sample-event #hasheq((parent-task-id . "parent-1"))))
  (check-equal? (task-ledger-event-parent-task-id e2) "parent-1"))

(test-case "causation-id may be #f"
  (define e (make-sample-event #hasheq((causation-id . #f))))
  (check-equal? (task-ledger-event-causation-id e) #f)
  (define e2 (make-sample-event #hasheq((causation-id . "cause-1"))))
  (check-equal? (task-ledger-event-causation-id e2) "cause-1"))

(test-case "empty evidence-refs is valid"
  (define e (make-sample-event #hasheq((evidence-refs . ()))))
  (check-equal? (task-ledger-event-evidence-refs e) '())
  (define e2 (make-sample-event #hasheq((evidence-refs . ("ref-1" "ref-2")))))
  (check-equal? (task-ledger-event-evidence-refs e2) '("ref-1" "ref-2")))

;; ============================================================
;; 11. Default values
;; ============================================================

(test-case "make-task-ledger-event with all defaults"
  (define e (make-sample-event #hasheq((schema-version . 1) (session-seq . 1) (event-id . "evt-1"))))
  (check-equal? (task-ledger-event-schema-version e) 1)
  (check-equal? (task-ledger-event-session-seq e) 1))

;; ============================================================
;; 12. Content digest for integrity
;; ============================================================

(test-case "content-digest is non-empty string"
  (define e (make-sample-event #hasheq()))
  (check-true (string? (task-ledger-event-content-digest e)))
  (check-false (string=? "" (task-ledger-event-content-digest e))))

(test-case "checkpoint content-digest is non-empty string"
  (define cp (make-sample-checkpoint #hasheq()))
  (check-true (string? (task-checkpoint-content-digest cp)))
  (check-false (string=? "" (task-checkpoint-content-digest cp))))

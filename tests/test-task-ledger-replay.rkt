#lang racket
;; @covers runtime/task-memory/replay.rkt

;; @speed fast  ;; @suite default
;; @boundary integration
;; tests/test-task-ledger-replay.rkt — Ledger event replay tests
;; STABILITY: internal

(require rackunit
         racket/hash
         "../runtime/task-memory/types.rkt"
         "../runtime/task-memory/codec.rkt"
         "../runtime/task-memory/replay.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-simple-event #:event-id [event-id "evt-0"]
                           #:session-seq [ss 1]
                           #:branch-id [branch "main"]
                           #:event-kind [kind 'task-started]
                           #:source-class [sc 'runtime-observed]
                           #:payload [pl #hasheq()]
                           #:content-digest [cd "abc123"]
                           #:timestamp [ts (current-milliseconds)]
                           #:causation-id [causation #f])
  (make-task-ledger-event 1
                          ss
                          event-id
                          "ses-1"
                          "proj-1"
                          "task-1"
                          #f
                          branch
                          "turn-1"
                          "req-1"
                          "asm-1"
                          "corr-1"
                          causation
                          sc
                          kind
                          pl
                          ts
                          '()
                          cd))

;; ============================================================
;; 1. Replay empty list produces zero-count checkpoint
;; ============================================================

(define (checkpoint-equal? c1 c2 . fields)
  (for-each (lambda (f) (check-equal? (f c1) (f c2)))
            (if (null? fields)
                (list task-checkpoint-session-id
                      task-checkpoint-task-id
                      task-checkpoint-branch-id
                      task-checkpoint-last-event-id
                      task-checkpoint-seq-count
                      task-checkpoint-event-count
                      task-checkpoint-payload
                      task-checkpoint-timestamp)
                fields)))

(test-case "replay: empty event list produces zero-event checkpoint"
  (define cp (replay-events '() "ses-1" "task-1" "main"))
  (check-true (task-checkpoint? cp))
  (check-equal? (task-checkpoint-session-id cp) "ses-1")
  (check-equal? (task-checkpoint-task-id cp) "task-1")
  (check-equal? (task-checkpoint-branch-id cp) "main")
  (check-equal? (task-checkpoint-event-count cp) 0)
  (check-equal? (task-checkpoint-seq-count cp) 0)
  (check-equal? (task-checkpoint-last-event-id cp) ""))

(test-case "replay: single event produces checkpoint with correct state"
  (define e (make-simple-event))
  (define cp (replay-events (list e) "ses-1" "task-1" "main"))
  (check-true (task-checkpoint? cp))
  (check-equal? (task-checkpoint-event-count cp) 1)
  (check-equal? (task-checkpoint-seq-count cp) 1)
  (check-equal? (task-checkpoint-last-event-id cp) "evt-0"))

(test-case "replay: three events sorted by session-seq then event-id"
  (define e1 (make-simple-event #:event-id "evt-0" #:session-seq 1))
  (define e2 (make-simple-event #:event-id "evt-1" #:session-seq 1))
  (define e3 (make-simple-event #:event-id "evt-2" #:session-seq 2))
  (define cp (replay-events (list e3 e1 e2) "ses-1" "task-1" "main"))
  (check-equal? (task-checkpoint-event-count cp) 3)
  (check-equal? (task-checkpoint-seq-count cp) 2)
  (check-equal? (task-checkpoint-last-event-id cp) "evt-2"))

(test-case "replay: branch filtering only applies branch events"
  (define e-main (make-simple-event #:event-id "evt-0" #:branch-id "main"))
  (define e-feat (make-simple-event #:event-id "evt-1" #:branch-id "feature" #:session-seq 2))
  (define e-main2 (make-simple-event #:event-id "evt-2" #:branch-id "main" #:session-seq 3))
  ;; replay all events on "main" branch
  (define cp (replay-events (list e-main e-feat e-main2) "ses-1" "task-1" "main"))
  (check-equal? (task-checkpoint-event-count cp) 2)
  (check-equal? (task-checkpoint-last-event-id cp) "evt-2"))
;; Note: branch-filtering is done by caller passing only branch events
;; or by using branch-aware walk.

(test-case "replay: duplicate events are idempotent (same session-seq + event-id)"
  (define e1 (make-simple-event #:event-id "evt-0" #:session-seq 1))
  (define cp (replay-events (list e1 e1 e1) "ses-1" "task-1" "main"))
  ;; Only one unique event, so event-count=1
  (check-equal? (task-checkpoint-event-count cp) 1)
  (check-equal? (task-checkpoint-seq-count cp) 1))

(test-case "replay: checkpoint payload accumulates event payload through merge"
  (define e1 (make-simple-event #:event-id "evt-0" #:payload #hasheq((step . 1))))
  (define e2 (make-simple-event #:event-id "evt-1" #:session-seq 2 #:payload #hasheq((step . 2))))
  (define cp (replay-events (list e1 e2) "ses-1" "task-1" "main"))
  (define pl (task-checkpoint-payload cp))
  (check-equal? (hash-ref pl 'step) 2)
  (check-equal? (task-checkpoint-event-count cp) 2))

(test-case "replay: with causality chain preserves event order"
  (define e1 (make-simple-event #:event-id "evt-0"))
  (define e2 (make-simple-event #:event-id "evt-1" #:session-seq 2 #:causation-id "evt-0"))
  (define e3 (make-simple-event #:event-id "evt-2" #:session-seq 3 #:causation-id "evt-1"))
  (define cp (replay-events (list e1 e2 e3) "ses-1" "task-1" "main"))
  (check-equal? (task-checkpoint-event-count cp) 3))

;; ============================================================
;; 2. Branch-aware ledger walk
;; ============================================================

(test-case "branch-walk: returns all events for exact branch match"
  (define e-main (make-simple-event #:event-id "evt-0"))
  (define cp (branch-walk-events (list e-main) "ses-1" "task-1" "main"))
  (check-equal? (task-checkpoint-event-count cp) 1))

(test-case "branch-walk: filters non-matching branch events"
  (define e-main (make-simple-event #:event-id "evt-0" #:branch-id "main"))
  (define e-other (make-simple-event #:event-id "evt-1" #:branch-id "other" #:session-seq 2))
  (define cp (branch-walk-events (list e-main e-other) "ses-1" "task-1" "main"))
  (check-equal? (task-checkpoint-event-count cp) 1)
  (check-equal? (task-checkpoint-last-event-id cp) "evt-0"))

(test-case "branch-walk: returns existing checkpoint when provided"
  (define e1 (make-simple-event #:event-id "evt-0"))
  (define cp1 (replay-events (list e1) "ses-1" "task-1" "main"))
  (define e2 (make-simple-event #:event-id "evt-1" #:session-seq 2))
  (define cp2 (branch-walk-events (list e2) "ses-1" "task-1" "main" #:from-checkpoint cp1))
  (check-equal? (task-checkpoint-event-count cp2) 2))

;; ============================================================
;; 3. Legacy store bridge
;; ============================================================

(test-case "legacy-store-read: reads and replays events from JSONL"
  (define jsonl
    (string-append "{\"schema-version\":1,\"session-seq\":1,\"event-id\":\"evt-0\","
                   "\"session-id\":\"ses-1\",\"project-id\":\"proj-1\",\"task-id\":\"task-1\","
                   "\"parent-task-id\":null,\"branch-id\":\"main\",\"turn-id\":\"tu1\","
                   "\"request-id\":\"r1\",\"assembly-id\":\"a1\",\"correlation-id\":\"c1\","
                   "\"causation-id\":null,\"source-class\":\"runtime-observed\","
                   "\"event-kind\":\"task-started\",\"payload\":{},\"timestamp\":1,"
                   "\"evidence-refs\":[],\"content-digest\":\"abc\"}\n"
                   "{\"schema-version\":1,\"session-seq\":2,\"event-id\":\"evt-1\","
                   "\"session-id\":\"ses-1\",\"project-id\":\"proj-1\",\"task-id\":\"task-1\","
                   "\"parent-task-id\":null,\"branch-id\":\"main\",\"turn-id\":\"tu1\","
                   "\"request-id\":\"r1\",\"assembly-id\":\"a1\",\"correlation-id\":\"c1\","
                   "\"causation-id\":null,\"source-class\":\"runtime-observed\","
                   "\"event-kind\":\"task-updated\",\"payload\":{},\"timestamp\":2,"
                   "\"evidence-refs\":[],\"content-digest\":\"def\"}\n"))
  (define cp (legacy-store-replay jsonl "ses-1" "task-1" "main"))
  (check-equal? (task-checkpoint-event-count cp) 2)
  (check-equal? (task-checkpoint-last-event-id cp) "evt-1"))

(test-case "legacy-store-replay: empty JSONL produces zero-event checkpoint"
  (define cp (legacy-store-replay "" "ses-1" "task-1" "main"))
  (check-equal? (task-checkpoint-event-count cp) 0))

(test-case "legacy-store-replay: malformed JSON line is skipped"
  (define jsonl
    (string-append "not-valid-json\n"
                   "{\"schema-version\":1,\"session-seq\":1,\"event-id\":\"evt-0\","
                   "\"session-id\":\"ses-1\",\"project-id\":\"proj-1\",\"task-id\":\"task-1\","
                   "\"parent-task-id\":null,\"branch-id\":\"main\",\"turn-id\":\"tu1\","
                   "\"request-id\":\"r1\",\"assembly-id\":\"a1\",\"correlation-id\":\"c1\","
                   "\"causation-id\":null,\"source-class\":\"runtime-observed\","
                   "\"event-kind\":\"task-started\",\"payload\":{},\"timestamp\":1,"
                   "\"evidence-refs\":[],\"content-digest\":\"abc\"}\n"))
  (define cp (legacy-store-replay jsonl "ses-1" "task-1" "main"))
  (check-equal? (task-checkpoint-event-count cp) 1))

;; ============================================================
;; 4. Replay checkpoint file round-trip
;; ============================================================

(test-case "replay-checkpoint-to-file: write and read back checkpoint"
  ;; Create test directory
  (define test-dir (make-temporary-directory))
  (define cp-file (build-path test-dir "checkpoint.json"))
  ;; Create checkpoint via replay
  (define e (make-simple-event))
  (define cp (replay-events (list e) "ses-1" "task-1" "main"))
  (check-true (write-checkpoint-to-file cp cp-file))
  ;; Read it back
  (define cp2 (read-checkpoint-from-file cp-file))
  (check-true (task-checkpoint? cp2))
  (check-equal? (task-checkpoint-session-id cp2) "ses-1")
  (check-equal? (task-checkpoint-task-id cp2) "task-1")
  (check-equal? (task-checkpoint-event-count cp2) 1)
  ;; Cleanup
  (delete-file cp-file)
  (delete-directory test-dir))

(test-case "replay-checkpoint-from-file: missing file raises error"
  (check-exn #rx"checkpoint file not found"
             (thunk (read-checkpoint-from-file "/nonexistent/checkpoint.json"))))

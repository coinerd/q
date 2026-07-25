#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: unit

;; tests/test-working-set-checkpoint-replay.rkt
;; W2 (#8939): Snapshot and restore working-set state across lifecycle boundaries.
;;
;; Before cancellation, compaction, shutdown, fork, or session switch,
;; the working set must be snapshotted. After resumption, it must be
;; restored by message/evidence ID, omitting unavailable references safely.

(require rackunit
         rackunit/text-ui
         "../runtime/working-set.rkt"
         "../runtime/task-memory/working-set-continuity.rkt")

(define checkpoint-replay-tests
  (test-suite "Working-set checkpoint snapshot and restore"

    ;; ── T01: snapshot captures entries ──
    (test-case "snapshot captures all working-set entries"
      (define ws (make-working-set))
      (working-set-add! ws "/tmp/a.rkt" "msg-1" 100)
      (working-set-add! ws "/tmp/b.rkt" "msg-2" 200)
      (define snap (working-set-snapshot ws))
      (check-equal? (length snap) 2)
      (check-equal? (ws-snapshot-entry-path (car snap)) "/tmp/b.rkt")
      (check-equal? (ws-snapshot-entry-message-id (car snap)) "msg-2"))

    ;; ── T02: snapshot of empty working set ──
    (test-case "snapshot of empty working set is empty"
      (define ws (make-working-set))
      (define snap (working-set-snapshot ws))
      (check-equal? (length snap) 0))

    ;; ── T03: restore into empty working set ──
    (test-case "restore populates an empty working set from snapshot"
      (define ws-src (make-working-set))
      (working-set-add! ws-src "/tmp/a.rkt" "msg-1" 100)
      (working-set-add! ws-src "/tmp/b.rkt" "msg-2" 200)
      (define snap (working-set-snapshot ws-src))
      (define ws-dst (make-working-set))
      (restore-from-snapshot! ws-dst snap)
      (check-equal? (working-set-entry-count ws-dst) 2)
      (check-equal? (working-set-token-count ws-dst) 300))

    ;; ── T04: restore filters unavailable message IDs ──
    (test-case "restore omits entries whose message ID is unavailable"
      (define ws-src (make-working-set))
      (working-set-add! ws-src "/tmp/a.rkt" "msg-1" 100)
      (working-set-add! ws-src "/tmp/b.rkt" "msg-2" 200)
      (working-set-add! ws-src "/tmp/c.rkt" "msg-3" 300)
      (define snap (working-set-snapshot ws-src))
      ;; Only msg-1 and msg-3 are available after compaction
      (define available-msg-ids (set "msg-1" "msg-3"))
      (define ws-dst (make-working-set))
      (restore-from-snapshot! ws-dst snap #:available-message-ids available-msg-ids)
      (check-equal? (working-set-entry-count ws-dst) 2)
      (check-false (member "/tmp/b.rkt" (map ws-entry-path (working-set-entries ws-dst)))))

    ;; ── T05: round-trip preserves entry metadata ──
    (test-case "snapshot+restore round-trips path, message-id, token-estimate"
      (define ws-src (make-working-set))
      (working-set-add! ws-src "/tmp/x.rkt" "msg-x" 42)
      (define snap (working-set-snapshot ws-src))
      (define ws-dst (make-working-set))
      (restore-from-snapshot! ws-dst snap)
      (define entries (working-set-entries ws-dst))
      (check-equal? (length entries) 1)
      (define e (car entries))
      (check-equal? (ws-entry-path e) "/tmp/x.rkt")
      (check-equal? (ws-entry-message-id e) "msg-x")
      (check-equal? (ws-entry-token-estimate e) 42))

    ;; ── T06: restore merges into existing working set ──
    (test-case "restore adds to existing entries without duplicates"
      (define ws-src (make-working-set))
      (working-set-add! ws-src "/tmp/a.rkt" "msg-1" 100)
      (define snap (working-set-snapshot ws-src))
      (define ws-dst (make-working-set))
      (working-set-add! ws-dst "/tmp/b.rkt" "msg-2" 200)
      (restore-from-snapshot! ws-dst snap)
      ;; Both entries present
      (check-equal? (working-set-entry-count ws-dst) 2))

    ;; ── T07: snapshot is serializable (list of hashes) ──
    (test-case "snapshot entries are hash tables for JSON serialization"
      (define ws (make-working-set))
      (working-set-add! ws "/tmp/a.rkt" "msg-1" 100)
      (define snap (working-set-snapshot ws))
      (check-true (and (pair? snap) (hash? (car snap)))))

    ;; ── T08: restore from raw hash list (deserialized) ──
    (test-case "restore works from raw hash list (post-JSON-deserialization)"
      (define raw-snap
        (list (hash 'path "/tmp/a.rkt" 'message-id "msg-1" 'token-estimate 100 'timestamp 12345)))
      (define ws-dst (make-working-set))
      (restore-from-snapshot! ws-dst raw-snap)
      (check-equal? (working-set-entry-count ws-dst) 1)
      (check-equal? (ws-entry-path (car (working-set-entries ws-dst))) "/tmp/a.rkt"))

    ;; ── T09: snapshot survives token budget ──
    (test-case "restore respects max-tokens budget"
      (define ws-src (make-working-set #:max-tokens 10000))
      (working-set-add! ws-src "/tmp/a.rkt" "msg-1" 100)
      (working-set-add! ws-src "/tmp/b.rkt" "msg-2" 200)
      (define snap (working-set-snapshot ws-src))
      (define ws-dst (make-working-set #:max-tokens 150))
      (restore-from-snapshot! ws-dst snap)
      ;; Only one entry fits in 150 tokens
      (check-true (<= (working-set-token-count ws-dst) 150))
      (check-equal? (working-set-entry-count ws-dst) 1))

    ;; ── T10: empty snapshot restore is no-op ──
    (test-case "restore from empty snapshot is a no-op"
      (define ws (make-working-set))
      (working-set-add! ws "/tmp/a.rkt" "msg-1" 100)
      (restore-from-snapshot! ws '())
      (check-equal? (working-set-entry-count ws) 1))))

(run-tests checkpoint-replay-tests)
